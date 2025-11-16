loans <- read.csv("C:/Users/dzmitry/Desktop/dyplom/main_loan_base.csv")
balance <- read.csv("C:/Users/dzmitry/Desktop/dyplom/monthly_balance_base.csv")
repayment <- read.csv("C:/Users/dzmitry/Desktop/dyplom/repayment_base.csv")
library(dplyr)

avg_bal <- balance %>%
  group_by(loan_acc_num) %>%
  summarize(avg_balance_amt = median(balance_amount, na.rm = TRUE)) %>%
  ungroup()

# 1. Suma spłat dla każdego loan_acc_num
sum_repayment <- repayment %>%
  group_by(loan_acc_num) %>%
  summarize(total_repayment = sum(repayment_amount, na.rm = TRUE)) %>%
  ungroup()

# 2. Połączenie loans z sum_repayment (inner join)
df <- loans %>%
  inner_join(sum_repayment, by = "loan_acc_num")

# 3. Połączenie z avg_bal (też inner join)
df <- df %>%
  inner_join(avg_bal, by = "loan_acc_num")

# 4. Filtrowanie: tylko obserwacje z default_date ≤ '2024-12-31'
df <- df %>%
  filter(default_date <= as.Date("2024-12-31"))
library(stringr)
find_city <- function(addresses) {
  sapply(addresses, function(x) {
    parts <- str_split(x, ",|\\s+")[[1]]
    # Szukamy tej części, która nie zawiera cyfr
    parts_clean <- str_replace_all(parts, "[^a-zA-Z]", " ")
    parts_clean <- str_squish(parts_clean)
    city <- parts_clean[which.max(nchar(parts_clean))]  # najdłuższe słowo bez cyfr
    str_to_title(city)
  })
}

# Dodajemy kolumnę "city"
df$city <- find_city(df$customer_address)
df <- df %>%
  mutate(
    rr = round(((collateral_value + total_repayment) / loan_amount) * 100, 2),
    RR = rr / 100  # wersja w ułamku (0-1)
  )

# Podstawowe statystyki kolumny rr
summary(df$RR)
rr_per_city <- df %>%
  group_by(city) %>%
  summarize(avg_rr_per_city = mean(RR, na.rm = TRUE)) %>%
  ungroup()

# Merge z oryginalnym df
df <- df %>%
  left_join(rr_per_city, by = "city")

library(dplyr)
library(ggplot2)

# --- 1. Przygotowanie danych ---
features <- c(
  'loan_amount', 'collateral_value', 'avg_balance_amt', 'interest',
  'vintage_in_months', 'number_of_loans', 'missed_repayments',
  'avg_rr_per_city', 'tenure_years', 'cheque_bounces'
)

# Konwersja daty, oczyszczenie RR
glm_data <- df %>%
  mutate(
    disbursal_date = as.Date(default_date),
    RR = ifelse(RR <= 0, 0, ifelse(RR >= 1, 1, RR))
  ) %>%
  select(default_date, all_of(features), RR)

# --- 2. Podział na zbiór treningowy i testowy ---
set.seed(42)
train_indices <- sample(1:nrow(glm_data), size = 0.8 * nrow(glm_data))
train_data <- glm_data[train_indices, ]
test_data <- glm_data[-train_indices, ]

# --- 3. Model quasibinomial ---
glm_model <- glm(RR ~ ., data = train_data %>% select(-default_date), family = quasibinomial())

# --- 4. Predykcja i przygotowanie daty miesiąca ---
test_data <- test_data %>%
  mutate(
    predicted_RR = predict(glm_model, newdata = ., type = "response"),
    default_date = as.Date(default_date),
    default_year = format(default_date, "%Y")
  )

# --- 5. Agregacja po miesiącach ---
agg_data <- test_data %>%
  group_by(default_year) %>%
  summarize(
    actual = mean(RR, na.rm = TRUE),
    predicted = mean(predicted_RR, na.rm = TRUE)
  ) %>%
  mutate(default_year = as.Date(paste0(default_year, "-01-01")))%>%
  arrange(default_year)

ggplot(agg_data, aes(x = default_year)) +
  geom_line(aes(y = actual, color = "Rzeczywista stopa odzysku"), linewidth = 1) +
  geom_line(aes(y = predicted, group=1 , color = "Przewidziana stopa odzysku"), 
            linewidth = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(
    title = " ",
    x = "Rok",
    y = "Stopa odzysku ",
    color = "Legenda"
  ) +
  theme_minimal()
mae <- mean(abs(test_data$RR - test_data$predicted_RR))
rmse <- sqrt(mean((test_data$RR - test_data$predicted_RR)^2))
rss <- sum((test_data$RR - test_data$predicted_RR)^2)
tss <- sum((test_data$RR - mean(test_data$RR))^2)
r_squared <- 1 - rss/tss
r_squared

glm_model

options(scipen = 999)
summary(glm_model)



# Zmienna zależna
y <- df$RR

# Przekształcenie wartości RR do przedziału [0, 1]
y <- ifelse(y <= 0, 0, ifelse(y >= 1, 1, y))

# Lista zmiennych z ekranu (redukowany model)
features_small <- c(
  'avg_balance_amt',
  'vintage_in_months',
  'number_of_loans',
  'missed_repayments',
  'cheque_bounces'
)

# Dane niezależne
X <- df[, features_small]

# Połączenie danych do modelu
glm_data_small <- cbind(y = y, X)

# Podział na zbiór treningowy i testowy
set.seed(42)
train_indices <- sample(1:nrow(glm_data_small), size = 0.8 * nrow(glm_data_small))
train_data_small <- glm_data_small[train_indices, ]
test_data_small <- glm_data_small[-train_indices, ]

# Dopasowanie modelu GLM z quasibinomial
glm_model_small <- glm(y ~ ., data = train_data_small, family = quasibinomial())

# Wyświetlenie wyników
summary(glm_model_small)

# Predykcje na zbiorze testowym
test_data_small$predicted_RR <- predict(glm_model_small, newdata = test_data_small, type = "response")

# MAE
mae <- mean(abs(test_data_small$y - test_data_small$predicted_RR))

# RMSE
rmse <- sqrt(mean((test_data_small$y - test_data_small$predicted_RR)^2))

# R-squared
rss <- sum((test_data_small$y - test_data_small$predicted_RR)^2)
tss <- sum((test_data_small$y - mean(test_data_small$y))^2)
r_squared <- 1 - rss / tss

# Wyniki
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
cat("R²:", r_squared, "\n")

mean(test_data$RR, na.rm = TRUE)
mean(test_data_small$y, na.rm = TRUE)
mean(test_data$predicted_RR, na.rm = TRUE)
mean(test_data_small$predicted_RR, na.rm = TRUE)

# Średnia zwykła (rzeczywiste RR)
mean_rr <- mean(test_data$RR, na.rm = TRUE)

# Średnia ważona (rzeczywiste RR z wagą loan_amount)
weighted_rr <- weighted.mean(test_data$RR, test_data$loan_amount, na.rm = TRUE)

# Średnia zwykła (przewidziane RR)
mean_pred <- mean(test_data$predicted_RR, na.rm = TRUE)

# Średnia ważona (przewidziane RR)
weighted_pred <- weighted.mean(test_data$predicted_RR, test_data$loan_amount, na.rm = TRUE)

# Wyniki
cat("🔹 Średni rzeczywisty RR:", round(mean_rr, 4), "\n")
cat("🔹 Średni przewidziany RR:", round(mean_pred, 4), "\n")
cat("🔹 Średni ważony rzeczywisty RR:", round(weighted_rr, 4), "\n")
cat("🔹 Średni ważony przewidziany RR:", round(weighted_pred, 4), "\n")

test_data_small$loan_amount <- df$loan_amount[as.numeric(rownames(test_data_small))]



# Średnia zwykła (rzeczywisty RR)
mean_rr_small <- mean(test_data_small$y, na.rm = TRUE)

# Średnia ważona (rzeczywisty RR z wagą loan_amount)
weighted_rr_small <- weighted.mean(test_data_small$y, test_data_small$loan_amount, na.rm = TRUE)

# Średnia zwykła (przewidziany RR)
mean_pred_small <- mean(test_data_small$predicted_RR, na.rm = TRUE)

# Średnia ważona (przewidziany RR)
weighted_pred_small <- weighted.mean(test_data_small$predicted_RR, test_data_small$loan_amount, na.rm = TRUE)

# Wyniki
cat("🔹 Średni rzeczywisty RR (mały model):", round(mean_rr_small, 4), "\n")
cat("🔹 Średni przewidziany RR (mały model):", round(mean_pred_small, 4), "\n")
cat("🔹 Średni ważony rzeczywisty RR (mały model):", round(weighted_rr_small, 4), "\n")
cat("🔹 Średni ważony przewidziany RR (mały model):", round(weighted_pred_small, 4), "\n")


