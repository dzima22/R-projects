
#czyszczenie środowiska

rm(list = ls())

# paths
#################################################
set.seed(123)
base_path <- getwd()
base_path <- setwd("C:\\Users\\Administrator\\Desktop\\semestr letni 2024\\Modelowanie ryzyka kredytowego\\pd1")
input_path <- paste0(base_path, "/input")
output_path <- paste0(base_path, "/output")

# libnames
#################################################

install.packages('generator')
library(generator)
install.packages('dplyr')
library(dplyr)
install.packages('moments')
library(moments)
install.packages('tictoc')
library(tictoc)
install.packages('ggplot2')
library(ggplot2)
install.packages('ROCR')
library(ROCR)
install.packages('MASS')
library(MASS)
install.packages('AUC')
library('AUC')
install.packages('pROC')
library(pROC)
install.packages('caret')
library(caret)

# first (client) database
################################################

# number of clients
n <- 5000

# generating client characteristics

creditcardnr <- r_credit_card_numbers(n)
agreement_start <- (r_date_of_births(n, start = as.Date("1990-01-01"), end = as.Date("2004-01-01")))
name <- r_full_names(n)
ID <- r_national_identification_numbers(n)
default <- rbinom(n, 1, 0.2)
time_of_default <- (r_date_of_births(n, start = as.Date("2004-01-01"), end = Sys.Date()))
time_of_default <- as.data.frame(time_of_default)

# binding all client characteristics
client_database <- as.data.frame(cbind(name, ID, agreement_start, creditcardnr, default, time_of_default))

# default date only in case of default
date_of_default <- ifelse(client_database$default == 0, NA, as.character(client_database$time_of_default))
head(date_of_default)
client_database <- cbind(dplyr::select(client_database, - time_of_default), date_of_default)

head(client_database)


# second (credits) database
################################################

# first client's credit

gross_value_mortgage <- round(100000 * rgamma(n, 5, rate = 1), 0)

have_mortgage <- rbinom(n, 1, 0.3)

value_mortgage <- have_mortgage * gross_value_mortgage
value_mortgage <- cbind(dplyr::select(client_database, ID), value_mortgage)


# second client's credit

value_nonmortgage <- round(10000 * rgamma(n, 2, rate = 2), 0)
value_nonmortgage <- cbind(dplyr::select(client_database, ID), value_nonmortgage)

loans <- right_join(value_mortgage, value_nonmortgage, by = 'ID')
# ifelse(loans$value_mortgage == 0, 'NA', loans$value_mortgage)
head(loans)

# 1. Krok pierwszy, generujemy pliki loans oraz clients. Export do csv.

write.csv2(loans, file = paste0(output_path, "/loans.csv"))
write.csv2(client_database, file = paste0(output_path, "/clients.csv"))

# 2. Łączymy dane z plików client_database i loans za pomocą ID, wykorzystując left join.

dane <- left_join(client_database, loans, by="ID")
head(dane)
write.csv2(dane, file = paste0(output_path, "/dane.csv"))

# Sprawdzamy czy wszyscy klienci zostali zmapowani.

check <-anti_join(client_database, loans, by="ID")
print(check)

# Wszysycy klienci zostali zmapowani, ponieważ przy wykorzystaniu funkcji anti_join otrzymujemy 0 wierszy.

# 3. Tworzymy nowe zmienne objaśniające do modelu PD.

# agreement_duration_days - liczba dni jaka upłynęla od daty zawarcia umowy do dziś.
difference_days <- as.numeric(difftime(Sys.Date(), dane$agreement_start, units = "days"))
difference_days
dane <- cbind(dane, difference_days)

# sum_of_loans - łączna wartość zadłużenia, suma kredytów hipotecznego oraz niehipotecznego.
dane$value_mortgage <- as.numeric(dane$value_mortgage)
dane$value_nonmortgage <- as.numeric(dane$value_nonmortgage)
dane <- dane %>%
  mutate(sum_of_loans = value_mortgage + value_nonmortgage)

# number_of_loans - liczba produktów kredytowych.
summary(value_nonmortgage)
summary(value_mortgage)
dane <- mutate(dane, two_loans = if_else(value_mortgage == 0, 0, 1))
# Kredyt non_mortgage występuje zawsze, wobec czego liczbę kredytów sprawdzam na kolumnie value_mortgage.

# industry_identifier - pierwsza cyfra na karcie kredytowej to identyfikator branżowy (https://www.bankrate.com/finance/credit-cards/what-do-the-numbers-on-your-credit-card-mean/#read)
dane <- dane %>%
  mutate(industry_identifier = as.numeric(substr(creditcardnr, 1, 1)))

# Kodowanie zmiennej identyfikatora branżowego.
dane <- dane %>%
  mutate(
    industry_Travel_and_Entertainment = ifelse(industry_identifier == 3, 1, 0),
    industry_Airlines_Financial = ifelse(industry_identifier %in% c(1, 2), 1, 0),
    industry_Banking_Finance = ifelse(industry_identifier %in% c(4, 5, 6),1,0),
    industry_Others = ifelse(industry_identifier %in% c(6, 7, 8, 9), 1, 0)
  )

# Utworzone zmienne na podstawie, których będzie opierać się dalsza analiza to:
 # agreement_duration_days, sum_of_loans, number_of_loans,industry_Travel_and_Entertainment, industry_Airlines_Financial, industry_Banking_and_Finance,industry_Others.

# Krok 4. Sprawdzenie czy zmienne objaśniające mają dużo obserwacji nietypowych.

# Sprawdzamy, dla których zmiennych występują obserwacje odstające.
sum_of_loans_outlier <- boxplot.stats(dane$sum_of_loans)$out
print(paste("Liczba obserwacji nietypowych dla zmiennej sum_of_loans",length(sum_of_loans_outlier)))

difference_days_outlier <- boxplot.stats(dane$difference_days)$out
print(paste("Liczba obserwacji nietypowych dla zmiennej difference_days",length(difference_days_outlier)))

two_loans_outlier <- boxplot.stats(dane$two_loans)$out
print(paste("Liczba obserwacji nietypowych dla zmiennej two_loans",length(two_loans_outlier)))


# Obserwacje odstające wykryto tylko w zmiennej sum_of_loans, w związku z czym poddano ją winsoryzacji.
sum_of_loans_raw <- dane$sum_of_loans
summary(sum_of_loans_raw)
kurtosis(sum_of_loans_raw)
# Wartość kurtozy > 3 co sugeruje, że wystąpić mogą tzw. "grube ogony".

density_sum_of_loans_raw <-
  tibble(sum_of_loans_raw) %>%
  ggplot(aes(x=sum_of_loans_raw)) +
  stat_density(geom = "line", alpha = 1, colour = "cornflowerblue")

density_sum_of_loans_raw


winsoryzacja <- function (x, fraction=.05)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}

# Wartości powyżej kwantyla 5 w jednym ogonie oraz poniżej kwantyla 5 w drugim ogonie zostają zastąpione wartością z momentu odcięcia.
sum_of_loans_winsor <- winsoryzacja(sum_of_loans_raw)
plot(sum_of_loans_raw, sum_of_loans_winsor)

density_sum_of_loans_winsor <-
  tibble(sum_of_loans_winsor) %>%
  ggplot(aes(x=sum_of_loans_winsor)) +
  stat_density(geom = "line", alpha = 1, colour = "cornflowerblue")

density_sum_of_loans_winsor +
  scale_x_continuous(labels = scales::number_format(scale = 1))

dane <- cbind(dane, sum_of_loans_winsor) # Dodajemy zmienną po winsoryzacji do zbioru.
summary(dane)

# Krok 5. Oszacowanie prawdopodobieństwa defaultu za pomocą modelu logitowego.

# Podział zbioru na treningowy i testowy
set.seed(123)
train <- dane[1:3500,]
summary(train)

test <- dane[3501:5000,]
summary(test)

# Budowa modelu logitowego Probability of default.
model_train_logit<- glm(default ~ difference_days + sum_of_loans_winsor + two_loans + industry_Airlines_Financial +industry_Travel_and_Entertainment + industry_Banking_Finance + industry_Others, family = binomial(link = 'logit'), data = train)
summary(model_train_logit)
# Jedna zmienna istotna statystycznie: industry_Airlines_Financial, industry_Others.

# Budowa modelu probitowego Probability od default.
probit_model_train<- glm(default ~ difference_days + sum_of_loans_winsor + two_loans + industry_Airlines_Financial +industry_Travel_and_Entertainment + industry_Banking_Finance + industry_Others, family = binomial(link = "probit"),data = train)
summary(probit_model_train)
# Jedna zmienna istotna statystycznie: difference_days, industry_Airlines_Financial, industry_Others. 

# Predykcja defaultu na danych treningowych.
predict <- predict(model_train_logit, type = 'response', newdata = train)

# Krok 6 & 7 Pokaż tablicę kontyngencji, wykreśl krzywą ROC oraz AUC. 

# Macierz pomyłek dla zbioru treningowego dla punktu odcięcia 0.5.Przy pomocy tego punktu rozgraniczamy obserwacje na te 'defaultowe' oraz nie.
table(train$default, predict > 0.5)

# Krzywa ROC dla danych treningowych.
ROCRpred <- prediction(predict, train$default)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf)

# Wartość AUC dla danych treningowych.
auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc
# Otrzymana miara AUC=0.54 wskazuje, na słabą wydajność modelu. Jest ona zbliżona do 0.5, co świadczy
# o tym, ze model rozpoznaje obserwacje defaultowe z takim samym prawdopodobieńśtwem jak 
# gdyby dokonano tego w sposób losowy. Taki wniosek wynika również, z analizy graficznej krzywej ROC.

# Predykcja defaultu dla danych testowych.
pr_logit <- predict(model_train_logit, test, se = TRUE)

# Macierz pomyłek dla danych testowych.
l <- cbind(test, pr_logit)
l <- mutate(l, prob = plogis(fit))
l <- mutate(l, pred = ifelse(prob < 0.5, 0, 1))
with(test, table(l$pred, test$default))

# Krzywa ROC dla zbioru testowego.
confusionMatrix(as.factor(l$pred), as.factor(test$default))
# Model prawidłowo zaklasyfikował 1210 obserwacji, natomiast 290 zaklasyfikował błędnie.

roc.logit <- AUC::roc(l$prob, as.factor(test$default))
qplot(x = roc.logit$cutoffs, y = roc.logit$tpr, geom = "line")
qplot(x = roc.logit$cutoffs, y = roc.logit$fpr, geom = "line")
qplot(x = roc.logit$fpr, y = roc.logit$tpr, geom = "line")
auc_score <- auc(roc(test$default, l$prob))

# Krzywa ROC dla danych testowych.
ROCRpred_t <- prediction(l$pred, test$default)
ROCRperf_t <- performance(ROCRpred_t, 'tpr','fpr')
plot(ROCRperf_t)

# Wartość AUC dla danych testowych.
auc_t <- performance(ROCRpred_t, measure = "auc")
auc_t <- auc_t@y.values[[1]]
auc_t
# AUC na poziomie 0.5, świadczy o losowości klasyfikatora.

# Krok 8. Bootstrap
N <- length(train$default)  # Określamy długość próbki
N
M <- 1000 # Określamy ile razy będziemy powtarzać tę procedurę
auc_vector <- c(1:M)  # Wektor zawierający wartości startowe
alpha <- 0.1   # Ustawiamy poziom istotności na 10%

tic()

for (i in 1:M){
  
# Losowanie wierszy z próby ze zwracaniem
  resample <- sample_n(dane, N, replace = TRUE)
  
# Wyliczamy wartość AUC dla próby
# Krzywa ROCR
  ROCRpred <- prediction(predict, resample$default)
  ROCRperf <- performance(ROCRpred, 'tpr','fpr')
# Wartość AUC
  auc <- performance(ROCRpred, measure = "auc")
  auc <- auc@y.values[[1]]
  auc_vector[i] <- auc  
}

toc()
# Budujemy przedział ufności
lower <- quantile(auc_vector, alpha / 2)
lower
upper <- quantile(auc_vector, 1 - (alpha / 2) )
upper

# Wyniki dla 90-procentowego przedziału ufności.
paste0('AUC = ', round(auc, 2), ' and ', (1 - alpha) * 100,
       '% bootstrapped confidence interval for AUC is between ', round(lower,2), ' and ', round(upper,2) )










