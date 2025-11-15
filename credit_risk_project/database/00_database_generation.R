
rm(list = ls())

# paths
#################################################
base_path <- getwd()
input_path <- paste0(base_path, "/input")
output_path <- paste0(base_path, "/output")

# libnames
#################################################

# install.packages('generator')
# install.packages('dplyr')

library(generator)
library(dplyr)


# first (client) database
################################################

# number of clients
n <- 5000

# generating client characteristics

creditcardnr <- r_credit_card_numbers(n)
agreement_start <- (r_date_of_births(n, start = as.Date("1990-01-01"), end = as.Date("2004-01-01")))
name <- r_full_names(n)
ID <- r_national_identification_numbers(n)
default <- rbinom(n, 1, 0.2)                                    # "1" - default, "0" - nondefault
time_of_default <- (r_date_of_births(n, start = as.Date("2004-01-01"), end = Sys.Date()))
time_of_default <- as.data.frame(time_of_default)

# binding all client characteristics
client_database <- as.data.frame(cbind(name, ID, agreement_start, creditcardnr, default, time_of_default))

# default date only in case of default
date_of_default <- ifelse(client_database$default == 0, NA, as.character(client_database$time_of_default))
head(date_of_default)
client_database <- cbind(select(client_database, - time_of_default), date_of_default)

head(client_database)


# second (credits) database
################################################

# first client's credit

gross_value_mortgage <- round(100000 * rgamma(n, 5, rate = 1), 0)

have_mortgage <- rbinom(n, 1, 0.3)

value_mortgage <- have_mortgage * gross_value_mortgage
value_mortgage <- cbind(select(client_database, ID), value_mortgage)


# second client's credit

value_nonmortgage <- round(10000 * rgamma(n, 2, rate = 2), 0)
value_nonmortgage <- cbind(select(client_database, ID), value_nonmortgage)

# binding to one database

loans <- right_join(value_mortgage, value_nonmortgage, by = 'ID')
# ifelse(loans$value_mortgage == 0, 'NA', loans$value_mortgage)
head(loans)

# export to csv

write.csv2(loans, file = paste0(output_path, "/loans.csv"))
write.csv2(client_database, file = paste0(output_path, "/clients.csv"))



