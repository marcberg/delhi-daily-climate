#install.packages("tidyverse")
#install.packages("corrplot")
# install.packages('bayesm')
# install.packages('MASS')
# install.packages('MCMCpack')

library(tidyverse)
library(corrplot)
library(bayesm)
library(MASS)
library(MCMCpack)
library(lubridate)

data_train <- read_csv("data/DailyDelhiClimateTrain.csv")
data_test <- read_csv("data/DailyDelhiClimateTest.csv")


corrs <- data_train %>% 
  select_if(is.numeric) %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  cor(method = "pearson")

p <- corrplot(as.matrix(corrs))

data_train %>%
  dplyr::select(-meanpressure) %>%
  gather(key, value, -date) %>%
  ggplot(mapping = aes(x = date, y = value)) + 
    geom_line(aes(color = key), size = 1) +
    theme_minimal()


data_train %>%
  mutate(month_num = month(date)) %>%
  group_by(month_num) %>%
  summarise(mean_meantemp = mean(meantemp)) %>%
  ggplot(mapping = aes(x = month_num, y = mean_meantemp)) + 
  geom_col() +
  theme_minimal()

data_train %>%
  mutate(month_num = month(date)) %>%
  group_by(month_num) %>%
  summarise(mean_humidity = mean(humidity)) %>%
  ggplot(mapping = aes(x = month_num, y = mean_humidity)) + 
  geom_col() +
  theme_minimal()


data_train %>%
  mutate(month_num = month(date)) %>%
  group_by(month_num) %>%
  summarise(mean_wind_speed = mean(wind_speed)) %>%
  ggplot(mapping = aes(x = month_num, y = mean_wind_speed)) + 
  geom_col() +
  theme_minimal()
