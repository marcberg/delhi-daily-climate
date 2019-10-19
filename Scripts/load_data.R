
data_train <- read_csv("data/DailyDelhiClimateTrain.csv")
data_test <- read_csv("data/DailyDelhiClimateTest.csv")


# date exist in both training and test
data_test <- data_test %>% filter(date != "2017-01-01")