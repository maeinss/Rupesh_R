rm(list = ls()) 
graphics.off()
library(dplyr)
library(caret)
library(rpart)
train <-  read.csv("https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv" ,stringsAsFactors = FALSE)
train <- train[order(train$Country),]
str(train)
train$Date <- as.character(train$Date)
train$Date <- as.Date(train$Date, format = "%Y-%m-%d")

# construct the test values
test <- train[, -1] #delete column 1
test <- test[!duplicated(test[,c(1)]),]
values =  seq(from = as.Date("2020-04-20"), to = as.Date("2020-04-20"), by = 1)
test<-cbind(test, Date = rep(values,each = nrow(test)))
test<-test[,c(5,1,2,3,4)]
test <- test[order(test$Country),]
test$Date <- as.character(test$Date)
test$Date <- as.Date(test$Date, format = "%Y-%m-%d")
#summary(train)
# test <- read.csv("test.csv")
# colnames(test)
# names(test)[2] <- "Province_State"
# names(test)[3] <- "Country_Region"
# str(test)
# test$Date <- as.character(test$Date)
# test$Date <- as.Date(test$Date, format = "%Y-%m-%d")
# test <- test %>%
#   filter(Date >= "2020-03-19" & Date <= "2020-03-20")
########ConfirmedCases
train$log_ConfirmedCases <- log(train$Confirmed + 1)
train$log_Fatalities <- log(train$Deaths + 1)
train$log_Recovered <- log(train$Recovered + 1)
train$Day <- as.integer(train$Date - min(train$Date)) + 1
test$Day <- as.integer(test$Date - min(train$Date)) + 1
summary(train$Day)
summary(test$Day)
num_folds <- trainControl(method = "cv", number = 5) # Specify 5-fold cross-validation.
parameter_grid <- expand.grid(.cp = seq(0, 0.01, 0.001)) # Explore values of `cp` b/w 0 and 0.01.
# Model 1: Predicting ConfirmedCases
grid_search_1 <- train(
  log_ConfirmedCases ~  Country + Day,
  data = train,
  method = "rpart", # CART algorithm
  trControl = num_folds,
  tuneGrid = parameter_grid
)
print(grid_search_1)

tree_1 <- rpart(
  log_ConfirmedCases ~  Country + Day,
  data = train, 
  cp = 0
)

##predicting Fatalities
grid_search_2 <- train(
  log_Fatalities ~ Country + Day + Confirmed,
  data = train,
  method = "rpart", # CART algorithm
  trControl = num_folds,
  tuneGrid = parameter_grid
)
print(grid_search_2)

tree_2 <- rpart(
  log_Fatalities ~  Country + Day + Confirmed  ,
  data = train, 
  cp = 0
)
#predicting Recoverd
grid_search_3 <- train(
  log_Recovered ~ Country + Day + Confirmed,
  data = train,
  method = "rpart", # CART algorithm
  trControl = num_folds,
  tuneGrid = parameter_grid
)
print(grid_search_3)
tree_3 <- rpart(
  log_Recovered ~  Country + Day + Confirmed,
  data = train, 
  cp = 0
)
###predictionandFinalReport###
test$Confirmed <- exp(predict(tree_1, newdata = test)) - 1
test$Deaths <- exp(predict(tree_2, newdata = test)) - 1
test$Recovered <- exp(predict(tree_3, newdata = test)) - 1
#FinalReport
submission <- test %>% select(Date,Country,Confirmed,Recovered, Deaths)
require(data.table)
setDT(submission)
cols <- names(submission)[c(3,4,5)]
submission[,(cols) := round(.SD,0), .SDcols=cols]
write.csv(submission, file = "submission.csv", row.names = FALSE)
