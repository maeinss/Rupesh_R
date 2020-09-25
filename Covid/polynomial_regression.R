rm(list = ls()) 
graphics.off()
train <- read.csv("https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv" ,stringsAsFactors = FALSE)
#train <- read.csv("train.csv", stringsAsFactors = FALSE)
#test <- read.csv("test.csv", stringsAsFactors = FALSE)
head(train)
colSums(is.na(train))
colSums(train[as.character(train$Date) == tail(train$Date,n=1), c("Confirmed", "Deaths","Recovered")])
#Subsetting
train <- subset(train, Date > "2020-01-22" & Date < "2020-04-21")
#variable initialization
train2 <- list()
confirmed.cases.model <- list()
Deaths.model <- list()
recovered.model <- list()

#extract Country information
Countrydata <- as.data.frame(table(train$Country))
Countrydata$Num <- row(Countrydata)

for(Country in Countrydata$Var1) {
  #get per Country data
  buffer <- train[train$Country == Country, ]
  rownames(buffer) <- NULL
  buffer$Day <- as.numeric(rownames(buffer))
  train2[[Country]] <- buffer
  
  #create models
  #note: polinomial is choosen by trial basis
  confirmed.cases.model[[Country]] <- lm(Confirmed ~ Day + I(Day^2) + I(Day^3) + I(Day^5), train2[[Country]])
  Deaths.model[[Country]] <- lm(Deaths ~ Day + I(Day^2) + I(Day^3) + I(Day^5), train2[[Country]])
  recovered.model[[Country]] <- lm(Recovered ~ Day + I(Day^2) + I(Day^3) + I(Day^5), train2[[Country]])
}
#set parameter
difftime(as.POSIXct(tail(train$Date,n=1)), as.POSIXct(train$Date[1], tz="UTC"), units="days")
day <- 89

#prepare the object
submission <- data.frame()
#iterate the confirmed vs prediction on each of Country
for(Country in Countrydata$Var1) {
  data <- train2[[Country]]
  buffer <- data.frame(Country = Country, 
                       ConfirmedCases = data$Confirmed[day], 
                       EstimatedCases = round(predict(confirmed.cases.model[[Country]], 
                                                      newdata = data.frame(Day = day))), 
                       ConfirmedDeaths = data$Deaths[day], 
                       EstimatedDeaths = round(predict(Deaths.model[[Country]],
                                                       newdata = data.frame(Day = day))),
                       ConfirmedRecovered = data$Recovered[day], 
                       EstimatedRecovered = round(predict(recovered.model[[Country]],
                                                          newdata = data.frame(Day = day)))
  )
  submission <- rbind(submission, buffer)
}
submission1<-submission[,c(1,3 ,7,5)]
names(submission1)[1] <- "Country"
names(submission1)[2] <- "Confirmed"
names(submission1)[3] <- "Recovered"
names(submission1)[4] <- "Deaths"
write.csv(submission1, file = "TestFileToComplete.csv", row.names = FALSE)

#For the accuracy

#prepare the object
accuracy <- submission

#calculate accuracy for cases and vatality; confirmed vs estimation
accuracy$AccuracyCases <- 1 - (abs(accuracy$ConfirmedCases - accuracy$EstimatedCases) / accuracy$ConfirmedCases)
accuracy$AccuracyDeaths <- 1 - (abs(accuracy$ConfirmedDeaths - accuracy$EstimatedDeaths) / accuracy$ConfirmedDeaths)
accuracy$AccuracyRecovered <- 1 - (abs(accuracy$ConfirmedRecovered - accuracy$EstimatedRecovered) / accuracy$ConfirmedRecovered)

#fix 
accuracy[is.nan(accuracy$AccuracyDeaths), "AccuracyDeaths"] <- 1
accuracy[is.nan(accuracy$AccuracyRecovered), "AccuracyRecovered"] <- 1
accuracy[is.infinite(accuracy$AccuracyRecovered), "AccuracyRecovered"] <- 1

accuracy<-accuracy[,c(1,3,7,5,8 ,10,9)]
colSums(accuracy[5])/length(accuracy$EstimatedCases)
colSums(accuracy[6])/length(accuracy$EstimatedRecovered)
colSums(accuracy[7])/length(accuracy$EstimatedDeaths)
