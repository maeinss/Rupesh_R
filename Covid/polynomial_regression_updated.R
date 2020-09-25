rm(list = ls()) 
graphics.off()
train <- read.csv("https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv" ,stringsAsFactors = FALSE)
#train <- read.csv("train.csv", stringsAsFactors = FALSE)
#test <- read.csv("test.csv", stringsAsFactors = FALSE)
head(train)
colSums(is.na(train))
colSums(train[as.character(train$Date) == tail(train$Date,n=1), c("Confirmed", "Deaths","Recovered")])
#Subsetting
train11<-train
train <- subset(train, Date >= "2020-01-22" & Date <= "2020-04-28")
#variable initialization
train2 <- list()
train22<-list()
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
  buffer1 <- train11[train11$Country == Country, ]
  rownames(buffer1) <- NULL
  buffer1$Day <- as.numeric(rownames(buffer1))
  train22[[Country]] <- buffer1
  #create models
  #note: polinomial is choosen by trial basis
  confirmed.cases.model[[Country]] <- lm(Confirmed ~ Day + I(Day^2) + I(Day^3) + I(Day^5), train2[[Country]])
  Deaths.model[[Country]] <- lm(Deaths ~ Day + I(Day^2) + I(Day^3) + I(Day^5), train2[[Country]])
  recovered.model[[Country]] <- lm(Recovered ~ Day + I(Day^2) + I(Day^3) + I(Day^5), train2[[Country]])
}
#set parameter
difftime(as.POSIXct(tail(train$Date,n=1)), as.POSIXct(train$Date[1], tz="UTC"), units="days")
day <- 101

#prepare the object
submission <- data.frame()
#iterate the confirmed vs prediction on each of Country
for(Country in Countrydata$Var1) {
  data <- train2[[Country]]
  data1<-train22[[Country]]
  buffer <- data.frame(Country = Country, 
                       ConfirmedCases = data1$Confirmed[day], 
                       EstimatedCases = round(predict(confirmed.cases.model[[Country]], 
                                                      newdata = data.frame(Day = day))), 
                       ConfirmedDeaths = data1$Deaths[day], 
                       EstimatedDeaths = round(predict(Deaths.model[[Country]],
                                                       newdata = data.frame(Day = day))),
                       ConfirmedRecovered = data1$Recovered[day], 
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
print(accuracy)

accuracy<-accuracy[,c(1,3,7,5,8 ,10,9)]
colSums(accuracy[5])/length(accuracy$EstimatedCases)
colSums(accuracy[6])/length(accuracy$EstimatedRecovered)
colSums(accuracy[7])/length(accuracy$EstimatedDeaths)

Country <- "US"

#retrieve the data
data1 <- as.data.frame(train22[[Country]])
data<- as.data.frame(train22[[Country]])

dcases=round(predict(confirmed.cases.model[[Country]], 
                     newdata = data.frame(Day = data$Day)));
drecovered=round(predict(recovered.model[[Country]], 
                         newdata = data.frame(Day = data$Day)));
dDeaths=round(predict(Deaths.model[[Country]], 
                      newdata = data.frame(Day = data$Day)));
datall<-cbind(data, dcases,drecovered,dDeaths)
write.csv(datall, file = "dataUS.csv", row.names = FALSE)

#create plot
plot(data1$Day, data1$Confirmed,type = "l", lty = 2,
     col = "blue",
     ylim = c(0, max(data1$Confirmed)),
     xlab = "Days (Since January 22, 2020)",
     ylab = "Number of People", main = paste("Covid-19 Confirmed Cases in", Country),
     cex.lab=0.8, lwd=2)
par(new = TRUE)
plot(data$Day, fitted(confirmed.cases.model[[Country]]),
     type = "l", lty = 3,
     ylim = c(0, max(data$Confirmed)),
     col = "red",
     xlab = "", ylab = "",cex.lab=0.8, lwd=3)
par(new = TRUE)
axis(side = 2)
legend("topleft", inset = .05, 
       legend = c("Confirmed Cases ",
                  "Estimated Cases (based on Model)" 
                  ), 
       col = c("blue", "red"), bg = "light yellow",
       lty = c(2, 3),
       cex = 0.9,lwd=2)