library(dplyr)
library(boot)
library(tidyverse)
library(readr)
rm(list = ls()) 
graphics.off()
myfile <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv"
df<- data.frame(read_csv(myfile))
df <- df[order(df$Country),]
df1 <- df[order(df$Country),]
df1$New_Cases = df1$Confirmed - lag(df1$Confirmed)
df1$New_Recovered = df1$Recovered - lag(df1$Recovered)
df1$New_Deaths = df1$Deaths - lag(df1$Deaths)
df1$Future_Confirmed <- lead(df1$Confirmed, offset = 1)
df1$Future_Recovery <- lead(df1$Recovered, offset = 1)
df1$Future_Deaths <- lead(df1$Deaths, offset = 1)
df1[df1<0] <- 0
df<- subset(df, Date >= "2020-01-22" & Date <= "2020-04-23")
df$New_Cases = df$Confirmed - lag(df$Confirmed)
df$New_Recovered = df$Recovered - lag(df$Recovered)
df$New_Deaths = df$Deaths - lag(df$Deaths)
df$Future_Confirmed <- lead(df$Confirmed, offset = 1)
df$Future_Recovery <- lead(df$Recovered, offset = 1)
df$Future_Deaths <- lead(df$Deaths, offset = 1)
df[df<0] <- 0
model_input <- na.omit(df)
model_input <- model_input[!(model_input$Date==max(model_input[["Date"]])),]
model_input_confirmed = model_input[model_input[['Date']] > (max(model_input[["Date"]]) - 15), ]
pred_input <- df[(df$Date==max(df[["Date"]])),] 
pred_input1 <- df1[(df1$Date==max(df1[["Date"]])),] 
library(boot)
deaths.poly <- glm(Future_Deaths~poly(Confirmed, degree=2, raw=T) + 
                     poly(Deaths, degree=2, raw=T) + poly(New_Cases, degree=2, raw=T) + 
                     poly(New_Deaths, degree=2, raw=T), data=model_input)
(cv.error <- cv.glm(model_input, deaths.poly, K=10)$delta)
summary(deaths.poly)

confirmed.poly <- glm(Future_Confirmed~poly(Confirmed, degree=2, raw=T) + 
                        poly(New_Cases, degree=2, raw=T) +
                        poly(New_Deaths, degree=2, raw=T), data=model_input)
(cv.error <- cv.glm(model_input, confirmed.poly, K=10)$delta)
summary(confirmed.poly)

recovered.poly <- glm(Future_Recovery~poly(Confirmed, degree=2, raw=T) + 
                        poly(Recovered, degree=2, raw=T) +
                        poly(Deaths, degree=2, raw=T) + poly(New_Cases, degree=2, raw=T) + 
                        poly(New_Recovered, degree=2, raw=T) +
                        poly(New_Deaths, degree=2, raw=T), data=model_input)
(cv.error <- cv.glm(model_input, recovered.poly, K=10)$delta)
summary(recovered.poly)

# accuracy = pred_input %>% select(Date, Country, Confirmed,Recovered,Deaths)
# accuracy$Pred_Confirmed = round(predict(confirmed.poly, pred_input))
# accuracy$Pred_Recovered = round(predict(recovered.poly, pred_input))
# accuracy$Pred_Deaths = round(predict(deaths.poly, pred_input))
# accuracy$Accuracy_Confirmed <- 1 - (abs(accuracy$Confirmed - accuracy$Pred_Confirmed) / accuracy$Confirmed)
# accuracy$Accuracy_Recovered <- 1 - (abs(accuracy$Recovered - accuracy$Pred_Recovered) / accuracy$Recovered)
# accuracy$Accuracy_Deaths <- 1 - (abs(accuracy$Deaths - accuracy$Pred_Deaths) / accuracy$Deaths)
accuracy = pred_input1 %>% select(Date, Country, Confirmed,Recovered,Deaths)
accuracy$Pred_Confirmed = round(predict(confirmed.poly, pred_input1))
accuracy$Pred_Recovered = round(predict(recovered.poly, pred_input1))
accuracy$Pred_Deaths = round(predict(deaths.poly, pred_input1))
accuracy$Accuracy_Confirmed <- 1 - (abs(accuracy$Confirmed - accuracy$Pred_Confirmed) / accuracy$Confirmed)
accuracy$Accuracy_Recovered <- 1 - (abs(accuracy$Recovered - accuracy$Pred_Recovered) / accuracy$Recovered)
accuracy$Accuracy_Deaths <- 1 - (abs(accuracy$Deaths - accuracy$Pred_Deaths) / accuracy$Deaths)

#Remove NAN and +/- Inf to calulate model accuracy
accuracy[is.infinite(accuracy$Accuracy_Confirmed), "Accuracy_Confirmed"] <- NA
accuracy[is.infinite(accuracy$Accuracy_Recovered), "Accuracy_Recovered"] <- NA
accuracy[is.infinite(accuracy$Accuracy_Deaths), "Accuracy_Deaths"] <- NA
accuracy <- na.omit(accuracy)

colSums(accuracy[9])/length(accuracy$Pred_Confirmed)
colSums(accuracy[10])/length(accuracy$Pred_Recovered)
colSums(accuracy[11])/length(accuracy$Pred_Deaths)

file<-round(predict(confirmed.poly, data.frame(pred_input,Date=pred_input$Date,Country="US"))) 

end=as.Date(c("2020-05-01"))
while (pred_input[['Date']] < end)
{
  # New Predictions
  pred_input$Future_Confirmed = round(predict(confirmed.poly, pred_input)) 
  pred_input$Future_Recovery = round(predict(recovered.poly, pred_input)) 
  pred_input$Future_Deaths = round(predict(deaths.poly, pred_input))
  
  #Update the Numbers
  pred_input$New_Cases = abs(pred_input$Future_Confirmed - pred_input$Confirmed)
  pred_input$New_Recovered = abs(pred_input$Future_Recovery - pred_input$Recovered)
  pred_input$New_Deaths = abs(pred_input$Future_Deaths - pred_input$Deaths)
  
  pred_input$Confirmed = pred_input$Future_Confirmed
  pred_input$Recovered = pred_input$Future_Recovery
  pred_input$Deaths = pred_input$Future_Deaths
  pred_input$Date = pred_input$Date + 1
}

#graphs
Country <- "Germany"
#get per Country data
buffer <- df1[df1$Country == Country, ]
df2 <- buffer



data <- df2
dcases=round(predict(confirmed.poly, data.frame(data,Date=data$Date,Country=Country)))
drecovered=round(predict(recovered.poly, data.frame(data,Date=data$Date,Country=Country)))
dDeaths=round(predict(deaths.poly, data.frame(data,Date=data$Date,Country=Country)))
datall<-cbind(data, dcases,drecovered,dDeaths)

datall$Day  <- 1:nrow(datall)

datall<-datall[,c(1,2,3,4,5,15,12,13,14)]
write.csv(datall, file = "dataGer.csv", row.names = FALSE)



pred_input[pred_input<0] <- 0
Final_Predictions <- pred_input[-c(1,6:11)]

write.csv(Final_Predictions, file = "Final_Predictions Poly Regression.csv", row.names = FALSE)




