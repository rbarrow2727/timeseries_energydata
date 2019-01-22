#install MySQL
install.packages("RMySQL")
library(RMySQL)
install.packages("DBI")
library(DBI)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics',
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

#list the tables contained in the database
dbListTables(con)

#pull year yr_2006
dbListFields(con, 'yr_2006')
#download data from each table
yr2006 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr2007 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr2008 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr2009 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr2010 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")


#create primary Data Frame, multi-year
#install dplyr
install.packages("dplyr")
library(dplyr)
#combine tables
newDF <- bind_rows(yr2007, yr2008, yr2009)
newDF <- na.omit(newDF)
str(newDF)
summary(newDF)
head(newDF)
tail(newDF)


##Pre-Processing
#combine date and time into one, new column/attribute
newDF <- cbind(newDF, paste(newDF$Date, newDF$Time), stringsAsFactors = FALSE)
#give the new attrbiute a header for the column, DateTime
colnames(newDF)[7] <- "DateTime"
#move DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
head(newDF)
ncol(newDF)
#convert DateTime attribute into data type POSIXct, then add time zones
#convert DateTime from POSIXlt to POSIXct
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
#add time zone
attr(newDF$DateTime, "tzone") <- "Europe/Paris"
#inspect data types
str(newDF)
View(newDF)

#install lubrdate package
install.packages("lubridate")
library(lubridate)

#create year attribute with lubridate, extract 'year' from DateTime
newDF$year <- year(newDF$DateTime)
#extract month from DateTime
newDF$month <- month(newDF$DateTime)
#extract day from DateTime
newDF$day <- day(newDF$DateTime)
#extract week from DateTime
newDF$week <- week(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)
newDF$weekday <- wday(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)

##Begin of Task 2
#plot all of sub_meter_1
plot(newDF$Sub_metering_1)
#subset the second weeks of 2008 - All observations
houseWeek <- filter(newDF, year == 2008 & month == 7 & week == 2)
houseWeek7 <- filter(newDF, year == 2008 & week == 29)
#plot subset houseWeek
plot(houseWeek$Sub_metering_1)

#visulaize single day with Plotly
install.packages("plotly")
install.packages("ggplot2")
library(ggplot2)
library(plotly)
packageVersion('plotly')
install.packages("later")
library(later)
install.packages("promises")
library(promises)
install.packages("mime")
library(mime)
#subset the 9th day of Jan 2008 - All observations
houseDay <- filter(newDF, year == 2008 & month == 1 & day == 9)
#plot Sub_meter_1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')
#plot houseDay Sub_meter 1,2,3 with title, legend and labels - All observations
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter',
        mode = 'lines') %>%
          add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
          add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
          layout(title = "Power Consumption January 9th, 2008",
                 xaxis = list(title = "Time"),
                 yaxis = list(title = "Power (watt-hours)"))
#plot houseWeek the same way 
plot_ly(houseWeek7, x = ~houseWeek7$DateTime, y = ~houseWeek7$Sub_metering_1, name = 'Kitchen', type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~houseWeek7$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek7$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 3rd Week of July 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))
#plot whole month using february 2008
feb2008 <- filter(newDF, year == 2008 & month == 2)
jan2008 <- filter(newDF, year == 2008 & month == 1)
jul2008 <- filter(newDF, year == 2008 & month == 7)
#plot month
plot_ly(jul2008, x = ~jul2008$DateTime, y = ~jul2008$Sub_metering_1, name = 'Kitchen', type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~jul2008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~jul2008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption July 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))

#Add frequencies to reduce granularity, this one in 10 mintue intervals
#subset the 9th day of January 2008 - 10 min frequency 
houseday10 <- filter(newDF, year == 2008 & month == 7 & day == 9 & (minute == 0 | minute == 10 | minute == 20 |
                                                                    minute == 30 | minute == 40 | minute == 50))
#plot sub_meter 1,2,3 with title and legend and lables - 10 minute frquency 
plot_ly(houseday10, x = ~houseday10$DateTime, y = ~houseday10$Sub_metering_1, name = 'Kitchen', type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~houseday10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseday10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption July 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
#create houseWeek 10 min frequency 
houseweek10 <- filter(newDF, year == 2008 & week == 28 & (minute == 0 | minute == 30))
#plot HouseWeek 10 min frequency 
plot_ly(houseweek10, x = ~houseweek10$DateTime, y = ~houseweek10$Sub_metering_1, name = 'Kitchen', type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~houseweek10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseweek10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2nd Week of July 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
#create month Viz pull data every 4 hours
Jan20088hrfreq <- filter(newDF, year == 2008 & month == 1 & (hour == 0  | hour == 8 |
                                                            hour == 16 | hour == 24))
Jul20088hrfreq <- filter(newDF, year == 2008 & month == 7 & (hour == 0  | hour == 8 |
                                                               hour == 16 | hour == 24))

#plot January 2008, 4 hour frequency 
plot_ly(Jul20088hrfreq, x = ~Jul20088hrfreq$DateTime, y = ~Jul20088hrfreq$Sub_metering_1, name = 'Kitchen', type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~Jul20088hrfreq$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Jul20088hrfreq$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption July 2008 (8 Hour Frequency)",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
##EXPERIMENT
#create a pie chart showing energy use per su_meter
plot_ly(houseWeek, type = 'pie')
write.csv(newDF, "newDF.csv")
newDFtest <- newDF
#create active_enery attribute using formula on UCI site, newDFtest as host
head(newDFtest)
newDFtest$active_energy <- ((newDFtest$Global_active_power*1000/60) - newDFtest$Sub_metering_1 - newDFtest$Sub_metering_2 - newDFtest$Sub_metering_3)
View(newDFtest)
###

#store your data.frame as a time series 
library(ggplot2)
install.packages("ggfortify")
library(ggfortify)
#subset data to one observation per week on Mondays at 8:00pm for 2007, 2008, 2009 
house070809weekly <- filter(newDF, weekday == 1 & hour == 20 & minute == 1)
newtimeWK <- filter(newDF, weekday %in% c(1,2,3,4,5))
View(newtimeWK)
newtimeWKD <- filter(newDF, (weekday == 6 | weekday == 7))
#create TimeSeries object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency = 52, start = c(2007,1), end = c(2010,1))
#plot sub_meter 3 with autoplot
autoplot(tsSM3_070809weekly)
## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly, ylim = c(0, 40), xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency = 52, start = c(2007,1), end = c(2010,1))
autoplot(tsSM2_070809weekly)
autoplot(tsSM2_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")
plot.ts(tsSM2_070809weekly, xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")

tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency = 52, start = c(2007,1), end = c(2010,1))
autoplot(tsSM1_070809weekly)
autoplot(tsSM1_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")
plot.ts(tsSM1_070809weekly, xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")

## (end step 2)


library(forecast)
#apply time series linear regression to the sub_meter 3 ts object and use the summary
#to obtain r2 and RMSE from the model 
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3)
accuracy(fitSM3)
fitSM3
#create a forecast for sub_meter 3. forecast ahead 20 time periods
forecastfitSM3 <- forecast(fitSM3, h = 26)
#plot forecast for sub_meter 3
plot(forecastfitSM3)
#create sub_meter 3 forecats with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h = 26, level = c(80,90))
#plot sub_meter 3, limit y and add labels 
plot(forecastfitSM3c, ylim = c(0, 35), ylab = "WattHours", xlab = "Time")
#sub_meter 2
fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season)
summary(fitSM2)
accuracy(fitSM2)
forecastfitSM2 <- forecast(fitSM2, h = 26)
plot(forecastfitSM2)
forecastfitSM2c <- forecast(fitSM2, h = 26, level = c(80,90))
plot(forecastfitSM2c, ylim = c(0, 35), ylab = "WattHours", xlab = "Time")
#SUb_meter 1
fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season)
summary(fitSM1)
accuracy(fitSM1)
forecastfitSM1 <- forecast(fitSM1, h = 26)
plot(forecastfitSM1)
forecastfitSM1c <- forecast(fitSM1, h = 26, level = c(80,90))
plot(forecastfitSM1c, ylim = c(0, 40), ylab = "WattHours", xlab = "Time")

## (end step 3)

##Seasonal decomposition 
#decompose sub_meter3 into trend, seasonal, and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
#plot decomposed sub_meter 3
plot(components070809SM3weekly)
#check summary statistics for decomposed sub_meter 3
summary(components070809SM3weekly$seasonal)
components070809SM3weekly$seasonal
components070809SM3weekly$trend
components070809SM3weekly$random
plot(components070809SM3weekly$seasonal)

#sub meter 2
component070809SM2weekly <- decompose(tsSM2_070809weekly)
plot(component070809SM2weekly)
autoplot(component070809SM2weekly)
summary(component070809SM2weekly$seasonal)
#sub_meter 1
component070809SM1weekly <- decompose(tsSM1_070809weekly)
plot(component070809SM1weekly)
autoplot(component070809SM1weekly)
summary(component070809SM1weekly)
##end (step 4)

##HoltWithers Forecasting
#seasonal adjusting sub_meter 3 by subtracting the seasonal componeent and plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
plot(tsSM3_070809Adjusted)
#test seasonal adjustment by running decompose again, Note the very small scale for Seasonal
#this one is great
plot(decompose(tsSM3_070809Adjusted))
#HoltWinters Simple Expotential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta = FALSE, gamma = FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))
#Forecast and plot HoltWinters
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h = 26)
plot(tsSM3_HW070809for, ylim = c(0, 30), ylab = "Watt-Hours", xlab = "Time - sub_meter 3")
#Forecast HoltWinters with diminshed confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h = 26, level = c(5, 15, 35))
#plot only the forecasted are6
plot(tsSM3_HW070809forC, ylim = c(0,20), ylab = "Watt-Hours", xlab = "Time - sub_meter 3", start(2010))

#sub_meter 2 seaonsal adjusting 
tsSM2_070809weeklyAdjusted <- tsSM2_070809weekly - component070809SM2weekly$seasonal
autoplot(tsSM2_070809weeklyAdjusted)
plot(decompose(tsSM2_070809weeklyAdjusted))
#HoltWinters expotential smoothing and plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809weeklyAdjusted, beta = FALSE, gamma = FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25), ylab = "Watt-Hours", xlab = "Time - sub_meter 2")
#forecast and plot HoltWinters
tsSM2_070809for <- forecast(tsSM2_HW070809, h = 26)
plot(tsSM2_070809for, ylim = c(0, 25), ylab = "Watt-Hours", xlab = "Time - Sub_meter 2")
#forecats HoltWinter with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h = 26, level = c(5, 15, 35))
#plot only forecasted area
plot(tsSM2_HW070809forC, ylim = c(0,20), ylab = "Watt-Hours", xlab = "Time - sub meter 2", start(2010))

#sub_meter 1 seasonal adjusting
tsSM1_070809weeklyAdjusted <- tsSM1_070809weekly - component070809SM1weekly$seasonal
autoplot(tsSM1_070809weeklyAdjusted)
plot(decompose(tsSM1_070809weeklyAdjusted))
#HoltWinters expotential smoothing and plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809weeklyAdjusted, beta = FALSE, gamma = FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25), ylab = "Watt-Hours", xlab = "Time - Sub_meter 1")
#forecast and plot HoltWinters
tsSM1_070809for <- forecast(tsSM1_HW070809, h = 26)
plot(tsSM1_070809for, ylim = c(0, 25), ylab = "Watt Hours", xlab = "Time - sub meter 1")
#forecast HoltWinter with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h = 26, level = c(5, 15, 35))
#plot only forecasted area
plot(tsSM1_HW070809forC, ylim = c(0,20), ylab = "WattHours", xlab = "Time - sub meter 1", start(2010))

##end(step 5)

tsSM1_070809weeklypredict <- predict(tsSM1_HW070809, n.ahead = 10, prediction.interval = TRUE)
plot.ts(tsSM1_070809weeklypredict)

plot.ts(tsSM1_HW070809$x)
lines(tsSM1_HW070809$fitted[,1], col = "green")
lines(tsSM1_070809weeklypredict[,1], col = "blue")
lines(tsSM1_070809weeklypredict[,2], col = "red")
lines(tsSM1_070809weeklypredict[,3], col = "red")

