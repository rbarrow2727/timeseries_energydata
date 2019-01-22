##Course 3, Task 1
##Examining Sub-Metering Data on home appliances in watt-hours

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
#pull iris table from database
dbListFields(con, 'iris')
#download all data or choose specific attribute interested in
  #use asterick to specify all attributes for download
  irisALL <- dbGetQuery(con, "SELECT * FROM iris")
  #use attribute names to specify specific attributes for downlaod 
  irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
#examine
summary(irisSELECT)
names(irisALL)
str(irisALL)

#pull year yr_2006
dbListFields(con, 'yr_2006')
#download data from each table
yr2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

#investigate dataframes
#2006
str(yr2006)
summary(yr2009)
head(yr2006)
tail(yr2006)

#create primary Data Frame, multi-year
#install dplyr
install.packages("dplyr")
library(dplyr)
#combine tables
mainDF <- bind_rows(yr2007, yr2008, yr2009)
str(mainDF)
summary(mainDF)
head(mainDF)
tail(mainDF)

##Pre-Processing
#combine date and time into one, new column/attribute
mainDF <- cbind(mainDF, paste(mainDF$Date, mainDF$Time), stringsAsFactors = FALSE)
#give the new attrbiute a header for the column, DateTime
colnames(mainDF)[6] <- "DateTime"
#move DateTime attribute within the dataset
mainDF <- mainDF[,c(ncol(mainDF), 1:(ncol(mainDF)-1))]
head(mainDF)
ncol(mainDF)
#convert DateTime attribute into data type POSIXct, then add time zones
#convert DateTime from POSIXlt to POSIXct
mainDF$DateTime <- as.POSIXct(mainDF$DateTime, "%Y/%m/%d %H:%M:%S")
#add time zone
attr(mainDF$DateTime, "tzone") <- "Europe/Paris"
#inspect data types
str(mainDF)
View(mainDF)

#install lubrdate package
install.packages("lubridate")
library(lubridate)
#create year attribute with lubridate, extract 'year' from DateTime
mainDF$year <- year(mainDF$DateTime)
#extract month from DateTime
mainDF$month <- month(mainDF$DateTime)
#extract day from DateTime
mainDF$day <- day(mainDF$DateTime)
View(mainDF)
mainDF$day

#filter rows
filter(mainDF, year == 2008, month == 2)
feb2008 <- filter(mainDF, year == 2008, month == 2)
nrow(feb2008)
#visualize feb2008
hist(feb2008$Sub_metering_3, col = "red",
      ylab = "Count", xlab = "WattHours")
#plot
plot(feb2008$DateTime, feb2008$Sub_metering_3, ylab = "WattHours", xlab = "")
#find what day of the week the date is on 
x <- as.Date("2008-02-02")
wday(x)
#assign a new column of extracted feb2008 dataset to include what day of the week it is
#makes it easier to pull weekday vs weekend date 
feb2008$dayofweek <- wday(feb2008$DateTime)
View(feb2008)
yr2009$dayofweek <- wday(yr2009$DateTime)

#filter weekends out, 1 = monday, 2 = tuesday, etc
weekdaysfeb2008 <- filter(feb2008, year == 2008, month == 2, dayofweek %in% c(1,2,3,4,5))
#insepct weekday data
ncol(weekdaysfeb2008)
nrow(weekdaysfeb2008)
tail(weekdaysfeb2008)
names(weekdaysfeb2008)
View(weekdaysfeb2008)
arrange(feb2008, desc(Sub_metering_3))

#plot
plot(weekdaysfeb2008$DateTime, weekdaysfeb2008$Sub_metering_3, ylab = "WattHours", xlab = "")

#filter weekdays out, 1 = monday, 2 = tuesday, etc
wkdsfeb2008 <- filter(feb2008, year == 2008, month == 2, dayofweek %in% c(6,7))
#weekds whole year 2009
wkdsyr2009 <- filter(yr2009, year == 2009, dayofweek %in% c(6,7))
weekdaysyr2009 <- filter(yr2009, year == 2009, dayofweek %in% c(1,2,3,4,5))
#plot
plot(wkdsfeb2008$DateTime, wkdsfeb2008$Sub_metering_3, ylab = "WattHours", xlab = "")

plot(weekdaysfeb2008$dayofweek, weekdaysfeb2008$Sub_metering_2, 
     main = "Days Energy was Consumed", type = "p", ylab = "WattHours", xlab = "")
#histogram
hist(feb2008$Sub_metering_1, col = "red",
     xlab = "Global Active Power (kilowatts)", main = "Global Active Power")

install.packages("ggplot2")
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)


#ggpplot
g1 <- (ggplot(wkdsfeb2008, aes(x = Sub_metering_1))
       + geom_histogram(fill = "#F8766D")
       + labs(x = "Global Active Power (kilowatts)", title = "Global Active Power"))
g1
#plot each submeter by month, sub_meter_1
ggplot(mainDF, aes(x=month, y=Sub_metering_1)) +
  geom_line() + xlab("") + ylab("Submeter_1")
#plot sub_meter_2
ggplot(mainDF, aes(x=month, y=Sub_metering_2)) +
  geom_line() + xlab("") + ylab("Submeter_2")
#plot sub_meter_3
ggplot(mainDF, aes(x=month, y=Sub_metering_3)) +
  geom_line() + xlab("") + ylab("Submeter_3") 

#plot weekend feb 2008 kitchen use 
plot(wkdsfeb2008$DateTime, wkdsfeb2008$Sub_metering_1, type = "l", ylab = "WattHours", xlab = "")
g2 <- (ggplot(wkdsfeb2008, aes(x = DateTime, y = Sub_metering_1)) + geom_line() + xlab("") + ylab("WattHours"))
g2
#filer out 3 days 1-3 of feb2008
day1_3feb2008 <- filter(feb2008, day %in% c(1,2,3))
#show kitchen use of those 3 days
g3 <- (ggplot(day1_3feb2008, aes(x = DateTime, y = kitchen)) + geom_line() + xlab("") + ylab("WattHours"))
g3

View(feb2008)

##Separte each year into seasons
yr2007 <- cbind(yr2007, paste(yr2007$Date, yr2007$Time), stringsAsFactors = FALSE)
#give the new attrbiute a header for the column, DateTime
colnames(yr2007)[6] <- "DateTime"
#move DateTime attribute within the dataset
yr2007 <- yr2007[,c(ncol(y2007), 1:(ncol(yr2007)-1))]
head(yr2007)
ncol(yr2007)
#convert DateTime attribute into data type POSIXct, then add time zones
#convert DateTime from POSIXlt to POSIXct
yr2007$DateTime <- as.POSIXct(yr2007$DateTime, "%Y/%m/%d %H:%M:%S")
#add time zone
attr(yr2007$DateTime, "tzone") <- "Europe/Paris"
View(yr2007)
#add year, time, day, and dayofweek 2007
yr2007$year <- year(yr2007$DateTime)
#extract month from DateTime
yr2007$month <- month(yr2007$DateTime)
#extract day from DateTime
yr2007$day <- day(yr2007$DateTime)

#rename sub meter attributes
yr2007 <- rename(yr2007, kitchen = Sub_metering_1, laundry = Sub_metering_2, water_ac = Sub_metering_3)
View(yr2007)
#create seasons, winter
winter07 <- filter(yr2007, year == 2007, month %in% c(1,2))
#spring
spring07 <- filter(yr2007, year == 2007, month %in% c(3,4,5,6))
#summer
summer07 <- filter(yr2007, year == 2007, month %in% c(7,8,9))
#fall
fall07 <- filter(yr2007, year == 2007, month %in% c(10,11,12))

##Separte each year into seasons
###
yr2008 <- cbind(yr2008, paste(yr2008$Date, yr2008$Time), stringsAsFactors = FALSE)
#give the new attrbiute a header for the column, DateTime
colnames(yr2008)[6] <- "DateTime"
#move DateTime attribute within the dataset
yr2008 <- yr2008[,c(ncol(y2008), 1:(ncol(yr2008)-1))]
head(yr2008)
ncol(yr2008)
#convert DateTime attribute into data type POSIXct, then add time zones
#convert DateTime from POSIXlt to POSIXct
yr2008$DateTime <- as.POSIXct(yr2008$DateTime, "%Y/%m/%d %H:%M:%S")
#add time zone
attr(yr2008$DateTime, "tzone") <- "Europe/Paris"
View(yr2008)
#add year, time, day, and dayofweek 2007
yr2008$year <- year(yr2008$DateTime)
#extract month from DateTime
yr2008$month <- month(yr2008$DateTime)
#extract day from DateTime
yr2008$day <- day(yr2008$DateTime)

#rename sub meter attributes
yr2008 <- rename(yr2008, kitchen = Sub_metering_1, laundry = Sub_metering_2, water_ac = Sub_metering_3)
View(yr2008)
#create seasons, winter, spring, summer, faLL
winter08 <- filter(yr2008, year == 2008, month %in% c(1,2))
spring08 <- filter(yr2008, year == 2008, month %in% c(3,4,5,6))
summer08 <- filter(yr2008, year == 2008, month %in% c(7,8,9))
fall08 <- filter(yr2008, year == 2008, month %in% c(10,11,12))

##Separte each year into seasons
###
yr2009 <- cbind(yr2009, paste(yr2009$Date, yr2009$Time), stringsAsFactors = FALSE)
#give the new attrbiute a header for the column, DateTime
colnames(yr2009)[6] <- "DateTime"
#move DateTime attribute within the dataset
yr2009 <- yr2009[,c(ncol(y2009), 1:(ncol(yr2009)-1))]
head(yr2009)
ncol(yr2009)
#convert DateTime attribute into data type POSIXct, then add time zones
#convert DateTime from POSIXlt to POSIXct
yr2009$DateTime <- as.POSIXct(yr2009$DateTime, "%Y/%m/%d %H:%M:%S")
#add time zone
attr(yr2009$DateTime, "tzone") <- "Europe/Paris"
View(yr2009)
#add year, time, day, and dayofweek 2007
yr2009$year <- year(yr2009$DateTime)
#extract month from DateTime
yr2009$month <- month(yr2009$DateTime)
#extract day from DateTime
yr2009$day <- day(yr2009$DateTime)
#rename sub meter attributes
yr2009 <- rename(yr2009, kitchen = Sub_metering_1, laundry = Sub_metering_2, water_ac = Sub_metering_3)
View(yr2009)
#create seasons, winter, spring, summer, faLL
winter09 <- filter(yr2009, year == 2009, month %in% c(1,2))
spring09 <- filter(yr2009, year == 2009, month %in% c(3,4,5,6))
summer09 <- filter(yr2009, year == 2009, month %in% c(7,8,9))
fall09 <- filter(yr2009, year == 2009, month %in% c(10,11,12))
View(summer09)

#evaluate
summary(winter07)
summary(summer07)
#08
summary(winter08)
summary(summer08)
#09
summary(winter09)
summary(summer09)

summary(spring07)
summary(fall07)
summary(spring08)
summary(fall08)
summary(spring09)
summary(fall09)

#winter09 3 submeter graph
plot(winter09$DateTime, winter09$kitchen, type = "l", ylab = "Energy Sub Metering", xlab = "Winter 2009")
points(winter09$DateTime, winter09$laundry, type = "l", col = "red")
points(winter09$DateTime, winter09$water_ac, type = "l", col = "blue")
legend("topright", lty = c(1,1,1), lwd = c(2,2,2), col = c("black", "red", "blue"),
       legend = c("kitchen", "laundry room", "water heater & AC"))

plot(summer09$DateTime, summer09$kitchen, type = "l", ylab = "Energy sub metering", xlab = "Summer 2009")
points(summer09$DateTime, summer09$laundry, type = "l", col = "red")
points(summer09$DateTime, summer09$water_ac, type = "l", col = "blue")
legend("topright", lty = c(1,1,1), lwd = c(2,2,2), col = c("black", "red", "blue"),
       legend = c("kitchen", "laundry room", "water heater & AC"))

#import data on seasons
seasontotals <- read.csv("seasontotals.csv")
avg_combined_mean <- seasontotals$total_combined_mean
sd(avg_combined_mean)
kitchen_mean <- seasontotals$kitchen_mean
sd(kitchen_mean)
winter09kitchen <- winter09$kitchen
sd(winter09kitchen)
winter09waterac <- winter09$water_ac
sd(winter09waterac)

seasontotals$seasons <- as.factor(seasontotals$seasons)
str(seasontotals)
#AvgWatthours x Seaons
plot(seasontotals$seasons, seasontotals$total_combined_mean, type = "b", 
     ylab = "AvgWattHours", xlab = "Seasons")
plot(seasontotals$seasons, seasontotals$water_ac_mean, type = "l",
     ylab = "AvgWattHours", xlab = "Seasons")
plot(seasontotals$seasons, seasontotals$laundry_mean, type = "l",
     ylab = "AvgWattHours", xlab = "Seasons")
plot(seasontotals$seasons, seasontotals$kitchen_mean, type = "l",
     ylab = "AvgWattHours", xlab = "Seasons")

jan_2009 <- filter(yr2009, year == 2009, month == 1)
week_jan_2009 <- filter(yr2009, year == 2009, month == 1, day %in% c(4,5,6))
july_2009 <- filter(yr2009, year == 2009, month == 7)
week_jul_2009 <- filter(yr2009, year == 2009, month == 7, day %in% c(13,14,15))


plot(week_jan_2009$DateTime, week_jan_2009$kitchen, type = "l", ylab = "WattHours", xlab = "Week of January 2009")
points(week_jan_2009$DateTime, week_jan_2009$laundry, type = "l", col = "red")
points(week_jan_2009$DateTime, week_jan_2009$water_ac, type = "l", col = "blue")
legend("topright", lty = c(1,1,1), lwd = c(2,2,2), col = c("black", "red", "blue"),
       legend = c("kitchen", "laundry room", "water heater & AC"))


plot(week_jul_2009$DateTime, week_jul_2009$kitchen, type = "l", ylab = "WattHours", xlab = "Week of July 2009")
points(week_jul_2009$DateTime, week_jul_2009$laundry, type = "l", col = "red")
points(week_jul_2009$DateTime, week_jul_2009$water_ac, type = "l", col = "blue")
legend("topright", lty = c(1,1,1), lwd = c(2,2,2), col = c("black", "red", "blue"),
       legend = c("kitchen", "laundry room", "water heater & AC"))
