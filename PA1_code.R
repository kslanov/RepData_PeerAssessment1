##loading libraries

library(dplyr)
library(stringr)
library(reshape2)
library(chron)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
##-----------------------------------FILE PREPARATION------------------------------------

##!!!!!!!!!!!!!!!!must enter the path for the directory with unzipped data files!!!!!!!!!

## Directory path for data files
dirPath <- getwd()
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip', destfile = paste(dirPath, '/activity.zip', sep = ''))
unzip(zipfile = 'activity.zip', exdir = getwd())


##--------------------------READING AND CONVERTING DATA----------------------------------
## Reading data
activity_dt <- read.table(file = 'activity.csv', header = TRUE, sep = ',')

##Transforming data types
activity_dt$date <- as.Date(activity_dt$date, origin = '2012-10-01')
activity_dt_NM <- na.omit(activity_dt)

##-----------------------------------PLOTTING-------------------------------------------
##Question 1
#calculating steps by day
steps_ByDay <- aggregate(activity_dt$steps, by = list(activity_dt$date), sum)
colnames(steps_ByDay) <- c('Days', 'Total_steps')
#calculating mean & median
steps_mean <- round(mean(steps_ByDay$Total_steps, na.rm = TRUE), digits = 4) 
steps_median <- round(median(steps_ByDay$Total_steps, na.rm = TRUE), digits = 4)
#plotting histogram
ggplot(data = steps_ByDay, aes(x=Days, y = Total_steps)) +
              geom_histogram(stat = 'identity', aes(fill = Total_steps)) +
#adding mean & median geoms
              geom_hline(aes(yintercept = steps_mean, color = paste('Mean: ', steps_mean)), linetype = 2, size = 1) +
              geom_hline(aes(yintercept = steps_median, color = paste('Median: ', steps_median)), linetype = 2, size = 1) +
              scale_color_manual('Mean & Median', values = c(1, 2))

##Question 2
steps_ByInterval <- aggregate(activity_dt_NM$steps, by = list(activity_dt_NM$interval), mean)
colnames(steps_ByInterval) <- c('Interval', 'Avg_steps')
max_AvgSteps <- max(steps_ByInterval$Avg_steps)
max_interval <- with(steps_ByInterval, Interval[Avg_steps == max_AvgSteps])
plot(steps_ByInterval$Interval, steps_ByInterval$Avg_steps, type = 'l', xlab = 'Timestamp interval', ylab = 'Average steps')
legend(1200, 185, legend=c(paste("Maximum steps: ", round(max_AvgSteps), digits = 4),paste("Maximum steps interval: ", max_interval)))

##Question 3
total_NA <- as.integer(nrow(activity_dt) - nrow(activity_dt_NM))
#adding column with average steps values by interval
activity_dt_steps <- merge(activity_dt, steps_ByInterval, by.x = 'interval', by.y = 'Interval')
#filling missing values with average values
activity_dt_steps$steps <- ifelse(is.na(activity_dt_steps$steps), activity_dt_steps$Avg_steps, activity_dt_steps$steps)
#calculating steps by day
steps_ByDay2 <- aggregate(activity_dt_steps$steps, by = list(activity_dt_steps$date), sum)
colnames(steps_ByDay2) <- c('Days', 'Total_steps')
#calculating mean & median
steps_mean2 <- round(mean(steps_ByDay2$Total_steps, na.rm = TRUE), digits = 4) 
steps_median2 <- round(median(steps_ByDay2$Total_steps, na.rm = TRUE), digits = 4)
#plotting histogram
ggplot(data = steps_ByDay2, aes(x=Days, y = Total_steps)) +
              geom_histogram(stat = 'identity', aes(fill = Total_steps)) +
  #adding mean & median geoms
              geom_hline(aes(yintercept = steps_mean2, color = paste('Mean: ', steps_mean2)), linetype = 2, size = 1) +
              geom_hline(aes(yintercept = steps_median2, color = paste('Median: ', steps_median2)), linetype = 2, size = 1) +
              scale_color_manual('Mean & Median', values = c(1, 2))

##Question 4
activity_dt_steps <- activity_dt_steps %>% mutate(day = weekdays(date))
#function for checking whether a day is a weekday or a weekend day
check_day <- function(day) {
  ifelse(day %in% c('понедельник', 'вторник', 'среда', 'четверг', 'пятница'), return('weekday'), return('weekend'))
}
#adding a weekday/weekend column
for (i in 1: nrow(activity_dt_steps)) {
  activity_dt_steps$dayType[i] <- check_day(activity_dt_steps$day[i])
}  
#plotting steps by day type
steps_ByInterval_mean <- aggregate(steps~interval+dayType, data=activity_dt_steps, mean)
xyplot(steps~interval|factor(dayType), data=steps_ByInterval_mean, aspect=1/2, type="l")