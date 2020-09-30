##loading libraries

library(dplyr)
library(stringr)
library(reshape2)
library(chron)
library(data.table)
library(ggplot2)
library(grid)
##-----------------------------------FILE PREPARATION------------------------------------

##!!!!!!!!!!!!!!!!must enter the path for the directory with unzipped data files!!!!!!!!!

## Directory path for data files
dirPath <- getwd()
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
steps_mean <- round(mean(steps_ByDay$Total_steps, na.rm = TRUE), digits = 2) 
steps_median <- median(steps_ByDay$Total_steps, na.rm = TRUE)
#plotting histogram
ggplot(data = steps_ByDay, aes(x=Days, y = Total_steps)) +
              geom_histogram(stat = 'identity', aes(fill = Days)) +
#adding mean & median geoms
              geom_hline(yintercept = steps_mean, color = 'blue', size = 1, show.legend = TRUE, label = 'Median') +
              geom_text(aes(0, steps_mean, label = paste('Mean: ', steps_mean), hjust = 0, vjust = 1)) +
              geom_hline(yintercept = steps_median, color = 'green', size = 1, show.legend = TRUE) +
              geom_text(aes(0, steps_mean, label = paste('Median: ', steps_median), hjust = -3, vjust = 1))

##Question 2
steps_ByInterval <- aggregate(activity_dt_NM$steps, by = list(activity_dt_NM$interval), mean)
colnames(steps_ByInterval) <- c('Interval', 'Avg_steps')
plot(steps_ByInterval$Interval, steps_ByInterval$Avg_steps, type = 'l', xlab = 'Timestamp interval', ylab = 'Average steps')
max_AvgSteps <- max(steps_ByInterval$Avg_steps)
max_interval <- steps_ByInterval$Interval[, steps_ByInterval$Avg_steps == max_AvgSteps]

##Question 3
total_NA <- as.integer(nrow(activity_dt) - nrow(activity_dt_NM))
activity_dt_steps <- merge(activity_dt, steps_ByInterval, by.x = 'interval', by.y = 'Interval')
for (i in list(nrow(activity_dt_steps))) {
  if (is.na(activity_dt_steps$steps[i])) {
    activity_dt_steps$steps[i] <- activity_dt_steps$Avg_steps[i]
    print ("hi")
  }
}

