
##1- Code for reading in the dataset and/or processing the data
Q1 <- read.csv("activity.csv")
sum(is.na(Q1))
Q1_no_na <- na.omit(Q1)


##2- Histogram of the total number of steps taken each day
h1 <- aggregate(steps ~ date, Q1_no_na, sum)

hist(h1$steps, main= "STEPS BY DAY", xlab="steps", ylab="Frecuency",col="purple",breaks = 20)

##3- Mean and median number of steps taken each day

mean(h1$steps)
median(h1$steps)

##4- Time series plot of the average number of steps taken

library(ggplot2)
par(mar = c(1.5, 1.5, 1, 1))
plot_time_series <- aggregate(steps ~ interval, Q1_no_na, mean)

ggplot(plot_time_series, aes(interval, steps)) +
  geom_line() +
  ggtitle("Time series plot of steps by interval") +
  xlab("Interval") +
  ylab("Number of steps")

##5- The 5-minute interval that, on average, contains the maximum number of steps

max_number_steps <- which.max(plot_time_series$steps)
print(plot_time_series[max_number_steps,])



##6- Code to describe and show a strategy for imputing missing data
number_na <- sum(is.na(Q1$steps))
print(number_na)


fill_na <- function(steps, interval) {
  fill_ac <- NA
  if (!is.na(steps))
    fill_ac <- c(steps)
  else
    fill_ac <- (plot_time_series[plot_time_series$interval==interval, "steps"])
  return(fill_ac)
}

new_activity <- Q1
new_activity$steps <- mapply(fill_na, new_activity$steps, new_activity$interval)
step_with_missings <- aggregate(steps ~ date, new_activity, sum)

##7- Histogram of the total number of steps taken each day after missing values are imputed

hist(
  step_with_missings$steps, 
  main = "Total number of steps per day", 
  xlab="Steps per day", 
  ylab="Frequency", 
  col = "purple", 
  breaks = 20
)


##8- Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

week_Q1 <- new_activity

week_Q1$date <- as.Date(strptime(week_Q1$date, format="%Y-%m-%d")) 

week_Q1$day <-  factor(ifelse(as.POSIXlt(week_Q1$date)$wday %in% c(0,6), 'weekend', 'weekday'))

averaged_week_Q1 <- aggregate(steps ~ interval + day, data=week_Q1, mean)

library(ggplot2)
ggplot(averaged_week_Q1, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(day ~ .) +
  xlab("5-minute interval") + 
  ylab("avarage number of steps")