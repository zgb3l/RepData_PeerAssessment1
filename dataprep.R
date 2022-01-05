library(readr)
library(tidyverse)

activity <- read_csv("activity.csv")

# Exploring the dataset. 

# glimpse(activity)
# sum(is.na(activity))
# summary(activity)

# Finding the weekdays and binding the column to the original set.

activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)

# Finding the total steps per day and defining column names.

activity_total_steps <- with(activity, aggregate(steps, by = list(date), 
                                                 FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("date", "steps")

# Making the histogram.

ggplot(activity_total_steps, aes(x=steps)) +
  geom_histogram(bins=20, color = "#5D7791", fill = "#7697B8") +
  theme_minimal() +
  labs(title="Total number of steps taken per day", x="Number of steps", 
       y = "Frequency")

# Calculating the mean and median number of steps.

mean(activity_total_steps$steps)
median(activity_total_steps$steps)

# Average daily activity pattern and defining column names.
average_daily_activity <- aggregate(activity$steps, by=list(activity$interval),
                                    FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")

# Making the line plot for activity interval.

plot(average_daily_activity$interval, average_daily_activity$mean, 
     type = "l", col="#5D7791", lwd = 2, xlab="Interval", 
     ylab="Average number of steps", 
     main="Average number of steps per interval")

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

average_daily_activity[which.max(average_daily_activity$mean), ]$interval

# Calculate and report the total number of missing values in the dataset.

sum(is.na(activity$steps))

# Strategy

imputed_steps <- average_daily_activity$mean[match(activity$interval, 
                                              average_daily_activity$interval)]

# New dataset without na's.

activity_imputed <- transform(activity, 
                              steps = ifelse(is.na(activity$steps), 
                                             yes = imputed_steps, 
                                             no = activity$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
names(total_steps_imputed) <- c("date", "daily_steps")

# Make a histogram.

ggplot(total_steps_imputed, aes(x=daily_steps)) +
  geom_histogram(bins=8, color = "#5D7791", fill = "#7697B8") +
  theme_minimal() +
  labs(title="Total number of steps taken each day", x="Number of steps", 
       y = "Frequency") +
  ylim(0,30)

# Mean and median.

mean(total_steps_imputed$daily_steps)
median(total_steps_imputed$daily_steps)

# Finding difference weekdays and weekends.

activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))

# Dividing weekdays from weekends.

activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "zaterdag" | weekdays(x) =="zondag") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})

# Making panel plot.

activity_by_day <- aggregate(steps~interval + datetype, activity, mean, 
                             na.rm = TRUE)

panelplot <- ggplot(activity_by_day, aes(x = interval , 
                                          y = steps, 
                                          color = datetype)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Average daily steps by type of date", 
       x = "Interval", y = "Average number of steps") +
  labs(color="Day type") +
  facet_wrap(~datetype, ncol = 1, nrow=2)

print(panelplot)





