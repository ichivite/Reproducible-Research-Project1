setwd("C:/Users/IÑIGO/Documents/coursera/")


#Folder where I am going to work for project
folder <- "./Reproducible research project 1"


# I create folder 
if(!file.exists(folder)){
  +   dir.create(folder)}




#I download data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,destfile="./Reproducible research project 1/data.zip",method="auto")
unzip(zipfile="./Reproducible research project 1/data.zip",exdir="./Reproducible research project 1")

#I set the WD where I am going to work
setwd("C:/Users/IÑIGO/Documents/coursera/Reproducible research project 1")

#Load the data into a variable
activity_raw <- read.csv("activity.csv", stringsAsFactors=FALSE)



###############
### 1. Code for reading in the dataset and/or processing the data
#################

activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")



activity_raw <- data.frame(date=activity_raw$date, weekday=tolower(weekdays(activity_raw$date)),steps=activity_raw$steps,interval=activity_raw$interval)

activity_raw <- cbind(activity_raw, daytype=ifelse(activity_raw$weekday == "sabado" | activity_raw$weekday == "domingo", "weekend", "weekday"))



activity <- data.frame(date=activity_raw$date, weekday=activity_raw$weekday, daytype=activity_raw$daytype, interval=activity_raw$interval, steps=activity_raw$steps)



######



#2. Histogram of the total number of steps taken each day

library(ggplot2)
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)

names(sum_data) <- c("date", "total")

x11()
ggplot2::ggplot(data = sum_data, mapping = aes(x= date))+ geom_histogram()



hist(sum_data$total,  breaks=seq(from=0, to=25000, by=2500),col="blue", xlab="Total number of steps", ylim=c(0, 20), main="Histogram of the total number of steps taken each day\n(NA removed)")




####3. Calculate and report the mean and median total number of steps taken per day



mean_data <- aggregate(activity$steps, by=list(activity$date), FUN=mean, na.rm=TRUE)
median_data <- aggregate(activity$steps, by=list(activity$date), FUN=median, na.rm=TRUE)

names(mean_data) <- c("date", "Daily Average")
names(median_data) <- c("date", "Median")


###########  4. Time series plot of the average number of steps taken

ggplot(mean_data, aes(x="date", y="Daily Average", na.rm=TRUE)) + geom_line() + labs(title="Time Series Chart", subtitle="Average Number of Steps Taken", caption="Source: Coursera", y="Steps taken")






########## 5. The 5-minute interval that, on average, contains the maximum number of steps


max_pos <- which(mean_data$`Daily Average` == max(mean_data$`Daily Average`, na.rm=TRUE))
max_interval <- mean_data[max_pos, 1]
print(max_interval)







####################
#Imputing missing values
#####################

#####6. Code to describe and show a strategy for imputing missing data

#NA_count <- sum(is.na(activity$steps)) #not asked for. Just checking


na_position <- which(is.na(mean_data$`Daily Average`))
input <- mean(mean_data$`Daily Average`, na.rm=TRUE)
average <- as.data.frame(replace(mean_data$`Daily Average`, na_position , input))
dates <- mean_data$date
mean_data_narm <- as.data.frame(cbind(dates, average))
View(mean_data_narm)


##### 7. Histogram of the total number of steps taken each day after missing values are imputed

x11()
ggplot2::ggplot(data = mean_data_narm, mapping = aes(x= dates))+ geom_histogram(binwidth=10)




####8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

library(lattice)

mean_data <- aggregate(activity$steps, by=list(activity$daytype,activity$weekday, activity$interval), mean)

names(mean_data) <- c("daytype", "weekday", "interval", "mean")

xyplot(mean ~ interval | daytype, mean_data, type="l", lwd=1, xlab="Interval", ylab="Number of steps", layout=c(1,2))


#####9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report


