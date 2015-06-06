# Reproducible Research: Peer Assessment 1
Radu Craioveanu  


## Loading and preprocessing the data


```r
require(knitr)
```

```
## Loading required package: knitr
```

```r
knitr::opts_chunk$set(echo=TRUE, fig=TRUE)
```


```r
dataPath <- "." 
#download dataset if not there already
fileName <- "activity.zip" 
filePath <- paste(dataPath,fileName,sep="/") 
if (!file.exists(filePath)) {  
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivit.zip" 
  download.file(url=fileURL,destfile=filePath,method="curl") 
  #Extract the data set files from the archive
  unzip(zipfile=filePath, exdir=dataPath) 
}
#read the file now into a data frame
df <- read.csv("activity.csv")
```



## What is mean total number of steps taken per day?

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#calculate the total number of steps taken per day 
dfday <- summarise(group_by(df,date), sum(steps))
names(dfday)[2] <- "SumOfStepsPerDay"
#remove NA
dfday <- na.omit(dfday)
#list the aray of Steps
dfday$SumOfStepsPerDay
```

```
##  [1]   126 11352 12116 13294 15420 11015 12811  9900 10304 17382 12426
## [12] 15098 10139 15084 13452 10056 11829 10395  8821 13460  8918  8355
## [23]  2492  6778 10119 11458  5018  9819 15414 10600 10571 10439  8334
## [34] 12883  3219 12608 10765  7336    41  5441 14339 15110  8841  4472
## [45] 12787 20427 21194 14478 11834 11162 13646 10183  7047
```

```r
#create a histogram of the numbers of steps taken each day
hist(dfday$SumOfStepsPerDay, xlab = "Sum of Steps per Day", ylab = "Frequency", main = "Histogram of Sum of Steps per Day")
```

![](PA1_template_files/figure-html/Run mean() while ignoring NA values-1.png) 

```r
#get the mean steps per day
mean(dfday$SumOfStepsPerDay, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
#get the median steps per day
median(dfday$SumOfStepsPerDay, na.rm=TRUE)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
avginterval <- aggregate(x=list(avgsteps = df$steps), by = list(interval = df$interval), FUN = mean, na.rm = TRUE)
ggplot(data=avginterval, aes(x=interval, y = avgsteps)) + geom_line() + xlab("5 Minute Interval") + ylab("Average of Steps per Interval") + ggtitle("Average Daily Pattern")
```

![](PA1_template_files/figure-html/find and plot the daily pattern-1.png) 


## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  We will replace the missing values with the average for that interval

```r
#calculate the number of missing rows with NA
missList <- is.na(df$steps)
#FALSE and TRUE count, TRUE being the number of missing values
table(missList)
```

```
## missList
## FALSE  TRUE 
## 15264  2304
```

```r
#create a merged list from the original data and the average interval data so that we can then replace the NA with the average data
dfm <- merge(df,avginterval,by.x = "interval", by.y = "interval", all)
for(i in 1:nrow(dfm) ) { if(is.na(dfm[i,"steps"])) dfm[i,"steps"] <- dfm[i,"avgsteps"]}
#now aggregate back to daily averages
dfmday <- summarise(group_by(dfm,date), sum(steps))
#change the sum column name 
names(dfmday)[2]="SumOfStepsPerDay"
#now run the histogram
hist(dfmday$SumOfStepsPerDay, xlab = "Sum of Steps per Day", ylab = "Frequency", main = "Histogram of Sum of Steps per Day with filled NA values")
```

![](PA1_template_files/figure-html/replacing missing values-1.png) 

```r
#get the mean steps per day
mean(dfmday$SumOfStepsPerDay)
```

```
## [1] 10766.19
```

```r
#get the median steps per day
median(dfmday$SumOfStepsPerDay)
```

```
## [1] 10766.19
```
#The impact of replacing NA values with the average Steps per index is minimal

```r
#mean with NA replaced
mean(dfday$SumOfStepsPerDay)
```

```
## [1] 10766.19
```

```r
#mean with NA removed
mean(dfmday$SumOfStepsPerDay)
```

```
## [1] 10766.19
```

```r
#median with NA replaced
median(dfmday$SumOfStepsPerDay)
```

```
## [1] 10766.19
```

```r
#median with NA removed
median(dfday$SumOfStepsPerDay)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?

```r
dfms <- dfm
mydates <- as.Date(dfms$date)
weekends <- weekdays(mydates) %in% c("Saturday", "Sunday")
dfms <- transform(dfms, week = ifelse(weekends ,'weekend', 'weekday'))
dfms$week <- as.factor(dfms$week)
```

#Graph the Average Steps per day on Weekdays and Weekends.


```r
#clean up the NAs
dfms <- na.omit(dfms)
dfmDay <- aggregate(dfms, list(interval = dfms$interval, week = dfms$week), FUN = mean, na.rm = TRUE)
```


```r
ggplot(dfmDay, aes(x = interval, y = steps)) + geom_line() + facet_wrap(~week, nrow=2) + xlab("Interval") + ylab("Number of steps") + ggtitle("Average Daily Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
