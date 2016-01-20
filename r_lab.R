library(dplyr)
library(fitbitScraper)
library(ggplot2)

cookie <- login("jreyro@gmail.com", "password", rememberMe = TRUE)

data <- get_daily_data(cookie, what="steps", start_date="2015-02-24",  
                       end_date="2015-08-17")

rm(list = ls())

# structure of data
str(data)
# dimession of data
dim(data)
# NAs
sum(is.na(data))

data$month <- as.numeric(format(data$time, "%m"))

graph <- ggplot(data, aes(time, steps)) 
graph <- graph + geom_bar(stat = "identity", colour = "Blue", width = 0.7) 
graph <- graph + facet_grid(month ~ ., scales = "free") 
graph <- graph + labs(title = "Histogram of Total Number of Steps Taken Each Day facet by month", x = "Dates", y = "Total number of steps")
# override the theme 
theme_set(theme_bw())
graph

group_data <- summarise(group_by(data, month), mean = mean(steps), sd = sd(steps), median = median(steps), variance = var(steps))

graph <- ggplot(group_data, aes(x=month, y=mean))
graph <- graph + geom_line(colour = "Blue")
graph <- graph + labs(title = "Mean vs month", x = "Months", y = "Mean")
graph

graph <- ggplot(data, aes(x=factor(month), y=steps))
graph <- graph + geom_boxplot(colour = "Blue")
graph <- graph + labs(title = "Whisker and box", x = "Months", y = "Steps")
graph

mtcars

