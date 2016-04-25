
# Loading the necessary packages :
library(dplyr) # Great package to work with dfs
library(tidyr) # ^^
library(ggplot2) # For plotting I just love ggplot2
library(lubridate) # For working with the dates
library(grid)
library(gridExtra)

# Function to do imputation
imputeNA <- function(steps,interval,Int_averages)
{ # If Steps has NA -> replace with a mean for that interval
for (i in 1:length(steps))
{
  step = steps[i]
  if (is.na(step)){
    steps[i] = Int_averages$StepsPerInt[Int_averages$interval ==interval[i]]
  } 
}
  steps
}


activity_list <- tbl_df(read.csv("./activity.csv")) # Read the data into a dataframe
# Dataset grouped by date
activity_list_1 <- activity_list %>% filter(!is.na(steps)) %>%  # Filter the NAs
  group_by(date) %>% # Group by the date
  summarize(StepsPerDay = sum(steps)) # Sum of the steps elements in each group
# Dataset grouped by interval
activity_list_2 <- activity_list %>% filter(!is.na(steps)) %>%  # Filter the NAs
  group_by(interval) %>% # Group by the interval
  summarize(StepsPerInt = mean(steps)) # Sum of the steps elements in each group

# Before imputing values,
p1 <- qplot(StepsPerDay,data = activity_list_1,geom = "histogram",bins = 20,main="Histogram of total steps per day")
Mean_Median <-as.data.frame(t(c(mean(activity_list_1$StepsPerDay),median(activity_list_1$StepsPerDay))))
colnames(Mean_Median) <- c("Mean of steps per day","Median of steps per day")
p2 <- qplot(interval,StepsPerInt,data = activity_list_2,geom="line",main=" Average # steps per interval")
print(Mean_Median)
grid.arrange(p1,p2,ncol=2)

# Now impute the values: I will use the mean for the corresponding interval:
activity_list_3 <- activity_list %>% mutate(steps = imputeNA(steps,interval,activity_list_2))
grouped_imputed  <- activity_list_3 %>% 
  group_by(date) %>% # Group by the date
  summarize(StepsPerDay = sum(steps)) # Sum of the steps elements in each group
p3 <- qplot(StepsPerDay,data = grouped_imputed,geom = "histogram",bins = 20,main="Histogram of total steps per day")

grid.arrange(p1,p3,ncol=2)

# Calculate the mean and the median numbers to see the change:
Mean_Median_Im <-as.data.frame(t(c(mean(grouped_imputed$StepsPerDay),median(grouped_imputed$StepsPerDay))))
colnames(Mean_Median_Im) <- c("(Imputed) Mean of steps per day","Median of steps per day")
print(Mean_Median_Im)
# Now use the imputed dataset to analyze the changes in the activity pattern between weekdays and weekends
# Make a isweekend function:
is.weekend <- function(x)
{
  if (x %in% c("Sat","Sun"))
  {
    y = "Weekend"
  }
  else {y = "Weekday"}
}
# Augment the imputed dataset with the is-week-end flags.
grouped_by_weekday <- activity_list_3 %>% mutate(isweekend = as.factor(sapply(wday(date,label=TRUE),is.weekend))) %>%
                    group_by(interval,isweekend) %>% summarize(StepsPerInt = mean(steps)) 
p4 <- qplot(interval,StepsPerInt,data = grouped_by_weekday,facets = isweekend~.,colour = isweekend)
print(p4)


















