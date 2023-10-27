library(tidyverse)
#The tidyverse package is an "umbrella-package" that installs several packages useful for data analysis which work together well such as tidyr(swiftly convert between different data formats), dplyr(data manipulation), ggplot2, tibble, etc. 
library(ggplot2)
library(corrplot)
library(gridExtra)
library(naniar) #check nan value
library(lessR) # pie chart
# cluster
library(NbClust) 
library(factoextra)
#library(tidyquant)


# Check the files locally using Excel and determine which file would be useful.
daily_activity <-read.csv("D:/work/job/prepare/FitnessTracker/dailyActivity_merged.csv")
daily_sleep <- read.csv("D:/work/job/prepare/FitnessTracker/sleepDay_merged.csv")
weight_log <- read.csv("D:/work/job/prepare/FitnessTracker/weightLogInfo_merged.csv")

# Check the number of unique Ids in the dataset
n_distinct(daily_activity$Id)
n_distinct(weight_log$Id)
n_distinct(daily_sleep$Id)
# The weight_log dataset may not be useful as it has only 8 contributors out of 33 Ids that we have from daily_activity dataset

# Data Structure
str(daily_activity)
str(daily_sleep)


# Make the date format consistent for daily_activity and data_sleep. We also need to rename both of their columns to "date"
# Having a same column name would make merging/joining data easier.
daily_activity <-daily_activity %>% 
  dplyr::rename(date= ActivityDate) %>%
  mutate(date= as_date(date, format= "%m/%d/%Y"))

daily_sleep <- daily_sleep %>%
  dplyr::rename(date= SleepDay) %>%
  mutate(date= as_date(date,format ="%m/%d/%Y %I:%M:%S %p" ))
  

# Checking the transformed tables
head(daily_activity,2)
glimpse(daily_activity)
head(daily_sleep,2)
glimpse(daily_sleep)

# Let's merge the sleep and activity table using Id and date as reference
merged_daily_activity <- merge(x = daily_activity, y = daily_sleep, by=c("Id","date"), all.x = TRUE)

# Checking if it merged properly
head(merged_daily_activity,2)

# Create day of the week feature
merged_daily_activity$day_week <- wday(merged_daily_activity$date, label = TRUE)


# Let’s check and deal with missing values.
cat("Number of missing value:", sum(is.na(merged_daily_activity)), "\n")
# plot percentage of missing values per feature 
# library(naniar)
gg_miss_var(merged_daily_activity,show_pct=TRUE)



# Correlation
# Correlation drop TrackerDistance (redundant with TotalDistance), sleep related columns(not all daily_activity have an equivalent daily_sleep recorded,))
data_correlation <- select(merged_daily_activity, TotalSteps:Calories, -TrackerDistance)

corrplot(cor(data_correlation))

# Correlation drop the observations with "NA" on sleep related data
data_corr_withsleep <- select(merged_daily_activity, TotalSteps:TotalTimeInBed, -TrackerDistance) %>% filter(!is.na(TotalTimeInBed))
corrplot(cor(data_corr_withsleep))



# Based on the Correlation Plot, we can see that TotalSteps, TotalDistance (also with high correlation with TotalSteps, collinear), VeryActiveDistance, VeryActiveMinutes, and surprisingly, LightActiveDistance has a high positive correlation with Calories burned. We can surmised that as long as you walk longer distance or greater steps, it won't matter if it is intense or light activity.

# Relationship of Calories burned with TotalDistance and TotalSteps
names_n <- c("TotalSteps", "VeryActiveDistance", "VeryActiveMinutes", "LightActiveDistance")
plt_list <- list()


for (name in names_n) {
  plt<-ggplot(data = merged_daily_activity, aes_string(x = merged_daily_activity$Calories, y = name)) + 
    geom_point(colour = "#33658A") + xlab('Calories')+ 
    geom_smooth()+
    theme_minimal()
  plt_list[[name]] <- plt
}
plt_grob <- arrangeGrob(grobs=plt_list, ncol=2)
grid.arrange(plt_grob)


# Analyze tracker Wearing Days by Users

user_usagedays <- merged_daily_activity %>%
  group_by(Id) %>%
  summarise(usagedays = n()) 


install.packages("lessR")
# library(lessR)

PieChart(usagedays, hole = 0.3, values = "%", data = user_usagedays,main = "Tracker Wearing Days by Users")     # hole = 0 pie chart

# https://rdrr.io/cran/lessR/man/PieChart.html


## Data transformation
user <- user_usagedays %>% 
  group_by(usagedays) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = n/sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) 

user$usagedays<- factor(user$usagedays, levels =user$usagedays)

ggplot(data = user, aes(x = usagedays, y=n)) + geom_bar(stat="identity", fill= "#F6AE2D", colour="black") + 
  ylab('Number of users') + xlab('Number of days of wearing') + ggtitle( "Tracker Wearing Days by Users") +
  geom_text(aes(label = n), vjust = 1.5, colour = "black")+
  theme_minimal()     #  "#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999"


ggplot(user, aes(x = "", y = perc, fill = usagedays)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Tracker Worn Days")) +
  coord_polar(theta = "y")+
  scale_fill_viridis_d() +
  theme_void()+    # want a blank slate 
  labs(x = NULL, y = NULL, fill = NULL, title = "Tracker Wearing Days by Users")




# Analyze user activity behavior by weekday
avg_weekday_activity <- merged_daily_activity %>% 
  group_by(day_week) %>% 
  summarise_at(c(Avgsteps = "TotalSteps", "Calories", "TotalMinutesAsleep"), mean,na.rm = TRUE) # see how it produces an error because of the "NA" record  



names <- names(Filter(is.numeric,avg_weekday_activity))
plt2_list <- list()

for (name in names) {
  plt<-ggplot(data = avg_weekday_activity, aes_string(x= avg_weekday_activity$day_week, y = name, fill = name)) + 
    geom_bar(stat="identity") + xlab('Day of Week')+    # geom_bar(stat="identity", fill= "#55DDE0", colour="black")
    scale_fill_gradient (low="light blue", high= "dark blue")+
    theme_minimal()+  
    theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=16),
                            legend.position= "none")
  plt2_list[[name]] <- plt
}
plt2_grob <- arrangeGrob(grobs=plt2_list, ncol=2)
grid.arrange(plt2_grob)


# Analyze user types
# Method 1, Based on daily steps, we group users into 4 types: Sedentary, Lightly Active, Fairly Active, and Very Active. 

daily_average <- merged_daily_activity %>% 
  group_by (Id) %>% 
  summarise(avg_daily_steps= mean(TotalSteps), 
            avg_daily_cal= mean(Calories), 
            avg_daily_sleep= mean(TotalMinutesAsleep, 
                                  na.rm = TRUE)) %>% 
  mutate(user_type= case_when(
    avg_daily_steps < 5000 ~ "sedentary",
    avg_daily_steps >= 5000 & avg_daily_steps <7499 ~"lightly_active",
    avg_daily_steps >= 7499 & avg_daily_steps <9999 ~"fairly_active",
    avg_daily_steps >= 10000 ~"very_active"
  ))

# summarize life style of users
user_type_sum <- daily_average %>%
  group_by(user_type) %>%
  summarise(user_n= n()) %>%
  mutate(user_perc= user_n/sum(user_n))


ggplot(user_type_sum, aes(x = 1, y = user_perc, fill = user_type)) +
  geom_col(color = "black") +
  geom_text(aes(label = scales::percent(user_perc)), colour = "white", position = position_stack(vjust = 0.5))  +
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Life styles")) +
  scale_fill_viridis_d() +
  theme_void()+    # want a blank slate 
  labs(x = NULL, y = NULL, fill = NULL, title = "Life styles by Users")

# Compare each life style: steps, calories, distance & sleep
p1<-ggplot(daily_average[daily_average[,"avg_daily_steps"] >0, ],    #daily_average[which(daily_average$TotalSteps>0),]
           aes(user_type,avg_daily_steps, fill=user_type))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Daily Steps by User Type", 
       x= " ", y="Steps",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")


p2<-ggplot(daily_average[daily_average[,"avg_daily_cal"] >0, ],   
           aes(user_type,avg_daily_cal, fill=user_type))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Daily Burned Calories by User Type", 
       x= " ", y="Calories",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")

p3<-ggplot(na.omit(daily_average[daily_average[,"avg_daily_sleep"] >0, ]),    
           aes(user_type,avg_daily_sleep, fill=user_type))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= " Daily Asleep by User Type", 
       x= " ", y="Minutes Asleep",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")
grid.arrange(p1,p2,p3,ncol=2)






group_daily_activity <- merge(merged_daily_activity, daily_average[c("Id","user_type")], by="Id") 

p1<-ggplot(group_daily_activity[group_daily_activity[,"TotalSteps"] >0, ],    #group_daily_activity[which(group_daily_activity$TotalSteps>0),]
       aes(user_type,TotalSteps, fill=user_type))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Daily Steps by User Type", 
       x= " ", y="Steps",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
       )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")


p2<-ggplot(group_daily_activity[group_daily_activity[,"Calories"] >0, ],    #group_daily_activity[which(group_daily_activity$TotalSteps>0),]
           aes(user_type,Calories, fill=user_type))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Daily Burned Calories by User Type", 
       x= " ", y="Calories",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")

p3<-ggplot(na.omit(group_daily_activity[group_daily_activity[,"TotalMinutesAsleep"] >0, ]),    #group_daily_activity[which(group_daily_activity$TotalSteps>0),]
           aes(user_type,TotalMinutesAsleep, fill=user_type))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= " Daily Asleep by User Type", 
       x= " ", y="Minutes Asleep",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")
grid.arrange(p1,p2,p3,ncol=2)

# Method 2: use clustering method to segment users
user_average <- merged_daily_activity %>% 
  group_by(Id) %>% 
  summarise_at(c(2:17),mean,na.rm = TRUE) %>% 
  rename_with(~str_replace(., 'Total', 'Avg'))  %>%
  mutate(Id = as.character(Id))

corrplot(cor(select(user_average, AvgSteps:Calories, -TrackerDistance)))

# Scale the data
user_average_scaled <- scale(select(user_average, AvgSteps,VeryActiveMinutes:Calories))

# Determine how many clusters we can make
# library(NbClust) 
nb <- NbClust(user_average_scaled, distance = "euclidean", min.nc = 2,
              max.nc = 6, method = "kmeans")
#fviz_nbclust(nb)

# We will try to use 3 clusters to group these users for simplicity
km_res <- kmeans(user_average_scaled, centers = 3, nstart = 35)
# library(factoextra)
fviz_cluster(km_res, geom = "point", data=user_average_scaled,ggtheme = theme_minimal())+ggtitle("k-means clustering K=3")

# Lets also check the amount of customers in each cluster.
km_res$size
# centroids from model on normalized data
km_res$centers 


# Analysis clustering results
# add cluster into user_average dataset
user_average <- user_average %>% 
  mutate(Cluster = km_res$cluster)

user_average <- user_average %>% 
  mutate(Segment = ifelse(Cluster == 1, "Very Active",
                          ifelse(Cluster == 2, "Lightly Active", "Sedentary")))

library(kableExtra)
user_K3 <- user_average %>% select(Segment, AvgSteps,VeryActiveMinutes:Calories)%>% group_by(Segment) %>% summarise_all("mean") %>% 
  ungroup() %>% kable() %>% kable_styling()
user_K3


## compare by users
user2_type_sum <- user_average %>%
  group_by(Segment) %>%
  summarise(user_n= n()) %>%
  mutate(user_perc= user_n/sum(user_n))%>% 
  arrange(user_perc)


ggplot(user2_type_sum, aes(x = 1, y = user_perc, fill = Segment)) +
  geom_col(color = "black") +
  geom_text(aes(label =scales::percent(user_perc)), colour = "white",  position = position_stack(vjust = 0.5))  +
  coord_polar(theta = "y", start=0)+
  guides(fill = guide_legend(title = "Life styles")) +
  scale_fill_viridis_d() +
  theme_void()+    # want a blank slate 
  labs(x = NULL, y = NULL, fill = NULL, title = "Life styles by Users -- Kmean")


p1<-ggplot(user_average[user_average[,"AvgSteps"] >0, ],    #group_daily_activity[which(group_daily_activity$TotalSteps>0),]
           aes(Segment,AvgSteps, fill=Segment))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Daily Steps by User Type", 
       x= " ", y="Steps",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")


p2<-ggplot(user_average[user_average[,"Calories"] >0, ],    #group_daily_activity[which(group_daily_activity$TotalSteps>0),]
           aes(Segment,Calories, fill=Segment))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Daily Burned Calories by User Type", 
       x= " ", y="Calories",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")

p3<-ggplot(na.omit(user_average[user_average[,"AvgMinutesAsleep"] >0, ]),    #group_daily_activity[which(group_daily_activity$TotalSteps>0),]
           aes(Segment,AvgMinutesAsleep, fill=Segment))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= " Daily Asleep by User Type", 
       x= " ", y="Minutes Asleep",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")
grid.arrange(p1,p2,p3,ncol=2)


p4<-ggplot(user_average[user_average[,"VeryActiveMinutes"] >0, ],    #group_daily_activity[which(group_daily_activity$TotalSteps>0),]
           aes(Segment,VeryActiveMinutes, fill=Segment))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Very Active Minutes  by User Type", 
       x= " ", y="VeryActiveMinutes ",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")


p5<-ggplot(user_average[user_average[,"FairlyActiveMinutes"] >0, ],    #group_daily_activity[which(group_daily_activity$TotalSteps>0),]
           aes(Segment,FairlyActiveMinutes, fill=Segment))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Fairly Active Minutes by User Type", 
       x= " ", y="Fairly Active Minutes ",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")

p6<-ggplot(user_average[user_average[,"LightlyActiveMinutes"] >0, ],    #group_daily_activity[which(group_daily_activity$TotalSteps>0),]
           aes(Segment,LightlyActiveMinutes, fill=Segment))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Lightly Active Minutes by User Type", 
       x= " ", y="Lightly Active Minutes ",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")

p7<-ggplot(na.omit(user_average[user_average[,"SedentaryMinutes"] >0, ]),    #group_daily_activity[which(group_daily_activity$TotalSteps>0),]
           aes(Segment,SedentaryMinutes, fill=Segment))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Sedentary Minutes by User Type", 
       x= " ", y="Sedentary Minutes   ",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=12),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")
grid.arrange(p4,p5,p6,p7, ncol=2)





# Analyze Heart Rate
heartrate <- read.csv("D:/work/job/prepare/FitnessTracker/heartrate_seconds_merged.csv")

# check data structure
str(heartrate)
n_distinct(heartrate$Id)

# Summary Statistics
summary(heartrate)
ggplot(data = heartrate, aes(x = Value)) + 
  geom_histogram(aes(y=100*(..count..)/sum(..count..)), color='black', fill="#55DDE0") + ylab('percentage') + 
  ggtitle("Heartrate Histogram")+
  theme_minimal() 

# hist(heartrate$Value)


# Find anomaly value
heartrate <- heartrate %>% 
  mutate(Time = mdy_hms(Time), Weekday=weekdays(Time), Hour= format(Time, format = "%H"), Date= format(Time, format = "%m-%d-%y"))

heartrate_anomaly <- heartrate %>% filter(Value >= 200)

distinct(heartrate %>% filter(Value >= 200), Id)

boxplot(heartrate[which(heartrate$Id==2022484408),]$Value)

########################
heartrate_p1 <- heartrate[which(heartrate$Id==2022484408),] %>%
  group_by(Date, Hour) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()


ggplot(heartrate_p1, aes(x=Date, y=Value)) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0)) +
  theme_minimal()
######################



heartrate_p2 <- heartrate[heartrate$Id==2022484408 & heartrate$Date=="04-21-16",]
head(heartrate_p2)

library(tidyquant)
ggplot(heartrate_p2, aes(x=Time, y=Value)) + 
  geom_line(color = 'darkgreen') + 
  geom_hline(aes(yintercept=200), colour="#990000", linetype="dashed") + #This is our control line 
  geom_text(x=strptime("2016-04-21 07:30:00", "%Y-%m-%d %H:%M:%S"), y=204, label="Heart Rate = 200") +
  geom_text(x=strptime("2016-04-21 12:00:00", "%Y-%m-%d %H:%M:%S"), y=100, label="1 hour SMA Line") +
  geom_ma(ma_fun = SMA, n = 720, color = 'blue', size = 5) +   # plot 1 hour moving average line
  theme(axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0)) +
  labs(title= "Heart Rate Of User ID = 2022484408 in April 12th", 
       y="Heart Rate BPM",
  )+
  theme_minimal()  


heartrate_avg <- heartrate %>%
  group_by(Id) %>%
  summarise(Avgheartrate = mean(Value)) %>%
  ungroup()


merged_heartrate <- merge(heartrate_avg, user_average[c("Id","Segment")], by="Id", all.x = TRUE)

ggplot(merged_heartrate,aes(Segment,Avgheartrate, fill=Segment))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Average Heart Rate by User Type", 
       x= " ", y="Heart Rate",
       #caption= 'Data Source: Fitabase Data 4.12.16-5.12.16'
  )+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=16),axis.text.x = element_text(angle = 15, vjust = 1.5, hjust=0.5),
        legend.position= "none")

  




