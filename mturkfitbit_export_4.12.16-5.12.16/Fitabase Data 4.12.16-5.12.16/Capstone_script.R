
setwd("/cloud/project/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16") 

# Installing and loading data packages 

install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("scales")
install.packages("RColorBrewer")
library(RColorBrewer)
library(scales)
library(tidyverse)
library(skimr)
library(janitor)

## Step 2: Import data
setwd("/cloud/project/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16") 
daily_activity <- read_csv(file = "dailyActivity_merged.csv")
sleep_logs <- read_csv(file = "sleepDay_merged.csv")
weight_logs <- read_csv(file = "weightLogInfo_merged.csv")
steps_hour <- read_csv(file = "hourlySteps_merged.csv")

# Previewing data

head(daily_activity,3)
head(sleep_logs,3)
head(weight_logs,3)
head(steps_hour,3)



## Step 3: Cleaning data 
daily_activity <- clean_names(daily_activity) #Clean_name function came from janitor package
sleep_logs <- clean_names(sleep_logs)
weight_logs <- clean_names(weight_logs)
steps_hour <- clean_names(steps_hour)

# Step 3.1: Checking duplicates.

get_dupes(daily_activity) #no duplicate combination of data found
get_dupes(sleep_logs) #found duplicates
sleep_duplicates <- get_dupes(sleep_logs) #found duplicates, saved them to anti-join later
get_dupes(weight_logs) #no duplicate combination of data found
get_dupes(steps_hour) #no duplicate combination of data found

#anti-join sleep_logs and sleep_duplicates to delete duplicates
sleep_logs <- anti_join(sleep_logs, sleep_duplicates)
rm(sleep_duplicates) # rm code is used for remove objects.
get_dupes(sleep_logs) # no duplicates found

# Step 3.2: Checking data types.

sapply(steps_hour,class) # sapply is used to check data types activity_hour is character,when it should be date-time

#Changing dates to date format
daily_activity$activity_date <- as.Date(strptime(daily_activity$activity_date, "%m/%d/%Y"))
sleep_logs$sleep_day <- as.Date(strptime(sleep_logs$sleep_day, "%m/%d/%Y"))
weight_logs$date <- as.Date(strptime(weight_logs$date, "%m/%d/%Y"))
steps_hour$activity_hour <- strptime(steps_hour$activity_hour, "%m/%d/%Y %I:%M:%S %p")

# finds the location of missing values

which(is.na(sleep_logs))

# finds the count of missing values 

sum(is.na(daily_activity))

#if null values found we use na.omit function to remove them

# Analysis
# To know the usage of watches we will assume that if person is taking less than 200 steps they are not using the device.
#We will filter out these inactive day and assign the following designations:
#Low Use - 1 to 14 days o Moderate Use - 15 to 21 days High Use - 22 to 31 days Breaking down the analysis further in this way will help us understand the different trends underlying each Usage Groups.

#data transformation to create dataframe for 'Usage Types' 

daily_use2 <- daily_activity %>%
  filter(total_steps >200 ) %>% 
  group_by(id) %>%
  summarize(activity_date=sum(n())) %>%
  mutate(Usage = case_when(
    activity_date >= 1 & activity_date <= 14 ~ "Low Use",
    activity_date >= 15 & activity_date <= 21 ~ "Moderate Use", 
    activity_date >= 22 & activity_date <= 31 ~ "High Use")) %>% 
  mutate(Usage = factor(Usage, level = c('Low Use','Moderate Use','High Use'))) %>% 
  rename(daysused = activity_date) %>% 
  group_by(Usage)
head(daily_use2)

#data transformation to create dataframe that is readable to create a pie chart
##the 'scales' package is used here

daily_use <- daily_activity %>% 
  left_join(daily_use2, by = 'id') %>%
  group_by(Usage) %>% 
  summarise(participants = n_distinct(id)) %>% # count the distinct values.
  mutate(perc = participants/sum(participants)) %>% # create column per for percentage
  arrange(perc) %>% # arrange is ascending order
  mutate(perc = scales::percent(perc))
head(daily_use)

#plot pie chart for distribution for different types of users
ggplot(daily_use,aes(fill=Usage ,y = participants, x="")) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  scale_fill_brewer(palette='Pastel1')+
  theme_void()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5,vjust= -3, size = 18, face = "bold")) +
  geom_text(aes(label = perc, x=1.2),position = position_stack(vjust = 0.5))+
  labs(title="Usage Group Distribution")+
  guides(fill = guide_legend(title = "Usage Type"))
options(repr.plot.width = 1, repr.plot.height = 1)

# IT'S CONCLUDED THAT 73% USERS HAVE HIGH USE OF THE DEVICES WGIKE 21% ARE USING IT MODERATELY AND 6% HAVE LOW USE.
# First find out the average of the steps.

#data manipulation to add Usage Types to 'daily_activity' df

daily_activity_usage <- daily_activity %>% 
  left_join(daily_use2, by = 'id') %>% 
  mutate(day = format(ymd(activity_date), format = '%a')) %>% 
  mutate(total_minutes_worn = sedentary_minutes+lightly_active_minutes+
           fairly_active_minutes+very_active_minutes) %>% 
  mutate(total_hours = seconds_to_period(total_minutes_worn * 60))
head(daily_activity_usage,6)

#data for steps 
hourly_steps <- daily_activity_usage %>% 
  group_by(day) %>%   
  summarise(mean_steps = round(mean(total_steps))) %>%
  mutate(day = factor(day, level = c('Mon', 'Tue', 'Wed','Thu', 'Fri', 'Sat', 'Sun')))
head(hourly_steps)

#plot for avg steps by day 
ggplot(hourly_steps, aes(x = day, y= mean_steps, fill = mean_steps)) +
  geom_col(color="darkblue", size = 0.1) +  
  scale_fill_gradientn(limits=c(0,10000), breaks=seq(0,10000, by = 2000), colours = brewer.pal(5, "Greens")) + 
  scale_y_continuous(limits=c(0,10000), breaks=seq(0, 10000, by = 2000))+ 
  labs(title= ("Average Steps"), subtitle = ('By Day'), x="" , y="Calories")+
  theme(plot.title=element_text(size = 16,hjust = 0))+
  theme(plot.subtitle=element_text(size = 14,hjust = 0))+
  theme(axis.text.y=element_text(size=14)) +
  theme(axis.text.x=element_text(size=14,hjust= 0.5))+
  theme(axis.title.x = element_text(margin = margin(t = 14, r = 0, b = 0, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(legend.position = "top")+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=8))+
  guides(fill = guide_colourbar(barwidth = 12))
options(repr.plot.width = 10, repr.plot.height = 8)


#Trends in Time

steps_hour$hour <- strftime(steps_hour$activity_hour, "%H:%M") #remove the date and just have time

p <- steps_hour %>%
  group_by(hour) %>%
  summarize(avg_steps_hourly = mean(step_total)) %>% #find average steps for each hour
  ggplot(aes(hour, avg_steps_hourly, group = 1)) +
  geom_line(color = "#900C3F") +
  ggtitle("Average Steps Per Hour") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(size = 8)) +
  xlab("Hour of the Day") + ylab("Average Step Count") +
  annotate("rect", xmin = "11:00", xmax = "15:00", 
           ymin = 0, ymax = 700, alpha = .1) +
  annotate("rect", xmin = "17:00", xmax = "20:00", 
           ymin = 0, ymax = 700, alpha = .1) +
  annotate("text", x = "13:00", y = 650, 
           label = "Afternoon", hjust = "center", size = 3) +
  annotate("text", x = 19.5, y = 650, 
           label = "Evening", hjust = "center", size = 3)

p

#Average Sleep Hours by Day, Group 
#data prep
head(daily_use2)
colnames(sleep_logs)

sleep_dayusage <- sleep_logs %>% 
  left_join(daily_use2, by = 'id') %>%
  mutate(day = format(ymd(sleep_day), format = '%a')) %>% 
  group_by(day, Usage) %>% 
  summarise(avg_hrs_asleep = round(mean(total_minutes_asleep)/60,2),
            avg_hrs_in_bed = round(mean(total_time_in_bed)/60,2),
            std_hrs_asleep = round((sd(total_minutes_asleep/60)),2),
            std_hrs_in_bed = round((sd(total_time_in_bed/60)),2)) %>% 
  mutate(day = factor(day, level = c('Mon', 'Tue', 'Wed','Thu', 'Fri', 'Sat', 'Sun')))
head(sleep_dayusage)

#visualize it

ggplot(sleep_dayusage, aes(x = day, y= avg_hrs_asleep, fill = avg_hrs_asleep)) +
  geom_col(color="darkblue", size = 0.2) +
  geom_errorbar(mapping=aes(ymin = avg_hrs_asleep - 0.5 * (std_hrs_asleep), 
                            ymax = avg_hrs_asleep + 0.5 * (std_hrs_asleep), 
                            width = 0.5)) +
  geom_text(aes(label = avg_hrs_asleep, y =0.4), position = position_dodge(0.9), size = 4.5, angle = 0,  color = "black", hjust = 0.5)+
  scale_fill_gradientn(limits=c(0,9), breaks=seq(0,9, by = 2), colours = brewer.pal(5, "Purples")) + 
  scale_y_continuous(limits=c(0,9), breaks=seq(0,9, by = 1))+
  labs(title= ("Average Hours Asleep"), subtitle = ('By Group, Days'), x='Day', y='Hours')+
  theme(plot.title=element_text(size = 16,hjust = 0))+
  theme(plot.subtitle=element_text(size = 14,hjust = 0))+
  theme(axis.text.y=element_text(size=9)) +
  theme(axis.text.x=element_text(size=14,hjust= 0.5))+
  theme(axis.title.x = element_text(margin = margin(t = 14, r = 0, b = 0, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(legend.position = "top")+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=8)) +
  facet_grid(~Usage)
options(repr.plot.width = 14, repr.plot.height = 10)

#Observations:

#The 'High Use' group has a relatively lower average hours of sleep. The variability of sleep for this group also follows a more consistent pattern as oppose to other groups.
#The 'Moderate Use' group has the highest average hours of sleep amongst all the groups. The variability of sleep is inconsistent through the week, pointing to various sleep patterns amongst its users.
#The 'Low Use' group only has a sample size of 1 and will not be considered for this analysis.


