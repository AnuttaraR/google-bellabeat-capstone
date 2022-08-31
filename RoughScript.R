dailyActivity <- read.csv("dailyActivity_merged.csv")
head(dailyActivity)
colnames(dailyActivity)

sleepDay <- read.csv("sleepDay_Newmerged.csv")
head(sleepDay)
colnames(sleepDay)

weightLog <- read.csv("weightLogInfo_Newmerged.csv")
head(weightLog)
colnames(weightLog)

heartrate <- read.csv("heartrate_seconds_merged.csv")
library(sqldf)
heartrate_new <- sqldf("SELECT Id, SUBSTR(Time, 1, 7) AS Date, SUBSTR(Time, 11, 11) AS Time, Value FROM heartrate")
head(heartrate_new)
colnames(heartrate_new)

minMET <- read.csv("minuteMETsNarrow_merged.csv")
minMET_new <- sqldf("SELECT Id, SUBSTR(ActivityMinute, 1, 7) AS Date, SUBSTR(ActivityMinute, 11, 11) AS Time, METs FROM minMET")
head(minMET_new)
colnames(minMET_new)

step <- read.csv("dailySteps_merged.csv")
head(step)
colnames(step)



sqldf("SELECT COUNT(DISTINCT Id) AS Num_Participants FROM dailyActivity")
sqldf("SELECT round(avg(TotalSteps), 0) AS Avg_TotSteps FROM dailyActivity")
sqldf("SELECT round(avg(TotalDistance), 0) AS Avg_TotSteps FROM dailyActivity")
sqldf("SELECT COUNT()
FROM dailyActivity AS activity 
LEFT JOIN step ON 
activity.Id = step.Id AND 
activity.ActivityDate = step.ActivityDay AND 
activity.Totalsteps = step.StepTotal")
sqldf("SELECT round(avg(Calories), 0) AS Avg_TotSteps FROM dailyActivity")
summary(dailyActivity)


sqldf("SELECT COUNT(DISTINCT Id) AS Num_Participants FROM sleepDay")
summary(sleepDay)


sqldf("SELECT COUNT(DISTINCT Id) AS Num_Participants FROM weightLog")
summary(weightLog)


sqldf("SELECT COUNT(DISTINCT Id) AS Num_Participants FROM heartrate_new")
sqldf('SELECT COUNT(Id) FROM heartrate_new WHERE Value>105')
summary(heartrate_new)


sqldf("SELECT COUNT(DISTINCT Id) AS Num_Participants FROM minMET")
summary(minMET)




library(ggplot2)
library(dplyr)


ggplot(data= dailyActivity)+
  geom_point(mapping=aes(x=TotalSteps, y=Calories, color=TotalDistance))+
  labs(title= "Total Daily Steps Vs. Total Calories Burned", caption= 'Data Source: Fitabase Data 4.12.16-5.12.16')+
  xlab("Total Daily Steps") +
  ylab("Calories Burnt")


Sum_mins<- summarize(dailyActivity, sum_very <- sum(VeryActiveMinutes), sum_fairly <- sum(FairlyActiveMinutes), sum_lightly <-sum(LightlyActiveMinutes), sum_sed <- sum(SedentaryMinutes))
Sum_mins


Sum_min_labels <- c('VeryActiveMins', 'FairlyActiveMins', 'LightlyActiveMins', 'SedentaryMins')
Sum_minutes<- c(19895, 12751, 181244, 931738)
Mins_df <- data.frame(group = Sum_min_labels, value=  Sum_minutes)
Mins_df


ggplot(Mins_df, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title= "Minutes Spent by the Users Split to Levels of Activity",
       caption= 'Data Source: Fitabase Data 4.12.16-5.12.16')


ggplot(data= dailyActivity)+
  geom_point(mapping=aes(x=VeryActiveMinutes, y=Calories))+
  geom_smooth(mapping = aes(x = VeryActiveMinutes, y = Calories))+
  labs(title= "Very Active Minutes Vs. Total Calories Burned", caption= 'Data Source: Fitabase Data 4.12.16-5.12.16')


sleepDay_v2 <- sqldf("SELECT Id, TotalMinutesAsleep, (TotalTimeInBed-TotalMinutesAsleep) AS TotalMinutesAwake FROM sleepDay")
head(sleepDay_v2)


summarize(sleepDay_v2, mean_TotSleep = mean(TotalMinutesAsleep), mean_TotAwake = mean(TotalMinutesAwake))

sleepValues<- c(419.2, 39.3)
sleepLables<- c("TotalMinsAsleep", "TotalMinsAwake")
sleepPie <- data.frame(group= sleepLables, value= sleepValues)
sleepPie

ggplot(sleepPie, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()+
  labs(title= "Minutes Spent by the Users In Bed",
       caption= 'Data Source: Fitabase Data 4.12.16-5.12.16')


weekday <- weekdays(dailyActivity$ActivityDate)

newdaily_Activity <- data.frame(dailyActivity, weekday)
head(newdaily_Activity)

ggplot(data=newdaily_Activity)+
  geom_bar(mapping=aes(x=weekday, fill=weekday))+
  labs(title= "Daily Activity Usage Logs Per Weekday", caption= 'Data Source: Fitabase Data 4.12.16-5.12.16')

weekday_Steps<- sqldf("SELECT SUM(TotalSteps) AS SumTotalSteps, weekday FROM newdaily_Activity GROUP BY weekday")
weekday_Steps


library(RColorBrewer)
coul <- brewer.pal(5, "Set2")
barplot(height=weekday_Steps$SumTotalSteps, names=weekday_Steps$weekday, col=coul, xlab="Days of the Week", ylab="Sum of Total Number of Steps", main="Analyzing the paterns of total steps taken in a week")


daysoftheweek <- weekdays(heartrate_new$Date)

heartrate_v2 <- data.frame(heartrate_new, daysoftheweek)
head(heartrate_v2)

heartrate_query<- sqldf("SELECT AVG(Value) AS Average_Heartrate, daysoftheweek FROM heartrate_v2 GROUP BY daysoftheweek")
heartrate_query


library(RColorBrewer)
coul <- brewer.pal(5, "Set2")
barplot(height=heartrate_query$Average_Heartrate, names=heartrate_query$daysoftheweek, col=coul, xlab="Days of the Week", ylab="Average Heartrate", main="Analyzing the average heartrate per day of the week")