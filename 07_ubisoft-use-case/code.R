setwd("D:/09_analytics_new_start/09_ubisoft")

library("readxl")
library("ggplot2")
library("sqldf")

require(dplyr)


df_level_start <- read_excel("Dataset.xlsx",sheet = "Level Start")
names(df_level_start) <- sub(" ", "_", names(df_level_start))

df_spenders <- read_excel("Dataset.xlsx",sheet = "Spenders")
names(df_spenders) <- sub(" ", "_", names(df_spenders))

df_session_start <- read_excel("Dataset.xlsx",sheet = "Session Start")
names(df_session_start) <- sub(" ", "_", names(df_session_start))


head(df_spenders)



#Question 5

df_question_5 <- sqldf("select Pack_Name, sum(money_Spent) as total_money from df_spenders group by Pack_Name order by sum(money_Spent) desc")
df_question_5[1,]
options(scipen=999)
barplot(df_question_5$total_money, names.arg=df_question_5$Pack_Name, ylab="Pack Name", horiz=TRUE, las = 1)


#Question 4
df_question_4 <- sqldf("select user_id, count(Level) as total_level from df_spenders group by user_id having count(Level) > 1 order by count(Level) desc")

#Percentage of user having repeated purchase
df_question_4_percentage <- (nrow(df_question_4)/nrow(df_spenders))*100

df_question_4_percentage

df_question_max_level <- sqldf("select Level, count(Level) as level_count from df_spenders s, df_question_4 q where s.user_id = q.user_id group by s.Level order by count(s.Level) desc")

df_plot_4 <- df_question_max_level[1:30,]

barplot(df_plot_4$Level, names.arg=df_plot_4$Level, ylab="Level Code", horiz=TRUE, las = 2)


#Question 3

nrow(df_spenders[df_spenders$Level>=1 & df_spenders$Level <= 7, ])

df_5_user_count = (nrow(df_spenders[df_spenders$Level>=1 & df_spenders$Level <= 7, ]) / nrow(df_spenders)) *100

df_5_user_count

#Question 2

head(df_level_start)

#Get total Funnel of level progression of all the levels
df_level_count <- sqldf("select Level__ID as description ,count(Level__ID) as value from df_level_start group by Level__ID order by count(Level__ID)")

df_level_count$description <- as.character(df_level_count$description)

str(df_level_count)

library(rAmCharts)

amFunnel(data = df_level_count, inverse = FALSE)

#Get the list of level played more than two times
df_user_level_count <- sqldf("select user_id, Level__ID ,count(Level__ID) as value from df_level_start group by user_id,Level__ID having count(1) > 1")

head(df_user_level_count)

#Count the repeated level across all users
df_repeated_level <- sqldf("select Level__ID as description ,count(Value) as value from df_user_level_count group by Level__ID order by count(Level__ID)")

df_repeated_level$description <- as.character(df_repeated_level$description)

amFunnel(data = df_repeated_level, inverse = FALSE)


#Question 1

library(lubridate)

df_session_start

df_event_filter <- unique(df_session_start[,c(1,2)])

df_event_filter

## update your date column to date object 
df_event_filter$event_date = ymd(df_event_filter$event_date)


expand.grid(date1 = unique(df_event_filter$event_date),                             ## create all combinations between dates
            date2 = unique(df_event_filter$event_date)) %>%
  filter(date1 < date2) %>%                                        ## keep only cases where 2nd date is after 1st date
  group_by(date1, date2) %>%                                       ## for each combination of dates
  do({ids_1 = setdiff(unique(df_event_filter[df_event_filter$event_date == ymd(.$date1),]$id), ## get new ids in date1 (they showed up first time at this date)
                      unique(df_event_filter[df_event_filter$event_date < ymd(.$date1),]$id))           
  N_ids_1 = length(ids_1)                                          ## count how many ids you have
  ids_2 = unique(df_event_filter[df_event_filter$event_date == ymd(.$date2),]$id)              ## get ids from date2
  N_ids_2 = length(intersect(ids_2, ids_1))                        ## count how many ids exist in previous dataset
  data.frame(Prc = N_ids_2/N_ids_1)}) %>%                          ## calculate the percentage          
  ungroup()




#Use Case

setwd("D:/09_analytics_new_start/09_ubisoft/Data Analyst Test")

data_use_case <- read.csv("Analytics Test Dataset.csv")

head(data_use_case)

barplot(data_use_case$Player.Level, names.arg = data_use_case$Revenue, las = 2, horiz=TRUE)


cor(data_use_case$Player.Level, data_use_case$Revenue)

cor(data_use_case$Player.Level, factor(data_use_case$Pack_ID))


dotchart(data_use_case$Revenue,labels = data_use_case$Pack_ID)

plot(data_use_case$Pack_ID, y = data_use_case$Revenue)

barplot(data_use_case$, names.arg=data_use_case$Pack_Name, ylab="Pack Name", horiz=TRUE, las = 1)

str(data_use_case$Pack_ID)
