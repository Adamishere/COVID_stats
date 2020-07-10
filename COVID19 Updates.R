#install.packages("gridExtra")
library(gridExtra)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(tidyverse)
#install.packages("dplyr")
#setwd("E:/Work/R workspace/COVID/COVID_stats/")

#loc_hist<-"https://covidtracking.com/api/v1/us/daily.csv"
#covid_hist<-as.data.frame(read.csv(loc_hist))

#Read from website:
loc_hist_state<-"https://covidtracking.com/api/v1/states/daily.csv"
covid_hist_state<-as.data.frame(read.csv(loc_hist_state))

#Data prep
state_results <-covid_hist_state %>%
                mutate(date = as.Date.character(date, "%Y%m%d"))%>%
                mutate(state_str = as.character(state))%>%
                filter(!state_str %in% c("AS","GU","MP","PR","VI"))%>%
                select(date, state,state_str, hospitalizedCurrently,hospitalizedIncrease, positive,negative,death) %>%
                group_by(state) %>%
                arrange(state,date) %>%
                mutate(death_diff = death - lag(death))%>%
                mutate(hosp_diff = round(hospitalizedCurrently - lag(hospitalizedCurrently)))%>%
                mutate(pos_diff = positive - lag(positive))%>%
                mutate(neg_diff = negative - lag(negative))%>%
                mutate(pct_pos = positive/(positive+negative)) %>%
                mutate(pct_pos_diff = pos_diff/(pos_diff+neg_diff)) %>%
                arrange(state,desc(date))

#subset to post 1st wave cases
state_results<-state_results[state_results$date > as.Date.character("2020-03-25", "%Y-%m-%d"),]

#Individual State Output
gg_state_hosp<-ggplot(#data=state_results,
  data=state_results[state_results$state_str %in% "TX",] ,
  aes(x=date,
      #y=pct_pos))+
      y=death_diff))+
      #y=hosp_diff))+
  geom_line() +   geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-1,color="green")+
  geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
                xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
                ymin=-Inf, ymax=Inf),fill="red",alpha=0.005)+
  ggtitle("Change in Deaths by State")+
  theme(legend.position = "none")+ 
    annotate("text", label = "1 week after Memorial Day",
             x = as.Date.character("2020-05-25", "%Y-%m-%d"),
             y = -10, size = 6, colour = "Black")
  
gg_state_hosp

#Key State Analysis                
#state_list<-c("CA","WA","GA","TX","AZ",
#              "SC","AL","MD","FL")
#state_list<-c("TX","FL","AZ","GA","WA")
state_list<-c("TX","FL","AZ")
#state_list<-c("TX","AZ","FL","NY","NJ","MA","WA")
gg_key_hosp<-ggplot(#data=state_results,
           data=state_results[state_results$state_str %in% state_list,] ,
           aes(x=date,y=hospitalizedCurrently,
               group=state, 
               color=state))+
      geom_line()+  geom_smooth()+
      geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
      geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
      geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-2,color="black")+
      geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
                    xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
                    ymin=-Inf, ymax=Inf),fill="white",alpha=0.005)+
      facet_wrap(~state,scales = "free_y")+
      ggtitle("Hospitalizations by Key States")+
      theme(legend.position = "none")
gg_key_hosp


# Stack charts ------------------------------------------------------------
gg_key_death<-ggplot(#data=state_results,
  data=state_results[state_results$state_str %in% state_list,] ,
  aes(x=date,y=death_diff,
      group=state, 
      color=state))+
  geom_line()+ 
  geom_line(aes(y=rollmean(death_diff, 7, na.pad=TRUE))) +
  #geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-08", "%Y-%m-%d"),color="blue")+
  #geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-2,color="black")+
  #geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
  #              xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
  #              ymin=-Inf, ymax=Inf),fill="white",alpha=0.005)+
  scale_x_date(limits = c(as.Date.character("2020-04-01", "%Y-%m-%d"),
                          NA))+
  facet_wrap(~state,
             #scales = "free_y",
             nrow=1)+
  ggtitle("Daily Death (Moving Average) by Key States")+
  theme(legend.position = "none")
gg_key_death

gg_key_pos<-ggplot(#data=state_results,
  data=state_results[state_results$state_str %in% state_list,] ,
  aes(x=date,y=pos_diff,
      group=state, 
      color=state))+
  geom_line()+
  geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-08", "%Y-%m-%d"),color="blue")+
  #geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-1,color="black")+
  #geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
  #              xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
  #              ymin=-Inf, ymax=Inf),fill="white",alpha=0.005)+
  scale_x_date(limits = c(as.Date.character("2020-04-01", "%Y-%m-%d"),
                          NA))+
  facet_wrap(~state,
             #scales = "free_y",
             nrow=1)+
  ggtitle("Daily Positives by Key States")+
  theme(legend.position = "none")
gg_key_pos

gg_key_pctpos<-ggplot(#data=state_results,
                   data=state_results[state_results$state_str %in% state_list,] ,
                   aes(x=date,y=pct_pos_diff,
                       group=state, 
                       color=state))+
  geom_line()+
  geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-08", "%Y-%m-%d"),color="blue")+
  #geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-1,color="black")+
  #geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
  #              xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
  #              ymin=-Inf, ymax=Inf),fill="white",alpha=0.005)+
  facet_wrap(~state, nrow=1)+
  scale_x_date(limits = c(as.Date.character("2020-04-01", "%Y-%m-%d"),
                                       NA))+
  scale_y_continuous(limits = c(0, .30),
                     #scales = "free_y",
                     labels = scales::number_format(accuracy = .1, 
                                                    decimal.mark = '.'))+
  ggtitle("Daily Percent Positives (Smoothed) by Key States")+
  theme(legend.position = "none")
gg_key_pctpos

gg_key_hosp<-ggplot(#data=state_results,
  data=state_results[state_results$state_str %in% state_list,] ,
  aes(x=date,y=hospitalizedCurrently),
      group=state, 
      color=state)+
  geom_line()+  geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-2,color="black")+
  geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
                xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
                ymin=-Inf, ymax=Inf),fill="white",alpha=0.005)+
  scale_x_date(limits = c(as.Date.character("2020-04-01", "%Y-%m-%d"),
                          NA))+
  facet_wrap(~state, 
             #scales = "free_y", 
             nrow=1)+
  ggtitle("Current Hospitalizations by Key States")+
  theme(legend.position = "none")
gg_key_hosp

#Print combined output:
grid.arrange(gg_key_pctpos, gg_key_hosp, gg_key_death) 



#Weekly aggregate results
tx<-state_results[state_results$state_str %in% "TX",c("hospitalizedCurrently","pct_pos_diff","date","death_diff","hosp_diff")]

weeklydeath<-tx %>% 
  group_by(week = week(date)) %>% 
  summarise(x = round(mean(death_diff),2))

weeklyhosp<-tx %>% 
  group_by(week = week(date)) %>% 
  summarise(x = round(mean(hospitalizedCurrently),2))

weeklypctpos<-tx %>% 
  group_by(week = week(date)) %>% 
  summarise(x = round(mean(pct_pos_diff),2))

wksummary<-cbind(pctpos=weeklypctpos$x,hosp=weeklyhosp$x,death=weeklydeath$x)



#NCHS Weekly Deaths by State
library(jsonlite)
#library(httr)
#install.packages("RSocrata")
library(RSocrata)

#https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/muzy-jte6
#https://data.cdc.gov/login

path<-"https://data.cdc.gov/resource/muzy-jte6.json" #all causes
#setwd("./COVID/COVID_stats")

#get API key but keep it secret on Git
fileName <- "../CDC_app_token.txt"
key<-readChar(fileName, file.info(fileName)$size)
wd_json<-read.socrata(path, app_token = key)

#convert to data frame
wd_df<-as.data.frame(wd_json)

#note date as string
table(wd_df$week_ending_date)
table(wd_df$jurisdiction_of_occurrence)

#create date variables of week end
wd_df$week<-as.Date.character(wd_df$week_ending_date, "%m/%d/%Y")
wd_df$state<-wd_df$jurisdiction_of_occurrence

#merge on abreviation
state_dict<-data.frame((cbind(name=datasets::state.name, 
                              abv=datasets::state.abb)),stringsAsFactors = FALSE)
wd_df<-merge(wd_df, state_dict, 
             by.x = "jurisdiction_of_occurrence", 
             by.y = "name")

#set missing to zero
wd_df$flupnu<-wd_df$influenza_and_pneumonia_j09_j18
wd_df[is.na(wd_df$flupnu),"flupnu"]<-0

wd2020<-wd_df[wd_df$week > as.Date.character("2020-03-25", "%Y-%m-%d"),]

state_list<-c("TX","FL","AZ","GA","WA")
gg_weekly_death<-ggplot(#data=wd2020,
               data=wd2020[wd2020$abv %in% state_list,] ,
               aes(x=week_ending_date,y=flupnu,
                   group=state, 
                   color=state))+
  geom_line()+  geom_smooth()+
  #geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
 # geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  #geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-1,color="black")+
  #geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), ymin=0, ymax=Inf))+
  facet_wrap(~state,scales = "free_y")+
  ggtitle("Flu & Pneumonia by All States")+
  theme(legend.position = "none")
gg_weekly_death



path2<-"https://data.cdc.gov/resource/r8kw-7aab.json" #covid
#get API key but keep it secret on Git
fileName <- "../CDC_app_token.txt"
key<-readChar(fileName, file.info(fileName)$size)

cwd_json<-read.socrata(path2, app_token = key)
#convert to data frame
cwd_df<-as.data.frame(wd_json)

#note date as string
table(cwd_df$end_week)
table(cwd_df$state)
substr()
#create date variables of week end
cwd_df$week<-as.Date.character(substr(cwd_df$end_week,1,10), "%Y-%m-%d")
#cwd_df$state<-cwd_df$jurisdiction_of_occurrence

#merge on abreviation
state_dict<-data.frame((cbind(name=datasets::state.name, 
                              abv=datasets::state.abb)),stringsAsFactors = FALSE)
cwd_df<-merge(cwd_df, state_dict, 
             by.x = "state", 
             by.y = "name")

#set missing to zero
cwd_df$cdeath<-as.numeric(cwd_df$covid_deaths)
cwd_df[is.na(cwd_df$cdeath),"cdeath"]<-0

cwd2020<-cwd_df[cwd_df$week > as.Date.character("2020-03-25", "%Y-%m-%d"),]

state_list<-c("TX","FL","AZ","GA","WA")
gg_weekly_cvdeath<-ggplot(#data=cwd2020,
  data=cwd2020[cwd2020$abv %in% state_list,] ,
  aes(x=week,y=cdeath,
      group=state, 
      color=state))+
  geom_line()+  geom_smooth()+
  #geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  # geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  #geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-1,color="black")+
  #geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), ymin=0, ymax=Inf))+
  facet_wrap(~state,scales = "free_y")+
  ggtitle("COVID cases by All States")+
  theme(legend.position = "none")
gg_weekly_cvdeath

