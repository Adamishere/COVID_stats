#install.packages("gridExtra")
library(gridExtra)
library(dplyr)
library(ggplot2)
#install.packages("dplyr")
#setwd("E:/Work/R workspace/COVID/COVID_stats/")

#loc_hist<-"https://covidtracking.com/api/v1/us/daily.csv"
#covid_hist<-as.data.frame(read.csv(loc_hist))

loc_hist_state<-"https://covidtracking.com/api/v1/states/daily.csv"
covid_hist_state<-as.data.frame(read.csv(loc_hist_state))

# 
# names(covid_hist_state)
# as.Date.character(covid_hist_state$date, "%Y%m%d")

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

#state_results[state_results$state_str =="FL","hosp_diff"]<-state_results[state_results$state_str =="FL","hospitalizedIncrease"]
state_results<-state_results[state_results$date > as.Date.character("2020-03-25", "%Y-%m-%d"),]

#head(state_results[state_results$state_str =="OK",c("date","positive","negative","pos_diff","neg_diff","pct_pos_diff")])
#head(state_results[state_results$state_str =="OK",c("date","hospitalizedCurrently","hosp_diff","pct_pos_diff")])
# head(state_results)


gg_tx<-ggplot(#data=state_results,
  data=state_results[state_results$state_str %in% "TX",] ,
  aes(x=date,y=hosp_diff))+
  geom_line() +   geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-1,color="black")+
  geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
                xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
                ymin=-Inf, ymax=Inf),fill="red",alpha=0.005)+
  ggtitle("Hospitalizations by Texas")+
  theme(legend.position = "none")+ annotate("text", label = "1 week after Memorial Day", x = as.Date.character("2020-05-25", "%Y-%m-%d"), y = 500, size = 6, colour = "Black")
gg_tx
head(state_results[state_results$state_str %in% "TX",c("date","hospitalizedCurrently")])
                   
state_list<-c("CA","WA","GA","TX","AZ",
              "SC","AL","MD","FL")
#state_list<-c("TX","AZ","FL","NY","NJ","MA","WA")
gg_key<-ggplot(#data=state_results,
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
gg_key

# ggsave("./COVID19 Hospitalizations by Key States.png",gg)
state_list<-c("TX","FL","AZ","GA","WA")
#state_list<-c("TX","FL","AZ")
#library(zoo)
#install.packages("zoo")



# Stack charts ------------------------------------------------------------
gg_death_key<-ggplot(#data=state_results,
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
gg_death_key

gg_pos_key<-ggplot(#data=state_results,
  data=state_results[state_results$state_str %in% state_list,] ,
  aes(x=date,y=pos_diff,
      group=state, 
      color=state))+
  #geom_line()+
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
gg_pos_key

gg_PCTpos_key<-ggplot(#data=state_results,
                   data=state_results[state_results$state_str %in% state_list,] ,
                   aes(x=date,y=pct_pos_diff,
                       group=state, 
                       color=state))+
  #geom_line()+
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
gg_PCTpos_key

gg_hosp_key<-ggplot(#data=state_results,
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
gg_hosp_key

grid.arrange(gg_PCTpos_key,gg_hosp_key,gg_death_key) 



#Cumulative Positive Count

gg_cum_cases<-ggplot(#data=state_results,
  data=state_results[state_results$state_str %in% state_list,] ,
  aes(x=date,y=positive,
      group=state, 
      color=state))+
  geom_line()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-1,color="black")+
  geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
                xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
                ymin=-Inf, ymax=Inf),fill="white",alpha=0.005)+
  facet_wrap(~state,scales = "free_y")+
  ggtitle("Positives by Key States")+
  theme(legend.position = "none")
gg_cum_cases
 
state_results[state_results$state_str %in% state_list,"pct_pos_diff"]

gg_all<-ggplot(data=state_results,
  #data=state_results[state_results$state_str %in% state_list,] ,
  aes(x=date,y=hospitalizedCurrently,
      group=state, 
      color=state))+
  geom_line()+  geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-1,color="black")+
  geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), ymin=0, ymax=Inf))+
  facet_wrap(~state,scales = "free_y")+
  ggtitle("Hospitalizations by All States")+
  theme(legend.position = "none")
gg_all

# ggsave("./COVID19 Hospitalizations by All States.png",gg,
#        , width = 12, height = 10)
gg_all_pp<-ggplot(data=state_results,
               #data=state_results[state_results$state_str %in% state_list,] ,
               aes(x=date,y=pct_pos_diff,
                   group=state, 
                   color=state))+
  geom_line()+  geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_vline(xintercept = as.Date.character(Sys.Date(), "%Y-%m-%d")-1,color="black")+
  #geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), ymin=0, ymax=Inf))+
  facet_wrap(~state)+ 
  scale_y_continuous(limits = c(0, .30))+
  ggtitle("Pct Positives by All States")
  #theme(legend.position = "none")
gg_all_pp


(33+50+44+57+21+10+27)/7
(6+20+36+33+21+31+11)/7

#Weekly aggregate results
#install.packages("tidyverse")
library(tidyverse)
library(lubridate)
tx<-state_results[state_results$state_str %in% "WA",c("pct_pos_diff","date","death_diff","hosp_diff")]

weeklydeath<-tx %>% 
  group_by(week = week(date)) %>% 
  summarise(x = round(mean(death_diff),2))

weeklyhosp<-tx %>% 
  group_by(week = week(date)) %>% 
  summarise(x = round(mean(hosp_diff),2))

weeklypctpos<-tx %>% 
  group_by(week = week(date)) %>% 
  summarise(x = round(mean(pct_pos_diff),2))

cbind(weeklypctpos$x,weeklyhosp$x,weeklydeath$x)


plot(lag(weeklydeath$x,0), lag(weeklypctpos$x,3))
cor(lag(weeklydeath$x,0), lag(weeklypctpos$x,3), use = "complete.obs")

cbind(lag(weeklydeath$x,0), lag(weeklyhosp$x,3))

plot(weeklydeath)

  DEATH HOSP

  1     
  2     1
  3     2
  4     3
  
write.csv(tx, "output.csv")
