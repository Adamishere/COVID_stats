library(dplyr)
library(ggplot2)

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
                select(date, state,state_str, hospitalizedCurrently,hospitalizedIncrease, positive,negative) %>%
                group_by(state) %>%
                arrange(state,date) %>%
                mutate(hosp_diff = hospitalizedCurrently - lag(hospitalizedCurrently))%>%
                mutate(pos_diff = positive - lag(positive))%>%
                mutate(neg_diff = negative - lag(negative))%>%
                mutate(pct_pos = positive/(positive+negative)) %>%
                mutate(pct_pos_diff = pos_diff/(pos_diff+neg_diff)) %>%
                arrange(state,desc(date))

state_results[state_results$state_str =="FL","hosp_diff"]<-state_results[state_results$state_str =="FL","hospitalizedIncrease"]
state_results<-state_results[state_results$date > as.Date.character("2020-03-25", "%Y-%m-%d"),]

#head(state_results[state_results$state_str =="OK",c("date","positive","negative","pos_diff","neg_diff","pct_pos_diff")])
#head(state_results[state_results$state_str =="OK",c("date","hospitalizedCurrently","hosp_diff","pct_pos_diff")])
# head(state_results)


gg_tx<-ggplot(#data=state_results,
  data=state_results[state_results$state_str %in% "TX",] ,
  aes(x=date,y=hospitalizedCurrently))+
  geom_line() +   geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
                xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
                ymin=-Inf, ymax=Inf),fill="red",alpha=0.005)+
  ggtitle("Hospitalizations by Texas")+
  theme(legend.position = "none")+ annotate("text", label = "1 week after Memorial Day", x = as.Date.character("2020-05-25", "%Y-%m-%d"), y = 500, size = 6, colour = "Black")
gg_tx

state_list<-c("WA","GA","TX","AZ","LA","OK","OH","KY","SC","AL","MD")
gg_key<-ggplot(#data=state_results,
           data=state_results[state_results$state_str %in% state_list,] ,
           aes(x=date,y=hospitalizedCurrently,
               group=state, 
               color=state))+
      geom_line()+  geom_smooth()+
      geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
      geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
      geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
                    xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
                    ymin=-Inf, ymax=Inf),fill="white",alpha=0.005)+
      facet_wrap(~state,scales = "free_y")+
      ggtitle("Hospitalizations by Key States")+
      theme(legend.position = "none")
gg_key

# ggsave("C:/Users/21509/OneDrive - ICF/_Common Programs/R/COVID/COVID19 Hospitalizations by Key States.png",gg)


#Cumulative Positive Count

gg_cum_cases<-ggplot(#data=state_results,
  data=state_results[state_results$state_str %in% state_list,] ,
  aes(x=date,y=positive,
      group=state, 
      color=state))+
  geom_line()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
                xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
                ymin=-Inf, ymax=Inf),fill="white",alpha=0.005)+
  facet_wrap(~state,scales = "free_y")+
  ggtitle("Positives by Key States")+
  theme(legend.position = "none")
gg_cum_cases

gg_cum_pctpos<-ggplot(#data=state_results,
  data=state_results[state_results$state_str %in% state_list,] ,
  aes(x=date,y=pct_pos_diff,
      group=state, 
      color=state,ymax=.25))+
  geom_line()+
  geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), 
                xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), 
                ymin=-Inf, ymax=Inf),fill="white",alpha=0.005)+
  facet_wrap(~state)+ scale_y_continuous(limits = c(0, .25))+
  ggtitle("Positives by Key States")+
  theme(legend.position = "none")
gg_cum_pctpos



gg_all<-ggplot(data=state_results,
  #data=state_results[state_results$state_str %in% state_list,] ,
  aes(x=date,y=hospitalizedCurrently,
      group=state, 
      color=state))+
  geom_line()+  geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), ymin=0, ymax=Inf))+
  facet_wrap(~state,scales = "free_y")+
  ggtitle("Hospitalizations by All States")+
  theme(legend.position = "none")
gg_all

# ggsave("C:/Users/21509/OneDrive - ICF/_Common Programs/R/COVID/COVID19 Hospitalizations by All States.png",gg,
#        , width = 12, height = 10)
gg_all_pp<-ggplot(data=state_results,
               #data=state_results[state_results$state_str %in% state_list,] ,
               aes(x=date,y=pct_pos_diff,
                   group=state, 
                   color=state))+
  geom_line()+  geom_smooth()+
  geom_vline(xintercept = as.Date.character("2020-05-25", "%Y-%m-%d"),color="red")+
  geom_vline(xintercept = as.Date.character("2020-06-01", "%Y-%m-%d"),color="blue")+
  geom_rect(aes(xmin=as.Date.character("2020-05-25", "%Y-%m-%d"), xmax=as.Date.character("2020-06-01", "%Y-%m-%d"), ymin=0, ymax=Inf))+
  facet_wrap(~state)+ scale_y_continuous(limits = c(0, .25))+
  ggtitle("Pct Positives by All States")+
  theme(legend.position = "none")
gg_all_pp