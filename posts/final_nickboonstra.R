# Setup -------------------------------------------------------------------



library(tidyverse)
library(lubridate)
library(ggridges)



# Functions ---------------------------------------------------------------



purge_dels<-function(x){
  x %>% 
    select(!contains("del")) %>% 
    return(x)
}

circ_search<-function(x){
  master_circ %>% filter(circ_id==x)
}


# Read in -----------------------------------------------------------------



## Circuits ====

master_circ<-read_csv(
  file="posts/_data/f1archive/circuits.csv",
  col_select=1:8,
  skip=1,
  col_types="nccccnnn",
  col_names=c("circ_id","circ_ref","circ_name","circ_loc","circ_nat","circ_lat","circ_lng","circ_alt")
)



## Constructors ====

results_cons<-read_csv(
  file="posts/_data/f1archive/constructor_results.csv",
  skip=1,
  col_names=c("result_id_con","race_id","con_id","pts_race_con","status_id_con")
)

results_cons<-results_cons %>% 
  na_if("\\N")



stands_cons<-read_csv(
  file="posts/_data/f1archive/constructor_standings.csv",
  skip=1,
  col_names=c("stands_id_con","race_id","con_id","pts_stands_con","con_rank","del","con_wins")
)

stands_cons<-purge_dels(stands_cons)




master_cons<-read_csv(
  file="posts/_data/f1archive/constructors.csv",
  col_select=1:4,
  skip=1,
  col_names=c("con_id","con_ref","con_name","con_nat")
)




## Drivers ====

stands_drivs<-read_csv(
  file="posts/_data/f1archive/driver_standings.csv",
  skip=1,
  col_names=c("stands_id_driv","race_id","driv_id","pts_stands_driv","driv_rank","del","driv_wins")
)

stands_drivs<-purge_dels(stands_drivs)


master_drivs<-read_csv(
  file="posts/_data/f1archive/drivers.csv",
  col_select=1:8,
  skip=1,
  col_types="ncnccc?c",
  col_names=c("driv_id","driv_ref","driv_num","driv_code","driv_first","driv_last","driv_dob","driv_nat")
)


results_drivs<-read_csv(
  file="posts/_data/f1archive/results.csv",
  skip=1,
  col_types="nnnnnnncnnncnnncnn",
  col_names=c(
    "result_id_driv","race_id","driv_id",
    "con_id","driv_num","grid_race",
    "del","classy_race","order_race",
    "pts_driv_race","laps_race","del",
    "time_ms_race","fastest_num_race","fastest_rank_race",
    "fastest_time_race","fastest_speed_race","status_driv_race"
  )
)

results_drivs<-purge_dels(results_drivs)
results_drivs<-results_drivs %>% 
  mutate(fastest_time_race=ms(fastest_time_race))

results_sprints<-read_csv(
  file="posts/_data/f1archive/sprint_results.csv",
  skip=1,
  col_names=c(
    "result_id_sprint","sprint_id","driv_id","con_id",
    "driv_num","grid_sprint","del","classy_sprint",
    "order_sprint","pts_driv_sprint","laps_sprint","del",
    "time_ms_sprint","fastest_num_sprint","fastest_time_sprint","status_driv_sprint"
  )
)

results_sprints<-purge_dels(results_sprints)
results_sprints<-results_sprints %>% 
  mutate(fastest_time_sprint=ms(fastest_time_sprint))


## Races ====

master_races<-read_csv(
  file="posts/_data/f1archive/races.csv",
  skip=1,
  col_select=1:6,
  col_names=c("race_id","seas_year","seas_round","circ_id","name_race","date_race")
)



lap_times<-read_csv(
  file="posts/_data/f1archive/lap_times.csv",
  skip=1,
  col_names=c("race_id","driv_id","lap","pos_race","time_lap_char","time_lap_ms"),
  col_types="nnnncn"
)

lap_times<-lap_times %>% 
  mutate(time_lap=ms(time_lap_char)) %>% 
  select(-"time_lap_char")


qual<-read_csv(
  file="posts/_data/f1archive/qualifying.csv",
  skip=1,
  col_names=c("qual_id","race_id","driv_id","con_id","driv_num","qual_pos","q1","q2","q3")
  )

qual<-qual %>% 
  mutate(q1=ms(q1)) %>% 
  mutate(q2=ms(q2)) %>% 
  mutate(q3=ms(q3)) %>% 
  pivot_longer(
    cols = q1:q3,
    names_to = "qual_round",
    values_to = "qual_time"
  ) %>% 
  mutate(qual_round=str_remove_all(qual_round,"[q]")) %>% 
  mutate(qual_round=as.numeric(qual_round))


# Ideas ----

## Cutoff lap times in Q3 by track (boxplots?)
## (Best) Race lap times by quali lap times?
## You might need to full_join stands_cons and results_cons, for consistency, and take NAs
## Write a loop that determines race number in the season (e.g. 4/22)




# Visualizations ----------------------------------------------------------


fastest_laps<-lap_times %>% 
  group_by(race_id) %>% 
  summarise(time_lap_ms=min(time_lap_ms)) %>% ## You bleeding genius. (This makes sure it joins back cleanly)
  left_join(lap_times) %>% 
  left_join(master_drivs) %>% 
  left_join(master_races) 

fastest_laps %>% 
  filter(circ_id==6) %>% ## Monaco
  ggplot(.,aes(x=date_race,y=time_lap_ms)) +
  geom_point() +
  geom_line() +
  theme_bw()  



fastest_laps_filter<-fastest_laps %>% 
  filter(seas_year>=2002) %>% 
  count(circ_id,name_race) %>% 
  filter(n>=10) %>% 
  left_join(fastest_laps) %>% 
  select(!n) %>% 
  filter(race_id!=1063) 
  





fastest_laps_filter %>% 
  filter(seas_year<=2021) %>% 
  left_join(master_circ) %>% 
  mutate(hybrid=case_when(
    seas_year>=2014 ~ 1,
    T ~ 0
  )) %>% 
  filter(time_lap_ms<=110000) %>% 
  filter(circ_id %in% c(1,2,4,6,7,11,14,18,22)) %>%
  ggplot(.,aes(x=date_race,y=as.numeric(time_lap),label=seas_year,color=as.factor(hybrid))) +
  geom_point() +
  geom_line() +
  geom_text(size=1.7,hjust=-0.3) +
  theme_linedraw() +
  facet_wrap(vars(circ_name),scales="free_y") +
  theme(legend.position = "none") +
  labs(x="Date",
       y="Lap Time (in seconds)",
       title="Fastest Laps Before and During Formula 1's Hybrid Era",
       caption="Color-coding indicates pre-Hybrid (red) vs. Hybrid (blue) Era")





## Circuit 10 takes a massive drop suddenly... why?

circ_search(10) ## Hockenheimring was shortened drastically for 2002.


## And what was with that massive dip on 3? 
## (This doesn't happen anymore now that I count by circ_id AND race_name)


circ_search(3) ## bahrain...
master_races %>% filter(circ_id==3) ## 2020 had Bahrain and Sakhir...
## ... and the Sakhir GP had a different track layout!!!!!!!!
## So I've gotta sort not by circ_id but rather by name_race... maybe?
## Or maybe the data set should just get good and differentiate between
## the old and new Hockenheimringen, or between Bahrain and Sakhir.


fastest_laps_filter %>% 
  filter(name_race!="Sakhir Grand Prix") %>% 
  ggplot(aes(x=date_race,y=time_lap_ms)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(vars(circ_id)) 





# fastest_laps %>% 
#   filter(circ_id==10) %>% ## Hockenheimring
#   ggplot(.,aes(x=date_race,y=time_lap_ms)) +
#   geom_point() +
#   geom_line() +
#   theme_bw()

# fastest_laps %>% 
#   filter(circ_id==1) %>% ## Australia
#   ggplot(.,aes(x=date_race,y=time_lap_ms)) +
#   geom_point() +
#   geom_line() +
#   theme_bw()





laps_races<-lap_times %>% left_join(master_races) %>% left_join(master_circ)


laps_races %>% 
  ggplot(aes(x=as.factor(seas_year),y=time_lap_ms)) +
  geom_boxplot() +
  ylim(NA,150000) +
  theme_bw()

laps_races %>% 
  group_by(seas_year) %>% 
  summarise(iqr=IQR(time_lap_ms)) %>% 
  arrange(desc(iqr))

## Average faster laps does not NECESSARILY mean racing has improved. 
## Higher IQR means more spread out lap times

laps_races %>% 
  filter(circ_id==6) %>% 
  ggplot(aes(x = time_lap_ms, y = as.factor(seas_year),fill=as.factor(seas_year))) +
  geom_density_ridges() +
  theme_ridges() +
  xlim(NA,125000) +
  theme(legend.position = "none")


laps_races %>% 
  mutate(hybrid=case_when(
    seas_year>=2014 ~ 1,
    T ~ 0
  )) %>% 
  filter(time_lap_ms<=115000) %>% 
  filter(seas_year<=2021) %>% 
  filter(circ_id %in% c(1,2,4,6,7,11,14,18,22)) %>% 
  filter(race_id!=c(1046)) %>% 
  ggplot(aes(x = as.numeric(time_lap), y = as.factor(seas_year),fill=as.factor(hybrid),alpha=0.5)) +
  geom_density_ridges() +
  theme_linedraw() +
  theme(axis.text = element_text(size = 6.5)) +
  facet_wrap(vars(circ_name),scales="free_x") +
  theme(legend.position = "none") +
  labs(
    title="Distribution of Race Lap Times by Year and Circuit",
    caption="Color-coding indicates pre-Hybrid (red) vs. Hybrid (blue) Era",
    x="Lap Time (in Seconds)",
    y="Season Year"
  )



qual_race<-qual %>% 
  left_join(master_races) %>% 
  left_join(master_circ)


qual_race %>% 
  mutate(hybrid=case_when(
    seas_year>=2014 ~ 1,
    T ~ 0
  )) %>% 
  filter(circ_id %in% c(1,2,4,6,7,11,13,14,18,22)) %>% 
  filter(race_id!=c(1046)) %>% 
  ggplot(aes(x=date_race,y=as.numeric(qual_time),color=as.factor(qual_round),fill=as.factor(hybrid))) +
  geom_point() +
  facet_wrap(vars(circ_name))


na.omit(qual_race) %>% 
  filter(seas_year<=2021) %>% 
  filter(circ_id %in% c(1,2,4,6,7,11,14,18,22)) %>%
  filter(qual_time<ms("2:45.00")) %>% 
  mutate(hybrid=case_when(
    seas_year>=2014 ~ 1,
    T ~ 0
  )) %>% 
  ggplot(aes(x=as.numeric(qual_time),y=as.factor(seas_year),fill=as.factor(hybrid),alpha=0.5)) +
  geom_density_ridges() +
  theme_linedraw() +
  theme(legend.position = "none") +
  facet_wrap(vars(circ_name),scales="free_x") +
  theme(axis.text = element_text(size = 6.5)) +
  labs(
    title="Distribution of Lap Times in Qualifying by Year and Circuit",
    caption="Color-coding indicates pre-Hybrid (red) vs. Hybrid (blue) Era",
    x="Lap Time (in Seconds)",
    y="Season Year"
  )



