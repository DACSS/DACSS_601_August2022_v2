# Setup -------------------------------------------------------------------



library(tidyverse)
library(lubridate)
library(GGally)



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
  mutate(race_fastest_time=ms(race_fastest_time))

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
  mutate(sprint_fastest_time=ms(sprint_fastest_time))


## Races ====

master_races<-read_csv(
  file="posts/_data/f1archive/races.csv",
  skip=1,
  col_select=1:6,
  col_names=c("race_id","seas_year","seas_round","circ_id","name_race","date_race")
)



races_laps<-read_csv(
  file="posts/_data/f1archive/lap_times.csv",
  skip=1,
  col_names=c("race_id","driv_id","lap","pos_race","time_lap_char","time_lap_ms"),
  col_types="nnnncn"
)

races_laps<-races_laps %>% 
  mutate(time_lap=ms(time_lap_char)) %>% 
  select(-"time_lap_char")



# Ideas ----

## Cutoff lap times in Q3 by track (boxplots?)
## (Best) Race lap times by quali lap times?
## You might need to full_join stands_cons and results_cons, for consistency, and take NAs
## Write a loop that determines race number in the season (e.g. 4/22)




# Visualizations ----------------------------------------------------------


fastest_laps<-races_laps %>% 
  group_by(race_id) %>% 
  summarise(time_lap_ms=min(time_lap_ms)) %>% ## You bleeding genius. (This makes sure it joins back cleanly)
  left_join(races_laps) %>% 
  left_join(master_drivs) %>% 
  left_join(master_races) 

fastest_laps %>% 
  filter(circ_id==6) %>% ## Monaco
  ggplot(.,aes(x=date_race,y=time_lap_ms)) +
  geom_point() +
  geom_line() +
  theme_bw()  



fastest_laps_filter<-fastest_laps %>% 
  count(circ_id) %>% 
  filter(n>=10) %>% 
  left_join(fastest_laps) %>% 
  select(!n) ## Fastest laps for circuits used 10+ times, joined with driver data
  

fastest_laps_filter %>% 
  ggplot(aes(x=date_race,y=time_lap_ms)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(vars(circ_id)) 




## Circuit 10 takes a massive drop suddenly... why?

circ_search(10)


## And what was with that massive dip on 3?


circ_search(3) ## bahrain...
master_races %>% filter(circ_id==3) ## 2020 had Bahrain and Sakhir...
## ... and the Sakhir GP had a different track layout!!!!!!!!
## So I've gotta sort not by circ_id but rather by name_race... maybe?
## Or maybe the data set should just get good and differentiate between
## the old and new Hockenheimringen, or between Bahrain and Sakhir.








fastest_laps %>% 
  filter(circ_id==10) %>% ## Hockenheimring
  ggplot(.,aes(x=date_race,y=time_lap_ms)) +
  geom_point() +
  geom_line() +
  theme_bw()

fastest_laps %>% 
  filter(circ_id==1) %>% ## Australia
  ggplot(.,aes(x=date_race,y=time_lap_ms)) +
  geom_point() +
  geom_line() +
  theme_bw()
















