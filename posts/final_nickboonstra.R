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


# Read in -----------------------------------------------------------------
# Circuits ====

circuits<-read_csv(
  file="posts/_data/f1archive/circuits.csv",
  col_select=1:8,
  skip=1,
  col_names=c("circuit_id","circuit_ref","circuit_name","circuit_loc","circuit_nation","circuit_lat","circuit_lng","circuit_alt")
)




# Constructors ====

#### You might need to full_join stands_cons and results_cons, for consistency, and take NAs
results_cons<-read_csv(
  file="posts/_data/f1archive/constructor_results.csv",
  skip=1,
  col_names=c("result_id_con","race_id","con_id","pts_race_con","status_id_con")
)

stands_cons<-read_csv(
  file="posts/_data/f1archive/constructor_standings.csv",
  skip=1,
  col_names=c("stands_id_con","race_id","con_id","pts_stands_con","con_stand","del","con_wins")
)

stands_cons<-purge_dels(stands_cons)


master_cons<-read_csv(
  file="posts/_data/f1archive/constructors.csv",
  col_select=1:4,
  skip=1,
  col_names=c("con_id","con_ref","con_name","con_nation")
)




# Drivers ====

stands_drivs<-read_csv(
  file="posts/_data/f1archive/driver_standings.csv",
  skip=1,
  col_names=c("stands_id_driv","race_id","driv_id","pts_stands_driv","driv_stand","del","driv_wins")
)

stands_drivs<-purge_dels(stands_drivs)


master_drivs<-read_csv(
  file="posts/_data/f1archive/drivers.csv",
  col_select=1:8,
  skip=1,
  col_names=c("driv_id","driv_ref","driv_num","driv_code","driv_first","driv_last","driv_dob","driv_nation")
)


results_drivs<-read_csv(
  file="posts/_data/f1archive/results.csv",
  skip=1,
  col_types="nnnnnnncnnncnnfcnn",
  col_names=c(
    "result_id_driv","race_id","driv_id",
    "con_id","driv_num","race_pos_grid",
    "race_pos_class","race_pos_char","race_pos_order",
    "pts_race_driv","laps_comp","del",
    "race_time_ms","fastest_num","fastest_rank",
    "fastest_time","fastest_speed","status_id_driv"
  )
)
results_drivs<-purge_dels(results_drivs)
results_drivs<-results_drivs %>% 
  mutate(race_time_per = ms(race_time_ms/1000)) %>%
  mutate(race_time_per = as.period(race_time_per, unit = "seconds"))



# Lap Times ====


lap_times<-read_csv(
  file="posts/_data/f1archive/lap_times.csv",
  skip=1,
  col_names=c("race_id","driv_id","lap_number","race_pos_lap","lap_time_char","lap_time_ms"),
  col_types="nnnncn"
)

lap_times<-lap_times %>% 
  mutate(lap_time_per = ms(lap_time_char)) %>%
  mutate(lap_time_per = as.period(lap_time_per, unit = "seconds"))



# Ideas ----

## Cutoff lap times in Q3 by track (boxplots?)
## (Best) Race lap times by quali lap times?





# Visualizations ----------------------------------------------------------

results_drivs %>% 
  ggplot(aes(x=race_pos_grid,y=race_pos_order)) +
  geom_bin2d()


start_finish<-data.frame(race_pos_grid=results_drivs$race_pos_grid,race_pos_order=results_drivs$race_pos_order)
ggpairs(start_finish)

