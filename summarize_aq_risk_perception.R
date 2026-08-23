## summarize the air quality data from the risk perception data. 


library(tidyverse)

## read in files

ozone.summary <- read_csv("./Processed Data/ozone.summary.csv")
names(ozone.summary)

ozone.summary.wide <- ozone.summary |> 
                      mutate(O3 = ozone.max*1000) |> 
                      select(House.Number,Visit,Location, season, ac.type,  O3.ppb,O3.LOD.ppb,O3.Below.detection, O3) |> 
                      pivot_wider(names_from=Location, values_from=c(O3.ppb,O3.LOD.ppb,O3.Below.detection,O3)) |> 
                      mutate(`I/O_O3`= O3_In/O3_Out)

pm.summary <- read_csv("./Processed Data/EvapCooler.summary.csv")

pm.summary.select <- pm.summary |> 
                     select(House.Number,Visit,season, ac.type, first.day, day.type,PM2.5.UDAQ.ug.m3,`I/O_PM`) |> 
                     mutate(PM_in = PM2.5.UDAQ.ug.m3*`I/O_PM`) |> 
                     rename(PM_out =PM2.5.UDAQ.ug.m3) |> 
                     mutate(year = year(first.day))

names(pm.summary)


survey <- read_csv("./Processed Data/survey_aq_data.csv")

names(survey)


## grab the 16 homes included in the survey
survey_homes <- survey |> 
                rename(House.Number = HouseID) |> 
                select(House.Number) |> 
                distinct() |> 
                mutate(survey = TRUE)


combine <- pm.summary.select |> 
           left_join(ozone.summary.wide, by=c('House.Number','Visit', 'season','ac.type')) |> 
           left_join(survey_homes,by='House.Number') |> 
           mutate(survey = ifelse(is.na(survey),FALSE,survey))

#View(combine)
names(combine)

n_homes <- combine |> 
            group_by(House.Number) |> 
            summarize(PM_in_house = mean(PM_in,na.rm=T),
                        O3_In_house = mean(O3_In,na.rm=T),
                      wildfire.house = any(day.type =='Wildfire Smoke'),
                      AC.homes = any(ac.type =='AC'), 
                      EC.homes = any(ac.type == 'EC'))|> 
            ungroup() |> 
            summarize(count_PM = sum(!is.na(PM_in_house)),
                      count_O3 =  sum(!is.na(O3_In_house)),
                      count_wildfire = sum(wildfire.house, na.rm=TRUE),
                      count_AC = sum(AC.homes, na.rm=T),
                      count_EC = sum(EC.homes,na.rm=T))

n_visits_by_year_season <- combine |> 
            group_by(season, year) |> 
            summarize(PM_in_house = sum(!is.na(PM_in)),
                        O3_In_house = sum(!is.na(O3_In)),
                      wildfire.house = sum(day.type =='Wildfire Smoke'),
                      AC.visits= sum(ac.type =='AC'), 
                      EC.visits = sum(ac.type == 'EC'))
            
write_csv(n_visits_by_year_season,"./Processed Data/n_visits_by_year_season.csv")



n_homes_risk <- combine |> 
            filter(survey ==TRUE ) |> 
            group_by(House.Number) |> 
            summarize(PM_in_house = mean(PM_in,na.rm=T),
                        O3_In_house = mean(O3_In,na.rm=T),
                      wildfire.house = any(day.type =='Wildfire Smoke'),
                      AC.homes = any(ac.type =='AC'), 
                      EC.homes = any(ac.type == 'EC'),
                      O3.below.LOD = any(O3.Below.detection_In==TRUE))|> 
            ungroup() |> 
            summarize(count_PM = sum(!is.na(PM_in_house)),
                      count_O3 =  sum(!is.na(O3_In_house)),
                      count_wildfire = sum(wildfire.house, na.rm=TRUE),
                      count_AC = sum(AC.homes, na.rm=T),
                      count_EC = sum(EC.homes,na.rm=T),
                      count_O3.below.LOD = sum(O3.below.LOD,na.rm=T)) |> 
            mutate(dataset = 'risk follow-up')


n_homes_combined <- bind_rows(n_homes, n_homes_risk)

write_csv(n_homes_combined,"./Processed Data/n_homes_combined.csv")

visit_summary_PM <- combine |> 
                   summarize(PM.visits = sum(!is.na(PM_in)),
                  PM_indoor_mean =mean(PM_in,na.rm = T),
                  PM_indoor_median=median(PM_in,na.rm = T),
                  PM_indoor_min = min(PM_in,na.rm=T), 
                  PM_indoor_max = max(PM_in, na.rm=T),
                  PM_outdoor_mean =mean(PM_out,na.rm = T),
                  PM_outdoor_median=median(PM_out,na.rm = T),
                  PM_outdoor_min = min(PM_out,na.rm=T), 
                  PM_outdoor_max = max(PM_out, na.rm=T),
                  PM_IO_mean = mean(`I/O_PM`,na.rm=T),
                  PM_IO_median = median(`I/O_PM`,na.rm=T),
                  PM_IO_min = min(`I/O_PM`,na.rm=T),
                  PM_IO_max = max(`I/O_PM`,na.rm=T),
                  )
                   
visit_summary_O3 <- combine |> 
                   summarize(O3.visits = sum(!is.na(O3_In)),
                  O3_Indoor_mean =mean(O3_In,na.rm = T),
                  O3_Indoor_median=median(O3_In,na.rm = T),
                  O3_Indoor_min = min(O3_In,na.rm=T), 
                  O3_Indoor_max = max(O3_In, na.rm=T),
                  O3_Outdoor_mean =mean(O3_Out,na.rm = T),
                  O3_Outdoor_median=median(O3_Out,na.rm = T),
                  O3_Outdoor_min = min(O3_Out,na.rm=T), 
                  O3_Outdoor_max = max(O3_Out, na.rm=T),
                  O3_IO_mean = mean(`I/O_O3`,na.rm=T),
                  O3_IO_median = median(`I/O_O3`,na.rm=T),
                  O3_IO_min = min(`I/O_O3`,na.rm=T),
                  O3_IO_max = max(`I/O_O3`,na.rm=T),
                  )

write_csv(visit_summary_O3,'./Processed Data/visit_summary_O3.csv')
write_csv(visit_summary_PM,'./Processed Data/visit_summary_PM.csv')

## subset


visit_summary_PM_risk <- combine |> 
                  filter(survey ==TRUE ) |> 
                  summarize(PM.visits = sum(!is.na(PM_in)),
                  PM_indoor_mean =mean(PM_in,na.rm = T),
                  PM_indoor_median=median(PM_in,na.rm = T),
                  PM_indoor_min = min(PM_in,na.rm=T), 
                  PM_indoor_max = max(PM_in, na.rm=T),
                  PM_indoor_sd = sd(PM_in, na.rm=T),
                  PM_outdoor_mean =mean(PM_out,na.rm = T),
                  PM_outdoor_median=median(PM_out,na.rm = T),
                  PM_outdoor_min = min(PM_out,na.rm=T), 
                  PM_outdoor_max = max(PM_out, na.rm=T),
                  PM_outdoor_sd = sd(PM_out, na.rm=T),
                  PM_IO_mean = mean(`I/O_PM`,na.rm=T),
                  PM_IO_median = median(`I/O_PM`,na.rm=T),
                  PM_IO_min = min(`I/O_PM`,na.rm=T),
                  PM_IO_max = max(`I/O_PM`,na.rm=T),
                  PM_IO_sd = sd(`I/O_PM`,na.rm=T),
                  )
                   
visit_summary_O3_risk <- combine |> 
                    filter(survey ==TRUE ) |> 
                   summarize(O3.visits = sum(!is.na(O3_In)),
                  O3_Indoor_mean =mean(O3_In,na.rm = T),
                  O3_Indoor_median=median(O3_In,na.rm = T),
                  O3_Indoor_min = min(O3_In,na.rm=T), 
                  O3_Indoor_max = max(O3_In, na.rm=T),
                  O3_Indoor_sd = sd(O3_In, na.rm=T),
                  O3_Outdoor_mean =mean(O3_Out,na.rm = T),
                  O3_Outdoor_median=median(O3_Out,na.rm = T),
                  O3_Outdoor_min = min(O3_Out,na.rm=T), 
                  O3_Outdoor_max = max(O3_Out, na.rm=T),
                  O3_Outdoor_sd = sd(O3_Out, na.rm=T),
                  O3_IO_mean = mean(`I/O_O3`,na.rm=T),
                  O3_IO_median = median(`I/O_O3`,na.rm=T),
                  O3_IO_min = min(`I/O_O3`,na.rm=T),
                  O3_IO_max = max(`I/O_O3`,na.rm=T),
                  O3_IO_sd = sd(`I/O_O3`,na.rm=T),
                  )

write_csv(visit_summary_O3_risk,'./Processed Data/visit_summary_O3_risk.csv')
write_csv(visit_summary_PM_risk,'./Processed Data/visit_summary_PM_risk.csv')

### look at the total wildfire visits and O3 measurements below detection limits

summary_visits <- combine |> 
                  summarize(O3.measurements = sum(!is.na(O3_In)),
                  PM.measurements = sum(!is.na(PM_in)),
                  O3.below.detection = sum(O3.Below.detection_In==TRUE,na.rm=T),
                  wildfire.days = sum(day.type=='Wildfire Smoke')) |> 
                  mutate(dataset = 'all')

summary_visits_risk <- combine |> 
                 filter(survey ==TRUE ) |> 
                  summarize(O3.measurements = sum(!is.na(O3_In)),
                  PM.measurements = sum(!is.na(PM_in)),
                  O3.below.detection = sum(O3.Below.detection_In==TRUE,na.rm=T),
                  wildfire.days = sum(day.type=='Wildfire Smoke')) |> 
                  mutate(dataset = 'risk survey')

summary_visits_combined <- summary_visits |> 
                            bind_rows(summary_visits_risk)

write_csv(summary_visits_combined,"./Processed Data/summary_visits_combined.csv")

