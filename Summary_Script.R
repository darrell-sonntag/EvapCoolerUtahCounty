#read in all libraries to be used
library(tidyverse)
library(lubridate)
library(hms)
library(stringr)
library(ggpmisc)
library(readxl)
library(dplyr)
library(RColorBrewer)
library(ggsignif)
library(patchwork)
#library(ggpubr)

 
#### 
#### 
####              SidePak_Script
####
#### 
#### 
### read in metadata and ac data

# make sure current directory is in the Github/EvapCoolerUtahCounty

path <- ".\\Data\\Research Data Master List.xlsx"
metadata <- read_excel(path = path)

acdata <- metadata %>%
  select("House.Number","Type of Air Conditioner") 


list.txt = list.files(".\\Data\\SidePak_CSV", pattern = "*.txt", full.names = TRUE)
list_2.txt = list.files(".\\Data\\SidePak_CSV_Corrected",full.names = T)

read_plus <- function(flnm) {
  read.table(flnm,header=T, skip=28, sep=",")[-1,] %>%
    mutate(date.time = mdy_hms(paste(Date,Time))) %>%
    mutate(Aerosol = as.numeric(Aerosol)) %>% 
    mutate(filename = flnm) 
}

sidepak.files <-
  list.txt %>% 
  map_df(~read_plus(.))

read_plus_2 <- function(flnm) {
  read.delim(flnm,header=T) %>%
    mutate(date.time = mdy_hms(paste(Date,Time))) %>%
    mutate(Aerosol.mg.m.3 = as.numeric(Aerosol.mg.m.3)) %>% 
    mutate(filename = flnm) 
}

sidepak.files_2 <-
  list_2.txt %>% 
  map_df(~read_plus_2(.)) 

sidepak.files_1b <- sidepak.files %>%
  mutate(filename = word(filename,2,sep="/"))

sidepak.files_2b <- sidepak.files_2 %>%
  mutate(filename = word(filename,2,sep="/")) %>%
  mutate(Aerosol = Aerosol.mg.m.3) %>%
  select(-Aerosol.mg.m.3,-Data.Point) %>%
  relocate(Aerosol,.after = Time)

sidepakfiles.all.data <- bind_rows(sidepak.files_1b,
                                   sidepak.files_2b) %>%
  filter(!is.na(Aerosol))

sidepakfiles.all <-
  sidepakfiles.all.data %>%
  mutate(Aerosol = Aerosol*1000 ) %>% ## change units of Aerosol from mg/m3 to ug/m3
  mutate(Date=mdy(Date)) %>%
  mutate(Time = lubridate::hms(Time)) %>%
  mutate(House.Number = word(filename,1,sep = "_")) %>%
  mutate(Visit = word(filename,2,sep = "_")) %>%
  mutate(Location = word(word(filename,3,sep = "_"),1,sep = ".txt"))

first.day <- sidepakfiles.all %>%
  group_by(House.Number,Visit,Location) %>%
  summarise(first.day = min(Date))

sidepakfiles.all.same.time <- sidepakfiles.all %>%
  #slice(100) %>%                          
  left_join(first.day,by=c('House.Number','Visit','Location')) %>%
  mutate(pseudo.day = Date - first.day) %>%
  mutate(pseudo.time = as.period(pseudo.day) + Time) %>%
  mutate(duration = as.duration(pseudo.time)) %>%
  mutate(duration.hours = duration/3600) %>%                              
  mutate(duration.hms = hms::hms(duration)) %>%
  left_join(acdata,by=c('House.Number')) %>%
  mutate(house.number.visit = paste(House.Number,Visit,sep=" ")) %>%
  mutate(round.time = round_date(date.time, unit = "minute")) %>%
  mutate(season = ifelse(month(Date) >= 6 & month(Date) <= 9,
                         "Summer", "Winter"))%>%
  mutate(House.number.int = as.integer(substring(House.Number, 2,3)))

all.same.time.table <- sidepakfiles.all.same.time %>%
  group_by(house.number.visit, Location) %>%
  dplyr::summarize(
    sum.Aerosol = sum(Aerosol,na.rm = T),
    min.time = min(round.time),
    max.time = max(round.time),
    interval = max.time - min.time,
    n.Aerosol = sum(!is.na(Aerosol)))%>% 
  mutate(interval.hour = as.numeric(as.duration(interval), "hours"))

sidepak.correlation <- sidepakfiles.all.same.time %>%
  select(House.Number, Visit, Location, "Type of Air Conditioner",house.number.visit,round.time,Aerosol) %>%
  pivot_wider(names_from = Location, values_from = Aerosol) %>%
  mutate(In = ifelse(In == "NULL",NA,In)) %>%
  mutate(Out = ifelse(Out == "NULL",NA,Out)) %>%
  mutate(In = as.numeric(In)) %>%
  mutate(Out= as.numeric(Out)) %>%
  mutate(in.check = ifelse(is.na(In),0,In)) %>%
  mutate(out.check = ifelse(is.na(Out),0,Out)) 


## house and visit list with homes that had both indoor and outdoor measurements  
hlist_nonmissing_table <- all.same.time.table %>%
  select(house.number.visit, Location, sum.Aerosol, n.Aerosol, interval.hour)%>%
  pivot_wider(names_from = Location, values_from = c(sum.Aerosol,n.Aerosol,interval.hour))

hlist_filtered <- hlist_nonmissing_table %>%
  filter(sum.Aerosol_In > 0 & sum.Aerosol_Out > 0) %>% ## remove houses with zero data
  filter(interval.hour_In > 4 & interval.hour_Out > 4) %>% ## remove data without at least 4 hours for both measurements
  filter(!(house.number.visit %in% c('H02 V1','H09 V2','H10 V1','H16 V1')))  

## H02 V1 (Sidepak data Indoor is on earlier day between 8pm to 5am measured at one second frequency)
## H09 V2 (indoor and outdoor data are separate dates, decided to remove)
## H10 V1 (indoor timestamp doesn't match outdoor)
## H16 V1 In & Out files are the same exact data

 
hlist_nonmissing <- hlist_filtered %>% 
  select(house.number.visit) %>%
  as.vector()

hlist_nonmissing <- unique(hlist_nonmissing$house.number.visit)
hlist_nonmissing  <- hlist_nonmissing[order(hlist_nonmissing )]

hlist <- unique(sidepakfiles.all.same.time$house.number.visit)
hlist <- hlist[order(hlist)]

###########
## Create sidepak.summary table 
## Only include hours with complete data 
##(e.g. if we are missing the outside sidepak data for certain hours, than filter those hours out)
## Make a new version of sidepak.file.all create it from sidepak correlation, so we can filter out the data. 
## Then  use pivot_longer to put it back into long format
## then make another sidepak.summary.qa where we have QA'd the data (and removed any sections where there appears to be an indoor air pollution source)

sidepak.correlation.complete <- sidepak.correlation %>%
  filter(house.number.visit %in% hlist_nonmissing) %>%
  filter(!is.na(In)&!is.na(Out)) %>%
  select(-c(in.check, out.check)) 

sidepak.incomplete <- sidepak.correlation.complete %>%
  pivot_longer(cols = c('In','Out'), names_to = 'Location', values_to ='Aerosol')


## files removed
## H08 V1 (outdoor missing data)
## H15 V1 (only short amount of indoor time)
## Note: H09 & H15 same house (not yet corrected)


## QAd files removed suspicious data (By Han)
## looks like indoor source of air pollution
sidepak.correlation.qa <- sidepak.correlation.complete %>%
  filter(!House.Number == 'H03' | !Visit == 'V1' | !between(round.time,ymd_hms('2022-07-27 19:54:00'),ymd_hms('2022-07-28 11:20:00')))  %>%
  filter(!House.Number == 'H03' | !Visit == 'V2' | !between(round.time,ymd_hms('2022-12-09 08:19:00'),ymd_hms('2022-12-09 11:01:00')))  %>%
  filter(!House.Number == 'H08' | !Visit == 'V2' | !between(round.time,ymd_hms('2022-09-09 10:15:00'),ymd_hms('2022-09-09 14:03:00')))  %>%
  filter(!House.Number == 'H10' | !Visit == 'V2' | !between(round.time,ymd_hms('2022-12-01 07:45:00'),ymd_hms('2022-12-01 09:55:00')))  %>%
  filter(!House.Number == 'H12' | !Visit == 'V1' | !between(round.time,ymd_hms('2022-08-12 10:30:00'),ymd_hms('2022-08-12 14:34:00')))  %>%
  filter(!House.Number == 'H17' | !Visit == 'V2' | !between(round.time,ymd_hms('2023-01-28 04:06:00'),ymd_hms('2023-01-28 05:43:00')))  %>%
  filter(!House.Number == 'H29' | !Visit == 'V2' | !between(round.time,ymd_hms('2023-08-21 20:10:00'),ymd_hms('2023-08-21 23:16:00')))  %>%
  filter(!House.Number == 'H33' | !Visit == 'V1' | !between(round.time,ymd_hms('2023-09-01 13:02:00'),ymd_hms('2023-09-01 16:36:00')))  %>%
  filter(!House.Number == 'H17' | !Visit == 'V3') %>%
  mutate(House.number.int = as.integer(substring(House.Number, 2,3)))

sidepak.complete.qa <- sidepak.correlation.qa %>%
  pivot_longer(cols = c('In','Out'), names_to = 'Location', values_to ='Aerosol')

sidepak.summary.qa <- sidepak.complete.qa %>%
  #break up into groups
  group_by(House.Number,Visit,Location) %>%
  dplyr::summarise(SidePak.ug.m3.avg =  mean(Aerosol),SidePak.ug.m3.stdev =  sd(Aerosol),
                   SidePak.ug.m3.min =  min(Aerosol), SidePak.ug.m3.max =  max(Aerosol),
                   SidePak.num.points = n(),
                   SidePak.min.time = min(round.time),
                   SidePak.max.time = max(round.time),
                   Sidepak.tenth = quantile(Aerosol, probs=0.1),
                   Sidepak.ninetieth = quantile(Aerosol, probs=0.9))%>%
  mutate(SidePak.interval.measured = as.numeric(as.duration(SidePak.max.time - SidePak.min.time),"hours")) %>%
  mutate(SidePak.is.valid = ifelse(SidePak.interval.measured < 4,"N","Y"))%>% #should have at least 12 hours for measurement
  filter(SidePak.is.valid =='Y') 

### How big is our sample now?
# 53 visits on 30 homes (note: H09 and H15 are the same home)
house.visit.counts.complete <- sidepak.summary.qa %>%
  left_join(acdata,by='House.Number') %>%
  mutate(house.number.visit = paste(House.Number,Visit,sep=" ")) %>%
  group_by(`Type of Air Conditioner`) %>%
  dplyr::summarize(house = length(unique(House.Number)),visits =length(unique(house.number.visit)))


## create empty data frame to store correlation stats

r.2.qa <- data.frame(House.Number = character(), 
                     Visit = character(),
                     `Type of Air Conditioner`=character(),
                     Date = POSIXct(),
                     intercept = numeric(),
                     intercept.lower =numeric(),
                     intercept.upper = numeric(),
                     slope = numeric(),
                     slope.lower = numeric(),
                     slope.upper = numeric(),
                     p.value = numeric(),
                     r.squared = numeric()
)

hlist_nonmissing.qa = hlist_nonmissing[hlist_nonmissing != 'H17 V3']
## H17 V3 has suspicously high indoor concentrations relative to outdoor remove from the QA list

i=1

for(i in 1:length(hlist_nonmissing.qa)){
  data.i<- filter(sidepak.correlation.qa,house.number.visit==hlist_nonmissing.qa[i])
  lm.i <- lm(In~Out,data.i,na.action = na.omit)
  confint(lm.i,3,.95)[1]
  
  r.2.qa[i,1] = unique(data.i$House.Number)
  r.2.qa[i,2] = unique(data.i$Visit)
  r.2.qa[i,3] = unique(data.i$`Type of Air Conditioner`)
  r.2.qa[i,4] = min(data.i$round.time)
  r.2.qa[i,5] = summary(lm.i)$coefficients['(Intercept)','Estimate']
  r.2.qa[i,6] = confint(lm.i,1,.95)[1]
  r.2.qa[i,7] = confint(lm.i,1,.95)[2]
  r.2.qa[i,8] = summary(lm.i)$coefficients['Out','Estimate']
  r.2.qa[i,9] = confint(lm.i,2,.95)[1]
  r.2.qa[i,10] = confint(lm.i,2,.95)[2]  
  r.2.qa[i,11] = summary(lm.i)$coefficients['Out','Pr(>|t|)']
  r.2.qa[i,12]= summary(lm.i)$r.squared
}

r.2.qa$sig = with(r.2.qa,ifelse(p.value<0.05,'sig','non-sig'))
r.2.qa$r.squared = format(r.2.qa$r.squared,scientific=F)

write.csv(r.2.qa,".//Processed Data//Sidepak.correlation.csv",row.names = FALSE)


#### 
#### 
#### 
####        HourlyMonitorData_Script
####
#### 
#### 


site.lat <- data.frame(Monitor =c('Spanish Fork','Lindon'), Latitude = c(40.136398,40.3414))

visitmaster.list <- read_excel(".\\Data\\Research Data Master List.xlsx", sheet = "Visit", 
                               col_types = c("text", "text", "date","date","date","date", "numeric","numeric","text")) 

nearestmonitor<- read.csv(".\\Data\\monitor_city_assignment.csv")%>%
  mutate(House.Number = paste("H",str_pad(House.ID,2,pad = "0"),sep = "")) %>%
  select(House.Number,Monitor.closest) 

### edit times, and bring in the nearestmonitor

visitmaster.list.2 <- visitmaster.list %>%
  mutate(start.time = as_hms(start.time)) %>%
  mutate(end.time =as_hms(end.time)) %>%
  mutate(start_date = as_datetime(paste(first.day,start.time))) %>%
  mutate(end_date = as_datetime(paste(end.day,end.time))) %>%
  left_join(nearestmonitor,by='House.Number') %>%
  mutate(Monitor.closest = ifelse(first.day ==ymd('2022-12-08'),'Lindon',Monitor.closest)) ## For some reason, the UDAQ data is missing for Spanish Fork on Dec.8,2012


## add in the closest monitor to visit master.list.2

Summer2022Hourly <- read.csv(".\\Data\\AirNowAPI\\2022 Summer Output.csv") %>%
  left_join(site.lat, by = "Latitude") %>%
  mutate(Date = paste(Date,":00",sep="")) %>%
  mutate(Date=ymd_hms(Date))

Winter2022Hourly <- read.csv(".\\Data\\AirNowAPI\\2022-2023 Winter PM Output.csv") %>%
  left_join(site.lat, by = "Latitude") %>%
  mutate(Date = paste(Date,":00",sep="")) %>%
  mutate(Date=ymd_hms(Date))

Summer2023Hourly <- read.csv(".\\Data\\AirNowAPI\\2023 Summer Output.csv") %>%
  left_join(site.lat, by = "Latitude") %>%
  mutate(Date = paste(Date,":00",sep="")) %>%
  mutate(Date=ymd_hms(Date))

Summer2022.23Hourly <- bind_rows(Summer2022Hourly,Summer2023Hourly)


Summer2022.23.Hourly.visits <- inner_join(Summer2022.23Hourly,visitmaster.list.2,join_by("Monitor" == "Monitor.closest",
                                                                                         between(Date,start_date,end_date))) 

Winter2022Hourly.visits <- inner_join(Winter2022Hourly,visitmaster.list.2,join_by("Monitor" == "Monitor.closest",
                                                                                  between(Date,start_date,end_date))) 

Hourly.visits.all <- bind_rows(Summer2022.23.Hourly.visits,Winter2022Hourly.visits) %>%
  filter(Concentration != -999)

## Next we can calculate the averages of the EPA data according to the house, visit, and pollutant, and then we can compare to our data.

# https://dplyr.tidyverse.org/reference/join_by.html

API.summary.all <- Hourly.visits.all %>%
  group_by(House.Number, Visit, Parameter,first.day) %>%
  dplyr::summarize(
    UDAQ.mean = mean(Concentration),
    UDAQ.n = sum(!is.na(Concentration))
  )

API.summary.wide.all <- API.summary.all  %>%
  pivot_wider(names_from = Parameter, values_from = c(UDAQ.mean,UDAQ.n))  %>%
  rename(PM2.5.UDAQ.ug.m3 = UDAQ.mean_PM2.5) %>%
  rename(Ozone.UDAQ.ppb = UDAQ.mean_OZONE) %>%
  mutate(season = ifelse(month(first.day) >= 6 & month(first.day) <= 9,
                         "Summer", "Winter"))

## How many total unique home visits and houses did we have? (whether we collected data or not)

house.visit.counts.raw <- API.summary.wide.all %>%
  left_join(acdata,by='House.Number') %>%
  mutate(house.number.visit = paste(House.Number,Visit,sep=" ")) %>%
  group_by(`Type of Air Conditioner`) %>%
  dplyr::summarize(house = length(unique(House.Number)),visits =length(unique(house.number.visit)))
 

#### 
#### 
####        TRH_Script
####
#### 

list_TRH.txt = list.files(".\\Data\\TRH_CSV",full.names = T)
list_TRH2023.txt = list.files(".\\Data\\TRH_CSV 2023",full.names = T)

#Creates a function to read in the TRH data
read_TRH <- function(flnm) 
{
  read.delim(flnm,header=T) %>%
    mutate(date.time = mdy_hms(paste(Date,Time))) %>%
    mutate(filename = flnm) 
}
#Reads in the the TRH data  
TRH.data.r <-
  list_TRH.txt %>% 
  map_df(~read_TRH(.))

#Reads in the the TRH data 
TRH.data.2023 <-
  list_TRH2023.txt %>% 
  map_df(~read_TRH(.))

# H23_V1 replace one erroneous value 
TRH.data.updated <- TRH.data.r %>%                               
  mutate(Ch1_Value = replace(Ch1_Value, Ch1_Value < 0, NA))  %>%
  mutate(Ch2_Value = replace(Ch2_Value, Ch2_Value == -39.4, NA))

# combine together
TRH.data <- bind_rows(TRH.data.updated,TRH.data.2023)

#Creates a summary table of the TRH Data
TRH_Summary_Table <- TRH.data %>%
  group_by(filename) %>%
  dplyr::summarize(
    average.RH = mean(Ch1_Value,na.rm=T),
    std.dev.RH = sd(Ch1_Value,na.rm=T),
    min.RH = min(Ch1_Value,na.rm=T),
    max.RH = max(Ch1_Value,na.rm=T),
    average.temperature.Celsius = mean(Ch2_Value,na.rm=T),
    std.dev.temperature = sd(Ch2_Value,na.rm=T),
    min.temperature = min(Ch2_Value,na.rm=T),
    max.temperature = max(Ch2_Value,na.rm=T),
    time.hours = length(Ch1_Value)*5/60
    
  ) %>%
  mutate(filename = word(filename,2,sep="/")) %>%
  mutate(filename = substr(filename,1,nchar(filename)-4)) %>%
  mutate(House.Number = word(filename,1,sep = "_")) %>%
  mutate(Visit = word(filename,2,sep = "_")) %>%
  mutate(Sample.Location = word(filename,3,sep = "_"))

#### merge the AC data to the TRH.data
TRH_Summary.wide <- TRH_Summary_Table %>%
  left_join(acdata,by=c('House.Number'))

write.csv(TRH_Summary.wide,".//Processed Data//TRH_Summary.csv",row.names = FALSE)

######
TRH.data2 <- TRH.data %>%
  mutate(filename = word(filename,2,sep="/")) %>%
  mutate(Date=mdy(Date)) %>%
  mutate(Time = lubridate::hms(Time)) %>%
  mutate(House.Number = word(filename,1,sep = "_")) %>%
  mutate(Visit = word(filename,2,sep = "_")) %>%
  mutate(Sample.Location = word(word(filename,3,sep = "_"),1,sep = ".txt"))

#### 
#### 
####
####        Monitor_Comparison_API
####
#### 
#### 

in.out <- data.frame(Location = c('In','Out'))

API.summary.wide.all.location <- API.summary.wide.all  %>%
  merge(in.out)

#Qa'd data
sidepak.summary.2 <- sidepak.summary.qa

## Bringing API.summary data from HourlyMonitorData_Script.R
## Compare SidePak summary data created at the end of SidePak_Script.R to the EPA monitor data
sidepak.epa.comparison <- sidepak.summary.2 %>%
  full_join(API.summary.wide.all.location,by = c("House.Number","Visit","Location"))%>%
  left_join(nearestmonitor,by='House.Number')%>%
  mutate(season = ifelse(month(SidePak.min.time) >= 6 & month(SidePak.min.time) <= 9,
                         "Summer", "Winter")) #%>% ## grab season from the sidepak time

sidepak.epa.summer <- sidepak.epa.comparison %>%
  filter(season=='Summer') %>%
  filter(Location == 'Out')

sidepak.epa.winter <- sidepak.epa.comparison %>%
  filter(season=='Winter') %>%
  filter(Location == 'Out')

sidepak.epa.out <- sidepak.epa.comparison %>%
  filter(Location == 'Out') %>%  ## Just compare the outdoor measurements
  filter(!is.na(SidePak.ug.m3.avg))

#Ozone Data and Plots
studyozone<- read_excel(".\\Data\\Ozone\\Ozone Data_corrected.xlsx")%>%
  select("House.Number","Visit","Location","Conc (mg/m3)","ppm","LOD ppm") %>%
  mutate(O3.Below.detection = grepl("<", `Conc (mg/m3)`)) %>%
  rename(O3.ppm = ppm) %>%
  rename(O3.mg.m3 = "Conc (mg/m3)") %>%
  rename(O3.LOD.ppm = "LOD ppm") 


study.summary <- sidepak.epa.comparison %>%
  left_join(studyozone, by = c('House.Number','Visit','Location')) 


write.csv(study.summary,".//Data//Processed Data//study.API.summary.csv",row.names = FALSE)

study.summary.out <- study.summary  %>%
  filter(Location == "Out")    %>%
  mutate(O3.ppb = 1000 * as.numeric(O3.ppm)) 

summary(lm(Ozone.UDAQ.ppb~O3.ppb,study.summary.out, na.action = na.omit))

# Join TRH data
# new summary table
summary <- study.summary  %>%
  left_join(unique(TRH_Summary_Table), by = c("House.Number","Visit","Location" = "Sample.Location")) %>%
  left_join(acdata,by=c('House.Number')) %>%
  mutate(O3.ppb = 1000 * as.numeric(O3.ppm)) 



#### 
#### 
####
####        MetaData_Script
####
#### 
#### 


#https://stackoverflow.com/questions/70347622/lubridate-get-hours-and-minutes-from-decimal-hours

smoke.event <- interval(ymd('2022-09-08'),ymd('2022-09-12'))

sidepak.complete <- summary %>%
  mutate(house.number.visit = paste(House.Number,Visit,sep=" ")) %>%
  mutate(house.number.visit.date = paste(first.day,house.number.visit,sep=" ")) %>%
  filter(house.number.visit %in% hlist_nonmissing) %>%
  mutate(ac.type = ifelse(`Type of Air Conditioner`=='Central','Central',ifelse(`Type of Air Conditioner`=='Evaporative','Evap',NA))) %>%
  mutate(ac.season = paste(ac.type,season,sep='-')) %>%
  mutate(day.type = ifelse(as_date(first.day) %within% smoke.event,'Wildfire Smoke','Normal')) %>%
  #mutate(day.type = ifelse(PM2.5.UDAQ.ug.m3>15 & season =='Summer','Wildfire Smoke','Normal')) %>%
  mutate(House.Number = ifelse(house.number.visit =='H15 V2','H09',House.Number)) %>% ### Change H15 V2 to H09 V4
  mutate(Visit = ifelse(house.number.visit =='H15 V2','V4',Visit)) %>% ### Change H15 V2 to H09 V4
  mutate(house.number.visit = ifelse(house.number.visit =='H15 V2','H09 V4',house.number.visit)) %>% ### Change H15 V2 to H09 V4
  filter(house.number.visit != 'H17 V3') 

#### 
#### 
####
####        SidePak_analysis
####
#### 
#### 

#####
## Overview Conduct statistical analysis on the SidePak data

## Steps: 
## Evaluate if there are significant differences between the I/O ratios, R2, and slopes between the different groups
## Combine the regression statistics with the summary data
## Visualize the data
## combine regression statistics with the summary data
## grab sidepak.complete from the MetaData_Script.R

sidepak.complete.io <- sidepak.complete %>%
  select(House.Number,Visit,Location,house.number.visit,house.number.visit.date,SidePak.ug.m3.avg,`Type of Air Conditioner`,ac.type,ac.season,
         first.day,Monitor.closest,PM2.5.UDAQ.ug.m3,season,day.type,average.temperature.Celsius,
         average.RH,min.temperature,max.temperature) %>%
  pivot_wider(names_from = Location,values_from = c(SidePak.ug.m3.avg,average.temperature.Celsius,
                                                    average.RH,min.temperature,max.temperature),names_sort = T) %>%
  mutate(`I/O` = SidePak.ug.m3.avg_In / SidePak.ug.m3.avg_Out) %>%
  mutate(Outdoor.SidePak.UDAQ.ratio = SidePak.ug.m3.avg_Out/PM2.5.UDAQ.ug.m3)  %>%
  mutate(Outdoor.SidePak.UDAQ.diff = SidePak.ug.m3.avg_Out -PM2.5.UDAQ.ug.m3 )


r.2.final <- r.2.qa

r.2.merge <- r.2.final %>%
  select(-c('Type.of.Air.Conditioner','Date'))

## merge together
sidepak.stats <- left_join(sidepak.complete.io,r.2.merge,by=c('House.Number','Visit')) %>%
  mutate(r.squared = as.numeric(r.squared))%>%
  rename(Cs = intercept) %>%
  rename(Cs.lower = intercept.lower) %>%
  rename(Cs.upper = intercept.upper) %>%
  rename(Fin = slope) %>%
  rename(Fin.lower = slope.lower) %>%
  rename(Fin.upper = slope.upper) %>%
  rename(R2 = r.squared) %>%
  mutate(ac.type = factor(ac.type,levels=c('Central','Evap'),ordered=T)) %>%
  filter(!(house.number.visit %in% c('H09 V4','H07 V2','H10 V2'))) %>%
  filter(!is.na(SidePak.ug.m3.avg_In)) ## remove any missing values (e.g. H17 V3)

### Remove "H09 V4" H15 V2 or  (unrealistically low outdoor concentration),
## (note-- it rained 0.25 inches on 8/22/2023 (the day of the pickup))
## That is causing a high indoor/outdoor concentration 
## Also remove other winter days, 'H07 V2','H10 V2' that had unrealistically low outdoor concentrations

unique(sidepak.stats$ac.type)

## House and visit counts

house.visit.season.counts <- sidepak.stats %>%
  group_by(season,ac.type) %>%
  dplyr::summarize(house = length(unique(House.Number)),visits =length(unique(house.number.visit)))

###

house.visit.counts <- sidepak.stats %>%
  group_by(ac.type) %>%
  dplyr::summarize(house = length(unique(House.Number)),visits =length(unique(house.number.visit)))

### create TRH_temp_summary_Table by season
TRH_summary_Table_season <- sidepak.stats %>%
  group_by(ac.type,season) %>%
  filter(house.number.visit != c('H04 V2','H05 V2')) %>%
  dplyr::summarize(
    count.In = sum(!is.na(average.temperature.Celsius_In)),
    count.Out = sum(!is.na(average.temperature.Celsius_Out)),
    average.temp.In = mean(average.temperature.Celsius_In,na.rm=T),
    std.dev.temp.In = sd(average.temperature.Celsius_In,na.rm=T),
    min.temp.In = min(average.temperature.Celsius_In,na.rm=T),
    max.temp.In = max(average.temperature.Celsius_In,na.rm=T),
    average.temp.Out = mean(average.temperature.Celsius_Out,na.rm=T),
    std.dev.temp.Out = sd(average.temperature.Celsius_Out,na.rm=T),
    min.temp.Out = min(average.temperature.Celsius_Out,na.rm=T),
    max.temp.Out = max(average.temperature.Celsius_Out,na.rm=T),
    average.RH.In = mean(average.RH_In,na.rm=T),
    std.dev.RH.In = sd(average.RH_In,na.rm=T),
    min.RH.In = min(average.RH_In,na.rm=T),
    max.RH.In = max(average.RH_In,na.rm=T),
    average.RH.Out = mean(average.RH_Out,na.rm=T),
    std.dev.RH.Out = sd(average.RH_Out,na.rm=T),
    min.RH.Out = min(average.RH_Out,na.rm=T),
    max.RH.Out = max(average.RH_Out,na.rm=T),
    
  )

write.csv(TRH_summary_Table_season, ".//data//Processed Data//TRH_summary_Table_season.csv",row.names = FALSE)

####

sidepak.stats$house.number.visit <- factor(sidepak.stats$house.number.visit,levels=rev(sort(unique(sidepak.stats$house.number.visit))),ordered=T)
sidepak.stats$house.number.visit.date <- factor(sidepak.stats$house.number.visit.date,levels=rev(sort(unique(sidepak.stats$house.number.visit.date))),ordered=T)

own.colors <- brewer.pal(n = 8, name = "Set1")[c(3:8)]

levels(sidepak.stats$house.number.visit)

############################
##### linear regression #####
############################

sidepak.stats.central.summer <- sidepak.stats %>%
                                filter(season =='Summer' & ac.type =='Central')

sidepak.stats.evap.summer <- sidepak.stats %>%
                                filter(season =='Summer' & ac.type =='Evap')

sidepak.stats.central.winter <- sidepak.stats %>%
                                  filter(season =='Winter' & ac.type =='Central')

sidepak.stats.evap.winter <- sidepak.stats %>%
                                  filter(season =='Winter' & ac.type =='Evap')


lm.central.summer <- lm(SidePak.ug.m3.avg_In~SidePak.ug.m3.avg_Out,data=sidepak.stats.central.summer,na.action = na.omit)
lm.evap.summer <- lm(SidePak.ug.m3.avg_In~SidePak.ug.m3.avg_Out,data=sidepak.stats.evap.summer,na.action = na.omit)

lm.central.winter <- lm(SidePak.ug.m3.avg_In~SidePak.ug.m3.avg_Out,data=sidepak.stats.central.winter,na.action = na.omit)
lm.evap.winter <- lm(SidePak.ug.m3.avg_In~SidePak.ug.m3.avg_Out,data=sidepak.stats.evap.winter,na.action = na.omit)

lm.coefficients.central.summer <- data.frame(summary(lm.central.summer)$coefficients) %>%
                                  mutate(obs = sum(!is.na(sidepak.stats.central.summer$SidePak.ug.m3.avg_In))) %>%
                                  mutate(group = 'Central + Summer') %>%
                                  mutate(qt = qt(.975,df=(obs-1))) %>%
                                  mutate(bound = qt*Std..Error) %>%
                                  mutate(lower.95 = Estimate-bound) %>%
                                  mutate(upper.95 = Estimate+bound)


lm.coefficients.evap.summer <- data.frame(summary(lm.evap.summer)$coefficients) %>%                        
                                mutate(obs = sum(!is.na(sidepak.stats.evap.summer$SidePak.ug.m3.avg_In))) %>%
                                  mutate(group = 'Evap + Summer') %>%
                                  mutate(qt = qt(.975,df=(obs-1))) %>%
                                  mutate(bound = qt*Std..Error) %>%
                                  mutate(lower.95 = Estimate-bound) %>%
                                  mutate(upper.95 = Estimate+bound)

lm.coefficients.central.winter <- data.frame(summary(lm.central.winter)$coefficients) %>%
                                  mutate(obs = sum(!is.na(sidepak.stats.central.winter$SidePak.ug.m3.avg_In))) %>%
                                  mutate(group = 'Central + Winter') %>%
                                  mutate(qt = qt(.975,df=(obs-1))) %>%
                                  mutate(bound = qt*Std..Error) %>%
                                  mutate(lower.95 = Estimate-bound) %>%
                                  mutate(upper.95 = Estimate+bound)
  
lm.coefficients.evap.winter <- data.frame(summary(lm.evap.winter)$coefficients) %>% 
                                mutate(obs = sum(!is.na(sidepak.stats.evap.winter$SidePak.ug.m3.avg_In))) %>%
                                  mutate(group = 'Central + Winter') %>%
                                  mutate(qt = qt(.975,df=(obs-1))) %>%
                                  mutate(bound = qt*Std..Error) %>%
                                  mutate(lower.95 = Estimate-bound) %>%
                                  mutate(upper.95 = Estimate+bound)

##########
### What about when the wildfire days are removed?
##########

sidepak.stats.central.summer.no.fire <- sidepak.stats %>%
  filter(season =='Summer' & ac.type =='Central' & day.type =='Normal')

sidepak.stats.evap.summer.no.fire <- sidepak.stats %>%
  filter(season =='Summer' & ac.type =='Evap' & day.type =='Normal')


lm.central.summer.no.fire <- lm(SidePak.ug.m3.avg_In~SidePak.ug.m3.avg_Out,data=sidepak.stats.central.summer.no.fire,na.action = na.omit)
lm.evap.summer.no.fire <- lm(SidePak.ug.m3.avg_In~SidePak.ug.m3.avg_Out,data=sidepak.stats.evap.summer.no.fire,na.action = na.omit)

lm.coefficients.central.summer.no.fire <- data.frame(summary(lm.central.summer.no.fire)$coefficients) %>%
                                    mutate(obs = sum(!is.na(sidepak.stats.central.summer.no.fire$SidePak.ug.m3.avg_In))) %>%
                                    mutate(group = 'Central + Summer + No Wildfire') %>%
                                    mutate(qt = qt(.975,df=(obs-1))) %>%
                                    mutate(bound = qt*Std..Error) %>%
                                    mutate(lower.95 = Estimate-bound) %>%
                                    mutate(upper.95 = Estimate+bound)

lm.coefficients.evap.summer.no.fire <- data.frame(summary(lm.evap.summer.no.fire)$coefficients) %>%                        
                                      mutate(obs = sum(!is.na(sidepak.stats.evap.summer.no.fire$SidePak.ug.m3.avg_In))) %>%
                                      mutate(group = 'Evap + Summer + No Wildfire') %>%
                                      mutate(qt = qt(.975,df=(obs-1))) %>%
                                      mutate(bound = qt*Std..Error) %>%
                                      mutate(lower.95 = Estimate-bound) %>%
                                      mutate(upper.95 = Estimate+bound)

## combine and export

lm.coefficients.ac.season <- bind_rows(lm.coefficients.central.summer,
                                       lm.coefficients.evap.summer,
                                       lm.coefficients.central.winter,
                                       lm.coefficients.evap.winter,
                                       lm.coefficients.central.summer.no.fire,
                                       lm.coefficients.evap.summer.no.fire)


#######################
### Pooled by season
### Evaluate interaction of the slope by evaporative type
######################

lapply(sidepak.stats,class)

levels(sidepak.stats$ac.type)

## Set the contrasts to be what I want them to be

## Central 0
## Evap 1

## change the order for the linear model
sidepak.stats$ac.type <- factor(sidepak.stats$ac.type,levels=c('Central','Evap'),ordered=T)

levels(sidepak.stats$ac.type)

## https://stats.oarc.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/


sidepak.stats.summer <- sidepak.stats %>%
  filter(season =='Summer') 

sidepak.stats.winter <- sidepak.stats %>%
  filter(season =='Winter') 

contrasts(sidepak.stats$ac.type)<- contr.treatment(2)
contrasts(sidepak.stats.summer$ac.type)

summer.in.lm <- lm(SidePak.ug.m3.avg_In~ac.type*SidePak.ug.m3.avg_Out,data=sidepak.stats.summer,na.action = na.omit)
summary(summer.in.lm)

winter.in.lm <- lm(SidePak.ug.m3.avg_In~ac.type*SidePak.ug.m3.avg_Out,data=sidepak.stats.winter,na.action = na.omit)

summary(winter.in.lm)

### is it still significant, even after removing the wildfire days?, moderately
sidepak.stats.summer.no.fire <- sidepak.stats %>%
  filter(season =='Summer') %>%
  filter(day.type =='Normal')

summer.in.no.fire.lm <- lm(SidePak.ug.m3.avg_In~ac.type*SidePak.ug.m3.avg_Out,data=sidepak.stats.summer.no.fire,na.action = na.omit)
summary(summer.in.no.fire.lm)

###

summer.lm.coefficients <- data.frame(summary(summer.in.lm)$coefficients)
summer.no.fire.lm.coefficients <- data.frame(summary(summer.in.no.fire.lm)$coefficients)
winter.lm.coefficients <- data.frame(summary(winter.in.lm)$coefficients)

summer.lm.coefficients$obs <- sum(!is.na(sidepak.stats.summer$SidePak.ug.m3.avg_In))
summer.no.fire.lm.coefficients$obs <- sum(!is.na(sidepak.stats.summer.no.fire$SidePak.ug.m3.avg_In))
winter.lm.coefficients$obs <- sum(!is.na(sidepak.stats.winter$SidePak.ug.m3.avg_In))

summer.lm.coefficients$regression <- 'Summer'
summer.no.fire.lm.coefficients$regression <-'Summer+no fire'
winter.lm.coefficients$regression <- 'Winter'

class(summer.lm.coefficients)
lm.coefficients <- bind_rows(summer.lm.coefficients,summer.no.fire.lm.coefficients,winter.lm.coefficients)

### calculate confidence intervals
confint(summer.in.lm, 3, level = 0.95)

sidepak.stats.summer.evap <- sidepak.stats %>%
  filter(season =='Summer') %>%
  filter(ac.type =='Evap')

summer.in.evap.lm <- lm(SidePak.ug.m3.avg_In~SidePak.ug.m3.avg_Out,data=sidepak.stats.summer.evap,na.action = na.omit)
summary(summer.in.evap.lm)

confint(summer.in.evap.lm, 2, level = 0.95)




################
## box plots
#############

## make plots of I/O, Fin, Cs, R2
## pivot longer

sidepak.stats.long <- sidepak.stats %>%
  rename('Indoor SidePak' = 'SidePak.ug.m3.avg_In') %>%
  rename('Outdoor SidePak' = 'SidePak.ug.m3.avg_Out') %>%
  rename('Outdoor UDAQ' = 'PM2.5.UDAQ.ug.m3') %>%
  select(-c( "Outdoor.SidePak.UDAQ.ratio","Outdoor.SidePak.UDAQ.diff")) %>%
  pivot_longer(cols = c('Indoor SidePak','Outdoor SidePak','Outdoor UDAQ',`I/O`,Cs,Fin,R2), names_to ="statistic",
               values_to = "value") %>%
  mutate(statistic = factor(statistic,levels=c('Outdoor UDAQ','Outdoor SidePak','Indoor SidePak','I/O','Cs','Fin','R2'),ordered=T))

## I/O

## calculate means and 95% CI
sidepak.stat.long.means <- sidepak.stats.long %>%    
  group_by(statistic,ac.type,season) %>%
  dplyr::summarize(mean = mean(value,na.rm=T),median = median(value,na.rm=T),
                   sd=sd(value,na.rm=T),n=sum(!is.na(value)),
                   min=min(value,na.rm=T),max=max(value,na.rm=T)) %>%
  mutate(tcrit = qt(.975,df=(n-1))) %>%
  mutate(bound = tcrit*sd/sqrt(n)) %>%
  mutate(lower.95 = mean-bound) %>%
  mutate(upper.95 = mean+bound ) %>%
  mutate(statistic = factor(statistic,levels=c('Outdoor UDAQ','Outdoor SidePak','Indoor SidePak','I/O','Cs','Fin','R2'),ordered=T))


own.colors <- brewer.pal(n = 9, name = "Set1")[c(3:9)]
# display.brewer.all()

## box plot with just concentrations

sidepak.stats.long.con <- sidepak.stats.long %>%
  filter(statistic %in% c('Indoor SidePak','Outdoor SidePak','Outdoor UDAQ','I/O'))

sidepak.stat.long.means.con <- sidepak.stat.long.means %>%
  filter(statistic %in% c('Indoor SidePak','Outdoor SidePak','Outdoor UDAQ','I/O'))

## box plot with just regression estimates: Cs, Fin, and R2

sidepak.stats.long.reg <- sidepak.stats.long %>%
  filter(statistic %in% c('Cs','Fin','R2'))

sidepak.stat.long.means.reg <- sidepak.stat.long.means %>%
  filter(statistic %in% c('Cs','Fin','R2'))

## box plot with key paired data

sidepak.stats.long.paired <- sidepak.stats.long %>%
  filter(statistic %in% c('I/O','Cs','Fin','R2'))

sidepak.stat.long.means.paired <- sidepak.stat.long.means %>%
  filter(statistic %in% c('I/O','Cs','Fin','R2'))

### summary table side by side

sidepak.stat.wide.means <- sidepak.stat.long.means %>%
  select(statistic,ac.type,season,mean,sd,n)%>%
  pivot_wider(names_from=ac.type, values_from=c(mean,sd,n)) %>%
  relocate(statistic,season,mean_Central,sd_Central,n_Central,mean_Evap,sd_Evap,n_Evap)

#### Evaluate 2-sample t-tests for multiple pollutants

## summer
sidepak.stats.summer <- sidepak.stats %>%
  filter(season=='Summer') %>%
  mutate(ac.type = factor(ac.type,levels=c('Evap','Central'),ordered=T))

## create empty data frame to store correlation stats

sidepak.t.test.summer <- data.frame(statistic = character(), 
                                    season = character(),
                                    p.value = numeric(),
                                    lower.bound = numeric(),
                                    upper.bound = numeric()
)

sidepak.t.test.summer[1,1] <- "I/O"
sidepak.t.test.summer[1,3] <- t.test(`I/O`~ac.type,data=sidepak.stats.summer,var.equal=F)$p.value
sidepak.t.test.summer[1,4] <- t.test(`I/O`~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[1]
sidepak.t.test.summer[1,5] <- t.test(`I/O`~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[2]

sidepak.t.test.summer[2,1] <- 'Cs'
sidepak.t.test.summer[2,3] <- t.test(Cs~ac.type,data=sidepak.stats.summer,var.equal=F)$p.value
sidepak.t.test.summer[2,4] <- t.test(Cs~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[1]
sidepak.t.test.summer[2,5] <- t.test(Cs~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[2]

sidepak.t.test.summer[3,1] <- 'Fin'
sidepak.t.test.summer[3,3] <- t.test(Fin~ac.type,data=sidepak.stats.summer,var.equal=F)$p.value
sidepak.t.test.summer[3,4] <- t.test(Fin~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[1]
sidepak.t.test.summer[3,5] <- t.test(Fin~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[2]

sidepak.t.test.summer[4,1] <- 'R2'
sidepak.t.test.summer[4,3] <- t.test(R2~ac.type,data=sidepak.stats.summer,var.equal=F)$p.value
sidepak.t.test.summer[4,4] <- t.test(R2~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[1]
sidepak.t.test.summer[4,5] <- t.test(R2~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[2]

sidepak.t.test.summer[5,1] <- 'Outdoor SidePak'
sidepak.t.test.summer[5,3] <- t.test(SidePak.ug.m3.avg_Out~ac.type,data=sidepak.stats.summer,var.equal=F)$p.value
sidepak.t.test.summer[5,4] <- t.test(SidePak.ug.m3.avg_Out~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[1]
sidepak.t.test.summer[5,5] <- t.test(SidePak.ug.m3.avg_Out~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[2]

sidepak.t.test.summer[6,1] <- 'Indoor SidePak'
sidepak.t.test.summer[6,3] <- t.test(SidePak.ug.m3.avg_In~ac.type,data=sidepak.stats.summer,var.equal=F)$p.value
sidepak.t.test.summer[6,4] <- t.test(SidePak.ug.m3.avg_In~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[1]
sidepak.t.test.summer[6,5] <- t.test(SidePak.ug.m3.avg_In~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[2]

sidepak.t.test.summer[7,1] <- 'Outdoor UDAQ'
sidepak.t.test.summer[7,3] <- t.test(PM2.5.UDAQ.ug.m3~ac.type,data=sidepak.stats.summer,var.equal=F)$p.value
sidepak.t.test.summer[7,4] <- t.test(PM2.5.UDAQ.ug.m3~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[1]
sidepak.t.test.summer[7,5] <- t.test(PM2.5.UDAQ.ug.m3~ac.type,data=sidepak.stats.summer,var.equal=F)$conf.int[2]

sidepak.t.test.summer$season <- 'Summer'

## winter

sidepak.stats.winter <- sidepak.stats %>%
  filter(season=='Winter') %>%
  mutate(ac.type = factor(ac.type,levels=c('Evap','Central'),ordered=T))

sidepak.t.test.winter <- data.frame(statistic = character(), 
                                    season = character(),
                                    p.value = numeric(),
                                    lower.bound = numeric(),
                                    upper.bound = numeric()
)

sidepak.t.test.winter[1,1] <- "I/O"
sidepak.t.test.winter[1,3] <- t.test(`I/O`~ac.type,data=sidepak.stats.winter,var.equal=F)$p.value
sidepak.t.test.winter[1,4] <- t.test(`I/O`~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[1]
sidepak.t.test.winter[1,5] <- t.test(`I/O`~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[2]

sidepak.t.test.winter[2,1] <- 'Cs'
sidepak.t.test.winter[2,3] <- t.test(Cs~ac.type,data=sidepak.stats.winter,var.equal=F)$p.value
sidepak.t.test.winter[2,4] <- t.test(Cs~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[1]
sidepak.t.test.winter[2,5] <- t.test(Cs~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[2]

sidepak.t.test.winter[3,1] <- 'Fin'
sidepak.t.test.winter[3,3] <- t.test(Fin~ac.type,data=sidepak.stats.winter,var.equal=F)$p.value
sidepak.t.test.winter[3,4] <- t.test(Fin~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[1]
sidepak.t.test.winter[3,5] <- t.test(Fin~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[2]

sidepak.t.test.winter[4,1] <- 'R2'
sidepak.t.test.winter[4,3] <- t.test(R2~ac.type,data=sidepak.stats.winter,var.equal=F)$p.value
sidepak.t.test.winter[4,4] <- t.test(R2~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[1]
sidepak.t.test.winter[4,5] <- t.test(R2~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[2]

sidepak.t.test.winter[5,1] <- 'Outdoor SidePak'
sidepak.t.test.winter[5,3] <- t.test(SidePak.ug.m3.avg_Out~ac.type,data=sidepak.stats.winter,var.equal=F)$p.value
sidepak.t.test.winter[5,4] <- t.test(SidePak.ug.m3.avg_Out~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[1]
sidepak.t.test.winter[5,5] <- t.test(SidePak.ug.m3.avg_Out~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[2]

sidepak.t.test.winter[6,1] <- 'Indoor SidePak'
sidepak.t.test.winter[6,3] <- t.test(SidePak.ug.m3.avg_In~ac.type,data=sidepak.stats.winter,var.equal=F)$p.value
sidepak.t.test.winter[6,4] <- t.test(SidePak.ug.m3.avg_In~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[1]
sidepak.t.test.winter[6,5] <- t.test(SidePak.ug.m3.avg_In~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[2]

sidepak.t.test.winter[7,1] <- 'Outdoor UDAQ'
sidepak.t.test.winter[7,3] <- t.test(PM2.5.UDAQ.ug.m3~ac.type,data=sidepak.stats.winter,var.equal=F)$p.value
sidepak.t.test.winter[7,4] <- t.test(PM2.5.UDAQ.ug.m3~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[1]
sidepak.t.test.winter[7,5] <- t.test(PM2.5.UDAQ.ug.m3~ac.type,data=sidepak.stats.winter,var.equal=F)$conf.int[2]


sidepak.t.test.winter$season <- 'Winter'

## combine together

sidepak.t.test <- bind_rows(sidepak.t.test.summer,sidepak.t.test.winter)

## Combine with means and n

sidepak.stats.summary <- sidepak.stat.wide.means %>%
  mutate(`Evap-Central` = mean_Evap-mean_Central) %>%
  left_join(sidepak.t.test,by=c('statistic','season')) %>%
  mutate(statistic = factor(statistic,levels=c('Outdoor UDAQ','Outdoor SidePak','Indoor SidePak','I/O','Cs','Fin','R2'),ordered=T)) %>%
  mutate(qt_Central = qt(.975,df=(n_Central-1))) %>%
  mutate(bound_Central = qt_Central*sd_Central/sqrt(n_Central)) %>%
  mutate(lower.95_Central = mean_Central-bound_Central) %>%
  mutate(upper.95_Central = mean_Central+bound_Central) %>%
    mutate(qt_Evap = qt(.975,df=(n_Evap-1))) %>%
    mutate(bound_Evap = qt_Evap*sd_Evap/sqrt(n_Evap)) %>%
    mutate(lower.95_Evap = mean_Evap-bound_Evap) %>%
    mutate(upper.95_Evap = mean_Evap+bound_Evap)  


### What is the average and 95% CI for the Fin for just the wildfire events?

Fin.wildfire <- sidepak.stats %>%
  group_by(ac.type,season,day.type) %>%
  dplyr::summarize(mean = mean(Fin,na.rm=T),median = median(Fin,na.rm=T),
            sd=sd(Fin,na.rm=T),n=sum(!is.na(Fin)),
            min=min(Fin,na.rm=T),max=max(Fin,na.rm=T) ) %>%
  mutate(qt = qt(.975,df=(n-1))) %>%
  mutate(bound = qt*sd/sqrt(n)) %>%
  mutate(lower.95 = mean-bound) %>%
  mutate(upper.95 = mean+bound)



## What about t-tests looking at differences in the individual Fin between wildfire and normal days?

## Is the Fin significantly higher during the wildfire event?

library(infer)

t_test_fire <- function(data){
  t.data  <- data %>%
    t_test(value~day.type, order = c("Normal", "Wildfire Smoke")) %>%
    mutate(statistic = data$statistic[1]) %>%
    mutate(season = data$season[1]) %>%
    mutate(ac.type = data$ac.type[1]) %>%
    relocate(season,1) %>%
    relocate(ac.type,2)
  return(t.data)
}


t.test.fire <- sidepak.stats.long %>%
  filter(season == 'Summer') %>%
  group_by(statistic,ac.type) %>%
  group_split() %>%
  map(function(df) t_test_fire(data = df)) %>%
  list_rbind() 

## Yes, the difference is statistically significant at a 95% confidence intervals

## What is the mean Fin during 