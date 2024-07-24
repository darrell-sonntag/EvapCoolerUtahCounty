
## Graph ozone 8-hr maximum alongside maximum temperature at BYU

library(tidyverse)
library(patchwork)
library(scales)


## read in the maximum temperature 

## downloaded from here: 
## https://www.ncdc.noaa.gov/cdo-web/datatools
## Select Provo -> 
## https://www.ncei.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USC00427064/detail

max.temp <- read_csv(".\\Data\\BYU Weather Station\\max_temp_2022_2023.csv")

## Read in the daily 8-hr maximum ozone
## downloaded from here: 
## https://www.epa.gov/outdoor-air-quality-data/download-daily-data

epa.ozone.8hr.2022 <- read_csv(".\\Data\\EPA\\Ozone_2022_data.csv") %>%
                      rename(Units = UNITS)

epa.ozone.8hr.2023 <- read_csv(".\\Data\\EPA\\Ozone_2023_data.csv") %>%
                      rename('Site Name' = 'Local Site Name')

epa.ozone.8hr <- bind_rows(epa.ozone.8hr.2022,epa.ozone.8hr.2023)

## Convert Dates to Dates

epa.ozone.8hr.date <- epa.ozone.8hr %>%
                      mutate(Date = mdy(Date)) %>%
                      mutate(Year = lubridate::year(Date)) %>%
                      mutate(ozone.8hr.ppb = `Daily Max 8-hour Ozone Concentration`*1000) %>%
                      rename(ozone.8hr = "Daily Max 8-hour Ozone Concentration") %>%
                      select("Year","Date","Source","Site ID","POC",
                      "ozone.8hr.ppb", "Site Name")

## Change dates on max.temp

max.temp <- max.temp %>%
  mutate(Date = ymd(DATE)) %>%
  rename(MaxTempF = TMAX) %>%
  mutate(MaxTempC = (MaxTempF-32)*(5/9))


names(epa.ozone.8hr)

## merge the two datasets together

ozone.8hr.temp <- epa.ozone.8hr.date %>%
                  left_join(max.temp,by='Date')


ozone.8hr.temp.lindon <- ozone.8hr.temp %>%
                         filter(`Site Name` == 'Lindon')

ozone.8hr.temp.sf <- ozone.8hr.temp %>%
  filter(`Site Name` == 'Spanish Fork')







### Create some plots and arrange using patchwork

## https://www.andrewheiss.com/blog/2022/06/23/long-labels-ggplot/#option-f-automatically-add-line-breaks

?label_wrap



a <- ggplot(ozone.8hr.temp.lindon, aes(x=Date,y=MaxTempC))+
    geom_line(color='grey',size=1) +
    labs(x = '', y= expression("Daily Maximum\n Temperature,"~degree*C),title='Brigham Young University, Provo')+
    theme_bw() +
    theme(axis.text.y = element_text(size=12),axis.text.x = element_text(size=12),
    axis.title = element_text(size = 12),plot.title = element_text(size = 14,,hjust = 0.5,vjust=0))

b <- ggplot(ozone.8hr.temp.lindon, aes(x=Date,y=ozone.8hr.ppb))+
  geom_line(color="#4DAF4A",size=1) +
  theme_bw()+
  expand_limits(y=c(0,75))+
  geom_abline(aes(intercept = 70, slope = 0), linetype='dashed')+
  annotate("text",label = "Ozone NAAQS",  x=as_date('2022-03-01'),  y=73 )+
  labs(x='', y='Daily Maximum\n 8-hour Ozone\nConcentration, ppb', title = 'Lindon')+
  theme(axis.text.y = element_text(size=12),axis.text.x = element_text(size=12),
        axis.title = element_text(size = 12),plot.title = element_text(size = 14,,hjust = 0.5,vjust=0))

c <- ggplot(ozone.8hr.temp.sf, aes(x=Date,y=ozone.8hr.ppb,color=`Site Name`))+
  geom_line(color =  "#984EA3",size=1) +
  theme_bw()+
  expand_limits(y=c(0,75))+
  geom_abline(aes(intercept = 70, slope = 0), linetype='dashed')+
  annotate("text",label = "Ozone NAAQS",  x=as_date('2022-03-01'),  y=73 )+
  labs(x='', y='Daily Maximum\n 8-hour Ozone\nConcentration, ppb',title = 'Spanish Fork')+
  theme(axis.text.y = element_text(size=12),axis.text.x = element_text(size=12),
        axis.title = element_text(size = 12),plot.title = element_text(size = 14,,hjust = 0.5,vjust=0))

a+b+c+ plot_layout(ncol = 1)
ggsave(".//Graphics//Ozone//utah.county.temp.ozone.png", width=6, height=8.5, units="in", dpi=900)




a1 <- ggplot(ozone.8hr.temp.lindon, aes(x=Date,y=MaxTempC))+
  geom_line(color='darkgrey',size=1) +
  labs(x = '', y= expression("Daily Maximum\n Temperature,"~degree*C),title='Brigham Young University, Provo')+
  theme_bw() +
  theme(axis.text.y = element_text(size=12),axis.text.x = element_text(size=12),
        axis.title = element_text(size = 12),plot.title = element_text(size = 14,,hjust = 0.5,vjust=0))

## grabed colors from https://document.airnow.gov/technical-assistance-document-for-the-reporting-of-daily-air-quailty.pdf
## found the corresponding hexidecmal code here: https://htmlcolorcodes.com/

b1 <- ggplot(ozone.8hr.temp.lindon, aes(x=Date,y=ozone.8hr.ppb))+
  annotate("rect",fill = "#00E400", xmin = as_date('2022-01-01'), xmax = as_date('2023-12-31'), ymin=0, ymax = 55,alpha=0.5)+
  annotate("rect",fill = "#FFFF00", xmin = as_date('2022-01-01'), xmax = as_date('2023-12-31'), ymin=55, ymax = 70,alpha=0.5)+
  annotate("rect",fill = "#FF7E00", xmin = as_date('2022-01-01'), xmax = as_date('2023-12-31'), ymin=70, ymax = 85,alpha=0.5)+
  annotate("rect",fill = "#FF0000", xmin = as_date('2022-01-01'), xmax = as_date('2023-12-31'), ymin=85, ymax = Inf,alpha=0.5)+
  annotate("text",label = "Good",  x=as_date('2022-02-15'),  y=25, hjust=0,size=3)+
  annotate("text",label = "Moderate",  x=as_date('2022-02-15'),  y=63, hjust=0,size=3 )+
  annotate("text",label = "Unhealthy for Sensitive Groups",  x=as_date('2022-02-15'),  y=78, hjust=0,size=3 )+
  annotate("text",label = "Unhealthy",  x=as_date('2022-02-15'),  y=88, hjust=0,size=3 )+
  geom_line(color="darkgrey",size=1) +
  theme_bw()+
  scale_y_continuous(breaks=seq(0,80,20),lim=c(0,88))+
  labs(x='', y='Daily Maximum\n 8-hour Ozone\nConcentration, ppb', title = 'Lindon')+
  theme(axis.text.y = element_text(size=12),axis.text.x = element_text(size=12),
        axis.title = element_text(size = 12),plot.title = element_text(size = 14,hjust = 0.5,vjust=0))

b1

c1 <- ggplot(ozone.8hr.temp.sf, aes(x=Date,y=ozone.8hr.ppb,color=`Site Name`))+
  annotate("rect",fill = "#00E400", xmin = as_date('2022-01-01'), xmax = as_date('2023-12-31'), ymin=0, ymax = 55,alpha=0.5)+
  annotate("rect",fill = "#FFFF00", xmin = as_date('2022-01-01'), xmax = as_date('2023-12-31'), ymin=55, ymax = 70,alpha=0.5)+
  annotate("rect",fill = "#FF7E00", xmin = as_date('2022-01-01'), xmax = as_date('2023-12-31'), ymin=70, ymax = 85,alpha=0.5)+
  annotate("rect",fill = "#FF0000", xmin = as_date('2022-01-01'), xmax = as_date('2023-12-31'), ymin=85, ymax = Inf,alpha=0.5)+
  annotate("text",label = "Good",  x=as_date('2022-02-15'),  y=25, hjust=0,size=3)+
  annotate("text",label = "Moderate",  x=as_date('2022-02-15'),  y=63, hjust=0,size=3 )+
  annotate("text",label = "Unhealthy for Sensitive Groups",  x=as_date('2022-02-15'),  y=78, hjust=0,size=3 )+
  annotate("text",label = "Unhealthy",  x=as_date('2022-02-15'),  y=88, hjust=0,size=3 )+
  geom_line(color="darkgrey",size=1) +
  geom_line(color =  "darkgrey",size=1) +
  theme_bw()+
  scale_y_continuous(breaks=seq(0,80,20),lim=c(0,88))+
  labs(x='', y='Daily Maximum\n 8-hour Ozone\nConcentration, ppb',title = 'Spanish Fork')+
  theme(axis.text.y = element_text(size=12),axis.text.x = element_text(size=12),
        axis.title = element_text(size = 12),plot.title = element_text(size = 14,,hjust = 0.5,vjust=0))

a1+b1+c1+ plot_layout(ncol = 1)
ggsave(".//Graphics//Ozone//utah.county.temp.ozone.aqi.png", width=6, height=8.5, units="in", dpi=900)


### what's the variation in the 8 hr summer ozone?

View(ozone.8hr.temp)


str(ozone.8hr.temp)

ozone.8hr.temp.summer <- ozone.8hr.temp %>%
                          mutate(month= month(Date)) %>%
                          mutate(year = year(Date)) %>%
                          filter(month %in% c(6:9)) ## filter all the data beyond July 12th. Because I'm missing the June - July 12th data in 2022.

names(ozone.8hr.temp.summer)

## summarize

ozone.8hr.temp.summer.summary <- ozone.8hr.temp.summer %>%
                                  group_by(`Site Name`) %>%
                                  summarize(mean = mean(ozone.8hr.ppb,na.rm=T),
                                            sd = sd(ozone.8hr.ppb,na.rm=T),
                                            min = min(ozone.8hr.ppb,na.rm=T),
                                            max = max(ozone.8hr.ppb,na.rm=T)) %>%
                                 mutate(max.min = max/min,cv = sd/mean) 
                                 

View(ozone.8hr.temp.summer.summary)
                         
