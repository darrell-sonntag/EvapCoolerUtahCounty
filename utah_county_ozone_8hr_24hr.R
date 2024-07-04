### Graph the average hourly ozone concentrations in Utah County for summer 2022 and summer 2023

## Calculate the 24-hour average 

## Compare to the daily maximum eight hour average

## read in the data in the Summary_Script.R

library(hms)
library(lubridate)

Summer2022.23Hourly.Ozone <- Summer2022.23Hourly %>%
  filter(Parameter == "OZONE") %>%
  rename(DateTime = Date) %>%
  mutate(Date = as_date(DateTime)) %>%
  mutate(Year = lubridate::year(Date)) %>%
  mutate(Time = hms::as_hms(DateTime)) %>%
  mutate(Hour = hour(Time)) %>%
  mutate(month= month(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  filter(month >= 7 & day >= 13) ## filter all the data beyond July 12th. Because I'm missing the June - July 12th data in 2022.


names(Summer2022.23Hourly.Ozone)

## calculate the average by year, hour, and Monitor         
         
Summer2022.23Hourly.Ozone.avg <- Summer2022.23Hourly.Ozone %>%
  group_by(Monitor,Year, Time,Hour) %>%
  summarise(avg = mean(Concentration), median = median(Concentration),
            percentile.05 = quantile(Concentration,.05),percentile.95 = quantile(Concentration,.95))

## Calculate the average diurnal temperature profile for BYU

## grab the BYU data from the Summary_Script.R

BYU_TRH_all <- bind_rows(BYU_TRH_2022,BYU_TRH_2023) %>%
  mutate(date.time = ymd_hms(paste(Date,Time))) %>%
  mutate(date.time = round_date(date.time,"5 minutes")) %>%
  mutate(round.time = hms::as_hms(date.time)) %>%
  mutate(Year = year(date.time)) %>%
  mutate(month= month(date.time)) %>%
  mutate(day = day(date.time)) %>%
  mutate(hour = hour(date.time)) %>%
  rename(Temp = 'Temperature (deg. C)', RH = 'Humidity (%)') %>%
  mutate(Location = 'BYU') %>% 
  filter(month >= 7 & day >= 13) ## filter all the data beyond July 12th. Because I'm missing the June - July 12th data in 2022. 

### how many repeat data are there in the BYU temperature database?
## Remove days that don't have both the 24-hr and the 8-hr (removed in csv files as discussed in the Summary_Script.R)

BYU_TRH_all.repeat <- BYU_TRH_all%>%
  group_by(date.time) %>%
  summarize(n = sum(!is.na(Temp))) %>%
  filter(n>2)

## just grab the time closest to the hour
# https://stackoverflow.com/questions/44535546/r-how-to-filter-subset-min-of-minutes-in-every-hour

BYU_TRH_hour <- BYU_TRH_all %>% 
  group_by(Date, hour) %>% 
  filter(date.time == min(date.time)) %>% 
  ungroup() 

## calculate the average temperature by hour

Summer2022.23.Temp.avg <- BYU_TRH_hour %>%
  group_by(Location,Year, round.time,hour) %>%
  summarise(avg = mean(Temp), median = median(Temp),
            percentile.05 = quantile(Temp,.05),percentile.95 = quantile(Temp,.95))

## plot the temperature data

t <- ggplot(Summer2022.23.Temp.avg, aes(x=hour,y=avg))+
  geom_ribbon(aes(ymin = percentile.05,ymax = percentile.95),col=NA,alpha=0.3) +
  geom_line(size=1) +
  theme_bw()+
  facet_grid(.~Year) +
  scale_x_continuous(breaks=seq(2,22,4))+
  labs(x = 'Hour of the day', y= expression("Average \n Temperature,"~degree*C),title='Brigham Young University, Provo')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title.x = element_text(size = 10,hjust=0.5),axis.title.y = element_text(size = 12,vjust=0,hjust=0.5),
        plot.title = element_text(size = 14,hjust = 0.5,vjust=0),
        legend.position='blank',  strip.text = element_text(size=14))


## plot hourly ozone data

o1 <- ggplot(filter(Summer2022.23Hourly.Ozone.avg, Monitor=='Lindon'), aes(x=Hour,y=avg))+
  geom_ribbon(aes(ymin = percentile.05,ymax = percentile.95),col=NA,alpha=0.3) +
  geom_line(size=1,color="#4DAF4A") +
  theme_bw()+
  facet_grid(~Year) +
  expand_limits(y=c(20,70))+
  scale_x_continuous(breaks=seq(2,22,4))+
  geom_abline(aes(intercept = 70, slope = 0), linetype='dashed')+
  labs(x='Hour of the day', y='Average Ozone\n Concentration, ppb',title='Lindon')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title.x = element_text(size = 10,hjust=0.5),axis.title.y = element_text(size = 12,hjust=0.5),
        plot.title = element_text(size = 14,hjust = 0.5,vjust=0),
        legend.position='blank',  strip.text = element_text(size=14))

o2 <- ggplot(filter(Summer2022.23Hourly.Ozone.avg, Monitor=='Spanish Fork'), aes(x=Hour,y=avg))+
  geom_ribbon(aes(ymin = percentile.05,ymax = percentile.95),col=NA,alpha=0.3) +
  geom_line(size=1,color="#984EA3") +
  theme_bw()+
  facet_grid(~Year) +
  expand_limits(y=c(20,70))+
  scale_x_continuous(breaks=seq(2,22,4))+
  geom_abline(aes(intercept = 70, slope = 0), linetype='dashed')+
  labs(x='Hour of the day', y='Average Ozone\n Concentration, ppb',title='Spanish Fork')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title.x = element_text(size = 10,hjust=0.5),axis.title.y = element_text(size = 12,hjust=0.5),
        plot.title = element_text(size = 14,hjust = 0.5,vjust=0),
        legend.position='blank',  strip.text = element_text(size=14))

t+o1+o2+plot_layout(ncol = 1)
ggsave(".//Graphics//Ozone//utah.county.diurnal.ozone.png",width=6.5, height=, units="in", dpi=900)



## Calculate 24-hour averages by day

Summer2022.23.24hr.avg <- Summer2022.23Hourly.Ozone %>%
  group_by(Monitor,Year, Date) %>%
  summarise(avg = mean(Concentration)) %>%
  mutate(metric = '24-hr')

## Calculate daily maximum 8-hour averages

## bring in epa.ozone.8hr.date from utah_county_ozone_temp.R

names(epa.ozone.8hr.date)

Summer2022.23.8hr.avg <- epa.ozone.8hr.date %>%
        mutate(Month = month(Date)) %>%
        filter(between(Month,6,9)) %>%
        rename(Monitor = `Site Name`) %>%
        rename(avg = ozone.8hr.ppb) %>%
        mutate(metric = '8-hr') %>%
        select(Monitor,Year,Date,metric,avg)
    
### combine the two datasets

Summer2022.23.8hr.24hr.avg <- rbind(Summer2022.23.24hr.avg,
                                    Summer2022.23.8hr.avg)

## Compare the 24-hour averages with the daily maximum 8-hour averages

annotate_text <- data.frame(lab = "X = Mean", Year=2022,metric = '24-hr', Year = 2022, metric = '24-hr')
annotate_text2 <- data.frame(Year=c(2022,2022),metric = c('24-hr','8-hr'))

ggplot(Summer2022.23.8hr.24hr.avg, aes(x=Monitor,y=avg,color=Monitor))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width=0.05,size=1)+
  stat_summary(fun.y=mean,geom='point',color='black',shape=4,size=3,stroke=1)+
  theme_bw()+
  facet_grid(metric~Year) +
  expand_limits(y=c(20,70))+
  scale_color_manual(name = 'Location', values = c("#4DAF4A","#984EA3"))+
  geom_abline(aes(intercept = 70, slope = 0), linetype='dashed')+
  #annotate("text",label = "X = Mean",  x=1.5,  y=60 )+
  geom_text(data = annotate_text,label = annotate_text$lab,color='black',x=1.5,y=55)+
  geom_text(data = annotate_text2,label = "Ozone Maximum 8-hr NAAQS",color='black',x=1.5,y=72)+
  labs(x='', y='Ozone Concentration, ppb')+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        legend.position='blank')
ggsave(".//Graphics//Ozone//utah.county.8hr.24hr.boxplot.png",width=6, height=6, units="in", dpi=600)


## Remove days that don't have both the 24-hr and the 8-hr

Summer2022.23.missing.days <- Summer2022.23.8hr.24hr.avg %>%
                              group_by(Monitor,Date) %>%
                              summarize(n = sum(!is.na(avg))) %>%
                              filter(n<2)

       
## What's up with these days?

Summer2022.23.8hr.24hr.avg.missing <- Summer2022.23.8hr.24hr.avg %>%
                  semi_join(Summer2022.23.missing.days,by=c('Monitor','Date'))

## they don't seem especially high or low...


## Calculate 24-hour averages across all days in a year

Summer2022.23.8hr.24hr.avg.year <- Summer2022.23.8hr.24hr.avg %>%
  anti_join(Summer2022.23.missing.days,by=c('Monitor','Date')) %>%
  group_by(Monitor,Year,metric) %>%
  summarise(avg2 = mean(avg), median = median(avg),
            percentile.05 = quantile(avg,.05),percentile.95 = quantile(avg,.95),n=n())

## comparison
Summer2022.23.8hr.24hr.compare <- Summer2022.23.8hr.24hr.avg.year %>%
                      select(Monitor,Year,avg2,n,metric) %>%
                      pivot_wider(names_from = metric, values_from= c(avg2,n), names_prefix='ozone_') %>%
                      mutate(ratio_24_8=`avg2_ozone_24-hr`/`avg2_ozone_8-hr`)

write_csv(Summer2022.23.8hr.24hr.compare,'.//Processed Data//compare.8hr.24.hr.csv')


## Compare the 24-hour averages means and 95th percentiles

ggplot(Summer2022.23.8hr.24hr.avg.year, aes(x=Monitor,y=avg2,fill=Monitor))+
     geom_col()+
     geom_errorbar(aes(ymin=percentile.05,ymax=percentile.95,width=0.25))+
     theme_bw()+
     facet_grid(metric~Year) +
     expand_limits(y=c(0,70))+
     scale_fill_manual(name = 'Location', values = c("#4DAF4A","#984EA3"))+
     geom_abline(aes(intercept = 70, slope = 0), linetype='dashed')+
     #annotate("text",label = "Ozone NAAQS",  x=as_date('2022-03-01'),  y=73 )+
     labs(x='', y='Ozone Concentration, ppb')+
     theme(axis.text.y = element_text(size=12),axis.text.x = element_text(size=12),
     axis.title = element_text(size = 12),plot.title = element_text(size = 14,hjust = 0.5,vjust=0))






















































