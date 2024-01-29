### Ozone analysis ####

#read in all libraries to be used
library(tidyverse)
library(ggpmisc)
library(readxl)
library(RColorBrewer)
library(GGally)

#set the working directory (Currently unique to Han's computer)
#setwd("C:\\Users\\cello\\Box\\Wildfire Indoor Air Quality Instrument Data\\Data")
#setwd("C:\\Users\\Sonntag\\Box\\Wildfire Indoor Air Quality Instrument Data\\Data")
setwd("C:\\Users\\cello\\OneDrive\\문서\\Github\\EvapCoolerUtahCounty")

own.colors <- brewer.pal(n = 9, name = "Set1")[c(8:9)]
display.brewer.all()

###

smoke.event <- interval(ymd('2022-09-08'),ymd('2022-09-12'))

ozone.summary <- summary %>%
  dplyr::filter(!is.na(O3.LOD.ppm)) %>% ## just keep the visits with Ozone data
  mutate(house.number.visit = paste(House.Number,Visit,sep=" ")) %>%
  mutate(house.number.visit.date = paste(first.day,house.number.visit,sep=" ")) %>%
  mutate(ac.type = ifelse(`Type of Air Conditioner`=='Central','Central',ifelse(`Type of Air Conditioner`=='Evaporative','Evap',NA))) %>%
  mutate(day.type = ifelse(as_date(first.day) %within% smoke.event,'Wildfire Smoke','Normal')) %>%
  mutate(House.Number = ifelse(house.number.visit =='H15 V1','H09',House.Number)) %>% ### Change H15 V1 to H09 V3
  mutate(Visit = ifelse(house.number.visit =='H15 V1','V3',Visit)) %>% ### Change H15 V1 to H09 V3
  mutate(house.number.visit = ifelse(house.number.visit =='H15 V1','H09 V3',house.number.visit)) %>% ### Change H15 V1 to H09 V3
  mutate(ozone.max = ifelse(O3.Below.detection==T,O3.LOD.ppm,O3.ppm)) %>%
  mutate(ozone.max = round(as.numeric(ozone.max),digits=4)) %>%
  mutate(O3.ppm = as.numeric(O3.ppm)) %>%
  select("House.Number","Visit", "house.number.visit","house.number.visit.date" ,"Location","first.day","Ozone.UDAQ.ppb", "UDAQ.n_OZONE" , "Monitor.closest",  "O3.mg.m3","O3.ppm", "O3.LOD.ppm", "O3.Below.detection" ,        
         "ozone.max", "average.RH", "min.RH" , "max.RH" ,"average.temperature.Celsius",
         "min.temperature" ,  "max.temperature" , "time.hours" ,  "Type of Air Conditioner","ac.type" )


## Bring in the plot of the study ozone vs. the UDAQ ozone measurements
#plot Ozone vs UDQA ozone (all homes)
png(".//Graphics//Ozone//Ozone.UDAQ.Comparison.png",width=3.6, height=3, units="in", res=300)
ggplot(data=study.summary.out,aes(x=O3.ppb, y=Ozone.UDAQ.ppb))+
  geom_point(aes(color=factor(House.Number)))+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(x='Study Ozone Concentration ppb', y='UDAQ Average Ozone ppb')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=3)+
  theme_bw()+
  scale_color_brewer(palette='Set1')+
 # theme(legend.position = 'right', 
  #      legend.margin=margin(-10,0,0,0),         # Adjust this to decrease space above the legend
  #      plot.margin = margin(5,5,5,5))+          # Adjust this to decrease space below x-axis title
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.position = 'none', strip.text = element_text(size=9))
dev.off()

#plot Ozone vs UDQA ozone (all homes)
## update the format

## make a plot where you y and x axis goes to zero
## include the black y=x line
## try chaning the y vs. x


ggplot(data=study.summary.out,aes(x=O3.ppb, y=Ozone.UDAQ.ppb))+
  geom_point(aes(color=factor(House.Number)))+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(x='Study Ozone Concentration ppb', y='UDAQ Average Ozone ppb')+
  expand_limits(y=0,x=0)+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")))

## ozone
## plot a summary plot
## indoor and outdoor ozone concentration
## minimum detection limit
## organized by date?


## graph with date instead
## Figure 1
ozone.summary$house.number.visit <- factor(ozone.summary$house.number.visit,levels=rev(sort(unique(ozone.summary$house.number.visit))),ordered=T)
ozone.summary$house.number.visit.date <- factor(ozone.summary$house.number.visit.date,levels=rev(sort(unique(ozone.summary$house.number.visit.date))),ordered=T)
ozone.summary$Location <- factor(ozone.summary$Location,levels=c("Out","In"),ordered=T)

lapply(ozone.summary,class)

# I prefer the plot by date...

png(".//Graphics//Ozone//ozone.indoor.outdoor.date.png", width=10, height=12, units="in", res=300)
ggplot(data = ozone.summary,  aes(y = house.number.visit.date, x = O3.ppm*1000,fill=Location))+  
  geom_col(position=position_dodge2(preserve='single'),width=0.7,color='white')+
  geom_point(aes(x=O3.LOD.ppm*1000,group=Location),color='grey',position=position_dodge(width=0.7)) +
  #geom_line(aes(x=O3.LOD.ppm*1000,group=house.number.visit.date),color="grey")+
  facet_grid(ac.type~.,scales='free_y' ,space='free') +
  scale_fill_brewer(palette = 'Set1')+
  labs(x = expression(paste("O"[3]," ppb")),title='',y='')+
  #coord_cartesian(xlim = c(0, 110)) +
  #scale_x_continuous(breaks=seq(0,110,10))+
  theme_bw()+
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=16),
        axis.title = element_text(size = 18),plot.title = element_text(size = 20),
        legend.title = element_text(size = 18),legend.text = element_text(size = 14),strip.text = element_text(size=14))
dev.off()


## plot again, but sort by house and visit
#png(".//Graphics//Ozone//ozone.indoor.outdoor.date.png", width=10, height=12, units="in", res=300)
ggplot(data = ozone.summary,  aes(y = house.number.visit, x = O3.ppm*1000,fill=Location))+  
  geom_col(position=position_dodge2(preserve='single'),width=0.7,color='white')+
  geom_point(aes(x=O3.LOD.ppm*1000,group=Location),color='grey',position=position_dodge(width=0.7)) +
  #geom_line(aes(x=O3.LOD.ppm*1000,group=house.number.visit.date),color="grey")+
  facet_grid(ac.type~.,scales='free_y' ,space='free') +
  scale_fill_brewer(palette = 'Set1')+
  labs(x = expression(paste("O"[3]," ppb")),title='',y='')+
  #coord_cartesian(xlim = c(0, 110)) +
  #scale_x_continuous(breaks=seq(0,110,10))+
  theme_bw()+
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=16),
        axis.title = element_text(size = 18),plot.title = element_text(size = 20),
        legend.title = element_text(size = 18),legend.text = element_text(size = 14),strip.text = element_text(size=14))
#dev.off()

## future option if we want to add separate legend for the points
## https://bookdown.dongzhuoer.com/hadley/ggplot2-book/legend-merge-split


### pivot to wide format
smoke.event <- interval(ymd('2022-09-08'),ymd('2022-09-12'))

ozone.wide <- ozone.summary %>%
  select(-O3.mg.m3,-time.hours,-O3.ppm,-O3.LOD.ppm) %>%
  pivot_wider(names_from = Location,values_from = c(ozone.max,O3.Below.detection,average.temperature.Celsius,
                                                    min.temperature,max.temperature,average.RH,min.RH,max.RH),names_sort = T) %>%
  filter(!(house.number.visit %in% c('H02 V2','H03 V2')))  %>%
  mutate(`I/O` = ozone.max_In / ozone.max_Out) %>%
  mutate(day.type = ifelse(as_date(first.day) %within% smoke.event,'Wildfire Smoke','Normal'))
  
## Calculate means

ozone.ave.house <- ozone.wide %>%
  group_by(House.Number,`Type of Air Conditioner` ) %>%
  summarize(ave_ozone_IO = mean(`I/O`))

#levels.house <-ozone.ave.house[order(ozone.ave.house$ave_ozone_IO),'House.Number']
levels.house <- ozone.ave.house$House.Number[order(ozone.ave.house$ave_ozone_IO)]
levels.house

ozone.wide <- ozone.wide %>%
  mutate(House.Number = factor(House.Number,levels=levels.house,ordered=T))


###
## 

png(".//Graphics//Ozone//ozone.io.png", width=6.5, height=4, units="in", res=300)
ggplot(data = ozone.wide,aes(x = House.Number, y = `I/O`,fill=House.Number)) + 
  geom_jitter(size=2,alpha=0.9,width=0.15,pch=21,color='black')+
  theme_bw()+
  expand_limits(y=0,x=0)+
  labs(x='House', y= 'I/O')+
  facet_grid(.~`Type of Air Conditioner`, scales='free') +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks=seq(0,1,.20))+
  theme(legend.position = 'blank')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(angle = 90,vjust =0.5, size=8),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

## 

### What's the correlation of the I/O between homes?

ozone.visit.wide <- ozone.wide %>%
  select("House.Number","Visit","Type of Air Conditioner","I/O") %>%
  pivot_wider(names_from = Visit,values_from = c("I/O"),names_sort = T) %>%
  mutate(V2 = ifelse(House.Number=='H09',V3,V2)) ## move V3 to V2 to compare

### There is a very high correlation. The I/O is quite repeatable across visits

#png(".//Graphics//Ozone//.png", width=6.5, height=7, units="in", res=300)
ggplot(data = ozone.visit.wide,aes(x = V1, y = V2)) + 
  geom_point(size = 2)+
  theme_bw()+
  expand_limits(y=0,x=0)+
  labs(x='I/O for Visit 1', y= 'I/O for Visit 2')+
  facet_grid(.~`Type of Air Conditioner`, scales='free_y') +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
#dev.off()

## interesting about the high correlation for the Central. 
## because the indoor air for the central air homes is constant, 
## then the outdoor concentrations must be correlated for the central air homes

## plot just the evap homes

#png(".//Graphics//Ozone//.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(ozone.visit.wide,`Type of Air Conditioner` =='Evaporative'),aes(x = V1, y = V2)) + 
  geom_point(size = 2)+
  theme_bw()+
  expand_limits(y=0,x=0)+
  labs(x='I/O for Visit 1', y= 'I/O for Visit 2')+
  facet_grid(.~`Type of Air Conditioner`, scales='free_y') +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
#dev.off()

###
# Calculate 95% condfidence intervals for both
### 

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


## Figure S-17
png(".//Graphics//Ozone//O3.IO.vs.temp.png", width=4.5, height=4, units="in", res=300)
ggplot(data=ozone.wide,aes(x=average.temperature.Celsius_Out, y=`I/O`))+
  geom_point(aes(color=day.type))+
  geom_smooth(color = "black",method='loess',span=2) +
  labs(x = "Outdoor Temperature, C ")+
  theme_bw()+
  facet_grid(.~`Type of Air Conditioner`) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 10),plot.title = element_text(size = 20),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        strip.text = element_text(size=10))
dev.off()

#plot Ozone vs UDQA ozone (all homes)
ggplot(data=study.summary.out,aes(x=O3.ppb, y=Ozone.UDAQ.ppb))+
  geom_point(aes(color=factor(House.Number)))+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(x='Study Ozone Concentration ppb', y='UDAQ Average Ozone ppb')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")))

