### Ozone analysis ####

#read in all libraries to be used
library(tidyverse)
library(ggpmisc)
library(readxl)
library(RColorBrewer)
library(GGally)

# make sure current directory is in the Github/EvapCoolerUtahCounty

own.colors <- brewer.pal(n = 9, name = "Set1")[c(8:9)]
display.brewer.all()

###

smoke.event <- interval(ymd('2022-09-08'),ymd('2022-09-12'))

ozone.summary <- summary %>%
  dplyr::filter(!is.na(O3.LOD.ppm)) %>% ## just keep the visits with Ozone data
  mutate(house.number.visit = paste(House.Number,Visit,sep=" ")) %>%
  mutate(house.number.visit.date = paste(first.day,house.number.visit,sep=" ")) %>%
  mutate(ac.type = ifelse(`Type of Air Conditioner`=='Central','AC',ifelse(`Type of Air Conditioner`=='Evaporative','EC',NA))) %>%
  mutate(day.type = ifelse(as_date(first.day) %within% smoke.event,'Wildfire Smoke','Normal')) %>%
  mutate(House.Number = ifelse(house.number.visit =='H15 V1','H09',House.Number)) %>% ### Change H15 V1 to H09 V3
  mutate(Visit = ifelse(house.number.visit =='H15 V1','V3',Visit)) %>% ### Change H15 V1 to H09 V3
  mutate(house.number.visit = ifelse(house.number.visit =='H15 V1','H09 V3',house.number.visit)) %>% ### Change H15 V1 to H09 V3
  mutate(ozone.max = ifelse(O3.Below.detection==T,O3.LOD.ppm,O3.ppm)) %>%
  mutate(ozone.max = round(as.numeric(ozone.max),digits=4)) %>%
  mutate(O3.ppm = as.numeric(O3.ppm)) %>%
  select("House.Number","Visit", "house.number.visit","house.number.visit.date" ,"Location","first.day","Ozone.UDAQ.ppb", "UDAQ.n_OZONE" ,"season" , "Monitor.closest",  "O3.mg.m3","O3.ppm", "O3.LOD.ppm", "O3.Below.detection" ,        
         "ozone.max", "average.RH", "min.RH" , "max.RH" ,"average.temperature.Celsius",
         "min.temperature" ,  "max.temperature" , "time.hours" ,  "Type of Air Conditioner","ac.type")


write_csv(ozone.summary,".//Processed Data//ozone.summary.csv")


## Figure S-1 UDAQ Ozone vs. Study Ozone (all homes)

own.colors.2 <- brewer.pal(n = 9, name = "Set1")[c(3:9)]


ggplot(data=study.summary.out,aes(x=Ozone.UDAQ.ppb, y=O3.ppb))+
  geom_point(aes(color=Monitor.closest))+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(y='Study Outdoor Ozone, ppb', x='UDAQ Monitor Ozone, ppb')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),
               size=5)+
  theme_bw()+
  coord_cartesian(xlim=c(18,50),ylim=c(18,50))+
  scale_color_manual(name = 'Closest Monitor', values = own.colors.2)+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        plot.margin= margin(t=1,r=1,b=1,l=1))+
  theme(legend.position = 'bottom')

ggsave(".//Graphics//Ozone//Ozone.UDAQ.Comparison.png",width=6, height=4.5, units="in", dpi=300)


## plot a summary plot by date
## Figure 1

ozone.summary$house.number.visit <- factor(ozone.summary$house.number.visit,levels=rev(sort(unique(ozone.summary$house.number.visit))),ordered=T)
ozone.summary$house.number.visit.date <- factor(ozone.summary$house.number.visit.date,levels=rev(sort(unique(ozone.summary$house.number.visit.date))),ordered=T)
ozone.summary$Location <- factor(ozone.summary$Location,levels=c("Out","In"),ordered=T)

lapply(ozone.summary,class)

# I prefer the plot by date...

### sustainability Figure 3
ann_text_LOD <- data.frame(lab = c("Above LOD","Below LOD","Below LOD"), Location = c('Out','Out','Out'),
                           house.number.visit.date = c('2022-08-11 H12 V1','2023-08-10 H29 V1','2023-08-21 H29 V2'),
                           ac.type = c("AC","EC","EC"), O3.ppm = c(11/1000,4/1000,4/1000))

# https://r-graphics.org/recipe-annotate-facet
# https://ggplot2.tidyverse.org/reference/geom_text.html
# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
# hjust and vjust are horizontal and vertical alignment
# nudge_x and nudge_y are the nudging!


ggplot(data = ozone.summary,  aes(y = house.number.visit.date, x = O3.ppm*1000,fill=Location))+  
  geom_col(position=position_dodge2(preserve='single'),width=0.7,color='white')+
  geom_point(aes(x=O3.LOD.ppm*1000,group=Location),color='grey',position=position_dodge(width=0.7)) +
  #geom_line(aes(x=O3.LOD.ppm*1000,group=house.number.visit.date),color="grey")+
  facet_grid(ac.type~.,scales='free_y' ,space='free') +
  scale_fill_brewer(palette = 'Set1')+
  labs(x ="Ozone concentration, ppb",title='',y='')+
  #coord_cartesian(xlim = c(0, 110)) +
  #scale_x_continuous(breaks=seq(0,110,10))+
  theme_bw()+
  guides(fill=guide_legend(reverse=TRUE))+
  geom_text(data = ann_text_LOD,size = 4,color='grey28',
            aes(y = house.number.visit.date, x = O3.ppm*1000, label=lab),nudge_y = 0,vjust=0,hjust = 0)+
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=16),
        axis.title = element_text(size = 16),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        plot.margin= margin(t=-10,r=1,b=-1,l=-1))
ggsave(".//Graphics//Ozone//ozone.indoor.outdoor.date.png", width=6, height=8.5, units="in", dpi=900)




#https://ggplot2.tidyverse.org/reference/ggsave.html
?ggsave
  
## plot again, but sort by house and visit

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
        legend.title = element_text(size = 18),legend.text = element_text(size = 14),
        strip.text = element_text(size=14))

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

ozone.summary.out <- ozone.summary %>%
  filter(Location == 'Out') %>%
  mutate(O3.ppb = 1000 * as.numeric(O3.ppm))

## Calculate means

ozone.ave.house <- ozone.wide %>%
  group_by(House.Number,ac.type ) %>%
  summarize(mean_house = mean(`I/O`))

#levels.house <-ozone.ave.house[order(ozone.ave.house$ave_ozone_IO),'House.Number']
levels.house <- ozone.ave.house$House.Number[order(ozone.ave.house$mean_house)]
levels.house

ozone.wide <- ozone.wide %>%
  mutate(House.Number = factor(House.Number,levels=levels.house,ordered=T))


ozone.wide$`Type of Air Conditioner` <- ifelse(ozone.wide$`Type of Air Conditioner` == "Central", "AC",
                                               ifelse(ozone.wide$`Type of Air Conditioner` == "Evaporative", "EC",
                                                      ozone.wide$`Type of Air Conditioner`))
###
## 

#plot Ozone vs Temp (Outdoor)
png(".//Graphics//Ozone//Outdoor.Ozone.Temp.Comparison.png",width=3.6, height=3, units="in", res=300)
ggplot(data=ozone.summary.out,aes(x=average.temperature.Celsius, y=O3.ppb))+
  geom_point(aes(color=Monitor.closest))+
  labs(y='Study Outdoor Ozone, ppb', x='Outdoor Temperature, C')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=3)+
  theme_bw()+
  coord_cartesian(xlim=c(20,35),ylim=c(10,55))+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.position = 'bottom', strip.text = element_text(size=9))
dev.off()

#plot Ozone vs RH (Outdoor)
png(".//Graphics//Ozone//Outdoor.Ozone.RH.Comparison.png",width=3.6, height=3, units="in", res=300)
ggplot(data=ozone.summary.out,aes(x=average.RH, y=O3.ppb))+
  geom_point(aes(color=Monitor.closest))+
  labs(y='Study Outdoor Ozone, ppb', x='Average.RH')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=3)+
  theme_bw()+
  #coord_cartesian(xlim=c(18,55),ylim=c(18,55))+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.position = 'bottom', strip.text = element_text(size=9))
dev.off()

#plot Ozone vs RH (Outdoor) color AC type
png(".//Graphics//Ozone//Outdoor.Ozone.RH.Comparison.ac.type.png",width=3.6, height=3, units="in", res=300)
ggplot(data=ozone.summary.out,aes(x=average.RH, y=O3.ppb))+
  geom_point(aes(color=ac.type))+
  labs(y='Study Outdoor Ozone, ppb', x='Average.RH')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=3)+
  theme_bw()+
  #coord_cartesian(xlim=c(18,55),ylim=c(18,55))+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.position = 'bottom', strip.text = element_text(size=9))
dev.off()

### Figure 2
## I/O sorted from lowest to highest

ggplot(data = ozone.wide,aes(x = House.Number, y = `I/O`,fill=House.Number)) + 
  geom_jitter(size=2,alpha=0.9,width=0.15,pch=21,color='black')+
  theme_bw()+
  expand_limits(y=0,x=0)+
  labs(x='House', y= 'I/O')+
  facet_grid(.~ac.type, scales='free') +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks=seq(0,1,.20))+
  theme(legend.position = 'blank')+
  theme(axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=10, angle = 90, vjust = 0.5),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        strip.text = element_text(size=14))

ggsave(".//Graphics//Ozone//ozone.io.png", width=6.5, height=4, units="in", dpi=300)


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
ggplot(data = filter(ozone.visit.wide,`Type of Air Conditioner` =='EC'),
       aes(x = V1, y = V2)) + 
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

## Look at potential correlation with respect to time

ozone.wide <- ozone.wide %>%
  mutate(year = year(first.day))

ggplot(data = ozone.wide,aes(x = first.day, y = `I/O`,fill=`House.Number`)) + 
  geom_jitter(size=2,alpha=0.9,width=0.2,pch=21,color='black')+
  theme_bw()+
  labs(x='House', y= 'I/O')+
  facet_grid(`Type of Air Conditioner`~ year, scales='free') +
  expand_limits(y=0)+ 
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(angle = 90,vjust =0.5, size=8))


###
#Now calculate the average of the averages, and a 95% confidence intervals
#

ozone.ave.type.2 <- ozone.ave.house %>%
  group_by(ac.type) %>%
  summarize(mean = mean(mean_house),
            sd = sd(mean_house),
            n = sum(!is.na(mean_house))) %>%
  mutate(tcrit = qt(.975,df=(n-1))) %>% ## two-sided 
  mutate(bound = tcrit*sd/sqrt(n)) %>%
  mutate(lower.95 = mean-bound) %>%
  mutate(upper.95 = mean+bound ) 

## plot mean with 95% CI

own.colors.3 <- brewer.pal(n = 9, name = "Paired")[c(7:8)]
display.brewer.all()

png(".//Graphics//Ozone//O3.IO.ratio.AC.type.png", width=4.5, height=4, units="in", res=300)
ggplot(data=ozone.ave.type.2,aes(x=ac.type, y= mean, fill=ac.type))+
  geom_col()+
  geom_errorbar(aes(ymin=lower.95,ymax=upper.95,width=0.25))+
  theme_bw()+
  labs(y='Mean I/O Ozone ratio',x='Air Conditioner Type') +
  #scale_fill_brewer(palette = 'Paired')+
  scale_fill_manual(name = 'Air Conditioner Type', values = own.colors.3)+
  theme(legend.position = 'none')+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=12),
        axis.title = element_text(size = 14))
dev.off()

## I/O vs. other factors
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


png(".//Graphics//Ozone//O3.IO.vs.RH.png", width=4.5, height=4, units="in", res=300)
ggplot(data=ozone.wide,aes(x=average.RH_Out, y=`I/O`))+
  geom_point(aes(color=day.type))+
  geom_smooth(color = "black",method='loess',span=2) +
  labs(x = "Relative Humidity ")+
  theme_bw()+
  facet_grid(.~`Type of Air Conditioner`) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 10),plot.title = element_text(size = 20),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        strip.text = element_text(size=10))
dev.off()


names(ozone.wide)

png(".//Graphics//Ozone//O3.IO.vs.Ozone.png", width=4.5, height=4, units="in", res=300)
ggplot(data=ozone.wide,aes(x=Ozone.UDAQ.ppb, y=`I/O`))+
  geom_point(aes(color=day.type))+
  geom_smooth(color = "black",method='loess',span=2) +
  labs(x = "O3 Concentration (ppb) at closest UDAQ monitor")+
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  expand_limits(y=0)+
  scale_y_continuous(breaks = seq(0,1,.2))+
  scale_x_continuous(breaks = seq(34,54,4))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 10),plot.title = element_text(size = 20),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        strip.text = element_text(size=10))
dev.off()


## does that mean there is a non-linear trend between indoor and outdoor concentrations?
names(ozone.wide)

png(".//Graphics//Ozone//Indoor.Outdoor.png", width=4.5, height=4, units="in", res=300)
ggplot(data=ozone.wide,aes(x=ozone.max_Out*1000, y=ozone.max_In*1000))+
  geom_point(aes(color=day.type, shape=day.type), alpha=.5, size=3)+
  geom_smooth(color = "black",method='loess',span=2) +
  labs(x = "Outdoor Ozone Concentration, ppb", y="Indoor Ozone Concentration, ppb")+
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 10),plot.title = element_text(size = 20),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        strip.text = element_text(size=10))
dev.off()




#plot Ozone vs UDAQ ozone (all homes)
ggplot(data=study.summary.out,aes(x=O3.ppb, y=Ozone.UDAQ.ppb))+
  geom_point(aes(color=factor(House.Number)))+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(x='Study Ozone Concentration ppb', y='UDAQ Average Ozone ppb')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")))

