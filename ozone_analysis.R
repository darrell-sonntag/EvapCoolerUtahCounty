### Ozone analysis ####

#read in all libraries to be used
library(tidyverse)
library(ggpmisc)
library(readxl)
library(RColorBrewer)
library(GGally)


own.colors.2 <- brewer.pal(n = 9, name = "Set1")[c(3:9)]

study.summary.out <- study.summary.out %>%
  mutate(flow_level = ifelse(`Ave Flow (L/min)`<0.3,"Low","High")) %>%
  mutate(flow_level = factor(flow_level,levels = c('Low','High'),ordered = T)) %>%
  mutate(diff = O3.ppb - Ozone.UDAQ.ppb) %>%
  mutate(norm_bias = diff/Ozone.UDAQ.ppb) %>%
  mutate(year = as.character(year(first.day)))

## Figure S-10 . Plot ozone diff by time-- is that important?

ggplot(data=filter(study.summary.out,!is.na(diff)),aes(x=as_date(first.day), y=diff, color=flow_level))+
  geom_point()+
  labs(x='Date', y='Study Ozone - UDAQ Ozone, ppb')+
  facet_wrap(~year,scales='free_x') +
  theme_bw()+
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        plot.margin= margin(t=1,r=20,b=1,l=1))+
  theme(legend.position = 'bottom')

ggsave(".//Graphics//Ozone//Ozone.UDAQ.Time.png",width=6, height=4.5, units="in", dpi=300)

own.colors.2

ggplot(data=filter(study.summary.out,!is.na(diff)),aes(x=as_date(first.day), y=diff, color=Monitor.closest))+
  geom_point()+
  labs(x='Date', y='Study Ozone - UDAQ Ozone, ppb')+
  facet_wrap(~year,scales='free_x') +
  theme_bw()+
  scale_color_manual(name = 'Closest Monitor', values = own.colors.2)+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        plot.margin= margin(t=1,r=20,b=1,l=1))+
  theme(legend.position = 'bottom')

ggsave(".//Graphics//Ozone//Ozone.UDAQ.Time.monitor.png",width=6, height=4.5, units="in", dpi=300)


## Look at Ozone from study vs. UDAQ

ozone.udaq.compare <- study.summary.out %>%
  filter(!is.na(O3.ppb)) %>%
  select('House.Number','Visit','Location',
         'Ozone.UDAQ.ppb','Monitor.closest', 'first.day','year',
         "Ave Flow (L/min)","Total Volume (L)","flow_level",
         "O3.Below.detection" , "O3.ppb") %>%
  rename(UDAQ = Ozone.UDAQ.ppb) %>%
  rename(Study = O3.ppb ) %>%
  pivot_longer(cols = c(UDAQ,Study), 
               names_to='O3_measure',values_to = 'O3_ppb')

## Plot

own.colors.2 <- brewer.pal(n = 9, name = "Set1")[c(3:9)]
#display.brewer.all()


?geom_boxplot

## Figure S-8
ggplot(data=ozone.udaq.compare,aes(x=O3_measure, y=O3_ppb, color=O3_measure))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width=0.05,shape=1)+
  #geom_point(shape=1)+
  labs(y='Average Outdoor Ozone Concentration, ppb',x='')+
  theme_bw()+
  facet_grid(Monitor.closest~year)+
  expand_limits(y=0)+
  #coord_cartesian(xlim=c(18,60),ylim=c(18,60))+
  scale_y_continuous(breaks = seq(0,50,10))+
  scale_color_manual(name = 'Measurement', values = own.colors.2)+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        plot.margin= margin(t=1,r=1,b=1,l=1))+
  theme(legend.position = 'blank')
ggsave(".//Graphics//Ozone//Ozone.UDAQ.boxplot.png",width=6, height=6, units="in", dpi=300)

## Look at the diff box plots....

## Figure S-9

ggplot(data=filter(study.summary.out,!is.na(diff)),aes(y=diff,x =as.character(year), color=Monitor.closest))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width=0.05,shape=1)+
  labs(y='Study Ozone - UDAQ Ozone, ppb', x='Monitor Location')+
  theme_bw()+
  facet_wrap(~Monitor.closest)+
  #coord_cartesian(xlim=c(18,60),ylim=c(18,60))+
  scale_color_manual(name = 'Year', values = own.colors.2)+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        plot.margin= margin(t=1,r=1,b=1,l=1))+
  theme(legend.position = 'blank')
ggsave(".//Graphics//Ozone//Ozone.UDAQ.diff.boxplot.png",width=6, height=4.5, units="in", dpi=300)



## thoughts:

## could calculate the difference in local and ambient monitor
## Could estimate a model: diff = flow_level. And then see if the 
## What I don't understand is that the overal mean seems less biased at low flow
## But on the other hand, the correlation between the actual and observed observations is lower
## So, which one is better?
## perhaps, display two statistics? relative difference? and R2?

names(study.summary.out)

ozone.diff.lm <- lm(formula =   diff ~ year*Monitor.closest + flow_level*Monitor.closest, data=study.summary.out, na.action = na.omit )
ozone.diff.lm <- lm(formula =   diff ~ year*Monitor.closest , data=study.summary.out, na.action = na.omit )
ozone.diff.lm <- lm(formula =   diff ~ year + Monitor.closest , data=study.summary.out, na.action = na.omit )
summary(ozone.diff.lm)

## in terms of model fit, the 3rd model has the best fit...

ozone.diff.lm <- lm(formula =   O3.ppb ~ Monitor.closest + flow_level*Ozone.UDAQ.ppb + I(Ozone.UDAQ.ppb^2), data=study.summary.out, na.action = na.omit )
summary(ozone.diff.lm)
##


####

## Scatterplot UDAQ Ozone vs. Study Ozone (all homes)


own.colors.4 <- brewer.pal(n = 9, name = "Dark2")[c(2:3)]
#display.brewer.all()

ggplot(data=study.summary.out,aes(x=Ozone.UDAQ.ppb, y=O3.ppb))+
  geom_point(aes(color=year))+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(y='Study Outdoor Ozone, ppb', x='UDAQ Monitor Ozone, ppb')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),
               size=5)+
  theme_bw()+
  coord_cartesian(xlim=c(18,60),ylim=c(18,60))+
  scale_color_manual(name = 'Year', values = own.colors.4)+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        plot.margin= margin(t=1,r=1,b=1,l=1))+
  theme(legend.position = 'bottom')

ggsave(".//Graphics//Ozone//Ozone.UDAQ.scatter.png",width=6, height=4.5, units="in", dpi=300)

## scatter plot by year
##  Figure S-11
ggplot(data=filter(study.summary.out,!is.na(diff)),aes(x=Ozone.UDAQ.ppb, y=O3.ppb))+
  geom_point(aes(color=Monitor.closest))+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(y='Study Outdoor Ozone, ppb', x='UDAQ Monitor Ozone, ppb')+
  facet_grid(.~year) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),
             size=4)+
  theme_bw()+
  coord_cartesian(xlim=c(18,60),ylim=c(18,60))+
  scale_color_manual(name = 'Closest Monitor', values = own.colors.2)+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        plot.margin= margin(t=1,r=1,b=1,l=1))+
  theme(legend.position = 'bottom')
ggsave(".//Graphics//Ozone//Ozone.UDAQ.scatter.year.png",width=6, height=4.5, units="in", dpi=300)


## Figure S-3, by flow rate and Monitor?

names(study.summary.out)

ggplot(data=filter(study.summary.out,!is.na(diff)),aes(x=Ozone.UDAQ.ppb, y=O3.ppb, color=Monitor.closest))+
  geom_point()+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(y='Study Outdoor Ozone, ppb', x='UDAQ Monitor Ozone, ppb')+
  facet_grid(Monitor.closest~flow_level) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),
               size=4)+
  theme_bw()+
  coord_cartesian(xlim=c(18,60),ylim=c(18,60))+
  scale_color_manual(name = 'Closest Monitor', values = own.colors.2)+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        plot.margin= margin(t=1,r=1,b=1,l=1))+
  theme(legend.position = 'blank')

ggsave(".//Graphics//Ozone//Ozone.UDAQ.Comparison.2.png",width=6, height=4.5, units="in", dpi=300)

## The low flows are not systematically higher than the high flow
## however, there is a significantly different relationship between the
# low and high flow across different outdoor ozone days
## I fit a model with interaction terms to see if the difference is significant...

ozone.flow.lm <- lm(formula =   O3.ppb ~ flow_level*Ozone.UDAQ.ppb, data=study.summary.out, na.action = na.omit )
summary(ozone.flow.lm)

## note: the intercept is higher under the low flow, and the slope is significantly lower

names(study.summary.out)

### Calculate goodness of fit for each of the statistics 
### grouped by flow level and 

fit.stats <- study.summary.out %>%
              group_by(year) %>%
              summarize(mean_norm_bias = mean(norm_bias,na.rm=T),count = sum(!is.na(norm_bias)))

ozone.2022.lm <- lm(formula =   O3.ppb ~ Ozone.UDAQ.ppb, data=filter(study.summary.out, year==2022), na.action = na.omit )
summary(ozone.2022.lm)

ozone.2023.lm <- lm(formula =   O3.ppb ~ Ozone.UDAQ.ppb, data=filter(study.summary.out, year==2023), na.action = na.omit )
summary(ozone.2023.lm)

fit.stats$R2[1] = summary(ozone.2022.lm)$r.squared
fit.stats$R2[2] = summary(ozone.2023.lm)$r.squared 

write_csv(fit.stats,'.//Processed Data//fit.stats.csv')






## how many valid measurements do we have, and missing ozone measurements?

names(summary)

missing.ozone <- summary %>%
  filter(season == 'Summer') %>%
  filter(is.na(O3.mg.m3))

View(missing.ozone)

missing.SidePak <- summary %>%
  filter(season == 'Summer') %>%
  filter(is.na(SidePak.ug.m3.avg))
#filter(SidePak.is.valid !='Y')


## number of unique homes with ozone measurements

home.counts <- summary %>%
  ungroup() %>%
  filter(season == 'Summer') %>%
  filter(!is.na(O3.mg.m3)) %>%
  select(House.Number,'Type of Air Conditioner') %>%
  unique()

## note: house 15 is the same as house 9


######### 
# ozone summary
#############

smoke.event <- interval(ymd('2022-09-08'),ymd('2022-09-12'))

names(summary)

ozone.summary <- summary %>%
  filter(season =='Summer') %>%
  #dplyr::filter(!is.na(O3.LOD.ppm)) %>% ## just keep the visits with Ozone data 
  mutate(house.number.visit = paste(House.Number,Visit,sep=" ")) %>%
  mutate(house.number.visit.date = paste(first.day,house.number.visit,sep=" ")) %>%
  mutate(ac.type = ifelse(`Type of Air Conditioner`=='Central','AC',ifelse(`Type of Air Conditioner`=='Evaporative','EC',NA))) %>%
  mutate(day.type = ifelse(as_date(first.day) %within% smoke.event,'Wildfire Smoke','Normal')) %>%
  mutate(House.Number = ifelse(house.number.visit =='H15 V1','H09',House.Number)) %>% ### Change H15 V1 to H09 V3
  mutate(Visit = ifelse(house.number.visit =='H15 V1','V3',Visit)) %>% ### Change H15 V1 to H09 V3
  mutate(house.number.visit = ifelse(house.number.visit =='H15 V1','H09 V3',house.number.visit)) %>% ### Change H15 V1 to H09 V3
  mutate(House.Number = ifelse(house.number.visit =='H15 V2','H09',House.Number)) %>% ### Change H15 V2 to H09 V4
  mutate(Visit = ifelse(house.number.visit =='H15 V2','V4',Visit)) %>% ### Change H15 V2 to H09 V4
  mutate(house.number.visit = ifelse(house.number.visit =='H15 V2','H09 V4',house.number.visit)) %>% ### Change H15 V2 to H09 V4
  mutate(O3.LOD.ppb = O3.LOD.ppm * 1000) %>%
  mutate(O3.ppb = O3.ppm*1000) %>%
  mutate(ozone.max = ifelse(O3.Below.detection==T,O3.LOD.ppm,O3.ppm)) %>%
  mutate(ozone.max = round(as.numeric(ozone.max),digits=4)) %>%
  mutate(O3.ppb = as.numeric(O3.ppb)) %>%
  mutate(flow_level = ifelse(`Ave Flow (L/min)`<0.3,"Low","High")) %>%
  mutate(O3.Below.detection = factor(as.character(O3.Below.detection),levels=c('TRUE','FALSE'), ordered = T)) %>%
  select("House.Number","Visit", "house.number.visit","house.number.visit.date" ,"Location","first.day","Ozone.UDAQ.ppb", "UDAQ.n_OZONE" ,"season" , "Monitor.closest",  "O3.mg.m3","O3.ppb", "O3.LOD.ppb", "O3.Below.detection" ,        
         "ozone.max", "average.RH", "min.RH" , "max.RH" ,"average.Temp",
         "min.Temp" ,  "max.Temp" , "time.hours" ,  "Type of Air Conditioner","ac.type","flow_level")

write_csv(ozone.summary,".//Processed Data//ozone.summary.csv")


## plot a summary plot by date
## Figure 3

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


###### 

own.colors <- brewer.pal(n = 9, name = "Set1")[c(8:9)]
#display.brewer.all()

###

names(ozone.summary)


ggplot(data = filter(ozone.summary,!is.na(O3.LOD.ppb)),  aes(y = house.number.visit.date, x = O3.ppb,fill=Location))+  
  geom_col(position=position_dodge2(preserve='single'),width=0.7,color='white')+
  geom_point(aes(x=O3.LOD.ppb,group=Location),color='grey',position=position_dodge(width=0.7)) +
  facet_grid(ac.type~.,scales='free_y' ,space='free') +
  scale_fill_brewer(palette = 'Set1')+
  labs(x =expression(paste("O"[3]," concentration, ppb")),title='',y='')+
  #labs(x='',y=expression(paste("Daily Maximum\n 8-hour O"[3],"\nConcentration, ppb")),title = 'Lindon')+
  #coord_cartesian(xlim = c(0, 110)) +
  #scale_x_continuous(breaks=seq(0,110,10))+
  theme_bw()+
  guides(fill=guide_legend(reverse=TRUE))+
  geom_text(data = ann_text_LOD,size = 3.5,color='grey28',
            aes(y = house.number.visit.date, x = O3.ppm*1000, label=lab),nudge_y = 0,vjust=0,hjust = 0)+
  scale_x_continuous(breaks = seq(0,50,10))+
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=12),axis.text.x = element_text(size=12),
        axis.title = element_text(size = 14),plot.title = element_text(size = 14),
        legend.title = element_text(size = 12),legend.text = element_text(size = 12),
        strip.text = element_text(size=12),
        plot.margin= margin(t=-10,r=1,b=-1,l=-1))
ggsave(".//Graphics//Ozone//ozone.indoor.outdoor.date.png", width=5.2, height=7, units="in", dpi=900)

## 



#https://ggplot2.tidyverse.org/reference/ggsave.html
?ggsave
  
## plot again, but sort by house and visit

ggplot(data = ozone.summary,  aes(y = house.number.visit, x = O3.ppb,fill=Location))+  
  geom_col(position=position_dodge2(preserve='single'),width=0.7,color='white')+
  geom_point(aes(x=O3.LOD.ppb*1000,group=Location),color='grey',position=position_dodge(width=0.7)) +
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

names(ozone.summary)


### pivot to wide format
smoke.event <- interval(ymd('2022-09-08'),ymd('2022-09-12'))

ozone.wide <- ozone.summary %>%
  select(-O3.mg.m3,-time.hours) %>%
  pivot_wider(names_from = Location,values_from = c(O3.ppb,O3.LOD.ppb,ozone.max,O3.Below.detection,flow_level,average.Temp,
                                                    min.Temp,max.Temp,average.RH,min.RH,max.RH),names_sort = T) %>%
  #filter(!(house.number.visit %in% c('H02 V2','H03 V2')))  %>% # why did I exclude these visits earlier?
  mutate(`I/O` = ozone.max_In / ozone.max_Out) %>%
  mutate(day.type = ifelse(as_date(first.day) %within% smoke.event,'Wildfire Smoke','Normal')) %>%
  mutate(`Type of Air Conditioner` = ifelse(`Type of Air Conditioner` == "Central", "AC",
                                            ifelse(`Type of Air Conditioner` == "Evaporative", "EC",
                                                   `Type of Air Conditioner`))) %>%
  mutate(Temp_diff = average.Temp_In - average.Temp_Out) %>%
  mutate(RH_diff = average.RH_In - average.RH_Out) %>%
  mutate(Temp_diff_max = max.Temp_In - max.Temp_Out) %>%
  mutate(RH_diff_min = min.RH_In - min.RH_Out)


## Calculate means

ozone.ave.house <- ozone.wide %>%
  group_by(House.Number,ac.type ) %>%
  summarize(io.house = mean(`I/O`,na.rm=T))


levels.house <- ozone.ave.house$House.Number[order(ozone.ave.house$io.house)]
levels.house

## update order of house in ozone.wide according to the levels of the house
ozone.wide <- ozone.wide %>%
  mutate(House.Number = factor(House.Number,levels=levels.house,ordered=T)) 

           

### export summary table for supplemental information


ozone.supp <- ozone.wide %>%
              select('House.Number','Visit','first.day','Type of Air Conditioner',
                     "O3.ppb_In",'O3.LOD.ppb_In',"O3.ppb_Out",'O3.LOD.ppb_Out','I/O') %>%
              arrange(`Type of Air Conditioner`,House.Number,Visit) %>%
              rename(Date = first.day) %>%
              mutate(Date = as.character(Date)) %>%
              mutate(O3.ppb_In= round(O3.ppb_In,digits=1)) %>%
              mutate(O3.LOD.ppb_In= round(O3.LOD.ppb_In,digits=1)) %>%
              mutate(O3.ppb_Out= round(O3.ppb_Out,digits=1)) %>%
              mutate(O3.LOD.ppb_Out= round(O3.LOD.ppb_Out,digits=1)) %>%
              mutate(`I/O`= round(`I/O`,digits=2))

# note, I can't get arrange to work as I want

write_csv(ozone.supp,".//Processed Data//ozone.supp.table.csv")


### export summary table of indoor and outdoor temp and RH for supplemental information

names(ozone.wide)

ozone.supp.2 <- ozone.wide %>%
  arrange(`Type of Air Conditioner`,House.Number,Visit) %>%
  rename(Date = first.day) %>%
  mutate(Date = as.character(Date)) %>%
  mutate(`I/O`= round(`I/O`,digits=2)) %>%
  rename(`Out C`=average.Temp_Out ) %>%
  rename(`In C`=average.Temp_In ) %>%
  rename(`Out max C`=max.Temp_Out ) %>%
  rename(`In max C`=max.Temp_In) %>%
  rename(`Out RH%`=average.RH_Out ) %>%
  rename(`In RH%`=average.RH_In ) %>%
  rename(`Out min RH%` = min.RH_Out ) %>%
  rename(`In min RH%` = min.RH_In ) %>%
  select(`Type of Air Conditioner`,House.Number,Visit,Date,`I/O`,`In C`,`Out C`,
         `In max C`,`Out max C`,`In RH%`, `Out RH%`,
         `In min RH%`,`Out min RH%`, Temp_diff,Temp_diff_max,RH_diff, RH_diff_min) %>%
  mutate(across(3:15,round,2))

write_csv(ozone.supp.2,".//Processed Data//ozone.supp.table.temp.rh.csv")

### export summary for Dawson Pitcher

names(sidepak.stats) ## this is wide format has I/O
names(ozone.wide)

EvapCooler.summary <- sidepak.stats %>%
    full_join(ozone.wide,by=c("House.Number", "Visit","house.number.visit",  "house.number.visit.date",
                                 "first.day", "Type of Air Conditioner", "ac.type" ,"season","Monitor.closest",
                                 "average.Temp_Out","average.RH_In",  "average.RH_Out" ,  "min.Temp_In" , 
                                 "min.Temp_Out"  , "max.Temp_In" , "max.Temp_Out"))


write_csv(EvapCooler.summary,".//Processed Data//EvapCooler.summary.csv")




###
## Graph I/O

ozone.wide.graph <- filter(ozone.wide,!is.na(`I/O`))


### Figure 
## I/O sorted from lowest to highest

?geom_jitter

ggplot(data = ozone.wide,aes(x = House.Number, y = `I/O`,fill=House.Number)) + 
  geom_jitter(size=2,alpha=0.9,width=0.15,height=0,pch=21,color='black')+
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

## Figure 5

names(ozone.wide)
class(ozone.wide$O3.Below.detection_In)
levels(ozone.wide$O3.Below.detection_In)

ggplot(data = filter(ozone.wide,!is.na(`I/O`)),aes(x = House.Number, y = `I/O`,
                                                   color=flow_level_In, shape = O3.Below.detection_In)) + 
  geom_jitter(size=2,alpha=0.9,width=0.2,height=0.005,stroke=1)+
  theme_bw()+
  expand_limits(y=0,x=0)+
  labs(x='House', y= 'I/O')+
  facet_grid(.~ac.type, scales='free') +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks=seq(0,1,.20))+
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12, angle = 90, vjust = 0.5),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        strip.text = element_text(size=14),legend.text = element_text(size = 12))

ggsave(".//Graphics//Ozone//ozone.io.flow.png", width=6.5, height=4, units="in", dpi=600)


## wildfire smoke
## Figure S-17


ggplot(data = filter(ozone.wide,!is.na(`I/O`)),aes(x = House.Number, y = `I/O`,
                                                   color=day.type, shape = O3.Below.detection_In)) + 
  geom_jitter(size=2,alpha=0.9,width=0.2,height=0.005,stroke=1)+
  theme_bw()+
  expand_limits(y=0,x=0)+
  labs(x='House', y= 'I/O')+
  facet_grid(.~ac.type, scales='free') +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks=seq(0,1,.20))+
  scale_color_manual(name = 'Day Type', values = c('darkseagreen4','sienna'))+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12, angle = 90, vjust = 0.5),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        strip.text = element_text(size=14),legend.text = element_text(size = 12))

ggsave(".//Graphics//Ozone//ozone.io.wildire.png", width=6.5, height=4, units="in", dpi=600)


## I/O vs. other factors

## Outdoor temperature
## Figure S-18

ggplot(data=ozone.wide.graph,aes(x=average.Temp_Out, y=`I/O`))+
  geom_point(aes(color=flow_level_In, shape = O3.Below.detection_In),size=2)+
  geom_smooth(color = "black",method='loess',span=2) +
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  expand_limits(y=0)+
  labs(x = expression("Average Daily Outdoor Temperature"~degree*C))+
  #labs(x = 'Average Daily Outdoor Temperature')+
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  scale_y_continuous(breaks = seq(0,1,.2))+
  #scale_x_continuous(breaks = seq(-16,0,4))+
  theme(axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        strip.text = element_text(size=14),legend.text = element_text(size = 12))
ggsave(".//Graphics//Ozone//O3.IO.vs.temp.png", width=6.5, height=4, units="in", dpi=600)

?geom_smooth


## relative humidity
ggplot(data=ozone.wide.graph,aes(x=average.RH_Out, y=`I/O`))+
  geom_point(aes(color=flow_level_In, shape = O3.Below.detection_In),size=2)+
  geom_smooth(color = "black",method='loess',span=2) +
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  expand_limits(y=0)+
  labs(x = "Relative Humidity, %")+
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  scale_y_continuous(breaks = seq(0,1,.2))+
  #scale_x_continuous(breaks = seq(-16,0,4))+
  theme(axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        strip.text = element_text(size=14),legend.text = element_text(size = 12))
ggsave(".//Graphics//Ozone//O3.IO.vs.RH.png", width=6.5, height=4, units="in", dpi=600)



names(ozone.wide)

## Figure 7
## I/O vs. UDAQ ozone

ggplot(data=ozone.wide.graph,aes(x=Ozone.UDAQ.ppb, y=`I/O`))+
  geom_point(aes(color=flow_level_In, shape = O3.Below.detection_In),size=2)+
  geom_smooth(color = "black",method='loess',span=2) +
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  expand_limits(y=0)+
  #labs(x = "Daily Average Ozone Concentration (ppb) at Closest UDAQ Monitor")+
  labs(x = expression(paste("Daily Average O"[3]," Concentration (ppb) at Closest UDAQ Monitor"))) +
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  scale_y_continuous(breaks = seq(0,1,.2))+
  #scale_x_continuous(breaks = seq(-16,0,4))+
  theme(axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size = 12),plot.title = element_text(size = 20),
        strip.text = element_text(size=12),legend.text = element_text(size = 12))
ggsave(".//Graphics//Ozone//O3.IO.vs.Ozone.png", width=5.3, height=3.5, units="in", dpi=600)

## I/O vs. study outdoor ozone

ggplot(data=ozone.wide.graph,aes(x=O3.ppb_Out, y=`I/O`))+
  geom_point(aes(color=flow_level_In, shape = O3.Below.detection_In),size=2)+
  geom_smooth(color = "black",method='loess',span=2) +
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  expand_limits(y=0)+
  labs(x = "Outdoor Ozone Concentration (ppb)")+
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  scale_y_continuous(breaks = seq(0,1,.2))+
  #scale_x_continuous(breaks = seq(-16,0,4))+
  theme(axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        strip.text = element_text(size=14),legend.text = element_text(size = 12))
ggsave(".//Graphics//Ozone//O3.IO.vs.Study.Ozone.png", width=6.5, height=4, units="in", dpi=600)


## I/O vs. Temp_diff

names(ozone.wide)

png(".//Graphics//Ozone//O3.IO.vs.Temp.diff.png", width=4.5, height=4, units="in", res=300)
ggplot(data=ozone.wide,aes(x=Temp_diff, y=`I/O`))+
  geom_point(aes(color=day.type))+
  geom_smooth(color = "black",method='loess',span=2) +
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  expand_limits(y=0)+
  scale_y_continuous(breaks = seq(0,1,.2))+
  #scale_x_continuous(breaks = seq(34,54,4))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 10),plot.title = element_text(size = 20),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        strip.text = element_text(size=10))
dev.off()

### I/O vs. Temp_diff_max
## Figure S-20

ggplot(data=ozone.wide.graph,aes(x=Temp_diff_max, y=`I/O`))+
  geom_point(aes(color=flow_level_In, shape = O3.Below.detection_In),size=2)+
  geom_smooth(color = "black",method='loess',span=2) +
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  expand_limits(y=0)+
  labs(x = 'Difference in Daily Maximum Temperature (Indoor - Outdoor)')+
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  scale_y_continuous(breaks = seq(0,1,.2))+
  scale_x_continuous(breaks = seq(-16,0,4))+
  theme(axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        strip.text = element_text(size=14),legend.text = element_text(size = 12))
ggsave(".//Graphics//Ozone//O3.IO.vs.Temp.diff.max.png", width=6.5, height=4, units="in", dpi=600)




## I/O vs. RH_diff

names(ozone.wide)

png(".//Graphics//Ozone//O3.IO.vs.RH.diff.png", width=4.5, height=4, units="in", res=300)
ggplot(data=ozone.wide,aes(x=RH_diff, y=`I/O`))+
  geom_point(aes(color=day.type))+
  geom_smooth(color = "black",method='loess',span=2) +
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  expand_limits(y=0)+
  scale_y_continuous(breaks = seq(0,1,.2))+
  #scale_x_continuous(breaks = seq(34,54,4))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 10),plot.title = element_text(size = 20),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        strip.text = element_text(size=10))
dev.off()

## min (loess)

ggplot(data=ozone.wide.graph,aes(x=RH_diff_min, y=`I/O`))+
  geom_point(aes(color=flow_level_In, shape = O3.Below.detection_In),size=2)+
  geom_smooth(color = "black",method='loess',span=2) +
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  expand_limits(y=0)+
  labs(x = 'Difference in Minimum Relative Humidity (Indoor - Outdoor)')+
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  scale_y_continuous(breaks = seq(0,1,.2))+
  scale_x_continuous(breaks = seq(-20,40,20))+
  theme(axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        strip.text = element_text(size=14),legend.text = element_text(size = 12))
ggsave(".//Graphics//Ozone//O3.IO.vs.RH.diff.min.png", width=6.5, height=4, units="in", dpi=600)

## Figure 6 min (linear)

ggplot(data=ozone.wide.graph,aes(x=RH_diff_min, y=`I/O`))+
  geom_point(aes(color=flow_level_In, shape = O3.Below.detection_In),size=2)+
  geom_smooth(method = "lm",  se = FALSE) + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=3)+
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  expand_limits(y=0)+
  labs(x = 'Difference in Daily Minimum Relative Humidity (Indoor - Outdoor)')+
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  scale_y_continuous(breaks = seq(0,1,.2))+
  scale_x_continuous(breaks = seq(-20,40,20))+
  theme(axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size = 12),plot.title = element_text(size = 20),
        strip.text = element_text(size=12),legend.text = element_text(size = 12))
ggsave(".//Graphics//Ozone//O3.IO.vs.RH.diff.min.lin.png", width=5.3, height=3.5, units="in", dpi=600)





## does that mean there is a non-linear trend between indoor and outdoor concentrations?
names(ozone.wide)

# Figure 4
## Indoor vs. Outdoor ozone

ggplot(data=ozone.wide.graph,aes(x=ozone.max_Out*1000, y=ozone.max_In*1000))+
  geom_point(aes(color=flow_level_In, shape = O3.Below.detection_In),size=2)+
  geom_smooth(color = "black",method='loess',span=2) +
  labs(x = expression(paste("Outdoor O"[3]," concentration, ppb")),
       y = expression(paste("Indoor O"[3]," concentration, ppb"))) +
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size = 12),plot.title = element_text(size = 20),
        strip.text = element_text(size=12),legend.text = element_text(size = 12))
ggsave(".//Graphics//Ozone//Indoor.Outdoor.png", width=5.3, height=3.5, units="in", dpi=600)


## indoor vs. Outdoor UDAQ ozone

ggplot(data=ozone.wide.graph,aes(x=Ozone.UDAQ.ppb, y=ozone.max_In*1000))+
  geom_point(aes(color=flow_level_In, shape = O3.Below.detection_In),size=2)+
  geom_smooth(color = "black",method='loess',span=2) +
  labs(x = "Outdoor Ozone Concentration, ppb", y="Indoor Ozone Concentration, ppb")+
  scale_color_manual(name = 'Flow rate', values = own.colors.2[c(6,7)])+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  theme_bw()+
  facet_grid(.~ac.type) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        strip.text = element_text(size=14),legend.text = element_text(size = 12))
ggsave(".//Graphics//Ozone//Indoor.Outdoor.UDAQ.png", width=6.5, height=4, units="in", dpi=600)


#plot Ozone vs UDAQ ozone (all homes)
ggplot(data=study.summary.out,aes(x=O3.ppb, y=Ozone.UDAQ.ppb))+
  geom_point(aes(color=factor(House.Number)))+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(x='Study Ozone Concentration ppb', y='UDAQ Average Ozone ppb')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")))




###
#Now calculate the average of the averages, and a 95% confidence intervals
#

## If we had multiple measurements of the same home, use only the high flow rate

names(ozone.wide)

ozone.wide.qa <- ozone.wide %>%
                 filter(!(house.number.visit 
                          %in% c('H02 V1','H03 V1','H05 V1','H08 V1','H09 V1',
                                              'H10 V1','H16 V1'))) %>% ## remove the low flow from multiple visits
                  filter(!(house.number.visit 
                     %in% c('H29 V1','H29 V2','H27 V2'))) ## remove the suspect EC homes

## label each point above or below the detection limit

LOD <- ozone.wide.qa %>%
  ungroup() %>%
  select(House.Number,O3.Below.detection_In) %>%
  filter(!is.na(O3.Below.detection_In)) %>%
  unique()

## Calculate means of each home

names(ozone.wide.qa)

ozone.ave.house.qa <- ozone.wide.qa %>%
  group_by(House.Number,ac.type ) %>%
  summarize(io.house = mean(`I/O`,na.rm=T),
            indoor.O3.house =  mean(ozone.max_In,na.rm=T)*1000) %>%
  left_join(LOD,by='House.Number') 

names(ozone.ave.house.qa)

## Boxplot

own.colors.3 <- brewer.pal(n = 9, name = "Paired")[c(7:8)]
display.brewer.all()

## Figure 8
## boxplot I/O by house
ggplot(data=ozone.ave.house.qa,aes(x=ac.type, y=io.house, color=ac.type))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width=0.05,size=2, aes(shape = O3.Below.detection_In))+
  labs(y='I/O',x='')+
  theme_bw()+
  expand_limits(y=c(0,1))+
  scale_y_continuous(breaks = seq(0,1,.2))+
  scale_color_manual(name = 'House Type', values = own.colors.3)+
  scale_shape_manual(name = 'Below LOD',values = c(21,24))+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        plot.margin= margin(t=1,r=1,b=1,l=1))+
  theme(legend.position = 'right')
ggsave(".//Graphics//Ozone//I.O.house.boxplot.png",width=6, height=4, units="in", dpi=600)




### Calculate mean I/O by ac type and 95% CI

ozone.ave.type.2 <- ozone.ave.house.qa %>%
  group_by(ac.type) %>%
  summarize(mean = mean(io.house),
            sd = sd(io.house),
            n = sum(!is.na(io.house))) %>%
  mutate(tcrit = qt(.975,df=(n-1))) %>% ## two-sided 
  mutate(bound = tcrit*sd/sqrt(n)) %>%
  mutate(lower.95 = mean-bound) %>%
  mutate(upper.95 = mean+bound ) 


write_csv(ozone.ave.type.2,'.//Processed Data//mean.i.o.confidence.csv')

### two-sided t-test

t.test.ozone <- t.test(io.house~
                         ac.type,
                       data=ozone.ave.house.qa,var.equal=F)

t.test.ozone
t.test.ozone$p.value
t.test.ozone$conf.int[1]
t.test.ozone$conf.int[2]

library(broom)
glance(t.test.ozone)

write_csv(glance(t.test.ozone),'.//Processed Data//t.test.io.ozone.csv')


## Figure 9

png(".//Graphics//Ozone//O3.IO.ratio.AC.type.png", width=4.5, height=4, units="in", res=300)
ggplot(data=ozone.ave.type.2,aes(x=ac.type, y= mean, fill=ac.type))+
  geom_col()+
  geom_errorbar(aes(ymin=lower.95,ymax=upper.95,width=0.25))+
  theme_bw()+
  expand_limits(y=c(0,1))+
  scale_y_continuous(breaks = seq(0,1,.2))+
  labs(y='Mean I/O',x='') +
  #scale_fill_brewer(palette = 'Paired')+
  scale_fill_manual(name = 'House', values = own.colors.3)+
  theme(legend.position = 'none')+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=12),
        axis.title = element_text(size = 14))
dev.off()

## Calculate mean indoor with 95% CI

ozone.ave.type.indoor <- ozone.ave.house.qa %>%
  group_by(ac.type) %>%
  summarize(mean = mean(indoor.O3.house),
            sd = sd(indoor.O3.house),
            n = sum(!is.na(indoor.O3.house))) %>%
  mutate(tcrit = qt(.975,df=(n-1))) %>% ## two-sided 
  mutate(bound = tcrit*sd/sqrt(n)) %>%
  mutate(lower.95 = mean-bound) %>%
  mutate(upper.95 = mean+bound ) 


write_csv(ozone.ave.type.indoor,'.//Processed Data//mean.indoor.confidence.csv')

### two-sided t-test

t.test.ozone.indoor <- t.test(indoor.O3.house~
                         ac.type,
                       data=ozone.ave.house.qa,var.equal=F)

t.test.ozone.indoor

glance(t.test.ozone.indoor)

write_csv(glance(t.test.ozone.indoor),'.//Processed Data//t.test.indoor.ozone.csv')

