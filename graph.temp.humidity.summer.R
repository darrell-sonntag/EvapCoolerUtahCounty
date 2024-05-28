##
## now graph the summer data by HouseID for the ozone paper
##


TRH.data.summer <- TRH.data4 %>%
  filter(season == 'Summer')

head(TRH.data.summer)

# plot UDAQ data corresponding to our ozone data collected timeline House 1 to 10

own.colors.10 <- c(brewer.pal(n = 9, name = "Set1")[c(2,1)],brewer.pal(n=5, name='Paired')[5])
#display.brewer.all()

png(".//Graphics//TRH//Temp.1to10.png", width=6.5, height=7, units="in", res=600)
ggplot(data = filter(TRH.data.summer, House.number.int < 11)) + 
  geom_line(aes(x = duration.hours, y = Temp, color=Location),size=1)+
  scale_color_manual(name = 'Location', values = own.colors.10)+
  theme_bw()+
  labs(x = expression(paste("Hour")),
       y = expression(Temperature~degree*C))+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  scale_x_continuous(breaks=seq(0,40,8))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 11),
        legend.title = element_text(size = 12),legend.text = element_text(size = 11),strip.text = element_text(size=11),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()


## plot the raw data along side BYU

png(".//Graphics//TRH//Temp.11to19.png", width=6.5, height=7, units="in", res=600)
ggplot(data = filter(TRH.data.summer,  House.number.int > 10 & House.number.int < 20)) + 
  geom_line(aes(x = duration.hours, y = Temp, color=Location),size=1)+
  scale_color_manual(name = 'Location', values = own.colors.10)+
  theme_bw()+
  labs(x = expression(paste("Hour")),
       y = expression(Temperature~degree*C))+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  scale_x_continuous(breaks=seq(0,40,8))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 11),
        legend.title = element_text(size = 12),legend.text = element_text(size = 11),strip.text = element_text(size=11),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()


png(".//Graphics//TRH//Temp.20.png", width=6.5, height=7, units="in", res=600)
ggplot(data = filter(TRH.data.summer,  House.number.int > 19 )) + 
  geom_line(aes(x = duration.hours, y = Temp, color=Location),size=1)+
  scale_color_manual(name = 'Location', values = own.colors.10)+
  theme_bw()+
  labs(x = expression(paste("Hour")),
       y = expression(Temperature~degree*C))+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  scale_x_continuous(breaks=seq(0,40,8))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 11),
        legend.title = element_text(size = 12),legend.text = element_text(size = 11),strip.text = element_text(size=11),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()


## AC temperature

png(".//Graphics//TRH//Temp.ac.png", width=7.5, height=9.25, units="in", res=600)
ggplot(data = filter(TRH.data.summer, ac.type == 'Central')) + 
  geom_line(aes(x = duration.hours, y = Temp, color=Location),size=1)+
  scale_color_manual(name = 'Location', values = own.colors.10)+
  theme_bw()+
  labs(x = expression(paste("Hour")),
       y = expression(Temperature~degree*C))+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  scale_x_continuous(breaks=seq(0,40,8))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 11),
        legend.title = element_text(size = 12),legend.text = element_text(size = 11),strip.text = element_text(size=11),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

## EC temperature

png(".//Graphics//TRH//Temp.ec.png", width=7.5, height=9.25, units="in", res=600)
ggplot(data = filter(TRH.data.summer, ac.type == 'Evap')) + 
  geom_line(aes(x = duration.hours, y = Temp, color=Location),size=1)+
  scale_color_manual(name = 'Location', values = own.colors.10)+
  theme_bw()+
  labs(x = expression(paste("Hour")),
       y = expression(Temperature~degree*C))+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  scale_x_continuous(breaks=seq(0,40,8))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 11),
        legend.title = element_text(size = 12),legend.text = element_text(size = 11),strip.text = element_text(size=11),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

#########


## AC RH

png(".//Graphics//TRH//RH.ac.png", width=7.5, height=9.25, units="in", res=600)
ggplot(data = filter(TRH.data.summer, ac.type == 'Central')) + 
  geom_line(aes(x = duration.hours, y = RH, color=Location),size=1)+
  scale_color_manual(name = 'Location', values = own.colors.10)+
  theme_bw()+
  labs(x = expression(paste("Hour")),
       y = "Relative Humidity, %")+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  scale_x_continuous(breaks=seq(0,40,8))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 11),
        legend.title = element_text(size = 12),legend.text = element_text(size = 11),strip.text = element_text(size=11),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

## EC RH

png(".//Graphics//TRH//RH.ec.png", width=7.5, height=9.25, units="in", res=600)
ggplot(data = filter(TRH.data.summer, ac.type == 'Evap')) + 
  geom_line(aes(x = duration.hours, y = RH, color=Location),size=1)+
  scale_color_manual(name = 'Location', values = own.colors.10)+
  theme_bw()+
  labs(x = expression(paste("Hour")),
       y = "Relative Humidity, %")+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  scale_x_continuous(breaks=seq(0,40,8))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 11),
        legend.title = element_text(size = 12),legend.text = element_text(size = 11),strip.text = element_text(size=11),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

#### 

View(TRH.data.summer)

#Creates a summary of the daily summary data

TRH.ac.type.summer.summary <- TRH_Summary_Table %>%
  left_join(acdata,by=c('House.Number')) %>%
  filter(season =='Summer') %>%
  group_by(`Type of Air Conditioner`,Location) %>%
  dplyr::summarize(
    count.RH = sum(!is.na(average.RH)),
    mean.mean.RH = mean(average.RH,na.rm=T),
    median.mean.RH = median(average.RH,na.rm=T),
    min.mean.RH = min(average.RH,na.rm = T),
    max.mean.RH = max(average.RH,na.rm = T),
    mean.min.RH = mean(min.RH,na.rm=T),
    median.min.RH = median(min.RH,na.rm=T),
    min.min.RH = min(min.RH,na.rm=T),
    max.min.RH = max(min.RH,na.rm=T),
    mean.max.RH = mean(max.RH,na.rm=T),
    median.max.RH = median(max.RH,na.rm=T),
    min.max.RH = min(max.RH,na.rm=T),
    max.max.RH = max(max.RH, na.rm=T),
    
    count.Temp = sum(!is.na(average.Temp)),
    mean.mean.Temp = mean(average.Temp,na.rm=T),
    median.mean.Temp = median(average.Temp,na.rm=T),
    min.mean.Temp = min(average.Temp,na.rm = T),
    max.mean.Temp = max(average.Temp,na.rm = T),
    mean.min.Temp = mean(min.Temp,na.rm=T),
    median.min.Temp = median(min.Temp,na.rm=T),
    min.min.Temp = min(min.Temp,na.rm=T),
    max.min.Temp = max(min.Temp,na.rm=T),
    mean.max.Temp = mean(max.Temp,na.rm=T),
    median.max.Temp = median(max.Temp,na.rm=T),
    min.max.Temp = min(max.Temp,na.rm=T),
    max.max.Temp = max(max.Temp, na.rm=T))



write_csv(TRH.ac.type.summer.summary,".//Processed Data//TRH.ac.type.summer.summary.csv",na="")

?write.csv
