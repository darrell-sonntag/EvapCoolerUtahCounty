Summer.All.Hourly <- inner_join(Summer2022.23Hourly,visitmaster.list.2,
                              join_by(between(Date,start_date,end_date))) 
                                                                                         
Winter.All.Hourly <- inner_join(Winter2022Hourly,visitmaster.list.2,
                              join_by(between(Date,start_date,end_date)))

Ozone.All.Hourly <- bind_rows(Summer.All.Hourly, Winter.All.Hourly) %>%
  filter(Parameter == "OZONE") %>%
  mutate(House.number.int = as.integer(substring(House.Number, 2,3)))

UDAQ.Ozone.Arranged <- Ozone.All.Hourly %>%
  group_by(Date,House.Number,Visit,Concentration,Monitor,House.number.int) %>%
  summarise(first.day = min(first.day))

UDAQ.Ozone.Arranged$Date <- as.Date(UDAQ.Ozone.Arranged$Date)
UDAQ.Ozone.Arranged$HourlyData <- as.POSIXct(UDAQ.Ozone.Arranged$HourlyData, format = "%H:%M:%S")


write_csv(UDAQ.Ozone.Arranged,".//Processed Data//UDAQ.Ozone.Arranged.csv")

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

setwd("C:\\Users\\cello\\OneDrive\\문서\\Github\\EvapCoolerUtahCounty")

png(".//Graphics//Ozone//Ozone.UDAQ.Hourly.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(UDAQ.Ozone.Arranged, House.number.int < 10)) + 
  geom_line(aes(x = Date, y = Concentration, color=Monitor))+
  theme_bw()+
  labs(x = expression(paste("Hour of the day")),
       y = expression(paste("Ozone Concentrations (ppb)")))+
  scale_color_brewer(palette = 'Set1')+
  facet_wrap(.~Date, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

png(".//Graphics//Ozone//Ozone.UDAQ.Hourly.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(UDAQ.Ozone.Arranged, House.number.int > 9 & House.number.int < 20)) + 
  geom_line(aes(x = Date, y = Concentration, color=Monitor))+
  theme_bw()+
  labs(x = expression(paste("Hour of the day")),
       y = expression(paste("Ozone Concentrations (ppb)")))+
  scale_color_brewer(palette = 'Set1')+
  facet_wrap(.~Date, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

png(".//Graphics//Ozone//Ozone.UDAQ.Hourly.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(UDAQ.Ozone.Arranged, House.number.int > 19)) + 
  geom_line(aes(x = Date, y = Concentration, color=Monitor))+
  theme_bw()+
  labs(x = expression(paste("Hour of the day")),
       y = expression(paste("Ozone Concentrations (ppb)")))+
  scale_color_brewer(palette = 'Set1')+
  facet_wrap(.~Date, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()
