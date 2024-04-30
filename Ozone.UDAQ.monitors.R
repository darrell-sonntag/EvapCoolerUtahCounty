setwd("C:\\Users\\cello\\OneDrive\\문서\\Github\\EvapCoolerUtahCounty")

Summer.All.Hourly <- inner_join(Summer2022.23Hourly,visitmaster.list.2,
                              join_by(between(Date,start_date,end_date))) 
                                                                                         
Winter.All.Hourly <- inner_join(Winter2022Hourly,visitmaster.list.2,
                              join_by(between(Date,start_date,end_date)))

Ozone.All.Hourly <- bind_rows(Summer.All.Hourly, Winter.All.Hourly) %>%
  filter(Parameter == "OZONE") %>%
  mutate(House.number.int = as.integer(substring(House.Number, 2,3))) %>%
  mutate(Date = as_datetime(Date)) %>% 
  mutate(Date = Date - hours(6)) %>%#### Changing UTC time to MDT time
  mutate(Time = as_hms(Date))

UDAQ.Ozone.Arranged <- Ozone.All.Hourly %>%
  group_by(Date,House.Number,Visit,Concentration,Monitor,House.number.int,Time) %>%
  summarise(first.day = min(first.day)) %>%
  mutate(first.day = as_date(first.day)) %>%
  mutate(Date = as_date(Date)) %>%
  mutate(Year = lubridate::year(Date)) %>%
  mutate(pseudo.day = Date - first.day) %>%
  mutate(pseudo.time = as.period(pseudo.day) + as.period(Time)) %>%
  mutate(duration = as.duration(pseudo.time)) %>%
  mutate(duration.hours = duration/3600) %>%                              
  mutate(duration.hms = hms::hms(duration)) %>%
  mutate(house.number.visit = paste(House.Number,Visit,sep=" ")) 

UDAQ.Ozone.Arranged.2022 <- UDAQ.Ozone.Arranged %>%
  filter(Year == 2022)

UDAQ.Ozone.Arranged.2023 <- UDAQ.Ozone.Arranged %>%
  filter(Year == 2023)

monitor_data <- UDAQ.Ozone.Arranged %>%
  filter(Monitor %in% c("Spanish Fork", "Lindon")) %>%
  pivot_wider(names_from = Monitor, values_from = Concentration) %>%
  drop_na()

#2022 Box plot by monitor
png(".//Graphics//Ozone//Ozone_Concentration_2022.png", width=6.5, height=7, units="in", res=300)
ggplot(UDAQ.Ozone.Arranged.2022, aes(x = Monitor, y = Concentration, fill = Monitor)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Lindon" = "brown3", "Spanish Fork" = "cornflowerblue")) +
  labs(title = "Ozone Concentration by Location for 2022",
       x = "Location",
       y = "Ozone Concentration (ppb)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        legend.position = "none")
dev.off()

#2023 Box plot by monitor
png(".//Graphics//Ozone//Ozone_Concentration_2023.png", width=6.5, height=7, units="in", res=300)
ggplot(UDAQ.Ozone.Arranged.2023, aes(x = Monitor, y = Concentration, fill = Monitor)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Lindon" = "brown3", "Spanish Fork" = "cornflowerblue")) +
  labs(title = "Ozone Concentration by Location for 2023",
       x = "Location",
       y = "Ozone Concentration (ppb)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        legend.position = "none") 
dev.off()

#plot UDAQ data corresponding to our ozone data collected timeline House 1 to 9
png(".//Graphics//Ozone//Ozone.UDAQ.Hourly.1to9.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(UDAQ.Ozone.Arranged, House.number.int < 10)) + 
  geom_line(aes(x = duration.hours, y = Concentration, color=Monitor))+
  theme_bw()+
  labs(x = expression(paste("Hour of the day")),
       y = expression(paste("Ozone Concentrations (ppb)")))+
  scale_color_brewer(palette = 'Set1')+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

#plot UDAQ data corresponding to our ozone data collected timeline House 10 to 19
png(".//Graphics//Ozone//Ozone.UDAQ.Hourly.10to19.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(UDAQ.Ozone.Arranged, House.number.int > 9 & House.number.int < 20)) + 
  geom_line(aes(x = duration.hours, y = Concentration, color=Monitor))+
  theme_bw()+
  labs(x = expression(paste("Hour of the day")),
       y = expression(paste("Ozone Concentrations (ppb)")))+
  scale_color_brewer(palette = 'Set1')+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

#plot UDAQ data corresponding to our ozone data collected timeline House from 20
png(".//Graphics//Ozone//Ozone.UDAQ.Hourly.20.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(UDAQ.Ozone.Arranged, House.number.int > 19)) + 
  geom_line(aes(x = duration.hours, y = Concentration, color=Monitor))+
  theme_bw()+
  labs(x = expression(paste("Hour of the day")),
       y = expression(paste("Ozone Concentrations (ppb)")))+
  scale_color_brewer(palette = 'Set1')+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()


#plot Lindon UDAQ ozone vs Spanish Fork UDAQ ozone House 1 to 9
png(".//Graphics//Ozone//UDAQ.Monitor.Comparison.1to9.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(monitor_data, House.number.int < 10)) + 
  geom_point(aes(x = `Spanish Fork`, y = `Lindon`),size=.5) +  
  geom_smooth(method = "lm", aes(x = `Spanish Fork`, y = `Lindon`), color = "cornflowerblue", se = FALSE) + 
  labs(x = "Ozone concentration in Spanish Fork (ppb)",
       y = "Ozone Concentration in Lindon (ppb)") +
  scale_color_brewer(palette = 'Set1') +
  facet_wrap(.~first.day+house.number.visit, scales='free_y', ncol=4) +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size=9),plot.title = element_text(size=9),
        legend.title = element_text(size=9),legend.text = element_text(size=9),strip.text = element_text(size=9),
        plot.margin = margin(t=10, r=5, b=0, l=0))
dev.off()

#plot Lindon UDAQ ozone vs Spanish Fork UDAQ ozone House 10 to 19
png(".//Graphics//Ozone//UDAQ.Monitor.Comparison.10to19.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(monitor_data, House.number.int > 9 & House.number.int < 20)) + 
  geom_point(aes(x = `Spanish Fork`, y = `Lindon`),size=.5) +  
  geom_smooth(method = "lm", aes(x = `Spanish Fork`, y = `Lindon`), color = "cornflowerblue", se = FALSE) + 
  labs(x = "Ozone concentration in Spanish Fork (ppb)",
       y = "Ozone Concentration in Lindon (ppb)") +
  scale_color_brewer(palette = 'Set1') +
  facet_wrap(.~first.day+house.number.visit, scales='free_y', ncol=4) +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size=9),plot.title = element_text(size=9),
        legend.title = element_text(size=9),legend.text = element_text(size=9),strip.text = element_text(size=9),
        plot.margin = margin(t=10, r=5, b=0, l=0))
dev.off()

#plot Lindon UDAQ ozone vs Spanish Fork UDAQ ozone from House 20
png(".//Graphics//Ozone//UDAQ.Monitor.Comparison.20.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(monitor_data, House.number.int > 19)) + 
  geom_point(aes(x = `Spanish Fork`, y = `Lindon`),size=.5) +  
  geom_smooth(method = "lm", aes(x = `Spanish Fork`, y = `Lindon`), color = "cornflowerblue", se = FALSE) + 
  labs(x = "Ozone concentration in Spanish Fork (ppb)",
       y = "Ozone Concentration in Lindon (ppb)") +
  scale_color_brewer(palette = 'Set1') +
  facet_wrap(.~first.day+house.number.visit, scales='free_y', ncol=4) +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size=9),plot.title = element_text(size=9),
        legend.title = element_text(size=9),legend.text = element_text(size=9),strip.text = element_text(size=9),
        plot.margin = margin(t=10, r=5, b=0, l=0))
dev.off()

