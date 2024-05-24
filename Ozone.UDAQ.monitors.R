setwd("C:\\Users\\cello\\OneDrive\\문서\\Github\\EvapCoolerUtahCounty")

Summer.All.Hourly <- inner_join(Summer2022.23Hourly,visitmaster.list.2,
                              join_by(between(Date,start_date,end_date))) 
                                                                                         
Winter.All.Hourly <- inner_join(Winter2022Hourly,visitmaster.list.2,
                              join_by(between(Date,start_date,end_date)))

Ozone.All.Hourly <- bind_rows(Summer.All.Hourly, Winter.All.Hourly) %>%
  filter(Parameter == "OZONE") %>%
  mutate(House.number.int = as.integer(substring(House.Number, 2,3))) %>%
  mutate(Date = as_datetime(Date)) %>% 
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

UDAQ.Ozone.daily <- UDAQ.Ozone.Arranged %>%
  group_by(first.day,Monitor,Year) %>%
  summarise(Ozone.avg = mean(Concentration))

UDAQ.Ozone.Arranged.2022 <- UDAQ.Ozone.Arranged %>%
  filter(Year == 2022)

UDAQ.Ozone.Arranged.2023 <- UDAQ.Ozone.Arranged %>%
  filter(Year == 2023)

monitor_data <- UDAQ.Ozone.Arranged %>%
  filter(Monitor %in% c("Spanish Fork", "Lindon")) %>%
  pivot_wider(names_from = Monitor, values_from = Concentration) %>%
  drop_na()

#Ozone Daily Average Box plot by monitor

own.colors.2 <- brewer.pal(n = 9, name = "Set1")[c(3:9)]

png(".//Graphics//Ozone//Ozone_Concentration.daily.png", width=6.5, height=7, units="in", res=300)
ggplot(UDAQ.Ozone.daily, aes(x = Monitor, y = Ozone.avg, color = Monitor)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width=0.05,shape=1)+
  facet_wrap(~factor(Year))+
  scale_color_manual(name = 'Measurement', values = own.colors.2)+
  #scale_fill_manual(values = c("Lindon" = "brown3", "Spanish Fork" = "cornflowerblue")) +
  labs(  x = "Year",
       y = "Daily Ozone Average Concentration (ppb)") +
  theme_bw()+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        plot.margin= margin(t=1,r=1,b=1,l=1),legend.position= 'bottom')
dev.off()

#2022 Box plot by monitor
png(".//Graphics//Ozone//Ozone_Concentration_2022.png", width=6.5, height=7, units="in", res=300)
ggplot(UDAQ.Ozone.Arranged.2022, aes(x = Monitor, y = Concentration, fill = Monitor)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Lindon" = "brown3", "Spanish Fork" = "cornflowerblue")) +
  labs(title = "Ozone Concentration by Location for 2023",
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

Ozone_daily_wider <- UDAQ.Ozone.daily %>%
  filter(Monitor %in% c("Spanish Fork", "Lindon")) %>%
  pivot_wider(names_from = Monitor, values_from = Ozone.avg) %>%
  drop_na()

#scatter plot UDAQ data by year
png(".//Graphics//Ozone//Ozone.UDAQ.yearly.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(Ozone_daily_wider)) + 
  geom_point(aes(x = `Spanish Fork`, y = `Lindon`),size=1) +  
  geom_smooth(method = "lm", aes(x = `Spanish Fork`, y = `Lindon`), color = "cornflowerblue", se = FALSE) + 
  theme_bw()+
  labs(x = "Ozone Average concentration in Spanish Fork (ppb)",
       y = "Ozone Average Concentration in Lindon (ppb)") +
  scale_color_brewer(palette = 'Set1')+
  facet_wrap(.~Year, scales='free_y',ncol=2) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()


own.colors.4 <- brewer.pal(n = 9, name = "Dark2")[c(2:3)]

png(".//Graphics//Ozone//Ozone.UDAQ.daily.scatter.png", width=6, height=5.5, units="in", res=300)
ggplot(data = Ozone_daily_wider, aes(x = `Spanish Fork`, y = `Lindon`)) + 
  geom_point(aes(color=factor(Year)))+ 
  geom_smooth(method = "lm",  se = FALSE) + 
  geom_abline(aes(intercept = 0, slope = 1))+
  theme_bw()+
  labs(x = "Daily ozone average concentration in Spanish Fork (ppb)",
       y = "Daily ozone average concentration in Lindon (ppb)") +
  scale_color_manual(name = 'Year', values = own.colors.4)+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=6)+
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),
        axis.title = element_text(size = 14),plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),
        plot.margin= margin(t=1,r=1,b=1,l=1))
dev.off()

#plot UDAQ data corresponding to our ozone data collected timeline House 1 to 10

own.colors.2 <- brewer.pal(n = 9, name = "Set1")[c(3:9)]

png(".//Graphics//Ozone//Ozone.UDAQ.Hourly.1to10.png", width=6.5, height=7, units="in", res=600)
ggplot(data = filter(UDAQ.Ozone.Arranged, House.number.int < 11)) + 
  geom_line(aes(x = duration.hours, y = Concentration, color=Monitor),size=1)+
  scale_color_manual(name = 'Monitor', values = own.colors.2)+
  theme_bw()+
  labs(x = expression(paste("Hour")),
       y = expression(paste("Ozone Concentration (ppb)")))+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  scale_x_continuous(breaks=seq(0,40,8))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 11),
        legend.title = element_text(size = 12),legend.text = element_text(size = 11),strip.text = element_text(size=11),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

#plot UDAQ data corresponding to our ozone data collected timeline House 11 to 19
png(".//Graphics//Ozone//Ozone.UDAQ.Hourly.11to19.png", width=6.5, height=7, units="in", res=600)
ggplot(data = filter(UDAQ.Ozone.Arranged, House.number.int > 10 & House.number.int < 20)) + 
  geom_line(aes(x = duration.hours, y = Concentration, color=Monitor),size=1)+
  theme_bw()+
  scale_color_manual(name = 'Monitor', values = own.colors.2)+
  labs(x = expression(paste("Hour")),
       y = expression(paste("Ozone Concentration (ppb)")))+
  facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  scale_x_continuous(breaks=seq(0,40,8))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 11),
        legend.title = element_text(size = 12),legend.text = element_text(size = 11),strip.text = element_text(size=11),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

#plot UDAQ data corresponding to our ozone data collected timeline House from 20
png(".//Graphics//Ozone//Ozone.UDAQ.Hourly.20.png", width=6.5, height=7, units="in", res=600)
ggplot(data = filter(UDAQ.Ozone.Arranged, House.number.int > 19)) + 
  geom_line(aes(x = duration.hours, y = Concentration, color=Monitor),size=1)+
  theme_bw()+
  scale_color_manual(name = 'Monitor', values = own.colors.2)+
  labs(x = expression(paste("Hour of the day")),
       y = expression(paste("Ozone Concentration (ppb)")))+
    facet_wrap(.~first.day+house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  scale_x_continuous(breaks=seq(0,40,8))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 11),
        legend.title = element_text(size = 12),legend.text = element_text(size = 11),strip.text = element_text(size=11),
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

