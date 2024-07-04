
## Repeat PM2.5 Graph for Utah county alongside temperature (like the ozone)

library(tidyverse)
library(patchwork)
library(scales)


## Read in the 24-average PM2.5
## downloaded from here: 
## https://www.epa.gov/outdoor-air-quality-data/download-daily-data


epa.pm25.2021 <- read_csv(".\\Data\\EPA\\PM25_2021_data.csv") %>%
  rename('Site Name' = 'Local Site Name')

epa.pm25.2022 <- read_csv(".\\Data\\EPA\\PM25_2022_data.csv") %>%
                  rename('Site Name' = 'Local Site Name')

epa.pm25.2023 <- read_csv(".\\Data\\EPA\\PM25_2023_data.csv") %>%
                      rename('Site Name' = 'Local Site Name')

epa.pm25 <- bind_rows(epa.pm25.2021,epa.pm25.2022,epa.pm25.2023)

## Convert Dates to Dates

epa.pm25.date <- epa.pm25 %>%
                      mutate(Date = mdy(Date)) %>%
                      mutate(Year = lubridate::year(Date)) %>%
                      select("Year","Date","Source","Site ID","Site Name","POC",
                      "Daily Mean PM2.5 Concentration", "Units")


epa.pm25.lindon <- epa.pm25.date %>%
                         filter(`Site Name` == 'Lindon')

epa.pm25.sf <- epa.pm25.date%>%
                      filter(`Site Name` == 'Spanish Fork')


## grabed colors from https://document.airnow.gov/technical-assistance-document-for-the-reporting-of-daily-air-quailty.pdf
## found the corresponding hexidecmal code here: https://htmlcolorcodes.com/

pm1 <- ggplot(epa.pm25.lindon, aes(x=Date,y=`Daily Mean PM2.5 Concentration`))+
  annotate("rect",fill = "#00E400", xmin = as_date('2021-01-01'), xmax = as_date('2023-12-31'), ymin=0, ymax = 9,alpha=0.5)+
  annotate("rect",fill = "#FFFF00", xmin = as_date('2021-01-01'), xmax = as_date('2023-12-31'), ymin=9, ymax = 35.4,alpha=0.5)+
  annotate("rect",fill = "#FF7E00", xmin = as_date('2021-01-01'), xmax = as_date('2023-12-31'), ymin=35.4, ymax = 55.4,alpha=0.5)+
  annotate("rect",fill = "#FF0000", xmin = as_date('2021-01-01'), xmax = as_date('2023-12-31'), ymin=55.4, ymax = Inf,alpha=0.5)+
  annotate("text",label = "Good",  x=as_date('2022-07-01'),  y=2, hjust=0,size=3)+
  annotate("text",label = "Moderate",  x=as_date('2022-06-01'),  y=32, hjust=0,size=3 )+
  annotate("text",label = "Unhealthy for Sensitive Groups",  x=as_date('2022-07-01'),  y=45, hjust=0,size=3 )+
  annotate("text",label = "Unhealthy",  x=as_date('2022-06-01'),  y=57, hjust=0,size=3 )+
  geom_line(color="darkgrey",size=1) +
  theme_bw()+
  scale_y_continuous(breaks=seq(0,50,10),lim=c(0,57))+
  scale_x_date(date_breaks = "4 months",date_labels="%b-%Y",expand=expansion(0))+
  labs(x='', title = 'Lindon')+
  labs(y=expression(paste("PM"[2.5]," conc (ug/m"^3~")")))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 14,hjust = 0.5,vjust=0))



pm2 <- ggplot(epa.pm25.sf, aes(x=Date,y=`Daily Mean PM2.5 Concentration`))+
  annotate("rect",fill = "#00E400", xmin = as_date('2021-01-01'), xmax = as_date('2023-12-31'), ymin=0, ymax = 9,alpha=0.5)+
  annotate("rect",fill = "#FFFF00", xmin = as_date('2021-01-01'), xmax = as_date('2023-12-31'), ymin=9, ymax = 35.4,alpha=0.5)+
  annotate("rect",fill = "#FF7E00", xmin = as_date('2021-01-01'), xmax = as_date('2023-12-31'), ymin=35.4, ymax = 55.4,alpha=0.5)+
  annotate("rect",fill = "#FF0000", xmin = as_date('2021-01-01'), xmax = as_date('2023-12-31'), ymin=55.4, ymax = Inf,alpha=0.5)+
  annotate("text",label = "Good",  x=as_date('2022-07-01'),  y=2, hjust=0,size=3)+
  annotate("text",label = "Moderate",  x=as_date('2022-06-01'),  y=32, hjust=0,size=3 )+
  annotate("text",label = "Unhealthy for Sensitive Groups",  x=as_date('2022-07-01'),  y=45, hjust=0,size=3 )+
  annotate("text",label = "Unhealthy",  x=as_date('2022-06-01'),  y=57, hjust=0,size=3 )+
  geom_line(color="darkgrey",size=1) +
  theme_bw()+
  scale_y_continuous(breaks=seq(0,50,10),lim=c(0,57))+
  scale_x_date(date_breaks = "4 months",date_labels="%b-%Y",expand=expansion(0))+
  labs(x='',title = 'Spanish Fork')+
  labs(y=expression(paste("PM"[2.5]," conc (ug/m"^3~")")))+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 14,hjust = 0.5,vjust=0))

pm1+pm2+ plot_layout(ncol = 1)
ggsave(".//Graphics//SidePak//utah.county.temp.pm25.aqi.png", width=6.5, height=6, units="in", dpi=900)

?scale_x_date
