
## archived R code


## how does this differ from sample.summary.out
## I think it just has less columns

ozone.summary.out <- ozone.summary %>%
  filter(Location == 'Out') 


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
