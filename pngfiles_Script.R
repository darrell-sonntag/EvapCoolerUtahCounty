sidepakfiles.all.graphs <- sidepakfiles.all.same.time %>%
  mutate(house.number.visit = ifelse(house.number.visit =='H15 V1','H09 V3',house.number.visit)) %>% ### Change H15 V2 to H09 V4 
  mutate(house.number.visit = ifelse(house.number.visit =='H15 V2','H09 V4',house.number.visit)) %>% ### Change H15 V2 to H09 V4
  mutate(House.number.int = ifelse(House.number.int == 15, 9,House.number.int))

sidepak.stats.graph <- sidepak.stats %>%
  mutate(ac.type = factor(ac.type,levels=c('Central','Evap'),ordered=T)) 

### Evaluate repeat samples
sidepak.stats.graph.repeat <- sidepak.stats %>%
  mutate(measurement = ifelse(season == 'Summer' & 
                                House.Number %in% c('H03','H08','H14','H16','H19','H26'),
                              'repeat','single'))

sidepak.stats.graph.no.fire <- sidepak.stats.summer.no.fire %>%
  mutate(ac.type = factor(ac.type,levels=c('Central','Evap'),ordered=T)) 

sidepak.stats.long.con.no.fire <- sidepak.stats.long.con %>%
  filter(season =='Summer') %>%
  filter(day.type =='Normal')

sidepak.stats.long.reg.no.fire <- sidepak.stats.long.reg %>%
  filter(season =='Summer') %>%
  filter(day.type =='Normal')

### Supplement Figure S-2
png(".//Graphics//Supplement.Figure.S-2.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(sidepakfiles.all.graphs, House.number.int < 10)) + 
  geom_line(aes(x = duration.hours, y = Aerosol, color=Location))+
  theme_bw()+
  labs(x = expression(paste("Hour of the day")),
       y = expression(paste("SidePak PM"[2.5]," concentrations (ug/m"^3~")")))+
  scale_color_brewer(palette = 'Set1')+
  facet_wrap(.~house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

### Supplement Figure S-3
png(".//Graphics//Supplement.Figure.S-3.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(sidepakfiles.all.graphs,House.number.int > 9 & House.number.int < 20)) + 
  geom_line(aes(x = duration.hours, y = Aerosol, color=Location))+
  theme_bw()+
  labs(x = expression(paste("duration.hours")),
       y = expression(paste("SidePak PM"[2.5]," concentrations (ug/m"^3~")")))+
  scale_color_brewer(palette = 'Set1')+
  facet_wrap(.~house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

### Supplement Figure S-4
png(".//Graphics//Supplement.Figure.S-4.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(sidepakfiles.all.graphs, House.number.int > 19)) + 
  geom_line(aes(x = duration.hours, y = Aerosol, color=Location))+
  theme_bw()+
  labs(x = expression(paste("duration.hours")),
       y = expression(paste("SidePak PM"[2.5]," concentrations (ug/m"^3~")")))+
  scale_color_brewer(palette = 'Set1')+
  facet_wrap(.~house.number.visit, scales='free_y',ncol=4) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

### Supplement Figure S-5
#plot outdoor UDAQ pm2.5 vs outdoor SidePak pm2.5 (all  homes)
png(".//Graphics//Supplement.Figure.S-5.png", width=3.6, height=3, units="in", res=300)
ggplot(data=sidepak.epa.out,aes(x=PM2.5.UDAQ.ug.m3, y=SidePak.ug.m3.avg))+
  geom_point(aes(color=season),size = .5)+
  labs(x = expression(paste("UDAQ PM"[2.5]," concentration, ","ug/m"^3)),
       y = expression(paste("SidePak concentration, ","ug/m"^3)))+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=3)+
  theme_bw()+
  scale_color_brewer(palette='Set1')+
  theme(legend.position = 'bottom', 
        legend.margin=margin(-10,0,0,0),         # Adjust this to decrease space above the legend
        plot.margin = margin(5,5,5,5))+          # Adjust this to decrease space below x-axis title
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9))
dev.off()

### Supplement Figure S-6
#plot outdoor UDAQ pm2.5 vs outdoor SidePak pm2.5 (during summer)
png(".//Graphics//Supplement.Figure.S-6.png", width=3.6, height=3, units="in", res=300)
ggplot(data=sidepak.epa.summer,aes(x=PM2.5.UDAQ.ug.m3, y=SidePak.ug.m3.avg))+
  geom_point(aes(color=Monitor.closest),size = 1)+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(x = expression(paste("UDAQ PM"[2.5]," concentration, ","ug/m"^3)),
       y = expression(paste("SidePak concentration, ","ug/m"^3)), title='Summer')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")), size=3)+
  theme_bw()+
  scale_color_brewer(palette='Set1')+
  theme(legend.position = 'bottom', 
        legend.margin=margin(-10,0,0,0),         # Adjust this to decrease space above the legend
        plot.margin = margin(5,5,5,5))+          # Adjust this to decrease space below x-axis title
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9))
dev.off()


### Supplement Figure S-7
#plot outdoor UDAQ pm2.5 vs outdoor SidePak pm2.5 (winter)
png(".//Graphics//Supplement.Figure.S-7.png", width=3.6, height=3, units="in", res=300)
ggplot(data=sidepak.epa.winter,aes(x=PM2.5.UDAQ.ug.m3, y=SidePak.ug.m3.avg))+
  geom_point(aes(color=Monitor.closest))+
  geom_abline(aes(intercept = 0, slope = 1))+
  labs(x = expression(paste("UDAQ PM"[2.5]," concentration, ","ug/m"^3)),
       y = expression(paste("SidePak concentration, ","ug/m"^3)), title='Winter')+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")), size=3)+
  theme_bw()+
  scale_color_brewer(palette='Set1')+
  theme(legend.position = 'bottom', 
        legend.margin=margin(-10,0,0,0),         # Adjust this to decrease space above the legend
        plot.margin = margin(5,5,5,5))+          # Adjust this to decrease space below x-axis title
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9))
dev.off()

## Supplement Figure S-8
png(".//Graphics//Supplement.Figure.S-8.png", width=6, height=7.5, units="in", res=300)
ggplot(data = sidepak.stats.graph.repeat ,  aes(y = house.number.visit, x = `I/O`,fill=measurement))+  
  geom_col(aes(),width=0.7)+
  facet_grid(season+ac.type ~.,scales='free_y' ,space='free') + 
  scale_fill_brewer(palette = 'Paired')+
  theme(axis.text = element_text(size = 12)) +
  labs(x = "I/O", title='',y='')+
  scale_x_continuous(breaks = seq(0,1.6,.2))+
  theme_bw()+
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position = 'bottom')+
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=8),
        axis.title = element_text(size = 12),plot.title = element_text(size = 18),
        strip.text = element_text(size=10),
        legend.text = element_text(size = 14))  
dev.off()

### Supplement Figure S-9
png(".//Graphics//Supplement.Figure.S-9.png", width=4, height=6, units="in", res=300)
ggplot(sidepak.stats.long.con.no.fire,aes(x=ac.type,y=value,color=ac.type)) +
  geom_boxplot()+
  geom_signif(comparisons=list(c('Central','Evap')),
              map_signif_level=FALSE,test='t.test',color='black',vjust=1.5,margin_top=0.1)+
  scale_color_manual(values = own.colors,drop=F)+
  facet_grid(statistic~season,scales='free_y') +
  labs(y = "", title='',x='')+
  theme_bw()+
  expand_limits(y = 0)+
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 18),
        strip.text = element_text(size=10),
        legend.text = element_text(size = 14),legend.position="none")  
dev.off()

### Supplement Figure S-10
png(".//Graphics//Supplement.Figure.S-10.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(sidepak.correlation.qa, House.number.int < 10),aes(x = Out, y = In)) + 
  geom_point(color='grey', size = .5)+
  theme_bw()+
  labs(x = expression(paste("Outdoor SidePak PM"[2.5]," Concentrations (ug/m"^3~")")),
       y = expression(paste("Indoor SidePak PM"[2.5]," Concentrations (ug/m"^3~")")))+
  facet_wrap(.~house.number.visit, scales='free',ncol=4) +
  expand_limits(y=0,x=0)+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=2) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()


### Supplement Figure S-11
png(".//Graphics//Supplement.Figure.S-11.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(sidepak.correlation.qa, House.number.int > 9 & House.number.int < 20),aes(x = Out, y = In)) + 
  geom_point(color='grey', size = .5)+
  theme_bw()+
  labs(x = expression(paste("Outdoor SidePak PM"[2.5]," Concentrations (ug/m"^3~")")),
       y = expression(paste("Indoor SidePak PM"[2.5]," Concentrations (ug/m"^3~")")))+
  facet_wrap(.~house.number.visit, scales='free',ncol=4) +
  expand_limits(y=0,x=0)+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=2) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

### Supplement Figure S-12
png(".//Graphics//Supplement.Figure.S-12.png", width=6.5, height=7, units="in", res=300)
ggplot(data = filter(sidepak.correlation.qa, House.number.int > 19),aes(x = Out, y = In)) + 
  geom_point(color='grey', size = .5)+
  theme_bw()+
  labs(x = expression(paste("Outdoor SidePak PM"[2.5]," Concentrations (ug/m"^3~")")),
       y = expression(paste("Indoor SidePak PM"[2.5]," Concentrations (ug/m"^3~")")))+
  facet_wrap(.~house.number.visit, scales='free',ncol=4) +
  expand_limits(y=0,x=0)+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),size=2) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=10,r=5,b=0,l=0))
dev.off()

## Supplement Figure S-13
png(".//Graphics//Supplement.Figure.S-13.png", width=4, height=6, units="in", res=300)
ggplot(sidepak.stats.long.reg.no.fire,aes(x=ac.type,y=value,color=ac.type)) +
  geom_boxplot()+
  geom_signif(comparisons=list(c('Central','Evap')),
              map_signif_level=FALSE,test='t.test',color='black',vjust=1.5,margin_top=0.1)+
  scale_color_manual(values = own.colors,drop=F)+
  facet_grid(statistic~season,scales='free_y') +
  labs(y = "", title='',x='')+
  theme_bw()+
  expand_limits(y = 0)+
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 18),
        strip.text = element_text(size=10),
        legend.text = element_text(size = 14),legend.position="none")  
dev.off()

## Supplement Figure S-14
png(".//Graphics//Supplement.Figure.S-14.png", width=4.5, height=4.5, units="in", res=300)
ggplot(data=sidepak.stats,aes(x=PM2.5.UDAQ.ug.m3, y=Cs))+
  geom_point(aes(color=day.type))+
  geom_smooth(color = "black",method='loess',span=2) +
  labs(x = expression(paste("UDAQ PM2.5 Concentration, ","ug/m"^3)))+
  theme_bw()+
  facet_grid(season~ac.type) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 10),plot.title = element_text(size = 20),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        strip.text = element_text(size=10))
dev.off()

## Supplement Figure S-15
png(".//Graphics//Supplement.Figure.S-15.png", width=6, height=6, units="in", res=300)
ggplot(data = sidepak.stats.graph.repeat,  aes(y = house.number.visit, x = Fin,fill=measurement))+  
  geom_col(aes(),width=0.7)+
  facet_grid(season+ac.type ~.,scales='free_y' ,space='free') +
  scale_fill_brewer(palette = 'Paired')+
  theme(axis.text = element_text(size = 12)) +
  labs(x = "Fin", title='',y='')+
  theme_bw()+
  guides(fill=guide_legend(reverse=TRUE))
theme(legend.position = 'bottom')+
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 12),
        strip.text = element_text(size=10),
        legend.text = element_text(size = 12))  
dev.off()

## New Figure of Fin (with uncertainty bars)

sidepak.stats$house.number.visit.date <- factor(sidepak.stats$house.number.visit.date,levels=rev(sort(unique(sidepak.stats$house.number.visit.date))),ordered=T)

own.colors.2 <- brewer.pal(n = 8, name = "Paired")[c(3:8)]
own.colors.2

### Supplement Figure S-16
png(".//Graphics//Supplement.Figure.S-16.png", width=6, height=7, units="in", res=300)
ggplot(data = sidepak.stats,  aes(y = house.number.visit.date, x = Fin,fill=day.type))+  
  geom_col(aes(),width=0.7)+
  geom_errorbar(aes(xmin =Fin.lower, xmax = Fin.upper),width=0.5) +
  facet_grid(season+ac.type ~.,scales='free_y' ,space='free') +
  labs(x = "Fin", title='',y='')+
  scale_x_continuous(breaks = seq(-0.2,2.2,0.2))+
  theme_bw()+
  labs(fill='')+ 
  theme(legend.position = 'bottom')+
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=9),
        axis.title = element_text(size = 12),plot.title = element_text(size = 12),
        strip.text = element_text(size=10),
        legend.text = element_text(size = 12))  
dev.off()

### Is there a relationship with temperature (people are using AC more?)
## no discernible trend with temperature
## Supplement Figure S-17
png(".//Graphics//Supplement.Figure.S-17.png", width=4.5, height=4.5, units="in", res=300)
ggplot(data=sidepak.stats,aes(x=average.temperature.Celsius_Out, y=Fin))+
  geom_point(aes(color=day.type))+
  geom_smooth(color = "black",method='loess',span=2) +
  labs(x = "Outdoor Temperature, C ")+
  theme_bw()+
  facet_grid(season~ac.type) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 10),plot.title = element_text(size = 20),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        strip.text = element_text(size=10))
dev.off()


### relationship between Fin and outdoor concentration 
## Supplement Figure S-18
png(".//Graphics//Supplement.Figure.S-18.png", width=4.5, height=4.5, units="in", res=300)
ggplot(data=sidepak.stats,aes(x=PM2.5.UDAQ.ug.m3, y=Fin))+
  geom_point(aes(color=day.type))+
  geom_smooth(color = "black",method='loess',span=2) +
  labs(x = expression(paste("UDAQ PM2.5 Concentration, ","ug/m"^3)))+
  theme_bw()+
  facet_grid(season~ac.type) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 10),plot.title = element_text(size = 20),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        strip.text = element_text(size=10))
dev.off()

### Supplement Figure S-19
png(".//Graphics//Supplement.Figure.S-19.png", width=6, height=3.5, units="in", res=300)
ggplot(data=sidepak.stats.graph.no.fire,aes(x=SidePak.ug.m3.avg_Out, y=SidePak.ug.m3.avg_In))+
  geom_point(aes(color=day.type))+
  
  labs(x = expression(paste("Outdoor SidePak, ","ug/m"^3)),
       y = expression(paste("Indoor SidePak, ","ug/m"^3)), title='Summer')+
  theme_bw()+
  coord_cartesian(ylim=c(0,14))+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),parse=TRUE,
               label.y = 1,label.x=0.95,size=3)+
  facet_grid(.~ac.type) +
  theme(legend.position = 'none')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_blank(),legend.text = element_text(size = 9),
        strip.text = element_text(size=9))
dev.off()


## graph with date instead
## Figure 1
sidepak.complete$house.number.visit <- factor(sidepak.complete$house.number.visit,levels=rev(sort(unique(sidepak.complete$house.number.visit))),ordered=T)
sidepak.complete$house.number.visit.date <- factor(sidepak.complete$house.number.visit.date,levels=rev(sort(unique(sidepak.complete$house.number.visit.date))),ordered=T)
sidepak.complete$Location <- factor(sidepak.complete$Location,levels=c("Out","In"),ordered=T)

### sustainability Figure 2
png(".//Graphics//Sustainability.Figure.2.png", width=10, height=12, units="in", res=300)
ggplot(data = sidepak.complete,  aes(y = house.number.visit.date, x = SidePak.ug.m3.avg,fill=Location))+  
  geom_col(aes(),position=position_dodge2(preserve='single'),width=0.7)+
  geom_errorbar(position=position_dodge2(preserve='single'),width =0.7,aes(xmin = Sidepak.tenth, xmax = Sidepak.ninetieth))+ 
  facet_grid(season+ac.type~.,scales='free_y' ,space='free') +
  scale_fill_brewer(palette = 'Set1')+
  labs(x = expression(paste("Average SidePak PM"[2.5]," concentrations ","ug/m"^3)),title='',y='')+
  coord_cartesian(xlim = c(0, 110)) +
  scale_x_continuous(breaks=seq(0,110,10))+
  theme_bw()+
  guides(fill=guide_legend(reverse=TRUE))+
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=16),
        axis.title = element_text(size = 18),plot.title = element_text(size = 20),
        legend.title = element_text(size = 18),legend.text = element_text(size = 14),strip.text = element_text(size=14))
dev.off()

### sustainability Figure 3
ann_text_3 <- data.frame(lab = "X = Mean", statistic='Outdoor UDAQ',season='Summer')
pvalue_text_3 <- data.frame(lab = "p-value =",statistic='Outdoor UDAQ',season='Summer')

ann_text_3$statistic <- factor(ann_text_3$statistic, levels = levels(sidepak.stats.long.con$statistic),ordered = TRUE)
pvalue_text_3$statistic <- factor(pvalue_text_3$statistic, levels = levels(sidepak.stats.long.con$statistic),ordered = TRUE)

### sustainability Figure 3
png(".//Graphics//Sustainability.Figure.3.png", width=6, height=6, units="in", res=300)
ggplot(sidepak.stats.long.con,aes(x=ac.type,y=value,color=ac.type)) +
  geom_boxplot()+
  geom_signif(comparisons=list(c('Central','Evap')),
              map_signif_level=FALSE,test='t.test',color='black',vjust=1.5,margin_top=0.1)+
  scale_color_manual(values = own.colors,drop=F)+
  geom_point(data=sidepak.stat.long.means.con,aes(x=ac.type,y=mean),color='black',shape=4,size=3,stroke=1)+
  facet_grid(statistic~season,scales='free_y',switch = 'y') +
  labs(y = "", title='',x='')+
  theme_bw()+
  expand_limits(y = 0)+
  geom_text(data = ann_text_3,label = ann_text_3$lab,color='black',
            mapping=aes(x=Inf,y=Inf,label=lab),hjust=2.3,vjust=9.7)+
  geom_text(data = pvalue_text_3,label = pvalue_text_3$lab,color='black',
            mapping=aes(x=Inf,y=Inf,label=lab),hjust=3.3,vjust=1.9)+
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 18),
        strip.text = element_text(size=10),
        legend.text = element_text(size = 14),legend.position="none",
        strip.placement='outside')  
dev.off()
 
## plot individual days for poster

sidepakfiles.all$Location <- factor(sidepakfiles.all$Location,levels=c("Out","In"),ordered=T)

sidepakfiles.i <- filter(sidepakfiles.all.same.time,House.Number =='H23' & Visit == 'V1') 

### sustainability Figure 4-1
png(".//Graphics//Sustainability.Figure.4-1.png", width=6.5, height=3.5, units="in", res=300)
ggplot(data = sidepakfiles.i) + 
  geom_line(aes(x = date.time, y = Aerosol, color=Location))+
  scale_color_brewer(palette = 'Set1')+
  theme_bw()+
  labs(x = '',
       y = expression(paste("SidePak PM"[2.5]," ug/m"^3)),title='(b)',x='')+
  facet_grid(.~`Type of Air Conditioner`+house.number.visit, scales='free_y') +
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(size = 9, hjust = 0.5)) +
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=0,r=5,b=0,l=0))

dev.off()

### individual correlation plots

sidepak.correlation.i <- filter(sidepak.correlation,House.Number =='H23' & Visit == 'V1') 

  
### sustainability Figure 5-1
png(".//Graphics//Sustainability.Figure.5-1.png", width=3.5, height=3, units="in", res=300)
ggplot(data = sidepak.correlation.i,aes(x = Out, y = In)) + 
  geom_point(color='grey',size = .5)+
  theme_bw()+
  labs(y = expression(paste("Indoor SidePak PM"[2.5]," ug/m"^3, "(C"["in"],")")),
       x = expression(paste("Outdoor SidePak PM"[2.5]," ug/m"^3,"(C"["out"],")")), caption ='(b)')+
  stat_poly_line() +
  annotate("text", x = Inf, y = Inf,  parse = T, label = as.character(expression(paste("C"["in"], "=" -0.692 + 1.03 * " C"["out"],))), 
           hjust = 2.35, vjust = 1.1, size = 3) +
  stat_poly_eq(aes(label = paste(after_stat(rr.label), sep = "*\", \"*")),parse=TRUE,
               label.y = .92,label.x=.05,size=3)+
  facet_grid(.~`Type of Air Conditioner`+house.number.visit, scales='free_y') +
  theme(legend.position = 'bottom')+
  theme(plot.caption = element_text(size = 9, hjust = 0.5)) +
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=0,r=5,b=0,l=0))
dev.off()

## plot individual days for poster

sidepakfiles.all$Location <- factor(sidepakfiles.all$Location,levels=c("Out","In"),ordered=T)

sidepakfiles.i <- filter(sidepakfiles.all.same.time,House.Number =='H16' & Visit == 'V2') 

### sustainability Figure 4-2
png(".//Graphics//Sustainability.Figure.4-2.png", width=6.5, height=3.5, units="in", res=300)
ggplot(data = sidepakfiles.i) + 
  geom_line(aes(x = date.time, y = Aerosol, color=Location))+
  scale_color_brewer(palette = 'Set1')+
  theme_bw()+
  labs(x = '',
       y = expression(paste("SidePak PM"[2.5]," ug/m"^3)),title='(a)',x='')+
  facet_grid(.~`Type of Air Conditioner`+house.number.visit, scales='free_y') +
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(size = 9, hjust = 0.5)) +
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=0,r=5,b=0,l=0))
dev.off()

### individual correlation plots

sidepak.correlation.i <- filter(sidepak.correlation,House.Number =='H16' & Visit == 'V2') 

### sustainability Figure 5-2
png(".//Graphics//Sustainability.Figure.5-2.png", width=3.5, height=3, units="in", res=300)
ggplot(data = sidepak.correlation.i,aes(x = Out, y = In)) + 
  geom_point(color='grey', size = .5)+
  theme_bw()+
  labs(y = expression(paste("Indoor SidePak PM"[2.5]," ug/m"^3, "(C"["in"],")")),
       x = expression(paste("Outdoor SidePak PM"[2.5]," ug/m"^3,"(C"["out"],")")), caption ='(a)')+
  stat_poly_line() +
  annotate("text", x = Inf, y = Inf,  parse = T, label = as.character(expression(paste("C"["in"], "=" -16.4 + 0.539 * " C"["out"],))), 
           hjust = 2.35, vjust = 1.1, size = 3) +
  stat_poly_eq(aes(label = paste(after_stat(rr.label), sep = "*\", \"*")),parse=TRUE,
               label.y = .92,label.x=.05,size=3)+
  facet_grid(.~`Type of Air Conditioner`+house.number.visit, scales='free_y') +
  theme(legend.position = 'bottom')+
  theme(plot.caption = element_text(size = 9, hjust = 0.5)) +
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_text(size = 9),legend.text = element_text(size = 9),strip.text = element_text(size=9),
        plot.margin= margin(t=0,r=5,b=0,l=0))
dev.off()

### sustainability Figure 6

ann_text <- data.frame(lab = "X = Mean", statistic='Cs',season='Summer')

pvalue_text <- data.frame(lab = "p-value =",statistic='Cs',season='Summer')

png(".//Graphics//Sustainability.Figure.6.png", width=6, height=6, units="in", res=300)
ggplot(sidepak.stats.long.reg,aes(x=ac.type,y=value,color=ac.type)) +
  geom_boxplot()+
  geom_signif(comparisons=list(c('Central','Evap')),
              map_signif_level=FALSE,test='t.test',
              color='black',vjust=1.5,margin_top=0.1)+
  scale_color_manual(values = own.colors,drop=F)+
  geom_point(data=sidepak.stat.long.means.reg,aes(x=ac.type,y=mean),color='black',shape=4,size=3,stroke=1)+
  facet_grid(statistic~season,scales='free_y',switch='y',
             labeller = as_labeller(c(Summer = 'Summer', Winter='Winter',Cs = "Cs (ug/m3)",Fin="Fin (ug/m3)",R2 = "R2"))) +
  labs(y = "", title='',x='')+
  theme_bw()+
  expand_limits(y = 0)+
  geom_text(data = ann_text,label = ann_text$lab,color='black',
            mapping=aes(x=Inf,y=Inf,label=lab),hjust=2.3,vjust=13)+
  geom_text(data = pvalue_text,label = pvalue_text$lab,color='black',
            mapping=aes(x=Inf,y=Inf,label=lab),hjust=3.3,vjust=2)+
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 18),
        strip.text = element_text(size=10),
        legend.text = element_text(size = 14),legend.position="none",
        strip.placement='outside')
dev.off()

### sustainability Figure 7
ann_text_Figure_7 <- data.frame(lab = c("a","b","c","d"), 
                                season = c("Summer","Summer","Winter","Winter"),
                                ac.type = c("Central","Evap","Central","Evap"))

# Create the labels with subscripted text
labels <- c(expression(paste("C"['in'], " = 3.02 + 0.239 * C"['out'],", ")),
            expression(paste("C"['in'], " = -1.32 + 0.964 * C"['out'],", ")),
            expression(paste("C"['in'], " = 3.21 + 0.0575 * C"['out'],", ")),
            expression(paste("C"['in'], " = 2.41 + 0.164 * C"['out'],", ")))

# Add the labels to the plot
for (i in 1:length(labels)) {
  ann_text_Figure_7$lab[i] <- labels[i]
}

png(".//Graphics//Sustainability.Figure.7.png", width=6, height=4, units="in", res=300)
ggplot(data=sidepak.stats,aes(x=SidePak.ug.m3.avg_Out, y=SidePak.ug.m3.avg_In))+
  geom_point(aes(color=day.type), size = 1.5)+
  labs(x = expression(paste("Outdoor SidePak PM"[2.5]," ug/m"^3,"(C"["out"],")")),
       y = expression(paste("Indoor SidePak PM"[2.5]," ug/m"^3,"(C"["in"],")")))+
  theme_bw()+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(rr.label), sep = "*\", \"*")),parse=TRUE,
               label.y = 1,label.x=.95,size=3)+
  geom_text(data=ann_text_Figure_7, label = ann_text_Figure_7$lab,color='black',
            mapping=aes(x=Inf,y=Inf, label=lab),hjust=1.5,vjust=1.35,size=3)+
  facet_grid(season~ac.type) +
  theme(legend.position = 'bottom')+
  theme(axis.text.y = element_text(size=9),axis.text.x = element_text(size=9),
        axis.title = element_text(size = 9),plot.title = element_text(size = 9),
        legend.title = element_blank(),legend.text = element_text(size = 9),
        strip.text = element_text(size=9))
dev.off()

