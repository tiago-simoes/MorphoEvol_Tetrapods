library(ggplot2)
library(cowplot)
library(plotly)
library(ggrepel)
library(plyr)
library(lattice)
library(Rmisc)
library(deeptime)
library(reshape2)
library(FSA)
library(car)
library(tidyr)
library(rstatix)


######################### Mr Bayes RATE STATS #######################
### Median Rates by clade

CladeRate <- read.csv("RateTable_Medians_Clades.csv")

Sum_CladeRate1<-Summarize(rates1 ~ clade,
                          data=CladeRate,
                          digits=3)
Sum_CladeRate1

Sum_CladeRate2<-Summarize(rates2 ~ clade,
                          data=CladeRate,
                          digits=3)
Sum_CladeRate2

Sum_CladeRate3<-Summarize(rates3 ~ clade,
                          data=CladeRate,
                          digits=3)
Sum_CladeRate3

AllCladeRates <- cbind (Sum_CladeRate1, Sum_CladeRate2, Sum_CladeRate3)

write.csv(AllCladeRates, file="Sum_AllCladeRates_Medians.csv")

### Mean Rates by clade


CladeRate <- read.csv("RateTable_Means_Clades.csv")

Sum_CladeRate1<-Summarize(rates1 ~ clade,
                          data=CladeRate,
                          digits=3)
Sum_CladeRate1

Sum_CladeRate2<-Summarize(rates2 ~ clade,
                          data=CladeRate,
                          digits=3)
Sum_CladeRate2

Sum_CladeRate3<-Summarize(rates3 ~ clade,
                          data=CladeRate,
                          digits=3)
Sum_CladeRate3

AllCladeRates <- cbind (Sum_CladeRate1, Sum_CladeRate2, Sum_CladeRate3)

write.csv(AllCladeRates, file="Sum_AllCladeRates_Means.csv")


################ Rates by morpho clock: medians

#Melt DATA

RateByClockrMelted<-melt(RateByClock, measure.vars = c(2,3,4),
                         variable.name = "clock", value.name = "rate")

write.csv(RateByClockrMelted, file = "RateByClockrMelted_medians.csv")


# Summary stats   


Sum_RateByClock1<-Summarize(rates1,
                            data=RateByClock,
                            digits=3)
Sum_RateByClock1

Sum_RateByClock2<-Summarize(rates2,
                            data=RateByClock,
                            digits=3)
Sum_RateByClock2

Sum_RateByClock3<-Summarize(rates3,
                            data=RateByClock,
                            digits=3)
Sum_RateByClock3

AllRateByClocks <- cbind (Sum_RateByClock1, Sum_RateByClock2, Sum_RateByClock3)
AllRateByClocks <- as.data.frame(AllRateByClocks)
names(AllRateByClocks)

write.csv(AllRateByClocks, file="Sum_AllRateByClocks_medians.csv")

################ Rates by morpho clock: means

RateTable_Means_Clades<- read.csv("RateTable_Means_Clades.csv")
names(RateTable_Means_Clades)

#Melt DATA

RateByClockrMelted<-melt(RateTable_Means_Clades, measure.vars = c(3,4,5),
                         variable.name = "clock", value.name = "rate")

write.csv(RateByClockrMelted, file = "RateByClockrMelted_means.csv")


# Summary stats   


Sum_RateByClock1<-Summarize(rates1,
                            data=RateTable_Means_Clades,
                            digits=3)
Sum_RateByClock1

Sum_RateByClock2<-Summarize(rates2,
                            data=RateTable_Means_Clades,
                            digits=3)
Sum_RateByClock2

Sum_RateByClock3<-Summarize(rates3,
                            data=RateTable_Means_Clades,
                            digits=3)
Sum_RateByClock3

AllRateByClocks <- cbind (Sum_RateByClock1, Sum_RateByClock2, Sum_RateByClock3)
AllRateByClocks <- as.data.frame(AllRateByClocks)
names(AllRateByClocks)

write.csv(AllRateByClocks, file="Sum_AllRateByClocks_means.csv")



# Clock and clade rate distribution

RateByClockrMelted <- read.csv("RateByClockrMelted_medians.csv")

#Hist (by clock)
Pl1a<- ggplot(RateByClockrMelted, aes(x=rate, fill = clock, color = clock))+
  geom_density(position="dodge",  alpha = 0.6)+
  scale_x_continuous()+
  #geom_jitter(aes(y = nodes), height = 0.01)+
  #scale_x_continuous()+
  #scale_y_continuous()+
  #geom_text_repel(aes(label=nodes))+
  theme(legend.position="bottom")
Pl1a

#Hist (by clade)
Pl1b<- ggplot(RateByClockrMelted, aes(x=rate, fill = clade, color = clade))+
  geom_density(alpha = 0.3)+
  scale_x_continuous()+
  #geom_jitter(aes(y = nodes), height = 0.01)+
  #scale_x_continuous()+
  #scale_y_continuous(trans = "sqrt")+
  #geom_text_repel(aes(label=nodes))+
  theme(legend.position="top")
Pl1b



# Clade rate by clock distribution (stacked Histo)

RateCladeClocks <- read.csv("RateTable_Medians_Clades.csv")

Pl2a<- ggplot(RateCladeClocks, aes(x=rates1, fill = clade, color = clade))+
  geom_density(position = "stack", alpha = 1, weight = 2)+
  scale_x_continuous()+
  #geom_jitter(aes(y = nodes), height = 0.01)+
  #scale_x_continuous()+
  #scale_y_continuous(trans = "sqrt")+
  #geom_text_repel(aes(label=nodes))+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  #theme_classic()+
  theme(legend.position="none")
Pl2a

Pl2b<- ggplot(RateCladeClocks, aes(x=rates2, fill = clade, color = clade))+
  geom_density(position = "stack", alpha = 1, weight = 2)+
  scale_x_continuous()+
  #geom_jitter(aes(y = nodes), height = 0.01)+
  #scale_x_continuous()+
  #scale_y_continuous(trans = "sqrt")+
  #geom_text_repel(aes(label=nodes))+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  #theme_classic()+
  theme(legend.position="bottom")
Pl2b

Pl2c<- ggplot(RateCladeClocks, aes(x=rates3, fill = clade, color = clade))+
  geom_density(position = "stack", alpha = 1, weight = 2)+
  scale_x_continuous()+
  #geom_jitter(aes(y = nodes), height = 0.01)+
  #scale_x_continuous()+
  #scale_y_continuous(trans = "sqrt")+
  #geom_text_repel(aes(label=nodes))+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  #theme_classic()+
  theme(legend.position="none")

Pl2c

#Combine plots
par(mar = c(7, 7, 7, 7))
A2<-plot_grid(Pl2a, Pl2c, Pl2b, ncol = 1)  
A2




# Clock corr

Lm2a<- lm(data=RateCladeClocks, rates1 ~ rates2)
summary(Lm2a)


R2a<-ggplot(RateCladeClocks, aes(y = rates1, x=rates2))+
  geom_point()+
  geom_smooth(method ="lm", se=TRUE)+
  scale_x_continuous()+
  scale_y_continuous()+
  labs (x="Skull1 rates", y="Postcranial rates")+
  theme_classic()
R2a

Lm2b<- lm(data=RateCladeClocks, rates1 ~ rates3)
summary(Lm2b)

R2b<-ggplot(RateCladeClocks, aes(y = rates1, x=rates3))+
  geom_point()+
  geom_smooth(method ="lm", se=TRUE)+
  scale_x_continuous()+
  scale_y_continuous()+
  labs (x="Skull1 rates", y="Skull2 rates")+
  theme_classic()
R2b

Lm2c<- lm(data=RateCladeClocks, rates2~rates3)
summary(Lm2c)


R2c<-ggplot(RateCladeClocks, aes(y = rates2, x=rates3))+
  geom_point()+
  geom_smooth(method ="lm", se=TRUE)+
  scale_x_continuous()+
  scale_y_continuous()+
  labs (x="Postcranium rates", y="Skull2 rates")+
  theme_classic()
R2c

#Combine plots
par(mar = c(7, 7, 7, 7))
A1<-plot_grid(R2a, R2b, R2c, ncol = 3)  
A1
