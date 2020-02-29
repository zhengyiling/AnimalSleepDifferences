rm(list = ls())
#Sleep in Mammals

sleep<-read.csv(file.choose())
colnames(sleep)<-c("species","bodyweight","brainweight","slowwave","paradoxical","totalsleep","maxlifespan","gestation","predation","exposure","danger")
View(sleep)
attach(sleep)
#note:missing values denoted by -999.0
sleep[sleep<0]<-NA
View(sleep)
ns<-na.omit(sleep)
View(ns)

#predation index (1-5)
#1 = minimum (least likely to be preyed upon)
#5 = maximum (most likely to be preyed upon)

#sleep exposure index (1-5)
#1 = least exposed (e.g. animal sleeps in a well-protected den)
#5 = most exposed

#overall danger index (1-5)
#(based on the above two indices and other information)
#1 = least danger (from other animals)
#5 = most danger (from other animals)

colMeans(ns[,-1])
cor_ns<-cor(ns[,-1])
cor_ns
summary(ns[,-1])

predation1<-ns$predation
View(predation1)
predation1[ns$predation==1]<-"least likely to be preyed upon"
predation1[ns$predation==2]<-"less likely to be preyed upon"
predation1[ns$predation==3]<-"likely to be preyed upon"
predation1[ns$predation==4]<-"more likely to be preyed upon"
predation1[ns$predation==5]<-"most likely to be preyed upon"
View(predation1)

exposure1<-ns$exposure
View(exposure1)
exposure1[ns$exposure==1]<-"least exposed"
exposure1[ns$exposure==2]<-"less exposed"
exposure1[ns$exposure==3]<-"exposed"
exposure1[ns$exposure==4]<-"more exposed"
exposure1[ns$exposure==5]<-"most exposed"
View(exposure1)

danger1<-ns$danger
View(danger1)
danger1[ns$danger==1]<-"least danger"
danger1[ns$danger==2]<-"less danger"
danger1[ns$danger==3]<-"danger"
danger1[ns$danger==4]<-"more danger"
danger1[ns$danger==5]<-"most danger"
View(danger1)

nns<-cbind(ns,predation1,exposure1,danger1)
View(nns)

library(ggplot2)
library(ggthemes)

ggplot(data = nns, aes(x=bodyweight,y=brainweight,color=danger1))+
  geom_point()+
  labs(title="Bodyweight VS Brainweight")+
  scale_x_log10()+scale_y_log10()+
  stat_smooth(method="lm", se=F, col="red")
  
ggplot(data = nns,aes(y=slowwave,x=bodyweight,color=danger1))+
  geom_point()+
  scale_x_log10()+
  stat_smooth(method="lm", se=F, col="red")+
  labs(title="Nondreaming Time VS Bodyweight")

ggplot(data = nns,aes(x=paradoxical,fill=predation1))+
  geom_histogram(binwidth = 0.5)+
  labs(title="Dreaming time in different Predation Index")

ggplot(data = nns,aes(x=totalsleep,fill=danger1))+
  geom_histogram(binwidth = 1)+
  labs(title="Total Sleeptime in different Danger Index")

ggplot(data = nns,aes(x=totalsleep,fill=exposure1))+
  geom_histogram(binwidth = 1)+
  labs(title="Total Sleeptime in different Exposure Index")

ggplot(data = nns, aes(x=paradoxical, fill=danger1))+
  geom_density(alpha=.5)+
  labs(x="Paradoxical Density")

