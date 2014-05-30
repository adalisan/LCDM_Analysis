
###SET UP AUTISM DATA### ----

setwd("C:/Users/Jacqui/Dropbox/master_thesis/AutismData/")
dat_orig<-read.csv(file="autism_stats.csv",sep=",")
dat_orig=dat_orig[(dat_orig$Sub_Id!=2492),]

dat_PT_r<-read.csv(file="rh_PT_0.5_new_lcdmstats.csv",sep=";")
dat_PT_l<-read.csv(file="lh_PT_0.5_new_lcdmstats.csv",sep=";")
dat_PT_r=dat_PT_r[(dat_PT_l$IDS!=2492),]
dat_PT_l=dat_PT_l[(dat_PT_l$IDS!=2492),]

dat_temp1=data.frame(Sub_Id=dat_orig[(dat_orig$Hemi == 'rh' & dat_orig$Region == 'PT'),]$Sub_Id,
                    Diag=dat_orig[(dat_orig$Hemi == 'rh' & dat_orig$Region == 'PT'),]$Diag,
                    Age=dat_orig[(dat_orig$Hemi == 'rh' & dat_orig$Region == 'PT'),]$Age,
                    Paness=dat_orig[(dat_orig$Hemi == 'rh' & dat_orig$Region == 'PT'),]$Paness,
                    Hemi=dat_orig[(dat_orig$Hemi == 'rh' & dat_orig$Region == 'PT'),]$Hemi,
                    Region=dat_orig[(dat_orig$Hemi == 'rh' & dat_orig$Region == 'PT'),]$Region)

dat_temp2=data.frame(Sub_Id=dat_PT_r$IDS,
                     T95=dat_PT_r$thk95.mm.,
                     T99=dat_PT_r$thk99.mm.,
                     V95=dat_PT_r$vol95.mm3.,
                     V99=dat_PT_r$vol99.mm3.,
                     S=dat_PT_r$surfArea.mm2.,
                     ICV=dat_PT_r$ICV)

dat_temp3=data.frame(Sub_Id=dat_orig[(dat_orig$Hemi == 'lh' & dat_orig$Region == 'PT'),]$Sub_Id,
                     Diag=dat_orig[(dat_orig$Hemi == 'lh' & dat_orig$Region == 'PT'),]$Diag,
                     Age=dat_orig[(dat_orig$Hemi == 'lh' & dat_orig$Region == 'PT'),]$Age,
                     Paness=dat_orig[(dat_orig$Hemi == 'lh' & dat_orig$Region == 'PT'),]$Paness,
                     Hemi=dat_orig[(dat_orig$Hemi == 'lh' & dat_orig$Region == 'PT'),]$Hemi,
                     Region=dat_orig[(dat_orig$Hemi == 'lh' & dat_orig$Region == 'PT'),]$Region)

dat_temp4=data.frame(Sub_Id=dat_PT_l$IDS,
                     T95=dat_PT_l$thk95.mm.,
                     T99=dat_PT_l$thk99.mm.,
                     V95=dat_PT_l$vol95.mm3.,
                     V99=dat_PT_l$vol99.mm3.,
                     S=dat_PT_l$surfArea.mm2.,
                     ICV=dat_PT_l$ICV)

dat_PT_r_new=merge(dat_temp1,dat_temp2,by="Sub_Id")
dat_PT_l_new=merge(dat_temp3,dat_temp4,by="Sub_Id")
dat_PT_r_new$Sub_Id=paste('subject',as.character(dat_PT_r_new$Sub_Id),sep='')
dat_PT_l_new$Sub_Id=paste('subject',as.character(dat_PT_l_new$Sub_Id),sep='')

dat_stg_r<-subset(dat_orig,Region=='stg' & Hemi=='rh')
dat_stg_l<-subset(dat_orig,Region=='stg' & Hemi=='lh')
dat_PT_r<-dat_PT_r_new
dat_PT_l<-dat_PT_l_new
dat_PT<-rbind(dat_PT_r,dat_PT_l)
dat_stg<-rbind(dat_stg_r,dat_stg_l)


dat_PT_l$S_diff<-dat_PT_l$S-dat_PT_r$S
dat_PT_l$V_diff<-dat_PT_l$V95-dat_PT_r$V95
dat_PT_l$T_diff<-dat_PT_l$T95-dat_PT_r$T95

dat_stg_l$S_diff<-dat_stg_l$S-dat_stg_r$S
dat_stg_l$V_diff<-dat_stg_l$V95-dat_stg_r$V95
dat_stg_l$T_diff<-dat_stg_l$T95-dat_stg_r$T95


###Graping correlations ----


graph_summary<-ddply(dat_PT_l,c("Diag"),summarize,N=length(Age),AVERAGE=mean(T95),SE=sqrt(var(T95)/length(T95)))
graph_summary                     



source('graphCor.R')
graphCor(dat_stg_r,"rh_stg","T95","Control","Autism",4.4,"Yes")
graphCor(dat_stg_r,"rh_stg","V95","Control","Autism",18000,"Yes")
graphCor(dat_stg_r,"rh_stg","S","Control","Autism",4100,"Yes")
graphCor(dat_stg_l,"lh_stg","T95","Control","Autism",4.4,"Yes")
graphCor(dat_stg_l,"lh_stg","V95","Control","Autism",20000,"Yes")
graphCor(dat_stg_l,"lh_stg","S","Control","Autism",4600,"Yes")
graphCor(dat_PT_r,"rh_PT","T95","Control","Autism",4.3,"Yes")
graphCor(dat_PT_r,"rh_PT","V95","Control","Autism",3500,"Yes")
graphCor(dat_PT_r,"rh_PT","S","Control","Autism",900,"Yes")
graphCor(dat_PT_l,"lh_PT","T95","Control","Autism",4.3,"Yes")
graphCor(dat_PT_l,"lh_PT","V95","Control","Autism",5000,"Yes")
graphCor(dat_PT_l,"lh_PT","S","Control","Autism",1100,"Yes")





## RH STG MEANS ---------
library(monash)
savefig("rh_stg_means",height=50,width=10)


dat_roi=rbind(dat_stg_l,dat_stg_r)
library(ggplot2)
library(plyr)
graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(T95),
	SE=sqrt(var(T95)/length(T95)))

ggplot(subset(graph_summary,Region=="STG"))+
  aes(x=Hemi,y=AVERAGE,fill=Diag,stat="identity")+
  geom_bar(position="dodge")+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE),position="dodge")

  opts(legend.position="none",axis.text.x = theme_text(hjust = 0, size=11),axis.text.y = theme_text(size=11),
     axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=12, vjust = -.02))+   
  scale_x_discrete("",labels=c('Right','Left'))
  scale_y_continuous("Mean 95% Thickness (mm)")

  geom_text(aes(x=Diag, y=AVERAGE+SE+.01, label=paste(round(AVERAGE,3),round(SE,3),sep=' +/- ')),size=2.2)



p1<-ggplot(graph_summary)+
aes(x=Diag, y=AVERAGE, colour=Diag)+
geom_point()+
geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
	axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
scale_x_discrete()+
scale_y_continuous("Mean 95% Thickness (mm)")+
geom_text(aes(x=Diag, y=AVERAGE+SE+.01, label=paste(round(AVERAGE,3),round(SE,3),sep=' +/- ')),size=2.2)

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(S),
	SE=sqrt(var(S)/length(S)))
p2<-ggplot(graph_summary)+
aes(x=Diag, y=AVERAGE, colour=Diag)+
geom_point()+
geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
	axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
scale_x_discrete()+
scale_y_continuous(expression(paste("Mean Surface Area ","(mm"^"2",")")))+
geom_text(aes(x=Diag, y=AVERAGE+SE+20, label=paste(round(AVERAGE),round(SE),sep=' +/- ')),size=2.2)
source("multiplot.R")

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(V95),
	SE=sqrt(var(V95)/length(V95)))
p3<-ggplot(graph_summary)+
aes(x=Diag, y=AVERAGE, colour=Diag)+
geom_point()+
geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
	axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
scale_x_discrete()+
scale_y_continuous(expression(paste("Mean 95% Volume ","(mm"^"3",")")))+
geom_text(aes(x=Diag, y=AVERAGE+SE+80, label=paste(round(AVERAGE),round(SE),sep=' +/- ')),size=2.05)

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(V95/ICV),
	SE=sqrt(var(V95/ICV)/length(V95)))
p4<-ggplot(graph_summary)+
aes(x=Diag, y=AVERAGE, colour=Diag)+
geom_point()+
geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
	axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
scale_x_discrete()+
scale_y_continuous("Mean 95% Volume/ICV")+
geom_text(aes(x=Diag, y=AVERAGE+SE+.00002, label=paste(round(AVERAGE,5),round(SE,5),sep=' +/- ')),size=2.1)

multiplot(p1,p2,p3,p4,cols=1)

dev.off()


## LH STG MEANS ---------
library(monash)
savefig("lh_stg_means",height=50,width=10)
dat_roi=dat_stg_l
library(ggplot2)
library(plyr)
graph_summary<-ddply(dat_roi,c("Diag","Hemi"),summarize,AVERAGE=mean(T95),
                     SE=sqrt(var(T95)/length(T95)))
p1<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous("Mean 95% Thickness (mm)")+
         geom_text(aes(x=Diag, y=AVERAGE+SE+.01, label=paste(round(AVERAGE,3),round(SE,3),sep=' +/- ')),size=2.2)

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(S),
                     SE=sqrt(var(S)/length(S)))
p2<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous(expression(paste("Mean Surface Area ","(mm"^"2",")")))+
         geom_text(aes(x=Diag, y=AVERAGE+SE+20, label=paste(round(AVERAGE),round(SE),sep=' +/- ')),size=2.2)
source("multiplot.R")

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(V95),
                     SE=sqrt(var(V95)/length(V95)))
p3<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous(expression(paste("Mean 95% Volume ","(mm"^"3",")")))+
         geom_text(aes(x=Diag, y=AVERAGE+SE+80, label=paste(round(AVERAGE),round(SE),sep=' +/- ')),size=2.05)

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(V95/ICV),
                     SE=sqrt(var(V95/ICV)/length(V95)))
p4<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous("Mean 95% Volume/ICV")+
         geom_text(aes(x=Diag, y=AVERAGE+SE+.00002, label=paste(round(AVERAGE,5),round(SE,5),sep=' +/- ')),size=2.1)

multiplot(p1,p2,p3,p4,cols=1)

dev.off()


## RH PT MEANS ---------
library(monash)
savefig("rh_PT_means",height=7,width=50)
dat_roi=rbind(dat_PT_l,dat_PT_r)
library(ggplot2)
library(plyr)

p1<-
  
  ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Hemi)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous("Mean 95% Thickness (mm)")+
         geom_text(aes(x=Diag, y=AVERAGE+SE+.01, label=paste(round(AVERAGE,3),round(SE,3),sep=' +/- ')),size=2.2)





barx<-barplot(cbind(c(mean(subset(dat_PT_r,Diag=="Control")$V95,subset(dat_PT_l,Diag=="Control")$V95)),
          c(subset(dat_PT_r,Diag=="Autsim")$V95,subset(dat_PT_l,Diag=="Autism")$V95)), beside=T)


###### ASYM Graph -----

savefig("Hemi_asym",height=18,width=15)
par(mfcol=c(3,2),oma=c(0,0,0,3))
par(xpd=FALSE)
graph_summary<-ddply(dat_stg,c("Diag","Hemi"),summarize,AVERAGE=mean(T95),
                     SE=sqrt(var(T95)/length(T95)))
barx1<-barplot(t(graph_summary$AVERAGE),beside=T,ylim=c(4,4.7),xpd = FALSE,
               ylab="mm",
               main="STG Thickness",
               names.arg=c("left STG","right STG","left STG","right STG"),
               col=c('red','red','blue','blue'),xlab='')
box()
arrows(barx1,graph_summary$AVERAGE+graph_summary$SE, x1=barx1, y1=graph_summary$AVERAGE, length=.1,angle=90, code=1)
graph_summary<-ddply(dat_stg,c("Diag","Hemi"),summarize,AVERAGE=mean(V95),
                     SE=sqrt(var(V95)/length(V95)))
barx1<-barplot(t(graph_summary$AVERAGE),beside=T,ylim=c(15000,22000),xpd = FALSE,
               ylab="cubic mm",
               main="STG Volume",
               names.arg=c("left STG","right STG","left STG","right STG"),
               col=c('red','red','blue','blue'),xlab='')
box()
arrows(barx1,graph_summary$AVERAGE+graph_summary$SE, x1=barx1, y1=graph_summary$AVERAGE, length=.1,angle=90, code=1)
graph_summary<-ddply(dat_stg,c("Diag","Hemi"),summarize,AVERAGE=mean(S),
                     SE=sqrt(var(S)/length(S)))
barx3<-barplot(t(graph_summary$AVERAGE),beside=T,ylim=c(3000,5100),xpd = FALSE,
               ylab="square mm",
               main="STG Surface Area",
               names.arg=c("left STG","right STG","left STG","right STG"),
               col=c('red','red','blue','blue'))
box()
arrows(barx3,graph_summary$AVERAGE+graph_summary$SE, x1=barx3, y1=graph_summary$AVERAGE, length=.1,angle=90, code=1)
graph_summary<-ddply(dat_PT,c("Diag","Hemi"),summarize,AVERAGE=mean(T95),
                     SE=sqrt(var(T95)/length(T95)))
barx2<-barplot(t(graph_summary$AVERAGE),beside=T,ylim=c(3.5,4.5),xpd = FALSE,
               ylab="mm",
               main="PT Thickness",
               names.arg=c("left PT","right PT","left PT","right PT"),
               col=c('red','red','blue','blue'))
box()
arrows(barx2,graph_summary$AVERAGE+graph_summary$SE, x1=barx2, y1=graph_summary$AVERAGE, length=.1,angle=90, code=1)

graph_summary<-ddply(dat_PT,c("Diag","Hemi"),summarize,AVERAGE=mean(V95),
                     SE=sqrt(var(V95)/length(V95)))
barx2<-barplot(t(graph_summary$AVERAGE),beside=T,ylim=c(2000,4700),xpd = FALSE,
               ylab="cubic mm",
               main="PT Volume",
               names.arg=c("left PT","right PT","left PT","right PT"),
               col=c('red','red','blue','blue'))
box()
arrows(barx2,graph_summary$AVERAGE+graph_summary$SE, x1=barx2, y1=graph_summary$AVERAGE, length=.1,angle=90, code=1)
graph_summary<-ddply(dat_PT,c("Diag","Hemi"),summarize,AVERAGE=mean(S),
                     SE=sqrt(var(S)/length(S)))
barx4<-barplot(t(graph_summary$AVERAGE),beside=T,ylim=c(400,1400),xpd = FALSE,
               ylab="square mm",
               main="PT Surface Area",
               names.arg=c("left PT","right PT","left PT","right PT"),
               col=c('red','red','blue','blue'))
box()
arrows(barx4,graph_summary$AVERAGE+graph_summary$SE, x1=barx4, y1=graph_summary$AVERAGE, length=.1,angle=90, code=1)

dev.off()

###### PANESS Graph -----


savefig("paness",height=7,width=17)
par(mfcol=c(1,3),oma=c(0,0,0,3))
par(xpd=FALSE)
plot<-plot(T95 ~ Paness, data=dat_PT_r[(dat_PT_r$Diag=="Control"),], 
           main=NA,xlab='PANESS score',ylab="T95 (mm)",cex=.6)
points(dat_roi[(dat_PT_r$Diag=="Autism"),]$Paness,dat_PT_r[(dat_roi$Diag=="Autism"),]$T95,
       col='red',cex=.6)
fitnorm<-lm(T95 ~ Paness, data=dat_PT_r[(dat_PT_r$Diag=="Control"),])
fitdiag2<-lm(T95 ~ Paness, data=dat_PT_r[(dat_PT_r$Diag=="Autism"),])
abline(fitnorm)
abline(fitdiag2,col='red')

plot<-plot(V95 ~ Paness, data=dat_PT_r[(dat_PT_r$Diag=="Control"),], 
           main=NA,xlab='PANESS score',ylab="V95",cex=.6)
points(dat_roi[(dat_PT_r$Diag=="Autism"),]$Paness,dat_PT_r[(dat_roi$Diag=="Autism"),]$V95,
       col='red',cex=.6)
fitnorm<-lm(V95 ~ Paness, data=dat_PT_r[(dat_PT_r$Diag=="Control"),])
fitdiag2<-lm(V95 ~ Paness, data=dat_PT_r[(dat_PT_r$Diag=="Autism"),])
abline(fitnorm)
abline(fitdiag2,col='red')

plot<-plot(S ~ Paness, data=dat_PT_r[(dat_PT_r$Diag=="Control"),], 
           main=NA,xlab='PANESS score',ylab="S",cex=.6)
points(dat_roi[(dat_PT_r$Diag=="Autism"),]$Paness,dat_PT_r[(dat_roi$Diag=="Autism"),]$S,
       col='red',cex=.6)
fitnorm<-lm(S ~ Paness, data=dat_PT_r[(dat_PT_r$Diag=="Control"),])
fitdiag2<-lm(S ~ Paness, data=dat_PT_r[(dat_PT_r$Diag=="Autism"),])
abline(fitnorm)
abline(fitdiag2,col='red')

dev.off()

###############################

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(S),
                     SE=sqrt(var(S)/length(S)))
p2<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous(expression(paste("Mean Surface Area ","(mm"^"2",")")))+
         geom_text(aes(x=Diag, y=AVERAGE+SE+20, label=paste(round(AVERAGE),round(SE),sep=' +/- ')),size=2.2)
source("multiplot.R")

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(V95),
                     SE=sqrt(var(V95)/length(V95)))
p3<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous(expression(paste("Mean 95% Volume ","(mm"^"3",")")))+
         geom_text(aes(x=Diag, y=AVERAGE+SE+80, label=paste(round(AVERAGE),round(SE),sep=' +/- ')),size=2.05)

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(V95/ICV),
                     SE=sqrt(var(V95/ICV)/length(V95)))
p4<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous("Mean 95% Volume/ICV")+
         geom_text(aes(x=Diag, y=AVERAGE+SE+.00002, label=paste(round(AVERAGE,5),round(SE,5),sep=' +/- ')),size=2.1)

multiplot(p1,p2,p3,p4,cols=4)

dev.off()


##LH PT MEANS ---------
library(monash)
savefig("lh_PT_means",height=7,width=50)
dat_roi=dat_PT_l
library(ggplot2)
library(plyr)
graph_summary<-ddply(dat_roi,c("Diag","Hemi"),summarize,AVERAGE=mean(T95),
                     SE=sqrt(var(T95)/length(T95)))
p1<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous("Mean 95% Thickness (mm)")+
         geom_text(aes(x=Diag, y=AVERAGE+SE+.01, label=paste(round(AVERAGE,3),round(SE,3),sep=' +/- ')),size=2.2)

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(S),
                     SE=sqrt(var(S)/length(S)))
p2<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous(expression(paste("Mean Surface Area ","(mm"^"2",")")))+
         geom_text(aes(x=Diag, y=AVERAGE+SE+20, label=paste(round(AVERAGE),round(SE),sep=' +/- ')),size=2.2)
source("multiplot.R")

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(V95),
                     SE=sqrt(var(V95)/length(V95)))
p3<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous(expression(paste("Mean 95% Volume ","(mm"^"3",")")))+
         geom_text(aes(x=Diag, y=AVERAGE+SE+80, label=paste(round(AVERAGE),round(SE),sep=' +/- ')),size=2.05)

graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(V95/ICV),
                     SE=sqrt(var(V95/ICV)/length(V95)))
p4<-ggplot(graph_summary)+
  aes(x=Diag, y=AVERAGE, colour=Diag)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
  opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=9),axis.text.y = theme_text(size=7),
       axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=9))+
         scale_x_discrete()+
         scale_y_continuous("Mean 95% Volume/ICV")+
         geom_text(aes(x=Diag, y=AVERAGE+SE+.00002, label=paste(round(AVERAGE,5),round(SE,5),sep=' +/- ')),size=2.1)

multiplot(p1,p2,p3,p4,cols=4)

dev.off()



############################################################

library(monash)

savefig("ICV_Diag",height=10,width=10)
dat_roi=dat_stg_r
graph_summary<-ddply(dat_roi,c("Diag"),summarize,AVERAGE=mean(ICV),
	SE=sqrt(var(ICV)/length(ICV)))
ggplot(graph_summary)+
aes(x=Diag, y=AVERAGE, colour=Diag)+
geom_point()+
geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
opts(legend.position="none",axis.text.x = theme_text(angle = 90, hjust = 0, size=11),
	axis.title.x = theme_blank(),axis.title.y = theme_text(angle = 90, size=11))+
scale_x_discrete()+
scale_y_continuous(expression(paste("ICV ","(mm"^"3",")")))+
geom_text(aes(x=Diag, y=AVERAGE+SE+10000, label=paste(round(AVERAGE),round(SE),sep=' +/- ')),size=3)
dev.off()

savefig("ICV_Age",height=10,width=11)
dat_roi=dat_stg_r
plot<-plot(ICV ~ Age, data=dat_roi[(dat_roi$Diag=="Control"),], 
	main=NA,xlab="Age (years)",ylab="ICV",col="blue",cex=1)
points(dat_roi[(dat_roi$Diag=="Autism"),]$Age,dat_roi[(dat_roi$Diag=="Autism"),]$ICV,
	col='red',cex=1)
fitnorm<-lm(ICV ~ Age, data=dat_roi[(dat_roi$Diag=="Control"),])
fitdiag2<-lm(ICV ~ Age, data=dat_roi[(dat_roi$Diag=="Autism"),])
fitall<-lm(ICV ~ Age, data=dat_roi)
abline(fitnorm,col='blue')
abline(fitdiag2,col='red')
abline(fitall,col='black')
legend(11.6,1310000,c('Autism','Control','Both'),col=c('red','blue','black'),lty=1)
dev.off()

savefig("V_ICV",height=10,width=11)
dat_roi=dat_stg_r
plot<-plot(ICV ~ Age, data=dat_roi[(dat_roi$Diag=="Control"),], 
	main=NA,xlab="Age (years)",ylab="ICV",col="blue",cex=1)
points(dat_roi[(dat_roi$Diag=="Autism"),]$Age,dat_roi[(dat_roi$Diag=="Autism"),]$ICV,
	col='red',cex=1)
fitnorm<-lm(ICV ~ Age, data=dat_roi[(dat_roi$Diag=="Control"),])
fitdiag2<-lm(ICV ~ Age, data=dat_roi[(dat_roi$Diag=="Autism"),])
fitall<-lm(ICV ~ Age, data=dat_roi)
abline(fitnorm,col='blue')
abline(fitdiag2,col='red')
abline(fitall,col='black')
legend(11.6,1310000,c('Autism','Control','Both'),col=c('red','blue','black'),lty=1)
dev.off()


summary(lm(T95 ~ Diag,data=rbind(dat_stg_l,dat_stg_r)))


summary(lm(ICV~Age+Age:Diag,data=dat_))
dat_roi=dat_stg_r

summary(lm(Age ~ Paness, data=dat_roi))
summary(lm(Age ~ ICV, data=dat_roi))
summary(lm(Paness ~ ICV, data=dat_roi))

summary(lm(Age ~ Diag, data=dat_roi))
summary(lm(ICV ~ Diag, data=dat_roi))
summary(lm(Paness ~ Diag, data=dat_roi))


summary(lm(S ~ Paness+Paness:Diag+Diag, data=dat_roi))
summary(lm(S ~ ICV, data=dat_roi))
summary(lm(S ~ Age, data=dat_roi))
summary(lm(S ~ Diag, data=dat_roi))
summary(lm(S ~ Diag+Paness+ICV+Age, data=dat_roi))

summary(lm(T95 ~ V95+S, data=dat_roi))


anova(lme(T95 ~ Diag, data=dat_roi))

summary(lm(T95 ~ Age, data=dat_roi))
summary(lm(T95 ~ Paness, data=dat_roi))
summary(lm(T95 ~ ICV, data=dat_roi))
library(scatterplot3d,)
s3d<-scatterplot3d(dat_roi$T95,dat_roi$V95,dat_roi$S,type="h",highlight.3d=T,angle=55)

plot(lm(T95 ~ V95,data=dat_roi))
summary(lm(S ~ T95,data=dat_roi))
summary(lm(S ~ V95,data=dat_roi))


dat_PT_adjust<-dat_roi
dat_stg_adjust<-dat_roi


###ANOVA STG T95 -------

source("setupAnova.R")
meas="T95"
dat_roi<-setupAnova(rbind(dat_stg_r,dat_stg_l),meas)
dat_roi_r<-setupAnova(dat_stg_r,meas)
dat_roi_l<-setupAnova(dat_stg_l,meas)

dat_roi_C<-setupAnova(subset(dat_roi,Diag=="Control"),meas)
dat_roi_A<-setupAnova(subset(dat_roi,Diag=="Autism"),meas)

###PRELIMINARY MODELS TO CHECK FOR INTERACTIONS WITH DIAGNOSIS###

lme.1a=lme(measure ~ cAge+Diag,data=dat_roi,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.1a=anova.lme(lme.1a,type="marginal",adjustSigma = F)
anova.1a

lme.1r=lme(measure ~ cAge+Diag, random= ~ 1 | Sub_Id, 
			weights=varIdent(form= ~1 | Hemi),data=dat_roi_r, method="ML")
anova.1r=anova.lme(lme.1r, type= "marginal", adjustSigma = F)
anova.1r

lme.1l=lme(measure ~ Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_l, method="ML")
anova.1l=anova.lme(lme.1l, type= "marginal", adjustSigma = F)
anova.1l


lme.2a=lme(measure ~ Hemi,data=dat_roi_C,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Hemi,data=dat_roi_A,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

t.test(subset(dat_roi_r,Diag=="Control")$measure,subset(dat_roi_l,Diag=="Control")$measure,paired=T)
t.test(subset(dat_roi_r,Diag=="Autism")$measure,subset(dat_roi_l,Diag=="Autism")$measure,paired=T)

###ANOVA STG V95 -------

source("setupAnova.R")
meas="V95"
dat_roi<-setupAnova(rbind(dat_stg_r,dat_stg_l),meas)
dat_roi_r<-setupAnova(dat_stg_r,meas)
dat_roi_l<-setupAnova(dat_stg_l,meas)

dat_roi_C<-setupAnova(subset(dat_roi,Diag=="Control"),meas)
dat_roi_A<-setupAnova(subset(dat_roi,Diag=="Autism"),meas)

###PRELIMINARY MODELS TO CHECK FOR INTERACTIONS WITH DIAGNOSIS###

lme.1a=lme(measure ~ cICV+Hemi+Diag,data=dat_roi,
           random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.1a=anova.lme(lme.1a,type="marginal",adjustSigma = F)
anova.1a

lme.1r=lme(measure ~ cICV+Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_r, method="ML")
anova.1r=anova.lme(lme.1r, type= "marginal", adjustSigma = F)
anova.1r

lme.1l=lme(measure ~ cICV+Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_l, method="ML")
anova.1l=anova.lme(lme.1l, type= "marginal", adjustSigma = F)
anova.1l


lme.2a=lme(measure ~ Hemi,data=dat_roi_C,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Hemi,data=dat_roi_A,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a


t.test(subset(dat_roi_r,Diag=="Control")$measure,subset(dat_roi_l,Diag=="Control")$measure,paired=T)
t.test(subset(dat_roi_r,Diag=="Autism")$measure,subset(dat_roi_l,Diag=="Autism")$measure,paired=T)


###ANOVA STG S -------

source("setupAnova.R")
meas="S"
dat_roi<-setupAnova(rbind(dat_stg_r,dat_stg_l),meas)
dat_roi_r<-setupAnova(dat_stg_r,meas)
dat_roi_l<-setupAnova(dat_stg_l,meas)

dat_roi_C<-setupAnova(subset(dat_roi,Diag=="Control"),meas)
dat_roi_A<-setupAnova(subset(dat_roi,Diag=="Autism"),meas)

###PRELIMINARY MODELS TO CHECK FOR INTERACTIONS WITH DIAGNOSIS###


###ICV

lme.1a=lme(cICV ~ Diag+cAge,data=dat_roi_r,
           random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.1a=anova.lme(lme.1a,type="marginal",adjustSigma = F)
anova.1a

lme.1a=lme(measure ~ cICV+Hemi+Diag,data=dat_roi,
           random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.1a=anova.lme(lme.1a,type="marginal",adjustSigma = F)
anova.1a

lme.1r=lme(measure ~ cICV+Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_r, method="ML")
anova.1r=anova.lme(lme.1r, type= "marginal", adjustSigma = F)
anova.1r

lme.1l=lme(measure ~ cICV+Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_l, method="ML")
anova.1l=anova.lme(lme.1l, type= "marginal", adjustSigma = F)
anova.1l


lme.2a=lme(measure ~ Hemi,data=dat_roi_C,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Hemi,data=dat_roi_A,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a


t.test(subset(dat_roi_r,Diag=="Control")$measure,subset(dat_roi_l,Diag=="Control")$measure,paired=T)
t.test(subset(dat_roi_r,Diag=="Autism")$measure,subset(dat_roi_l,Diag=="Autism")$measure,paired=T)


###ANOVA PT T95 -------

source("setupAnova.R")
meas="T95"
dat_roi<-setupAnova(rbind(dat_PT_r,dat_PT_l),meas)
dat_roi_r<-setupAnova(dat_PT_r,meas)
dat_roi_l<-setupAnova(dat_PT_l,meas)

dat_roi_C<-setupAnova(subset(dat_roi,Diag=="Control"),meas)
dat_roi_A<-setupAnova(subset(dat_roi,Diag=="Autism"),meas)

###PRELIMINARY MODELS TO CHECK FOR INTERACTIONS WITH DIAGNOSIS###

lme.1a=lme(measure ~ Diag,data=dat_roi,
           random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.1a=anova.lme(lme.1a,type="marginal",adjustSigma = F)
anova.1a

lme.1r=lme(measure ~ cPaness:Diag+Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_r, method="ML")
anova.1r=anova.lme(lme.1r, type= "marginal", adjustSigma = F)
anova.1r

lme.1l=lme(measure ~ Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_l, method="ML")
anova.1l=anova.lme(lme.1l, type= "marginal", adjustSigma = F)
anova.1l


lme.2a=lme(measure ~ Hemi,data=dat_roi_C,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Hemi,data=dat_roi_A,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a


t.test(subset(dat_roi_r,Diag=="Control")$measure,subset(dat_roi_l,Diag=="Control")$measure,paired=T)
t.test(subset(dat_roi_r,Diag=="Autism")$measure,subset(dat_roi_l,Diag=="Autism")$measure,paired=T)



lme.2a=lme(measure ~ Paness+Hemi:Paness,data=dat_roi_C,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Paness+Hemi:Paness,data=dat_roi_A,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Paness,data=subset(dat_roi_C,Hemi=='lh'),random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Paness,data=subset(dat_roi_A,Hemi=='lh'),random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

###ANOVA PT V95 -------

source("setupAnova.R")
meas="V95"
dat_roi<-setupAnova(rbind(dat_PT_r,dat_PT_l),meas)
dat_roi_r<-setupAnova(dat_PT_r,meas)
dat_roi_l<-setupAnova(dat_PT_l,meas)

dat_roi_C<-setupAnova(subset(dat_roi,Diag=="Control"),meas)
dat_roi_A<-setupAnova(subset(dat_roi,Diag=="Autism"),meas)

###PRELIMINARY MODELS TO CHECK FOR INTERACTIONS WITH DIAGNOSIS###

lme.1a=lme(measure ~ cICV+Hemi+Diag,data=dat_roi,
           random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.1a=anova.lme(lme.1a,type="marginal",adjustSigma = F)
anova.1a

lme.1r=lme(measure ~ Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_r, method="ML")
anova.1r=anova.lme(lme.1r, type= "marginal", adjustSigma = F)
anova.1r

lme.1l=lme(measure ~ cICV+Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_l, method="ML")
anova.1l=anova.lme(lme.1l, type= "marginal", adjustSigma = F)
anova.1l


lme.2a=lme(measure ~ Hemi,data=dat_roi_C,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Hemi,data=dat_roi_A,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a


t.test(subset(dat_roi_r,Diag=="Control")$measure,subset(dat_roi_l,Diag=="Control")$measure,paired=T)
t.test(subset(dat_roi_r,Diag=="Autism")$measure,subset(dat_roi_l,Diag=="Autism")$measure,paired=T)


lme.2a=lme(measure ~ Paness,data=subset(dat_roi_C,Hemi=='rh'),random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Paness,data=subset(dat_roi_A,Hemi=='rh'),random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a


###ANOVA PT S -------


source("setupAnova.R")
meas="S"
dat_roi<-setupAnova(rbind(dat_PT_r,dat_PT_l),meas)
dat_roi_r<-setupAnova(dat_PT_r,meas)
dat_roi_l<-setupAnova(dat_PT_l,meas)

dat_roi_C<-setupAnova(subset(dat_roi,Diag=="Control"),meas)
dat_roi_A<-setupAnova(subset(dat_roi,Diag=="Autism"),meas)

###PRELIMINARY MODELS TO CHECK FOR INTERACTIONS WITH DIAGNOSIS###

lme.1a=lme(measure ~ cICV+Hemi+Diag,data=dat_roi,
           random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.1a=anova.lme(lme.1a,type="marginal",adjustSigma = F)
anova.1a

lme.1r=lme(measure ~ cICV+Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_r, method="ML")
anova.1r=anova.lme(lme.1r, type= "marginal", adjustSigma = F)
anova.1r

lme.1l=lme(measure ~ cICV+Diag, random= ~ 1 | Sub_Id, 
           weights=varIdent(form= ~1 | Hemi),data=dat_roi_l, method="ML")
anova.1l=anova.lme(lme.1l, type= "marginal", adjustSigma = F)
anova.1l


lme.2a=lme(measure ~ Hemi,data=dat_roi_C,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Hemi,data=dat_roi_A,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a


t.test(subset(dat_roi_r,Diag=="Control")$measure,subset(dat_roi_l,Diag=="Control")$measure,paired=T)
t.test(subset(dat_roi_r,Diag=="Autism")$measure,subset(dat_roi_l,Diag=="Autism")$measure,paired=T)


lme.2a=lme(measure ~ Paness,data=dat_roi_C,random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a


lme.2a=lme(measure ~ Paness,data=subset(dat_roi_C,Hemi=='lh'),random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

lme.2a=lme(measure ~ Paness,data=subset(dat_roi_A,Hemi=='lh'),random= ~1 | Sub_Id,weights=varIdent(form= ~1 | Hemi),method="ML")
anova.2a=anova.lme(lme.2a,type="marginal",adjustSigma = F)
anova.2a

