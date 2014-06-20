#CensorStatTests

# Censor_dist.r
# Analysis of censored distances with all VMPFCs kept
# i.e., when outliers are NOT removed
# OUTPUT FROM THIS FILE IS USED FOR THE CENSORED DISTANCE ARTICLE
# 09/16/04

library(MASS)
library(stats)
library(nortest) # package for tests of normality
library(car)
library(ggplot2)
library(dplyr)


set.seed(1)

# 1 == "Healthy"
# 2 == "DefScz"
# 3 == "NondefScz"

ptm<-proc.time()[1]

MDDL <- scan("./data/pt_censor/lh_healthy.txt") #pooled MDD distances
MDDR <- scan("./data/pt_censor/rh_healthy.txt")
DSCZL <- scan("./data/pt_censor/lh_defscz.txt")
DSCZR <- scan("./data/pt_censor/rh_defscz.txt")
NDSCZL <- scan("./data/pt_censor/lh_nondefscz.txt")
NDSCZR <- scan("./data/pt_censor/rh_nondefscz.txt")


#Only include patients whose data have passed quality control (8 subjects are removed)
#patients.pass.qc<- unlist(c(patient_id.defscz,patient_id.NondefScz,patient_id.nondefscz))
#pt_lcdm_stats <- pt_lcdm_stats[ pt_lcdm_stats$IDS%in%patients.pass.qc,]

LDv<-c(MDDL,DSCZL,NDSCZL)
Lcl<-c(rep(1,length(MDDL)),rep(2,length(DSCZL)),rep(3,length(NDSCZL)))
Lcl<-factor(Lcl,labels=c("Healthy","DefScz","NondefScz"))



RDv<-c(MDDR,DSCZR,NDSCZR)
Rcl<-c(rep(1,length(MDDR)),rep(2,length(DSCZR)),rep(3,length(NDSCZR)))
Rcl<-factor(Rcl,labels=c("Healthy","DefScz","NondefScz"))

merged.LCDM<- data.frame(measure = LDv, diag=Lcl ,side = "left")
merged.LCDM<- rbind(merged.LCDM,data.frame(measure = RDv, diag=Rcl ,side = "right"))

#min.dis <- 0.5
min.dis<-min(c(LDv,RDv))
min.dis <- max(-1,min.dis )
max.dis<-max(c(LDv,RDv))
max.dis <- min(max.dis,4.0)  
merged.LCDM <- subset(merged.LCDM,subset=merged.LCDM$measure<max.dis & merged.LCDM$measure>min.dis)




#pt_lcdm_stats$ICV<- pt_lcdm_stats$IntraCranialVol

lcdm.breaks<-seq(min.dis+.05,max.dis,by=.01)
num.breaks<-length(lcdm.breaks)
stats.df <- data.frame(value=numeric(0), test.type=character(0),side=character(0),censoring_idx=numeric(0),diag_groups=character(0))

for (i in 1:num.breaks)
{
  ldv<-filter(merged.LCDM,side=="left", measure  < lcdm.breaks[i] )
  rdv<-filter(merged.LCDM,side=="right", measure  < lcdm.breaks[i])
  
  mddL <- filter(ldv,diag=="Healthy")$measure
  DSCZL  <- filter(ldv,diag=="DefScz") $measure
  NDSCZL<- filter(ldv,diag=="NondefScz")$measure
  
  mddR <-filter(ldv,diag=="Healthy")$measure
  DSCZR  <-filter(ldv,diag=="DefScz")$measure
  NDSCZR<-filter(ldv,diag=="NondefScz")$measure
  
  
  ldv_grouped <- group_by(ldv, diag)
  normality.tests.left <- as.data.frame(do(ldv_grouped, data.frame(p.value= lillie.test(.$measure)$p.value)))
  rdv_grouped <- group_by(rdv, diag)
  normality.tests.right <- as.data.frame(do(rdv_grouped, data.frame(p.value= lillie.test(.$measure)$p.value)))
  stats.df<- rbind(stats.df,data.frame(value=normality.tests.left$p.value,test.type="lillie",side="left",censoring_idx=i,
                                       diag_groups= normality.tests.left$diag))
  stats.df<- rbind(stats.df,data.frame(value=normality.tests.right$p.value,test.type="lillie",side="right",censoring_idx=i,
                                       diag_groups= normality.tests.right$diag))
  
  #multi-group comparisons
  PmgKWL <-kruskal.test(ldv$measure,ldv$diag)$p.value
  PmgKWR <-kruskal.test(rdv$measure,rdv$diag)$p.value
  
  stats.df<- rbind(stats.df,data.frame(value= PmgKWL ,test.type="kruskal",side="left",censoring_idx=i,
                                       diag_groups= "all"))
  stats.df<- rbind(stats.df,data.frame(value= PmgKWR ,test.type="kruskal",side="right",censoring_idx=i,
                                       diag_groups= "all"))
  
  
  mod1<-lm(measure~diag, data=ldv)
  anova.mod1<-anova(mod1)
  PmgAOV1L <-anova.mod1[1,5] #ANOVA with HOV (homegeneity of variances)
  PmgAOV2L <-oneway.test(measure~diag,data=ldv)$p.value  #ANOVA w/o HOV
  stats.df<- rbind(stats.df,data.frame(value= PmgAOV1L ,test.type="anova.homosk",side="left",censoring_idx=i,
                                       diag_groups= "all"))
  stats.df<- rbind(stats.df,data.frame(value= PmgAOV1L ,test.type="anova.heterosk",side="left",censoring_idx=i,
                                       diag_groups= "all"))
  
  mod1<-lm(measure~diag, data=rdv)
  anova.mod1<-anova(mod1)
  PmgAOV1R <-anova.mod1[1,5] #ANOVA with HOV
  PmgAOV2R <-oneway.test(measure~diag, data=rdv)$p.value  #ANOVA w/o HOV
  
  stats.df<- rbind(stats.df,data.frame(value= PmgAOV1R ,test.type="anova.homosk",side="right",censoring_idx=i,
                                       diag_groups= "all"))
  stats.df<- rbind(stats.df,data.frame(value= PmgAOV1R ,test.type="anova.heterosk",side="right",censoring_idx=i,
                                       diag_groups= "all"))
  
  #HOV for multi-group comparisons
  PmghovL <- leveneTest(measure~diag, data=ldv)[[3]][1] #B-F test for HOV
  PmghovR <- leveneTest(measure~diag, data=rdv)[[3]][1]
  
  stats.df<- rbind(stats.df,data.frame(value= PmghovL ,test.type="levene.hov",side="left",censoring_idx=i,
                                       diag_groups= "all"))
  stats.df<- rbind(stats.df,data.frame(value= PmghovR ,test.type="levene.hov",side="right",censoring_idx=i,
                                       diag_groups= "all"))
  
  
  #pairwise left comparisons
  #wilcoxon tests
  p.wilcox.two.sided.1<-wilcox.test(mddL,DSCZL,paired=F)$p.value
  p.wilcox.two.sided.2<-wilcox.test(mddL,NDSCZL,paired=F)$p.value
  p.wilcox.two.sided.3<-wilcox.test(DSCZL,NDSCZL,paired=F)$p.value
  
  
  plt1<-wilcox.test(mddL,DSCZL,alternative="l",paired=F)$p.value
  plt2<-wilcox.test(mddL,NDSCZL,alternative="l",paired=F)$p.value
  plt3<-wilcox.test(DSCZL,NDSCZL,alternative="l",paired=F)$p.value
  

  
  #without Holm's correction
  PpwWL12lt  <- plt1
  PpwWL13lt  <- plt2
  PpwWL23lt  <- plt3
  
  #with Holm's correction
  Pltadj<-p.adjust(c(plt1,plt2,plt3),method="holm")
  PpwWL12ltHC  <- Pltadj[1]
  PpwWL13ltHC  <- Pltadj[2]
  PpwWL23ltHC  <- Pltadj[3]
  
  Pgtadj<-p.adjust(c(1-plt1,1-plt2,1-plt3),method="holm")
  PpwWL12gtHC  <- Pgtadj[1]
  PpwWL13gtHC  <- Pgtadj[2]
  PpwWL23gtHC  <- Pgtadj[3]
  
  stats.df<- rbind(stats.df,data.frame(value= PpwWL12lt ,test.type="wilcox.less",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWL13lt ,test.type="wilcox.less",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWL23lt ,test.type="wilcox.less",side="left",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  stats.df<- rbind(stats.df,data.frame(value= PpwWL12ltHC ,test.type="wilcox.less.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWL13ltHC ,test.type="wilcox.less.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWL23ltHC ,test.type="wilcox.less.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  stats.df<- rbind(stats.df,data.frame(value= PpwWL12gtHC ,test.type="wilcox.greater.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWL13gtHC ,test.type="wilcox.greater.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWL23gtHC ,test.type="wilcox.greater.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  #t-tests
  p.t.test.two.sided.1<-t.test(mddL,DSCZL,paired=F)$p.value
  p.t.test.two.sided.2<-t.test(mddL,NDSCZL,paired=F)$p.value
  p.t.test.two.sided.3<-t.test(DSCZL,NDSCZL,paired=F)$p.value
  
  
  plt1<-t.test(mddL,DSCZL,alternative="l",paired=F)$p.value
  plt2<-t.test(mddL,NDSCZL,alternative="l",paired=F)$p.value
  plt3<-t.test(DSCZL,NDSCZL,alternative="l",paired=F)$p.value
  
  #without Holm's correction
  PpwtL12lt  <- plt1
  PpwtL13lt  <- plt2
  PpwtL23lt  <- plt3
  
  #with Holm's correction
  Pltadj<-p.adjust(c(plt1,plt2,plt3),method="holm")
  PpwtL12ltHC  <- Pltadj[1]
  PpwtL13ltHC  <- Pltadj[2]
  PpwtL23ltHC  <- Pltadj[3]
  
  Pgtadj<-p.adjust(c(1-plt1,1-plt2,1-plt3),method="holm")
  PpwtL12gtHC  <- Pgtadj[1]
  PpwtL13gtHC  <- Pgtadj[2]
  PpwtL23gtHC  <- Pgtadj[3]
  
  
  stats.df<- rbind(stats.df,data.frame(value= PpwtL12lt ,test.type="t.less",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtL13lt ,test.type="t.less",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtL23lt ,test.type="t.less",side="left",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  stats.df<- rbind(stats.df,data.frame(value= PpwtL12ltHC ,test.type="t.less.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtL13ltHC ,test.type="t.less.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtL23ltHC ,test.type="t.less.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  stats.df<- rbind(stats.df,data.frame(value= PpwtL12gtHC ,test.type="t.greater.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtL13gtHC ,test.type="t.greater.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtL23gtHC ,test.type="t.greater.holm.corr",side="left",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  
  #pairwise right comparisons
  #wilcoxon tests
  plt1<-wilcox.test(mddR,DSCZR,paired=F)$p.value
  plt2<-wilcox.test(mddR,NDSCZR,paired=F)$p.value
  plt3<-wilcox.test(NDSCZR,DSCZR,paired=F)$p.value
  
  #without Holm's correction
  PpwWR12lt  <- plt1
  PpwWR13lt  <- plt2
  PpwWR23lt  <- plt3
  
  #with Holm's correction
  Pltadj<-p.adjust(c(plt1,plt2,plt3),method="holm")
  PpwWR12ltHC  <- Pltadj[1]
  PpwWR13ltHC  <- Pltadj[2]
  PpwWR23ltHC  <- Pltadj[3]
  
  Pgtadj<-p.adjust(c(1-plt1,1-plt2,1-plt3),method="holm")
  PpwWR12gtHC  <- Pgtadj[1]
  PpwWR13gtHC  <- Pgtadj[2]
  PpwWR23gtHC  <- Pgtadj[3]
  
  stats.df<- rbind(stats.df,data.frame(value= PpwWR12lt ,test.type="wilcox.less",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWR13lt ,test.type="wilcox.less",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWR23lt ,test.type="wilcox.less",side="right",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  stats.df<- rbind(stats.df,data.frame(value= PpwWR12ltHC ,test.type="wilcox.less.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWR13ltHC ,test.type="wilcox.less.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWR23ltHC ,test.type="wilcox.less.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  stats.df<- rbind(stats.df,data.frame(value= PpwWR12gtHC ,test.type="wilcox.greater.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWR13gtHC ,test.type="wilcox.greater.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwWR23gtHC ,test.type="wilcox.greater.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  
  #t-tests
  plt1<-t.test(mddR,DSCZR,alternative="l",paired=F)$p.value
  plt2<-t.test(mddR,NDSCZR,alternative="l",paired=F)$p.value
  plt3<-t.test(DSCZR,NDSCZR,alternative="l",paired=F)$p.value
  
  #without Holm's correction
  PpwtR12lt  <- plt1
  PpwtR13lt  <- plt2
  PpwtR23lt  <- plt3
  
  #with Holm's correction
  Pltadj<-p.adjust(c(plt1,plt2,plt3),method="holm")
  PpwtR12ltHC  <- Pltadj[1]
  PpwtR13ltHC  <- Pltadj[2]
  PpwtR23ltHC  <- Pltadj[3]
  
  Pgtadj<-p.adjust(c(1-plt1,1-plt2,1-plt3),method="holm")
  PpwtR12gtHC  <- Pgtadj[1]
  PpwtR13gtHC  <- Pgtadj[2]
  PpwtR23gtHC  <- Pgtadj[3]
  
  
  
  stats.df<- rbind(stats.df,data.frame(value= PpwtR12lt ,test.type="t.less",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtR13lt ,test.type="t.less",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtR23lt ,test.type="t.less",side="right",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  stats.df<- rbind(stats.df,data.frame(value= PpwtR12ltHC ,test.type="t.less.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtR13ltHC ,test.type="t.less.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtR23ltHC ,test.type="t.less.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
  stats.df<- rbind(stats.df,data.frame(value= PpwtR12gtHC ,test.type="t.greater.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,DefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtR13gtHC ,test.type="t.greater.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "Healthy,NondefScz"))
  stats.df<- rbind(stats.df,data.frame(value= PpwtR23gtHC ,test.type="t.greater.holm.corr",side="right",censoring_idx=i,
                                       diag_groups= "DefScz,NondefScz"))
  
}
stats.df.bak<- stats.df

end.time<-proc.time()[1]
print("Took ")
print(end.time-ptm)
print("secs")

stats.df$lcdm.breaks <- lcdm.breaks[stats.df$censoring_idx]
stats.df$diag_groups<-as.factor(stats.df$diag_groups)
stats.df$side<-as.factor(stats.df$side)
stats.df$test.type<-as.factor(stats.df$test.type)
#stats.df.subset.1<-
censored_plot.lil <- ggplot(aes(x=lcdm.breaks,y=value,color= diag_groups),
                        data=subset(stats.df,
                                    test.type%in% c("lillie")))+        
  facet_grid(side~diag_groups)+geom_line()+
  scale_y_continuous(limits= c(0,1))+labs(x="Censoring Distance(mm)",y="p-value")+theme_bw()
print(censored_plot.lil)

ggsave("./graphs/censored_statistics_normality.pdf")



censored_plot <- ggplot(aes(x=lcdm.breaks,y=value,color=side),
                        data=subset(stats.df,
                                    test.type%in% c("kruskal","levene.hov","anova.homosk","anova.heterosk")))+
                          facet_grid(side~test.type)+geom_line()+
  labs(x="Censoring Distance(mm)",y="p-value")+theme_bw()+
  scale_color_manual(values=c("#0000DD", "#CC1111"))
  #theme(strip.background = element_rect(colour=c("blue","red")))
#+theme(legend.position="none")
print(censored_plot)

ggsave("./graphs/censored_statistics_threeway.pdf")


censored_plot.2 <- ggplot(aes(x=lcdm.breaks,y=value,colour=diag_groups),
                        data=subset(stats.df, 
                                    test.type%in% c("wilcox.less", "wilcox.less.holm.corr" , "wilcox.greater.holm.corr")))+ 
                  facet_grid(side~test.type)+
  geom_line()+labs(x="Censoring Distance(mm)",y="p-value")+theme_bw()
print(censored_plot.2)

ggsave("./graphs/censored_statistics_twoway_wilcox.pdf")
censored_plot.3 <- ggplot(aes(x=lcdm.breaks,y=value,color=diag_groups),
                          data=subset(stats.df, 
                                      test.type%in% c( "t.less", "t.less.holm.corr", "t.greater.holm.corr" )))+                          
                  facet_grid(side~test.type,drop=TRUE)+
  geom_line()+labs(x="Censoring Distance(mm)",y="p-value")+theme_bw()
print(censored_plot.3)

ggsave("./graphs/censored_statistics_twoway_t.pdf")



