pt_lcdm_stats<-read.csv("./data/pt__perc_stats_all_1013.csv")
attach(pt_lcdm_stats)
lm.fit.t<-lm(Dx~X95...Thickness+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.t)
p.val.thk.ancova <- ancova.res$Pr(>F)


lm.fit.a<-lm(Dx~Surface.Area+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.a)
p.val.area.ancova <- ancova.res$Pr(>F)

lm.fit.v<-lm(Dx~X95..Volume+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.v)
p.val.vol.ancova <- ancova.res$Pr(>F)


all.data<-read.csv("./data/ACC.csv")
all.data[,23:34] <-lapply(all.data[,23:34],as.factor) 
options(na.action= na.omit)
for (i in 4:13){
  lm.form <- as.formula(paste(names(all.data)[i],'~','hand'))
  lm.fit<-aov(lm.form,data=all.data,na.action= na.omit)
  anova.res <- anova(lm.fit)
  print(anova.res)
  
  lm.form <- as.formula(paste(names(all.data)[i],'~','hand+Dx'))
  lm.fit<-aov(lm.form,data=all.data,na.action= na.omit)
  anova.res <- anova(lm.fit)
  print(anova.res)
  summary(anova.res)
  
  for (j in c(13:17,21:38)){
    if (is.factor(all.data[,j])){
      cont.table.test<-fisher.test(all.data[,i],all.data[,j])
      summary(cont.table.test)
    } else {
      
      lm.form <- as.formula(paste(names(all.data)[i],'~',names(all.data)[j],'+ age+ sex + IntraCranialVol'))
      print(lm.form)
      aov.fit<-aov(lm.form,data=all.data,na.action= na.omit)
      summary(aov.fit)
      lm.fit<-lm(lm.form,data=all.data,na.action= na.omit)
      ancova.res <- anova(lm.fit)
      summary(ancova.res)
      print(paste("p value is ",ancova.res$`Pr(>F)`[1]))
    }
  }
}


pt_lcdm_stats<-read.csv("./data/pt__perc_stats_all_1013.csv")
attach(pt_lcdm_stats)
lm.fit.t<-lm(Dx~X95...Thickness+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.t)
p.val.thk.ancova <- ancova.res$Pr(>F)


lm.fit.a<-lm(Dx~Surface.Area+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.a)
p.val.area.ancova <- ancova.res$Pr(>F)

lm.fit.v<-lm(Dx~X95..Volume+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.v)
p.val.vol.ancova <- ancova.res$Pr(>F)


all.data<-read.csv("./data/ACC.csv")
all.data[,23:34] <-lapply(all.data[,23:34],as.factor) 
options(na.action= na.omit)
for (i in 4:13){
  lm.form <- as.formula(paste(names(all.data)[i],'~','hand'))
  lm.fit<-aov(lm.form,data=all.data,na.action= na.omit)
  anova.res <- anova(lm.fit)
  print(anova.res)
  
  lm.form <- as.formula(paste(names(all.data)[i],'~','hand+Dx'))
  lm.fit<-aov(lm.form,data=all.data,na.action= na.omit)
  anova.res <- anova(lm.fit)
  print(anova.res)
  summary(anova.res)
  
  for (j in c(14:17,21:38)){
   # if (is.factor(all.data[,j])){
  #    cont.table.test<-chisq.test(all.data[,i],all.data[,j])
  #    summary(cont.table.test)
  #  } else {
      if (length(table(all.data[,j]))==1)
        next
      lm.form <- as.formula(paste(names(all.data)[i],'~',names(all.data)[j],'+ age+ sex + IntraCranialVol'))
      print(lm.form)
      aov.fit<-aov(lm.form,data=all.data,na.action= na.omit)
      summary(aov.fit)
      lm.fit<-lm(lm.form,data=all.data,na.action= na.omit)
      ancova.res <- anova(lm.fit)
      summary(ancova.res)
      print(paste("p value is ",ancova.res$`Pr(>F)`[1]))
  #  }
  }
}

