pt_lcdm_stats<-read.csv("./data/pt__perc_stats_all_1013.csv")
attach(pt_lcdm_stats)
lm.fit.t<-lm(Dx~X95...Thickness+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.t)
p.val.thk.ancova <- ancova.res$Pr(>F)
print(paste0("Null: Diagnosis is not associated with X95...Thickness controlled for side, age and sex  (p-value = ",p.val.thk.ancova,")") )

lm.fit.a<-lm(Dx~Surface.Area+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.a)
p.val.area.ancova <- ancova.res$Pr(>F)

print(paste0("Null: Diagnosis is not associated with Surface.Area  controlled for side, age and sex (p-value = ",p.val.area.ancova,")") )

lm.fit.v<-lm(Dx~X95..Volume+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.v)
p.val.vol.ancova <- ancova.res$Pr(>F)
print(paste0("Null: Diagnosis is not associated with Surface.Area  controlled for side, age and sex (p-value = ",p.val.vol.ancova,")") )


all.data<-read.csv("./data/ACC.csv")
all.data[,23:34] <-lapply(all.data[,23:34],as.factor) 
options(na.action= na.omit)
for (i in 4:13){
  lm.form <- as.formula(paste(names(all.data)[i],'~','hand'))
  lm.fit<-aov(lm.form,data=all.data,na.action= na.omit)
  anova.res <- anova(lm.fit)
  print(anova.res)
  print(paste0("Null: PT measure ",names(all.data)[i] ,
               " is not associated with  side (p-value = ",anova.res$`Pr(>F)`[1]," )") )  
  
  lm.form <- as.formula(paste(names(all.data)[i],'~','hand+Dx'))
  lm.fit<-aov(lm.form,data=all.data,na.action= na.omit)
  anova.res <- anova(lm.fit)
  print(anova.res)
  summary(anova.res)
  print(paste0("Null: PT measure ",names(all.data)[i] ,
               " is not associated with  interaction of side and diagnosis (p-value = ",anova.res$`Pr(>F)`[1]," )") )
  
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
      print(paste0("Null: PT measure ",names(all.data)[i] ,
                   " is not associated with  ",names(all.data)[j]  ,
                   "controlled for age, sex and ICV (p-value = ",anova.res$`Pr(>F)`[1]," )") )
      
    }
  }
}


