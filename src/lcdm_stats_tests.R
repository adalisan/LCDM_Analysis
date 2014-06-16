# Statistical Tests for quantile statistics (%95 Thickness, Surface.Area, Volume)
region <- "pt"
p.cutoff <- 0.005
pt_lcdm_stats<-read.csv("./data/pt__perc_stats_all_1013.csv")


patient_id.defscz<-read.table("./data/defscz.txt") #file that contains file names or path names of the subjects
patient_id.healthy<-read.table("./data/healthy.txt")
patient_id.nondefscz<-read.table("./data/nondefscz.txt")

#Only include patients whose data have passed quality control (8 subjects are removed)
patients.pass.qc<- unlist(c(patient_id.defscz,patient_id.healthy,patient_id.nondefscz))
pt_lcdm_stats <- pt_lcdm_stats[ pt_lcdm_stats$IDS%in%patients.pass.qc,]

pt_lcdm_stats$ICV<- pt_lcdm_stats$IntraCranialVol


sink(paste0("./results/LCDM_quantile_stat_",region,"_tests.txt"))
pt_lcdm_stats$Side<-as.factor(pt_lcdm_stats$Side)
pt_lcdm_stats$Dx.num <- pt_lcdm_stats$Dx
pt_lcdm_stats$Dx <- as.factor(pt_lcdm_stats$Dx)

attach(pt_lcdm_stats)

print(summary(X95_perc.Thickness[Side=="L"]))
print(summary(X95_perc.Thickness[Side=="R"]))

print(summary(Surface.Area[Side=="L"]))
print(summary(Surface.Area[Side=="R"]))

print(summary(X95_perc.Volume[Side=="L"]))
print(summary(X95_perc.Volume[Side=="R"]))



lm.fit.t<-lm(Dx.num~X95_perc.Thickness+Side+age+sex+ICV,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.t)
p.val.thk.ancova <- ancova.res$`Pr(>F)`[1]
print(paste0("Null: Diagnosis is not associated with %95 Thickness controlled for side, age and sex  (p-value = ",p.val.thk.ancova,")") )
if (p.val.thk.ancova <p.cutoff) print("**Significant**")

lm.fit.a<-lm(Dx.num~Surface.Area+Side+age+sex+ICV,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.a)
p.val.area.ancova <- ancova.res$`Pr(>F)`[1]

print(paste0("Null: Diagnosis is not associated with Surface.Area  controlled for side, age and sex (p-value = ",p.val.area.ancova,")") )
if (p.val.area.ancova <p.cutoff) print("**Significant**")

lm.fit.v<-lm(Dx.num~X95_perc.Volume+Side+age+sex+ICV,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.v)
p.val.vol.ancova <- ancova.res$`Pr(>F)`[1]
print(paste0("Null: Diagnosis is not associated with 95% Volume  controlled for side, age and sex (p-value = ",p.val.vol.ancova,")") )
if (p.val.vol.ancova <p.cutoff) print("**Significant**")


for (pt_measure in c("X95_perc.Thickness", "Surface.Area","X95_perc.Volume")){
  lm.form <- as.formula(paste0(pt_measure,'~Side+cAge+sex+cICV'))
lm.fit<-lm (lm.form,na.action= na.omit)
 anova.res<- anova(lm.fit)
print("")
print(paste0("Main Effect of side on ",pt_measure ))
print(anova.res)
p.val <- anova.res$`Pr(>F)`[1]
print(paste0("Null: PT measure ",pt_measure ,
             " is not associated with  side (p-value = ",p.val," )") )  
if (p.val <p.cutoff) print("**Significant**")


lm.form <- as.formula(paste0(pt_measure,'~Side*Dx+cAge+sex+cICV'))
lm.fit<-lm (lm.form,na.action= na.omit)
  anova.res<- anova(lm.fit)
print("")
print(paste0("Interaction Effect of side on ",pt_measure ))
print(anova.res)
p.val <- anova.res$`Pr(>F)`[1]
print(paste0("Null: PT measure ",names(all.data)[i] ,
             " is not associated with  interaction of side and diagnosis (p-value = ",p.val," )") )
if (p.val <p.cutoff) print("**Significant**")

print("")
}






all.data<-read.csv("./data/ACC.csv")
all.data[,22:34] <-lapply(all.data[,22:34],as.factor) 
options(na.action= na.omit)







for (i in 4:13){
  
 
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
    p.val <- ancova.res$`Pr(>F)`[1]
   # print(paste("p value is ",)
    print(paste0("Null: PT measure ",names(all.data)[i] ,
                 " is not associated with  ",names(all.data)[j]  ,
                 " controlled for age, sex and ICV (p-value = ",p.val," )") )
    if (p.val <p.cutoff) print("**Significant**")
    
  }
}


sink()
detach(pt_lcdm_stats)