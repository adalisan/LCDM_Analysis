# Create table of descriptive statistics
library(psych)

column.names <- c("Healthy controls", "Non-deficit SZ","Deficit SZ")
column.names <- outer(column.names, c(" Mean", " SD"), paste0)
column.names <- as.vector(t(column.names))


row.names <- outer(c("Left","Right"), c(" volume (mm^3)", " thickness (mm)"," surface area (mm^2)"), paste0)
row.names <- as.vector(row.names)


LCDM.stat.table <- data.frame(matrix(0,6,6), row.names=row.names )
names(LCDM.stat.table ) <- column.names
  
anova.table.col.names<-outer(c("Diagnosis","Side","Diagnosis x Side"),c(" F statistic", " p-value"),paste0)
anova.table.col.names <- as.vector(t(anova.table.col.names))



LCDM.anova.table <- data.frame(matrix(-1,6,6),row.names= row.names)
names(LCDM.anova.table ) <- anova.table.col.names

region <- "pt"
p.cutoff <- 0.005
pt_lcdm_stats<-read.csv("./data/pt__perc_stats_all_1013.csv",header=TRUE)


patient_id.defscz<-read.table("./data/defscz.txt") #file that contains file names or path names of the subjects
patient_id.healthy<-read.table("./data/healthy.txt")
patient_id.nondefscz<-read.table("./data/nondefscz.txt")

#Only include patients whose data have passed quality control (8 subjects are removed)
patients.pass.qc<- unlist(c(patient_id.defscz,patient_id.healthy,patient_id.nondefscz))
pt_lcdm_stats <- pt_lcdm_stats[ pt_lcdm_stats$IDS%in%patients.pass.qc,]

pt_lcdm_stats$ICV<- pt_lcdm_stats$IntraCranialVol


pt_lcdm_stats$Side<-as.factor(pt_lcdm_stats$Side)
pt_lcdm_stats$Dx.num <- pt_lcdm_stats$Dx
pt_lcdm_stats$Dx <- as.factor(pt_lcdm_stats$Dx)


attach(pt_lcdm_stats)

sink(paste0("./results/LCDM_quantile_stat_",region,"_tests.txt"))

print(summary(X95_perc.Thickness[Side=="L"]))



print(summary(X95_perc.Thickness[Side=="R"]))

print(summary(Surface.Area[Side=="L"]))
print(summary(Surface.Area[Side=="R"]))

print(summary(X95_perc.Volume[Side=="L"]))
print(summary(X95_perc.Volume[Side=="R"]))

desc.stats.s<-describeBy(Surface.Area,group=list(Dx,Side),mat=TRUE)[,1:7]
desc.stats.t<-describeBy(X95_perc.Thickness,group=list(Dx,Side),mat=TRUE)[,1:7]
desc.stats.v<-describeBy(X95_perc.Volume,group=list(Dx,Side),mat=TRUE)[,1:7]

LCDM.stat.table["Left volume (mm^3)",]<-c(desc.stats.v$mean[1:3],desc.stats.v$sd[1:3])
LCDM.stat.table["Right volume (mm^3)",]<-c(desc.stats.v$mean[4:6],desc.stats.v$sd[4:6])
LCDM.stat.table["Left thickness (mm)",]<-c(desc.stats.t$mean[1:3],desc.stats.t$sd[1:3])
LCDM.stat.table["Right thickness (mm)",]<-c(desc.stats.t$mean[4:6],desc.stats.t$sd[4:6])
LCDM.stat.table["Left surface area (mm^2)" ,]<-c(desc.stats.s$mean[1:3],desc.stats.s$sd[1:3])
LCDM.stat.table["Right surface area (mm^2)",]<-c(desc.stats.s$mean[4:6],desc.stats.s$sd[4:6])
LCDM.stat.table <- LCDM.stat.table[,c(1,4,2,5,3,6)]
write.csv(LCDM.stat.table,file="./results/LCDM.desc.stats.csv")

lm.fit.v<-lm(Dx~X95_perc.Volume+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.v)
p.val.vol.ancova <- ancova.res$`Pr(>F)`[1]
print(paste0("Null: Diagnosis is not associated with 95% Volume  controlled for side, age and sex (p-value = ",p.val.vol.ancova,")") )
if (p.val.vol.ancova <p.cutoff) print("**Significant**")


LCDM.anova.table[1,1:2] <- ancova.res [1,4:5]


lm.fit.t<-lm(Dx.num~X95_perc.Thickness+Side+age+sex+ICV,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.t)
p.val.thk.ancova <- ancova.res$`Pr(>F)`[1]
print(paste0("Null: Diagnosis is not associated with %95 Thickness controlled for side, age and sex  (p-value = ",p.val.thk.ancova,")") )
if (p.val.thk.ancova <p.cutoff) print("**Significant**")

LCDM.anova.table[3,1:2] <- ancova.res [1,4:5]


lm.fit.a<-lm(Dx.num~Surface.Area+Side+age+sex+ICV,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.a)
p.val.area.ancova <- ancova.res$`Pr(>F)`[1]

print(paste0("Null: Diagnosis is not associated with Surface.Area  controlled for side, age and sex (p-value = ",p.val.area.ancova,")") )
if (p.val.area.ancova <p.cutoff) print("**Significant**")

LCDM.anova.table[5,1:2] <- ancova.res [1,4:5]

i <- 1
for (pt_measure in c("X95_perc.Volume","X95_perc.Thickness", "Surface.Area")){
  lm.form <- as.formula(paste0(pt_measure,'~Side+age+sex+ICV'))
  lm.fit<-lm (lm.form,na.action= na.omit)
  anova.res<- anova(lm.fit)
  print("")
  print(paste0("Main Effect of side on ",pt_measure ))
  print(anova.res)
  p.val <- anova.res$`Pr(>F)`[1]
  print(paste0("Null: PT measure ",pt_measure ,
               " is not associated with  side (p-value = ",p.val," )") )  
  if (p.val <p.cutoff) print("**Significant**")
  
  
  
  lm.form <- as.formula(paste0(pt_measure,'~Dx*Side+age+sex+ICV'))
  lm.fit<-lm (lm.form,na.action= na.omit)
  anova.res<- anova(lm.fit)
  print("")
  print(paste0("Interaction Effect of side on ",pt_measure ))
  print(anova.res)
  p.val <- anova.res$`Pr(>F)`[1]
  print(paste0("Null: PT measure ",pt_measure ,
               " is not associated with  interaction of side and diagnosis (p-value = ",p.val," )") )
  if (p.val <p.cutoff) print("**Significant**")
  LCDM.anova.table [2*(i-1)+1, c(1,3,5)] <- anova.res[c(1,2,6),c(4)]
  LCDM.anova.table [2*(i-1)+1, c(2,4,6)] <- anova.res[c(1,2,6),c(5)]
  print("")
  i<- i +1
}

write.csv(LCDM.anova.table,file="./results/LCDM.anova.pt_measure.csv")

sink()
detach(pt_lcdm_stats)