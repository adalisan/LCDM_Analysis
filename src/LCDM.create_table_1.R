# Create table of descriptive statistics
column.names <- c("Healthy controls", "Non-deficit SZ","Deficit SZ")
column.names <- outer(column.names, c(" Mean", " SD"), paste0)
column.names <- t(column.names)
dim(column.names) <-c(6,1)

row.names <- outer(c("Left","Right"), c(" volume (mm^3)", " thickness (mm)"," surface area (mm^2)"), paste0)
dim(row.names) <-c(6,1)

LCDM.stat.table <- data.frame(0, row.names=row.names )
names(LCDM.stat.table ) <- column.names
  
anova.table.col.names<-outer(c("Diagnosis","Side","Diagnosis x Side"),c(" F statistic", " p-value"),paste0)
anova.table.col.names <- t(anova.table.col.names)
dim(anova.table.col.names) <-c(6,1)


LCDM.anova.table <- data.frame(0,row.names= row.names)
names(LCDM.anova.table ) <- anova.table.col.names

region <- "pt"
p.cutoff <- 0.005
pt_lcdm_stats<-read.csv("./data/pt__perc_stats_all_1013.csv")
sink(paste0("./results/LCDM_quantile_stat_",region,"_tests.txt"))
attach(pt_lcdm_stats)
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


lm.fit.v<-lm(Dx~X95_perc.Volume+Side+age+sex,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.v)
p.val.vol.ancova <- ancova.res$`Pr(>F)`[1]
print(paste0("Null: Diagnosis is not associated with 95% Volume  controlled for side, age and sex (p-value = ",p.val.vol.ancova,")") )
if (p.val.vol.ancova <p.cutoff) print("**Significant**")


LCDM.anova.table[1,1:2] <- ancova.res [1,4:5]


lm.fit.t<-lm(Dx~X95_perc.Thickness+Side+age+sex+ICV,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.t)
p.val.thk.ancova <- ancova.res$`Pr(>F)`[1]
print(paste0("Null: Diagnosis is not associated with %95 Thickness controlled for side, age and sex  (p-value = ",p.val.thk.ancova,")") )
if (p.val.thk.ancova <p.cutoff) print("**Significant**")

LCDM.anova.table[3,1:2] <- ancova.res [1,4:5]


lm.fit.a<-lm(Dx~Surface.Area+Side+age+sex+ICV,data = pt_lcdm_stats)
ancova.res <- anova(lm.fit.a)
p.val.area.ancova <- ancova.res$`Pr(>F)`[1]

print(paste0("Null: Diagnosis is not associated with Surface.Area  controlled for side, age and sex (p-value = ",p.val.area.ancova,")") )
if (p.val.area.ancova <p.cutoff) print("**Significant**")

LCDM.anova.table[5,1:2] <- ancova.res [1,4:5]


for (pt_measure in c("X95_perc.Volume","X95_perc.Thickness", "Surface.Area")){
  lm.form <- as.formula(paste0(pt_measure,'~Side+age+sex+ICV'))
  lm.fit<-lm (lm.form,na.action= na.omit)
  
  print("")
  print(paste0("Main Effect of side on ",pt_measure ))
  print(anova.res)
  p.val <- anova.res$`Pr(>F)`[1]
  print(paste0("Null: PT measure ",pt_measure ,
               " is not associated with  side (p-value = ",p.val," )") )  
  if (p.val <p.cutoff) print("**Significant**")
  
  
  lm.form <- as.formula(paste0(pt_measure,'~Side*Dx+age+sex+ICV'))
  lm.fit<-lm (lm.form,na.action= na.omit)
  
  print("")
  print(paste0("Interaction Effect of side on ",pt_measure ))
  print(anova.res)
  p.val <- anova.res$`Pr(>F)`[1]
  print(paste0("Null: PT measure ",names(all.data)[i] ,
               " is not associated with  interaction of side and diagnosis (p-value = ",p.val," )") )
  if (p.val <p.cutoff) print("**Significant**")
  
  print("")
}
