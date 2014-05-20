# Censor_dist.r
# Analysis of censored distances with all VMPFCs kept
# i.e., when outliers are NOT removed
# OUTPUT FROM THIS FILE IS USED FOR THE CENSORED DISTANCE ARTICLE
# 09/16/04

library(MASS)
library(stats)
library(nortest) # package for tests of normality
library(car)

set.seed(1)

# 1 == "MDD"
# 2 == "HR"
# 3 == "Ctrl"

ptm<-proc.time()[1]

MDDL <- scan("/cis/home/epostell/Projects/def_scz/pt_censor/lh_healthy.txt") #pooled MDD distances
MDDR <- scan("/cis/home/epostell/Projects/def_scz/pt_censor/rh_healthy.txt")
HRL <- scan("/cis/home/epostell/Projects/def_scz/pt_censor/lh_defscz.txt")
HRR <- scan("/cis/home/epostell/Projects/def_scz/pt_censor/rh_defscz.txt")
CtrlL <- scan("/cis/home/epostell/Projects/def_scz/pt_censor/lh_nondefscz.txt")
CtrlR <- scan("/cis/home/epostell/Projects/def_scz/pt_censor/rh_nondefscz.txt")


LDv<-c(MDDL,HRL,CtrlL)
Lcl<-c(rep(1,length(MDDL)),rep(2,length(HRL)),rep(3,length(CtrlL)))
Lcl<-factor(Lcl,labels=c("MDD","HR","Ctrl"))

RDv<-c(MDDR,HRR,CtrlR)
Rcl<-c(rep(1,length(MDDR)),rep(2,length(HRR)),rep(3,length(CtrlR)))
Rcl<-factor(Rcl,labels=c("MDD","HR","Ctrl"))


MDDL  <- LDv[Lcl=="MDD"]
HRL   <- LDv[Lcl=="HR"]
CtrlL <- LDv[Lcl=="Ctrl"]

MDDR  <- RDv[Rcl=="MDD"]
HRR   <- RDv[Rcl=="HR"]
CtrlR <- RDv[Rcl=="Ctrl"]

######CENSORING DISTANCES
max.dis<-max(c(LDv,RDv))
cd<-seq(0,max.dis,by=.01)
lcd<-length(cd)

#Non-parametric and parametric tests

PWLR<-PMDDWLRlt<-PHRWLRlt<-PCtrlWLRlt<-rep(0,lcd)
PMDDWLRltHC<-PHRWLRltHC<-PCtrlWLRltHC<-PMDDWLRgtHC<-PHRWLRgtHC<-PCtrlWLRgtHC<-rep(0,lcd)
PtLR<-PMDDtLRlt<-PHRtLRlt<-PCtrltLRlt<-rep(0,lcd)
PMDDtLRltHC<-PHRtLRltHC<-PCtrltLRltHC<-PMDDtLRgtHC<-PHRtLRgtHC<-PCtrltLRgtHC<-rep(0,lcd)

PL1norm<-PL2norm<-PL3norm<-PR1norm<-PR2norm<-PR3norm<-rep(0,lcd)

PmgKWL<-PmgKWR<-PmgAOV1L<-PmgAOV1R<-PmgAOV2L<-PmgAOV2R<-rep(0,lcd)

PpwWL12lt<-PpwWL13lt<-PpwWL23lt<-PpwWR12lt<-PpwWR13lt<-PpwWR23lt<-rep(0,lcd)
PpwWL12ltHC<-PpwWL13ltHC<-PpwWL23ltHC<-PpwWR12ltHC<-PpwWR13ltHC<-PpwWR23ltHC<-rep(0,lcd)
PpwtL12lt<-PpwtL13lt<-PpwtL23lt<-PpwtR12lt<-PpwtR13lt<-PpwtR23lt<-rep(0,lcd)
PpwtL12ltHC<-PpwtL13ltHC<-PpwtL23ltHC<-PpwtR12ltHC<-PpwtR13ltHC<-PpwtR23ltHC<-rep(0,lcd)

PpwWL12gtHC<-PpwWL13gtHC<-PpwWL23gtHC<-PpwWR12gtHC<-PpwWR13gtHC<-PpwWR23gtHC<-rep(0,lcd)
PpwtL12gtHC<-PpwtL13gtHC<-PpwtL23gtHC<-PpwtR12gtHC<-PpwtR13gtHC<-PpwtR23gtHC<-rep(0,lcd)

PmghovL<-PmghovR<-rep(0,lcd)
PpwhovL12lt<-PpwhovL13lt<-PpwhovL23lt<-PpwhovR12lt<-PpwhovR13lt<-PpwhovR23lt<-rep(0,lcd)
PpwhovL12ltHC<-PpwhovL13ltHC<-PpwhovL23ltHC<-PpwhovR12ltHC<-PpwhovR13ltHC<-PpwhovR23ltHC<-rep(0,lcd)
PpwhovL12gtHC<-PpwhovL13gtHC<-PpwhovL23gtHC<-PpwhovR12gtHC<-PpwhovR13gtHC<-PpwhovR23gtHC<-rep(0,lcd)

for (i in 1:lcd)
{
ldv<-LDv[LDv<cd[i]]
rdv<-RDv[RDv<cd[i]]

lcl<-Lcl[LDv<cd[i]]
rcl<-Rcl[RDv<cd[i]]

mddL<-MDDL[MDDL<cd[i]]
hrL<-HRL[HRL<cd[i]]
ctrlL<-CtrlL[CtrlL<cd[i]]

mddR<-MDDR[MDDR<cd[i]]
hrR<-HRR[HRR<cd[i]]
ctrlR<-CtrlR[CtrlR<cd[i]]

#tests for normality (by group)
PL1norm[i]<-lillie.test(mddL)$p.value
PL2norm[i]<-lillie.test(hrL)$p.value
PL3norm[i]<-lillie.test(ctrlL)$p.value

PR1norm[i]<-lillie.test(mddR)$p.value
PR2norm[i]<-lillie.test(hrR)$p.value
PR3norm[i]<-lillie.test(ctrlR)$p.value

#multi-group comparisons
PmgKWL[i]<-kruskal.test(ldv,lcl)$p.value
PmgKWR[i]<-kruskal.test(rdv,rcl)$p.value

mod1<-lm(ldv~lcl)
anova.mod1<-anova(mod1)
PmgAOV1L[i]<-anova.mod1[1,5] #ANOVA with HOV
PmgAOV2L[i]<-oneway.test(ldv~lcl)$p.value  #ANOVA w/o HOV

mod1<-lm(rdv~rcl)
anova.mod1<-anova(mod1)
PmgAOV1R[i]<-anova.mod1[1,5] #ANOVA with HOV
PmgAOV2R[i]<-oneway.test(rdv~rcl)$p.value  #ANOVA w/o HOV

#HOV for multi-group comparisons
PmghovL[i]<-levene.test(ldv,lcl)[[3]][1] #B-F test for HOV
PmghovR[i]<-levene.test(rdv,rcl)[[3]][1]

#pairwise left comparisons
#wilcoxon tests
plt1<-wilcox.test(mddL,hrL,alternative="l",paired=F)$p.value
plt2<-wilcox.test(mddL,ctrlL,alternative="l",paired=F)$p.value
plt3<-wilcox.test(hrL,ctrlL,alternative="l",paired=F)$p.value

#without Holm's correction
PpwWL12lt[i] <- plt1
PpwWL13lt[i] <- plt2
PpwWL23lt[i] <- plt3

#with Holm's correction
Pltadj<-p.adjust(c(plt1,plt2,plt3),method="holm")
PpwWL12ltHC[i] <- Pltadj[1]
PpwWL13ltHC[i] <- Pltadj[2]
PpwWL23ltHC[i] <- Pltadj[3]

Pgtadj<-p.adjust(c(1-plt1,1-plt2,1-plt3),method="holm")
PpwWL12gtHC[i] <- Pgtadj[1]
PpwWL13gtHC[i] <- Pgtadj[2]
PpwWL23gtHC[i] <- Pgtadj[3]

#t-tests
plt1<-t.test(mddL,hrL,alternative="l",paired=F)$p.value
plt2<-t.test(mddL,ctrlL,alternative="l",paired=F)$p.value
plt3<-t.test(hrL,ctrlL,alternative="l",paired=F)$p.value

#without Holm's correction
PpwtL12lt[i] <- plt1
PpwtL13lt[i] <- plt2
PpwtL23lt[i] <- plt3

#with Holm's correction
Pltadj<-p.adjust(c(plt1,plt2,plt3),method="holm")
PpwtL12ltHC[i] <- Pltadj[1]
PpwtL13ltHC[i] <- Pltadj[2]
PpwtL23ltHC[i] <- Pltadj[3]

Pgtadj<-p.adjust(c(1-plt1,1-plt2,1-plt3),method="holm")
PpwtL12gtHC[i] <- Pgtadj[1]
PpwtL13gtHC[i] <- Pgtadj[2]
PpwtL23gtHC[i] <- Pgtadj[3]

#pairwise right comparisons
#wilcoxon tests
plt1<-wilcox.test(mddR,hrR,alternative="l",paired=F)$p.value
plt2<-wilcox.test(mddR,ctrlR,alternative="l",paired=F)$p.value
plt3<-wilcox.test(hrR,ctrlR,alternative="l",paired=F)$p.value

#without Holm's correction
PpwWR12lt[i] <- plt1
PpwWR13lt[i] <- plt2
PpwWR23lt[i] <- plt3

#with Holm's correction
Pltadj<-p.adjust(c(plt1,plt2,plt3),method="holm")
PpwWR12ltHC[i] <- Pltadj[1]
PpwWR13ltHC[i] <- Pltadj[2]
PpwWR23ltHC[i] <- Pltadj[3]

Pgtadj<-p.adjust(c(1-plt1,1-plt2,1-plt3),method="holm")
PpwWR12gtHC[i] <- Pgtadj[1]
PpwWR13gtHC[i] <- Pgtadj[2]
PpwWR23gtHC[i] <- Pgtadj[3]

#t-tests
plt1<-t.test(mddR,hrR,alternative="l",paired=F)$p.value
plt2<-t.test(mddR,ctrlR,alternative="l",paired=F)$p.value
plt3<-t.test(hrR,ctrlR,alternative="l",paired=F)$p.value

#without Holm's correction
PpwtR12lt[i] <- plt1
PpwtR13lt[i] <- plt2
PpwtR23lt[i] <- plt3

#with Holm's correction
Pltadj<-p.adjust(c(plt1,plt2,plt3),method="holm")
PpwtR12ltHC[i] <- Pltadj[1]
PpwtR13ltHC[i] <- Pltadj[2]
PpwtR23ltHC[i] <- Pltadj[3]

Pgtadj<-p.adjust(c(1-plt1,1-plt2,1-plt3),method="holm")
PpwtR12gtHC[i] <- Pgtadj[1]
PpwtR13gtHC[i] <- Pgtadj[2]
PpwtR23gtHC[i] <- Pgtadj[3]

}
#HERE WRITE THE P-VALUES to an external file!
write.matrix(
x<-cbind(PWLR,
PMDDWLRlt,PHRWLRlt,PCtrlWLRlt,
PMDDWLRltHC,PHRWLRltHC,PCtrlWLRltHC,PMDDWLRgtHC,PHRWLRgtHC,PCtrlWLRgtHC,
PtLR,
PMDDtLRlt,PHRtLRlt,PCtrltLRlt,
PMDDtLRltHC,PHRtLRltHC,PCtrltLRltHC,PMDDtLRgtHC,PHRtLRgtHC,PCtrltLRgtHC,
PL1norm,PL2norm,PL3norm,PR1norm,PR2norm,PR3norm,
PmgKWL,PmgKWR,PmgAOV1L,PmgAOV1R,PmgAOV2L,PmgAOV2R,
PpwWL12lt,PpwWL13lt,PpwWL23lt,PpwWR12lt,PpwWR13lt,PpwWR23lt,
PpwWL12ltHC,PpwWL13ltHC,PpwWL23ltHC,PpwWR12ltHC,PpwWR13ltHC,PpwWR23ltHC,
PpwtL12lt,PpwtL13lt,PpwtL23lt,PpwtR12lt,PpwtR13lt,PpwtR23lt,
PpwtL12ltHC,PpwtL13ltHC,PpwtL23ltHC,PpwtR12ltHC,PpwtR13ltHC,PpwtR23ltHC,
PpwWL12gtHC,PpwWL13gtHC,PpwWL23gtHC,PpwWR12gtHC,PpwWR13gtHC,PpwWR23gtHC,
PpwtL12gtHC,PpwtL13gtHC,PpwtL23gtHC,PpwtR12gtHC,PpwtR13gtHC,PpwtR23gtHC,
PmghovL,PmghovR,
PpwhovL12lt,PpwhovL13lt,PpwhovL23lt,PpwhovR12lt,PpwhovR13lt,PpwhovR23lt,
PpwhovL12ltHC,PpwhovL13ltHC,PpwhovL23ltHC,PpwhovR12ltHC,PpwhovR13ltHC,PpwhovR23ltHC,
PpwhovL12gtHC,PpwhovL13gtHC,PpwhovL23gtHC,PpwhovR12gtHC,PpwhovR13gtHC,PpwhovR23gtHC),
file="/cis/home/epostell/Projects/def_scz/pt_censor/censored_pooled_results_pt_HC_1106.txt")

print(paste("elapsed time is =", proc.time()[1]-ptm))
