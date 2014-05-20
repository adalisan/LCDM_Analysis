#PlotsCensorDist.r
# 09/16/04
#plots of the censored distances for the Twin CON data
pdf( file = "Censor_plots1107.pdf", height = 10, width = 16)
set.seed(1)


# 1 == "CON"
# 2 == "SCZ"
# 3 == "BPD"

ptm<-proc.time()[1]

pval<-read.table("censored_pooled_results_pt_HC_1106.txt", header=T)

attach(pval)

lcd<-nrow(pval)
cd<-seq(0,length=lcd,by=.01)

#####################################
#plots for tests of normality 
#####################################
#postscript("PvalnormalL.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PL1norm,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",col=1,lwd=3,
main="normality of left censored distances (by group)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")))
lines(cd,PL2norm,lty=2,col=2,lwd=3)
lines(cd,PL3norm,lty=3,col=4,lwd=3)
abline(h=c(.05))
legend(3,0.2,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c("HealthyL","DefSczL","NonDefSczL"))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalnormalR.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PR1norm,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="normality of right censored distances (by group)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
lines(cd,PR2norm,lty=2,col=2,lwd=3)
lines(cd,PR3norm,lty=3,col=4,lwd=3)
abline(h=c(.05))
legend(3,0.2,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c("HealthyR","DefSczR","NonDefSczR"))
par(mfrow=c(1,1))
#dev.off()

#max p-values for the tests of normality
#     PL1norm      PL2norm      PL3norm      PR1norm      PR2norm      PR3norm
#8.566065e-18 1.084106e-11 1.884463e-35 9.116240e-16 1.310156e-16 4.808166e-27

##################################
#Multi-group comparison plot
##################################

#postscript("PvalmgLR.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5,cex.main=1.5)
plot(cd,PmgKWL,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="K-W tests for multi-group comparisons of\n 
left and right censored distances",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
#lines(cd,PmgKWR,lty=3,col=2,lwd=7)
lines(cd,PmgKWR,lty=3,col=2,lwd=3)
abline(h=.05)
legend(3,0.2,lty=c(1,3),lwd=c(3,3),col=1:2,legend=c("LEFT","RIGHT"))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalmgLR.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5,cex.main=1.5)
plot(cd,PmgAOV1L,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="Multi-group ANOVA F-tests with HOV for \n 
left and right censored distances",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
#lines(cd,PmgAOV1R,lty=3,col=2,lwd=7)
lines(cd,PmgAOV1R,lty=3,col=2,lwd=3)
abline(h=.05)
legend(3,0.2,lty=c(1,3),lwd=c(3,3),col=1:2,legend=c("LEFT","RIGHT"))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalmgLR.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PmgAOV2L,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="Multi-group ANOVA F-tests without HOV for \n 
left and right censored distances",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
lines(cd,PmgAOV2R,lty=3,col=2,lwd=3)
abline(h=.05)
legend(3,0.2,lty=c(1,3),lwd=c(3,3),col=1:2,legend=c("LEFT","RIGHT"))
par(mfrow=c(1,1))
#dev.off()

######################################################
#pairwise comparison plots: WILCOXON TEST FOR LEFT VMPFC
######################################################

#postscript("PvalpwLlt.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwWL13lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="Wilcoxon tests for\n 
left censored distances (Healthy<NonDefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
abline(h=c(.05,.95))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalpwLlt.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwWL23lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="Wilcoxon tests for\n 
left censored distances (DefScz<NonDefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
abline(h=c(.05,.95))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalpwLlt.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwWL12lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="Wilcoxon tests for\n 
left censored distances (Healthy<DefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
#lines(cd,PpwWL13lt,lty=2,col=2,lwd=3)
#lines(cd,PpwWL23lt,lty=3,col=4,lwd=3)
abline(h=c(.05,.95))
#legend(2.0,1,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(CON<SCZ),expression(CON<BPD),expression(SCZ<BPD)))
par(mfrow=c(1,1))
#dev.off()

###############################################
#pairwise comparison plots: t TEST FOR LEFT VMPFC
###############################################

#postscript("PvalpwLlt.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwtL13lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="t-tests for\n 
left censored distances (Healthy<NonDefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
abline(h=c(.05,.95))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalpwLlt.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwtL23lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="t-tests for\n 
left censored distances (DefScz<NonDefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
abline(h=c(.05,.95))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalpwLlt.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwtL12lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="t-tests for\n 
left censored distances (Healthy<DefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
abline(h=c(.05,.95))
par(mfrow=c(1,1))
#dev.off()

######################################################
#pairwise comparison plots: WILCOXON TEST FOR RIGHT VMPFC
######################################################

#postscript("PvalpwRlt.ps",width=8)
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwWR13lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="Wilcoxon tests for\n 
right censored distances (Healthy<NonDefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
abline(h=c(.05,.95))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalpwRlt.ps",width=8)
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwWR23lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="Wilcoxon tests for\n 
right censored distances (DefScz<NonDefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
abline(h=c(.05,.95))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalpwRlt.ps",width=8)
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwWR12lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="Wilcoxon tests for\n 
right censored distances (Healthy<DefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
#lines(cd,PpwWR13lt,lty=2,col=2,lwd=3)
#lines(cd,PpwWR23lt,lty=3,col=4,lwd=3)
abline(h=c(.05,.95))
#legend(3.5,.3,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(CON<SCZ),expression(CON<BPD),expression(SCZ<BPD)))
par(mfrow=c(1,1))
#dev.off()

###############################################
#pairwise comparison plots: t TEST FOR LEFT VMPFC
###############################################

par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwtR13lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="t-tests for\n 
right censored distances (Healthy<NonDefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
abline(h=c(.05,.95))
par(mfrow=c(1,1))
#dev.off()

par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwtR23lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="t-tests for\n 
right censored distances (DefScz<NonDefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
abline(h=c(.05,.95))
par(mfrow=c(1,1))
#dev.off()

par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwtR12lt,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="t-tests for\n 
right censored distances (Healthy<DefScz)",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
#lines(cd,PpwtR13lt,lty=2,col=2,lwd=3)
#lines(cd,PpwtR23lt,lty=3,col=4,lwd=3)
abline(h=c(.05,.95))
#legend(3.5,.3,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(CON<SCZ),expression(CON<BPD),expression(SCZ<BPD)))
par(mfrow=c(1,1))
#dev.off()

#pairwise comparison plots
#postscript("PvalpwLlt.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwWL12ltHC,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="pairwise Wilcoxon tests for\n 
left censored distances (<) with Holm's correction",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
lines(cd,PpwWL13ltHC,lty=2,col=2,lwd=3)
lines(cd,PpwWL23ltHC,lty=3,col=4,lwd=3)
abline(h=c(.05))
legend(3,0.2,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(Healthy<DefScz),expression(Healthy<NonDefScz),expression(DefScz<NonDefScz)))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalpwLlt.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwtL12ltHC,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="pairwise t-tests for\n 
left censored distances (<) with Holm's correction",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
lines(cd,PpwtL13ltHC,lty=2,col=2,lwd=3)
lines(cd,PpwtL23ltHC,lty=3,col=4,lwd=3)
abline(h=c(.05))
legend(3,0.2,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(Healthy<DefScz),expression(Healthy<NonDefScz),expression(DefScz<NonDefScz)))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalpwLgt.ps",width=8)
#par(pty="s")
#main="pairwise Wilcoxon tests for\n left censored distances (>) with Holm's correction"
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwWL12gtHC,lty=1, xlim=(c(0,4)), ylim=c(0,1),xaxt="n",yaxt="n",type="l",lwd=3,
main="pairwise Wilcoxon tests for\n 
left censored distances (>) with Holm's correction",
xlab="Censoring Distance (mm)", ylab=expression(paste(P,"-value")),col=1)
lines(cd,PpwWL13gtHC,lty=2,col=2,lwd=3)
lines(cd,PpwWL23gtHC,lty=3,col=4,lwd=3)
box(lwd=2)
abline(h=c(.05),lwd=1.5)
legend(3,0.2,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(Healthy>DefScz),expression(Healthy>NonDefScz),expression(DefScz>NonDefScz)),cex=1.5)
par(mfrow=c(1,1))
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
#dev.off()

#postscript("PvalpwLgt.ps",width=8)
#par(pty="s")
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwtL12gtHC,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="pairwise t-tests for\n 
left censored distances (>) with Holm's correction",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
lines(cd,PpwtL13gtHC,lty=2,col=2,lwd=3)
lines(cd,PpwtL23gtHC,lty=3,col=4,lwd=3)
abline(h=c(.05))
legend(3,0.2,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(Healthy>DefScz),expression(Healthy>NonDefScz),expression(DefScz>NonDefScz)))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalpwRlt.ps",width=8)
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwWR12ltHC,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="pairwise Wilcoxon tests for\n 
right censored distances (<) with Holm's correction",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
lines(cd,PpwWR13ltHC,lty=2,col=2,lwd=3)
lines(cd,PpwWR23ltHC,lty=3,col=4,lwd=3)
abline(h=c(.05))
legend(3,0.2,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(Healthy<DefScz),expression(Healthy<NonDefScz),expression(DefScz<NonDefScz)))
par(mfrow=c(1,1))
#dev.off()

par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwtR12ltHC,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="pairwise t-tests for\n 
right censored distances (<) with Holm's correction",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
lines(cd,PpwtR13ltHC,lty=2,col=2,lwd=3)
lines(cd,PpwtR23ltHC,lty=3,col=4,lwd=3)
abline(h=c(.05))
legend(3,0.2,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(Healthy<DefScz),expression(Healthy<NonDefScz),expression(DefScz<NonDefScz)))
par(mfrow=c(1,1))
#dev.off()

#postscript("PvalpwRgt.ps",width=8)
par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwWR12gtHC,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="pairwise Wilcoxon tests for\n 
right censored distances (>) with Holm's correction",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
lines(cd,PpwWR13gtHC,lty=2,col=2,lwd=3)
lines(cd,PpwWR23gtHC,lty=3,col=4,lwd=3)
abline(h=c(.05))
legend(3,0.2,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(Healthy>DefScz),expression(Healthy>NonDefScz),expression(DefScz>NonDefScz)))
par(mfrow=c(1,1))
#dev.off()

par(mfrow=c(1,1),cex.lab=1.5, cex.main=1.5)
plot(cd,PpwtR12gtHC,lty=1, xlim=(c(0,4)), ylim=c(0,1),type="l",lwd=3,
main="pairwise t-tests for\n 
right censored distances (>) with Holm's correction",
xlab="censoring distance (mm)", ylab=expression(paste(p,"-value")),col=1)
lines(cd,PpwtR13gtHC,lty=2,col=2,lwd=3)
lines(cd,PpwtR23gtHC,lty=3,col=4,lwd=3)
abline(h=c(.05))
legend(3,0.2,lty=c(1,2,3),lwd=c(3,3,3),col=c(1,2,4),legend=c(expression(Healthy>DefScz),expression(Healthy>NonDefScz),expression(DefScz>NonDefScz)))
par(mfrow=c(1,1))
#dev.off()

dev.off()
