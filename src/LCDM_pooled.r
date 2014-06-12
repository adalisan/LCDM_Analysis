#arg6=region
#arg7=number of MCI
#arg8=number of normal

args=commandArgs()

REGION="pt"
LCDM <- function(fileNames,condTests,REGION){
#DATA_NUMB<-c(as.integer(args[7]),as.integer(args[8]))

### Pre-Processing ###
library(MASS)
b<-list(); dists<-c(); distsl<-c();
for (i in 1:length(fileNames)){ 
    distsl[i]<-read.table(fileNames[i])
    distsl[[i]]=distsl[[i]][distsl[[i]]>-2] 
    distsl[[i]]=distsl[[i]][distsl[[i]]<8] 
     }
minlength=min(as.numeric(lapply(distsl,length)))
dists=sapply(distsl,sample,minlength) #for tests requiring same number in group

######### Simple Summary Statistics ########

pdf(file=paste("output/summarystat_",REGION,".pdf",sep=''))

col=c('black','red','blue')
thk95<-c()
thk99<-c()
vol95<-c()
vol99<-c()
for(i in 1:length(fileNames)){ 
	if (i==1) 
           hist(distsl[[i]], 100, freq=FALSE, col="white", border="white", xlim=c(-4,10), ylim=c(0,.4),main=NA,xlab="Distance (mm)",ylab="Probability Density")
    	   par(new=T);
	dens1=density(distsl[[i]])
    	lines(dens1, lwd=1, col=col[i])
	thk95[i]=quantile(distsl[[i]],.95)
	thk99[i]=quantile(distsl[[i]],.99)
	}
legend(6, .3, paste(c('healthy','nondef','def')), cex=1,  col= c('black','red','blue'),lwd=2, bty="n");

for(i in 1:length(fileNames)){ 
	cdf1=ecdf(round(distsl[[i]],3))
	if (i==1) 
	   {
	   plot(cdf1, verticals=TRUE, xlim=c(-2,7),do.points=F, lwd=.5, col=col[i], main=NA,xlab="Distance (mm)", ylab="Cumulative Probability Density")
    	   par(new=T);
	   }		
	else { lines(cdf1, lwd=.5, col=col[i]) }
	}
legend(5, .3, paste(c('healthy','nondef','def'),sep=", "), cex=1,  col= c('black','red','blue'),lwd=2, bty="n");

dev.off()

########## 2-group tests ###########

## Mann-Whitney Test (Wilcoxon Rank Sum) ##
if(condTests[1]==TRUE){
b[[1]]<-wilcox.test(distsl[[2]],distsl[[1]],alternative="t",exact = FALSE,correct = FALSE)
b[[2]]<-wilcox.test(distsl[[2]],distsl[[1]],alternative="g",exact = FALSE,correct = FALSE)
b[[3]]<-wilcox.test(distsl[[2]],distsl[[1]],alternative="l",exact = FALSE,correct = FALSE)
}

## Mann-Whitney Test (Wilcoxon Rank Sum) ##
if(condTests[1]==TRUE){
b[[4]]<-wilcox.test(distsl[[3]],distsl[[1]],alternative="t",exact = FALSE,correct = FALSE)
b[[5]]<-wilcox.test(distsl[[3]],distsl[[1]],alternative="g",exact = FALSE,correct = FALSE)
b[[6]]<-wilcox.test(distsl[[3]],distsl[[1]],alternative="l",exact = FALSE,correct = FALSE)
}
## Mann-Whitney Test (Wilcoxon Rank Sum) ##
if(condTests[1]==TRUE){
b[[7]]<-wilcox.test(distsl[[3]],distsl[[2]],alternative="t",exact = FALSE,correct = FALSE)
b[[8]]<-wilcox.test(distsl[[3]],distsl[[2]],alternative="g",exact = FALSE,correct = FALSE)
b[[9]]<-wilcox.test(distsl[[3]],distsl[[2]],alternative="l",exact = FALSE,correct = FALSE)
}

## Kolmogorov-Smirnov Test (KS Test) ##
if(condTests[2]==TRUE){
b[[10]]<-ks.test(distsl[[2]],distsl[[1]],alternative="t",exact=NULL)
b[[11]]<-ks.test(distsl[[2]],distsl[[1]],alternative="l",exact=NULL)
b[[12]]<-ks.test(distsl[[2]],distsl[[1]],alternative="g",exact=NULL)
}

## Kolmogorov-Smirnov Test (KS Test) ##
if(condTests[2]==TRUE){
b[[13]]<-ks.test(distsl[[3]],distsl[[1]],alternative="t",exact=NULL)
b[[14]]<-ks.test(distsl[[3]],distsl[[1]],alternative="l",exact=NULL)
b[[15]]<-ks.test(distsl[[3]],distsl[[1]],alternative="g",exact=NULL)
}

## Kolmogorov-Smirnov Test (KS Test) ##
if(condTests[2]==TRUE){
b[[16]]<-ks.test(distsl[[3]],distsl[[2]],alternative="t",exact=NULL)
b[[17]]<-ks.test(distsl[[3]],distsl[[2]],alternative="l",exact=NULL)
b[[18]]<-ks.test(distsl[[3]],distsl[[2]],alternative="g",exact=NULL)
}

## Welch's t Test for Independent Samples ##
if(condTests[3]==TRUE){
b[[19]]<-t.test(distsl[[2]],distsl[[1]],alternative = "t",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
b[[20]]<-t.test(distsl[[2]],distsl[[1]],alternative = "g",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
b[[21]]<-t.test(distsl[[2]],distsl[[1]],alternative = "l",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
}

## Welch's t Test for Independent Samples ##
if(condTests[3]==TRUE){
b[[22]]<-t.test(distsl[[3]],distsl[[1]],alternative = "t",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
b[[23]]<-t.test(distsl[[3]],distsl[[1]],alternative = "g",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
b[[24]]<-t.test(distsl[[3]],distsl[[1]],alternative = "l",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
}


## Welch's t Test for Independent Samples ##
if(condTests[3]==TRUE){
b[[25]]<-t.test(distsl[[3]],distsl[[2]],alternative = "t",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
b[[26]]<-t.test(distsl[[3]],distsl[[2]],alternative = "g",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
b[[27]]<-t.test(distsl[[3]],distsl[[2]],alternative = "l",mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95)
}

return(b)
}
file1=paste("./data/pooled_files/",REGION,"_healthy_wave1_pooled_antsy.txt",sep='')
file2=paste("./data/pooled_files/",REGION,"_nondefscz_wave1_pooled_antsy.txt",sep='')
file3=paste("./data/pooled_files/",REGION,"_defscz_wave1_pooled_antsy.txt",sep='')

fileNames=c(file1,file2,file3)

b <- c("Kolmogorov-Smirnov Test","Welch's t-test","Mann Whitney U Test")
condTests<-c()
condTests[1]="Mann Whitney U Test"%in%b
condTests[2]="Kolmogorov-Smirnov Test"%in%b
condTests[3]="Welch's t-test"%in%b
condTests[4]="Kruskal-Wallis Test"%in%b
condTests[5]="ANOVA F-test"%in%b
b<-condTests
out<-LCDM(fileNames,condTests,REGION)


### write results into file ###

OUTFILE=paste("./results/LCDM_pooled_test_results_",REGION,".txt",sep='')

cat("Statistical Summary for ",REGION,"\n",file=OUTFILE)

if(length(out)!=0)
	{
	if(b[1]){
		cat("p-value for MWU Test, norm-ndef (two-sided, greater, less): \t",out[[1]]$p.value,"\t",out[[2]]$p.value,"\t",out[[3]]$p.value,"\n",file=OUTFILE,append=TRUE)
		cat("p-value for MWU Test, norm-defs (two-sided, greater, less): \t",out[[4]]$p.value,"\t",out[[5]]$p.value,"\t",out[[6]]$p.value,"\n",file=OUTFILE,append=TRUE)
		cat("p-value for MWU Test, ndef-defs (two-sided, greater, less): \t",out[[7]]$p.value,"\t",out[[8]]$p.value,"\t",out[[9]]$p.value,"\n",file=OUTFILE,append=TRUE)
		}

	if(b[2]){
		cat("p-value for KS Test, norm-ndef (two-sided, less, greater): \t", out[[10]]$p.value,"\t",out[[11]]$p.value,"\t",out[[12]]$p.value,"\n",file=OUTFILE,append=TRUE)
		cat("p-value for KS Test, norm-defs (two-sided, less, greater): \t",out[[13]]$p.value,"\t",out[[14]]$p.value,"\t",out[[15]]$p.value,"\n",file=OUTFILE,append=TRUE)
		cat("p-value for KS Test, ndef-defs (two-sided, less, greater): \t",out[[16]]$p.value,"\t",out[[17]]$p.value,"\t",out[[18]]$p.value,"\n",file=OUTFILE,append=TRUE)
#		cat("D for KS Test (two-sided, less, greater): \t", out[[4]]$statistic,"\t",out[[5]]$statistic,"\t",out[[6]]$statistic,"\n",file=OUTFILE,append=TRUE)
		}
	if(b[3]){
		cat("p-value for Welch's t-test, norm-ndef (two-sided, greater, less): \t", out[[19]]$p.value,"\t",out[[20]]$p.value,"\t",out[[21]]$p.value,"\n",file=OUTFILE,append=TRUE)
		cat("p-value for Welch's t-test, norm-defs (two-sided, greater, less): \t", out[[22]]$p.value,"\t",out[[23]]$p.value,"\t",out[[24]]$p.value,"\n",file=OUTFILE,append=TRUE)
		cat("p-value for Welch's t-test, ndef-defs (two-sided, greater, less): \t", out[[25]]$p.value,"\t",out[[26]]$p.value,"\t",out[[27]]$p.value,"\n",file=OUTFILE,append=TRUE)
		}
	if(b[4]){
		cat("p-value for KW Test: ", out[[10]]$p.value,"\n",file=OUTFILE,append=TRUE)
		}
	if(b[5]){
		cat("p-value for ANOVA F Test: ", out[[11]]$p.value,"\n",file=OUTFILE,append=TRUE)
		}
	} else { cat("No test was performed",file=OUTFILE,append=TRUE)}
