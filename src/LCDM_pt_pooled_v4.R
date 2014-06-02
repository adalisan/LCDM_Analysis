

LCDM <- function(fileNames,condTests){
  
  
  #fileNames=c("data/s12349_lant_cing_AntsyGrey.txt","data/s12360_lant_cing_AntsyGrey.txt","data/s12497_lant_cing_AntsyGrey.txt")
  #fileNames=c("lh_defscz.txt","lh_nondefscz.txt")
  b1=TRUE; b2=TRUE; b3=TRUE; b4=TRUE; b5=TRUE; b6=TRUE; b7=TRUE;
  condTests<-c(b1,b2,b3,b4,b5,b6,b7)
  
  ### Pre-Processing ###
  library(MASS)
  b<-list(); dists<-c(); distsl<-c();
  
  for (i in 1:length(fileNames)){ 
    distsl[i]<-read.table(fileNames[i]) 
  }
  minlength=min(as.numeric(lapply(distsl,length)))
  dists=sapply(distsl,sample,minlength) #for tests requiring same number in group
  
  # ######### Simple Summary Statistics ########
  # 
  # pdf(file="summarystat.pdf")
  # if(length(fileNames)==2){boxplot(dists[,1],dists[,2])}
  # if(length(fileNames)==3){boxplot(dists[,1],dists[,2],dists[,3])}
  # for(i in 1:length(fileNames)){ hist(dists[,i],
  # main = paste("Histogram of Data", i),) } 
  # dev.off()
  
  ########## 2-group tests ###########
  if(ncol(dists)==2){
    
    ## Mann-Whitney Test (Wilcoxon Rank Sum) ##
    if(condTests[1]==TRUE){
      b[[1]]<-wilcox.test(distsl[[1]],distsl[[2]],alternative="two.sided",exact = FALSE, 
                          correct = FALSE)
    }
    
    ## Kolmogorov-Smirnov Test (KS Test) ##
    if(condTests[2]==TRUE){
      b[[2]]<-ks.test(distsl[[1]],distsl[[2]],alternative="two.sided",exact=NULL)
    }
    
    ## Kolmogorov-Smirnov Test (KS Test) ##
    if(condTests[3]==TRUE){
      b[[3]]<-ks.test(distsl[[1]],distsl[[2]],alternative="greater",exact=NULL)
    }
    
    ## Kolmogorov-Smirnov Test (KS Test) ##
    if(condTests[4]==TRUE){
      b[[4]]<-ks.test(distsl[[1]],distsl[[2]],alternative="less",exact=NULL)
    }
    
    ## Welch's t Test for Independent Samples ##
    if(condTests[5]==TRUE){
      b[[5]]<-t.test(distsl[[1]],distsl[[2]],alternative = "two.sided",
                     mu = 0, paired = FALSE, var.equal = FALSE,
                     conf.level = 0.95)
    }
  }
  
  ########## Multi-group tests ###########
  
  
  
  ## Kruskal-Wallis one-way ANOVA (KW Test) ##
  if(condTests[6]==TRUE){
    b[[6]]<-kruskal.test(distsl)
  }
  
  ## ANOVA F-test ##
  if(condTests[7]==TRUE){
    g<-rep(1:length(distsl),sapply(distsl,length))
    distsv<-unlist(distsl)
    b[[7]]<-oneway.test(distsv~g,var.equal=FALSE)
  }
  
  return(b)
}

#fnx=c("data/s12349_lant_cing_AntsyGrey.txt","data/s12360_lant_cing_AntsyGrey.txt","data/s12497_lant_cing_AntsyGrey.txt")
#fnx=c("data/CDR00_master.txt","data/CDR05_master.txt")

b <- c("Mann Whitney U Test","Kolmogorov-Smirnov Test Two-sided","Kolmogorov-Smirnov Test Greater","Kolmogorov-Smirnov Test Less","Welch's t-test","Kruskal-Wallis Test","ANOVA F-test")
condTests<-c()
condTests[1]="Mann Whitney U Test"%in%b
condTests[2]="Kolmogorov-Smirnov Test Two-sided"%in%b
condTests[3]="Kolmogorov-Smirnov Test Greater"%in%b
condTests[4]="Kolmogorov-Smirnov Test Less"%in%b
condTests[5]="Welch's t-test"%in%b
condTests[6]="Kruskal-Wallis Test"%in%b
condTests[7]="ANOVA F-test"%in%b
b<-condTests

lh.pooled.LCDM.files <- c("lh_defscz.txt","lh_nondefscz.txt","lh_healthy")
rh.pooled.LCDM.files <- c("rh_defscz.txt","rh_nondefscz.txt","rh_healthy")

contrast.groups<-array(c(1,2,3,2,3,1,3,1,2),dim=c(3,3))
sides<- c('lh','rh')

for (it in 1:3) 
lh.out<-LCDM(lh.pooled.LCDM.files[contrast.groups[it,]],condTests)
rh.out<-LCDM(rh.pooled.LCDM.files[contrast.groups[it,]],condTests)


### write results into file ###
#for (test.res in out){
#  if (test.res$p.value<1E-5)
#}


for (side in sides){
  if(length(out)!=0){
    if (side=='lh'){
      out <- lh.out
    }
    else{
      out <- rh.out
    }
        #echo $results.txt
        if(b[1]){cat("p-value for MWU Test: ", out[[1]]$p.value,"\n",file="./results/pt_pooled/lh_deno.txt")}
      if(b[2]){cat("p-value for KS Test Two-sided: ", out[[2]]$p.value,"\n",file="./results/pt_pooled/lh_deno.txt",append=TRUE)}
      if(b[3]){cat("p-value for KS Test Greater: ", out[[3]]$p.value,"\n",file="./results/pt_pooled/lh_deno.txt",append=TRUE)}
      if(b[4]){cat("p-value for KS Test Less: ", out[[4]]$p.value,"\n",file="./results/pt_pooled/lh_deno.txt",append=TRUE)}
      if(b[5]){cat("p-value for Welch's t-test: ", out[[5]]$p.value,"\n",file="./results/pt_pooled/lh_deno.txt",append=TRUE)}
      if(b[6]){cat("p-value for KW Test: ", out[[6]]$p.value,"\n",file="./results/pt_pooled/lh_deno.txt",append=TRUE)}
      if(b[7]){cat("p-value for ANOVA F Test: ", out[[7]]$p.value,"\n",file="./results/pt_pooled/lh_deno.txt",append=TRUE)}
      
      #Translating directly by unlisting
      #if(b[1]){cat("Result for MWU Test: ",unlist(lh.out[[1]]),"\n",file="result.txt")}
      #if(b[2]){cat("\nResult for KS Test: ",unlist(lh.out[[2]]),"\n",file="result.txt",append=TRUE)}
      #if(b[3]){cat("\nResult for Welch's t-test: ",unlist(lh.out[[3]]),"\n",file="result.txt",append=TRUE)}
      #if(b[4]){cat("\nResult for KW Test: ",unlist(lh.out[[4]]),"\n",file="result.txt",append=TRUE)}
      #if(b[5]){cat("\nResult for ANOVA F Test: ",unlist(lh.out[[5]]),"\n",file="result.txt",append=TRUE)}
    } else { 
      cat("No test was performed",file="result.txt",append=TRUE)
    }
  
}