#pooled LCDM statistics analysis
LCDM <- function(distsl,condTests){
  
  
  ### Pre-Processing ###
  library(MASS)
  b<-list(); dists<-c(); 
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
  {
    
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


p.cutoff <-0.001

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


lh.pooled.LCDM.files <- c("lh_healthy.txt","lh_nondefscz.txt","lh_defscz.txt")
rh.pooled.LCDM.files <- c("rh_healthy.txt","rh_nondefscz.txt","rh_defscz.txt")



lh.pooled.LCDM.files <- paste("./data/",lh.pooled.LCDM.files,sep="")
rh.pooled.LCDM.files <- paste("./data/",rh.pooled.LCDM.files,sep="")


contrast.groups<-array(c(1,2,3,2,3,1,1,3,2),dim=c(3,3))
contrast.groups<- t(contrast.groups)
sides<- c('lh','rh')
test.names <- c("MWU Test" ,  "KS Test Two-sided",  "KS Test Greater" ,
                "KS Test Less", "Welch's t-test", "KW Test", "ANOVA F Test")

out <- list()
for (it in 1:3) {
  for (side in sides){
    distsl<-list();
    fileNames<- rh.pooled.LCDM.files[contrast.groups[it,]]
    for (i in 1:length(fileNames)){ 
      distsl[[i]]<-scan(fileNames[i]) 
    }
    if (side=='lh'){
      out<-LCDM(distsl,condTests)
      
    }
    else{
      out<-LCDM(distsr,condTests)
      
    }    
        
    ### write results into file ###
    #for (test.res in out){
    #  if (test.res$p.value<1E-5)
    #}
    
    fname<- paste0("./results/pt_pooled/",side,"_",it,".txt")
    
    if(length(out)!=0){      
      for (i in 1:length(test.names)){
        test = test.names[i]
        p.val = out[[i]]$p.value
        #echo $results.txt
        if(b[i]){
          cat("p-value for ",test, ": ",p.val,"\n",file=fname,append=(i>1))
          if(!is.null(p.val) && p.val<p.cutoff) cat("**Significant \n",file=fname,append=TRUE)
        }
      }
      
      #Translating directly by unlisting
      #if(b[1]){cat("Result for MWU Test: ",unlist(lh.out[[1]]),"\n",file="result.txt")}
      #if(b[2]){cat("\nResult for KS Test: ",unlist(lh.out[[2]]),"\n",file="result.txt",append=TRUE)}
      #if(b[3]){cat("\nResult for Welch's t-test: ",unlist(lh.out[[3]]),"\n",file="result.txt",append=TRUE)}
      #if(b[4]){cat("\nResult for KW Test: ",unlist(lh.out[[4]]),"\n",file="result.txt",append=TRUE)}
      #if(b[5]){cat("\nResult for ANOVA F Test: ",unlist(lh.out[[5]]),"\n",file="result.txt",append=TRUE)}
    } else { 
      cat("No test was performed",file=fname,append=TRUE)
    }
    
  }
}