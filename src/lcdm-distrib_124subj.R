#modify here

library(ggplot2)


#QC 10 subjects at a time
#LENGTH=124

READPATH<-c()
patient_id.defscz<-read.table("./data/defscz.txt") #file that contains file names or path names of the subjects
patient_id.healthy<-read.table("./data/healthy.txt")
patient_id.nondefscz<-read.table("./data/nondefscz.txt")

patient_id_status_lookup<-combine(patient_id.defscz,patient_id.healthy,patient_id.nondefscz,names=c("defscz","healthy","nondefscz"))
LENGTH <- nrow(patient_id_status_lookup)


side <- 'lh'
REGION <- "pt"
region_tag <- "" #"pt"
tag1 <- '_EP'
tag2 <- '_restrict'
for (i in 1:LENGTH){
    READPATH[i]<-paste("./data/pt_antsy_082013/restricted_qc/",
                       patient_id_status_lookup[i,1],"_",side,region_tag,"_AntsyGrey",
                       tag1,tag2,".txt",sep="") #create path of individual files here.
}

side <- 'rh'
for (i in (1:LENGTH)+LENGTH){
  READPATH[i]<-paste("./data/pt_antsy_082013/restricted_qc/",
                     patient_id_status_lookup[i-LENGTH,1],"_",side,region_tag,"_AntsyGrey",
                     tag1,tag2,".txt",sep="") #create path of individual files here.
}



lcdm.data<-list()
#titles and labels on the plot
lcdm.data.merged<-data.frame(measure=c(),patient_id=c(),patient_status=c())
for (i in 1:(2*LENGTH)){
  j =  1+((i-1)%%LENGTH)
  print(paste0("patient_id_status_lookup[i,1]",patient_id_status_lookup[j,1]))
  lcdm.data[[j]]<-scan(READPATH[i])
  lcdm.data.merged <- rbind(lcdm.data.merged,data.frame(measure= lcdm.data[[j]],patient_id=patient_id_status_lookup[j,1],
                                                        patient_status=patient_id_status_lookup[j,2],side=ifelse(i>LENGTH,"rh","lh") )
  )
}

lcdm.data.merged$patient_id <- as.factor(lcdm.data.merged$patient_id)
#hist(x1, 100, freq=FALSE, col="white", border="white", xlim=c(-2,8), ylim=c(0.0,0.6),main="PT",xlab="Distance (mm)",ylab="Probability Density")
par(new=T)
pdf( file = paste0("./graphs/LCDM_distrib_",REGION,"_","all.pdf"), height = 10, width = 16)
ggplot(aes(x=measure, colour=patient_id,alpha= 0.5),data=lcdm.data.merged)+geom_density(alpha=0.5,  data=lcdm.data.merged)+facet_grid(patient_status~side)+guides(colour=FALSE)

dev.off()
