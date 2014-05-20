
#input command line arguments
args=commandArgs()
FILENAME="defscz.txt"
#REGION=args[7]
DATA_DIR="/cis/home/epostell/Projects/def_scz/pt_antsy_082013/restricted_qc/output/"
SUBJECT_DIR="/cis/home/epostell/Projects/def_scz/pt_antsy_082013/restricted_qc"
VOXEL_SIZE=0.5 #mm^3


#name of output file
pdf( file = paste(DATA_DIR,"lh_pt_defscz.pdf",sep=""), height = 10, width = 16)

#calculating data number and number of histgrams to make given there are a max of 10 datasets per histogram
DATANUMB=nrow(read.table(FILENAME))
HISTNUMB=ceiling(DATANUMB/10)
SKIP<-c(seq(0,HISTNUMB*10,10))

for (n in 1:HISTNUMB) {
    READPATH<-c()
    names<-read.table(FILENAME,nrows=10,skip=SKIP[n]) #file that contains file names or path names of the subjects
    ROWS=nrow(na.omit(names))
    col= c('red','blue','orange','green','gray','pink',70,5,'yellow','brown')
    thk95<-c()
    thk99<-c()
    vol95<-c()
    vol99<-c()
    for (i in 1:ROWS){
    	fn1=paste(SUBJECT_DIR,"/",names[[1]][i],"_lh_AntsyGrey_EP_restrict.txt",sep="") 
	#fn2=paste(SUBJECT_DIR,names[[1]][i],"/",REGION,"_1_quartile_pialmsk_AntsyGrey.txt",sep="")
    	#if ( file.exists(fn1) )
	   xdata=scan(fn1)
	#else xdata=scan(fn2)
	if (i==1) 
#           hist(xdata, 100, freq=FALSE, col="white", border="white", xlim=c(-2,12), ylim=c(0,1),xlab="Distance",ylab="Density")
           hist(xdata, prob=T, 100, freq=FALSE, col="white", border="white", main="Lh pt", xlim=c(-2,12), ylim=c(0,0.5),xlab="Distance",ylab="Voxels")
    	   par(new=T);

 	dens1=density(xdata)
    	lines(dens1$x,dens1$y, lwd=2, col=col[i])
#       lines(dens1$x,dens1$y*length(xdata), lwd=2, col=col[i])
	thk95[i]=quantile(xdata,.95)
	thk99[i]=quantile(xdata,.99)
	vol95[i]=length(xdata[xdata<thk95[i]])*VOXEL_SIZE^3
	vol99[i]=length(xdata[xdata<thk99[i]])*VOXEL_SIZE^3
    }
    legend(6, 0.3, paste(c('ID',t(names[1])),c(' thk95',round(thk95,4)),c(' thk99',round(thk99,4)),c(' vol95',vol95),c(' vol99',vol99),sep=", "), cex=1,  col= c('white','red','blue','orange','green','gray','pink',70,5,'yellow','brown'),lwd=2, bty="n");
    stats=data.frame(IDS=names$V1,thk95=thk95,thk99=thk99,vol95=vol95,vol99=vol99)
    if (n==1) 
       lcdmstats=stats
    else 
       lcdmstats<-rbind(lcdmstats,stats)
}
write.table(lcdmstats, file=paste(DATA_DIR,"lh_pt_defscz_lcdmstats.txt",sep=""), row.names=F)

dev.off()
