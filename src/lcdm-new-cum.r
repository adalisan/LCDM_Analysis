pdf( file = "Pooled_Right_PT_cdf.pdf", height = 10, width = 16)

#QC 10 subjects at a time
 LENGTH=3

READPATH<-c()
names<-read.table("diag.txt") #file that contains file names or path names of the subjects

for (i in 1:LENGTH){
    READPATH[i]<-paste("/cis/home/epostell/Projects/def_scz/pt_pooled/rh_",
        names[[1]][i],".txt",sep="") #create path of individual files here.
}

#titles and labels on the plot

x1=scan(READPATH[1])
x2=scan(READPATH[2])
x3=scan(READPATH[3])
# x4=scan(READPATH[4])


histx1 <- density(x1)
histx2 <- density(x2)
histx3 <- density(x3)
# histx4 <- density(x4)


ylimn <- range(c(histx1$y,histx2$y,histx3$y))


#hist(x1, 100, freq=FALSE, col="white", border="white", xlim=c(-2,12), ylim=ylimn,main="M1 LEFT PT",xlab="Distance",ylab="Density")
#par(new=T)

#lines(histx1, lwd=2, col='red')
#lines(histx2, lwd=2, col='blue')
#lines(histx3, lwd=2, col='orange')
#lines(histx4, lwd=2, col='green')
#lines(histx5, lwd=2, col='gray')
#lines(histx6, lwd=2, col='pink')
#lines(histx7, lwd=2, col=70)
#lines(histx8, lwd=2, col=5)
#lines(histx9, lwd=2, col='yellow')
#lines(histx10, lwd=2, col='brown')

plot(histx1$x,cumsum(histx1$y)/sum(histx1$y),type="l",lwd=2,col="blue",xlim=c(-5,12),main="Pooled Right PT", xlab="Distance",ylab="Cumulation")
points(histx2$x,cumsum(histx2$y)/sum(histx2$y),type="l",lwd=2,col="orange")
points(histx3$x,cumsum(histx3$y)/sum(histx3$y),type="l",lwd=2,col="green")
# points(histx4$x,cumsum(histx4$y)/sum(histx4$y),type="l",lwd=2,col="blue")



legend(6,0.6, c("Healthy","DefScz","NonDefScz"), cex=2,  col= c('blue','orange','green'),lwd=2, bty="n");

dev.off()
