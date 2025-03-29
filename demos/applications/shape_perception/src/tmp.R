png(file = "../../figures/shapeEx_postpred_AccRate.png", width = 7, height = 5, units="in",res=300) 
  layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
  layout.show(n = 5)
  par(pty="m", mai=c(0.45,0.5,0.4,0), oma= c(0,0,0,0.5), bg = NA)
  plot_ppAcc(pp_accRate, ezdata)
dev.off()


png(file = "../../figures/shapeEx_postpred_meanRT.png", width = 7, height = 5, units="in",res=300) 
  layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
  par(pty="m", mai=c(0.45,0.5,0.25,0), oma= c(0,0,0,0.5), bg = NA)
  plot_ppMeanRT(pp_meanRT, ezdata)
dev.off()

png(file = "../../figures/shapeEx_postpred_varRT.png", width = 7, height = 5, units="in",res=300) 
  layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
  par(pty="m", mai=c(0.45,0.5,0.25,0), oma= c(0,0,0,0.5), bg = NA)
  plot_ppvarRT(pp_meanRT, ezdata)
dev.off()


png(file = "../../figures/shapeEx_postpred_Full.png", width = 8, height = 11, units="in",res=300) 
layout(mat = matrix(1:15, ncol=3, byrow=FALSE))
layout.show(n = 15)
par(pty="m", mai=c(0.1,0.5,0,0), oma= c(2.4,3.4,2.5,0.4), bg=NA)


for(k in 1:5){
  plot(4.5, 10, col="white", ylim=c(0,1), xlim=c(0.5,9.5),ann=F,axes=F)
  axis(1,seq(1,9,length.out=9),rep("",9), line=-0.65)
  axis(2,seq(0,1,0.1), seq(0,1,0.1),las=1, cex.axis=0.7)
  for(i in 1:9){
    this.Y <- pp_accRate[,seq(k,ncol(pp_accRate),5)[i]]
    CI <- quantile(this.Y, probs = c(0.025,0.975))
    polygon(x=c(i-0.4,i+0.4,i+0.4,i-0.4),y=c(CI[1],CI[1],CI[2],CI[2]), col="gray85", border="gray70")
    points(i+jitter.x,this.Y, col=myCol(r[,1],g[,1],b[,1],i,0.05), pch=16, cex=0.8)
    points(i, ezdata$acc_rate[which(ezdata$cond==k)][i], pch=8)
  }
  mtext(paste("Condition", k), 2, f=2, line=6, cex=1)
  mtext("Accuracy rate", 2, line=2.1, cex=0.6) 
  if(k==1){
    mtext("Accuracy rate", f=2, line=.5, cex=1)
    mtext("Qualitative change", 2, f=2, line=5, cex=0.75, col="gray50")
    mtext("in Convexity", 2, f=2, line=4.1, cex=0.75, col="gray50")}
  if(k==2){
    mtext("Quantitative change", 2, f=2, line=5, cex=0.75, col="gray50")
    mtext("in Convexity", 2, f=2, line=4.1, cex=0.75, col="gray50")}
  if(k==3){
    mtext("Qualitative change", 2, f=2, line=5, cex=0.75, col="gray50")
    mtext("in Concavity", 2, f=2, line=4.1, cex=0.75, col="gray50")}
  if(k==4){
    mtext("Quantitative change", 2, f=2, line=5, cex=0.75, col="gray50")
    mtext("in Concavity", 2, f=2, line=4.1, cex=0.75, col="gray50")}
  if(k==5){mtext("No change at all", 2, f=2, line=5, cex=0.75, col="gray50")
    mtext("Participant",1,line=1.7,cex=0.8)
    axis(1,seq(1,9,length.out=9),paste("p", 1:9, sep=""), line=-1, tick=FALSE, cex.axis=0.8)}
}


minY <- min(pp_meanRT)
maxY <- max(pp_meanRT)
for(k in 1:5){
  plot(4.5, 10, col="white", ylim=c(minY,maxY), xlim=c(0.5,9.5),ann=F,axes=F)
  axis(1,seq(1,9,length.out=9),rep("",9), line=-0.65)
  axis(2,seq(minY,maxY,length.out=10), round(seq(minY,maxY,length.out=10),1),las=1, cex.axis=0.7)
  for(i in 1:9){
    this.Y <- pp_meanRT[,seq(k,ncol(pp_meanRT),5)[i]]
    CI <- quantile(this.Y, probs = c(0.025,0.975))
    polygon(x=c(i-0.4,i+0.4,i+0.4,i-0.4),y=c(CI[1],CI[1],CI[2],CI[2]), col="gray85", border="gray70")
    points(i+jitter.x,this.Y, col=myCol(r[,2],g[,2],b[,2],i,0.05), pch=16, cex=0.8)
    points(i, ezdata$meanRT[which(ezdata$cond==k)][i], pch=8)
  }
  mtext("Mean RT (secs)", 2, line=2.1, cex=0.6) 
  if(k==1){mtext("Mean RT (secs)", f=2, line=.5, cex=1)}
  if(k==5){mtext("Participant",1,line=1.7,cex=0.8)
           axis(1,seq(1,9,length.out=9),paste("p", 1:9, sep=""), line=-1, tick=FALSE, cex.axis=0.8)}
}


ppl_varRT <- log(pp_varRT)
minY <- min(ppl_varRT)
maxY <- max(ppl_varRT)
for(k in 1:5){
  plot(4.5, 10, col="white", ylim=c(minY,maxY), xlim=c(0.5,9.5),ann=F,axes=F)
  axis(1,seq(1,9,length.out=9),rep("",9), line=-0.65)
  axis(2,seq(minY,maxY,length.out=10), round(seq(minY,maxY,length.out=10),1),las=1, cex.axis=0.7)
  for(i in 1:9){
    this.Y <- ppl_varRT[,seq(k,ncol(ppl_varRT),5)[i]]
    CI <- quantile(this.Y, probs = c(0.025,0.975))
    polygon(x=c(i-0.4,i+0.4,i+0.4,i-0.4),y=c(CI[1],CI[1],CI[2],CI[2]), col="gray85", border="gray70")
    points(i+jitter.x,this.Y, col=myCol(r[,3],g[,3],b[,3],i,0.05), pch=16, cex=0.8)
    points(i, log(ezdata$varRT[which(ezdata$cond==k)][i]), pch=8)
  }
  mtext("RT variance (log-scale)", 2, line=2.1, cex=0.6)
  if(k==1){
    mtext("RT variance (logs)", f=2, line=.5, cex=1)}
  if(k==5){mtext("Participant",1,line=1.7,cex=0.8)
    axis(1,seq(1,9,length.out=9),paste("p", 1:9, sep=""), line=-1, tick=FALSE, cex.axis=0.8)}
  
}
dev.off()