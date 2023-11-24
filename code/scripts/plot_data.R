if(!exists("data")){
  if(!exists("a")){
    a = 1.50
    v = 0.00
    t = 0.30
    n = 10000
  }
  data <- wdmrnd(a, v, t, n)
}

par(mfrow = c(1, 3), 
    mar =c(5.1,2,4.1,2),
    bty = "o")

rt <- data$RT
accuracy <- data$accuracy

hist(rt[accuracy==1], ann=F, axes=F, breaks=50, col="lightgreen",
     border=NA)
mtext("Distribution of Reaction Times", 3, cex=0.65, line=0.5)
mtext("Reaction Time", 1, cex=0.6, line=2)
mtext("Count", 2, cex=0.6, line=1)
legend("topright", col="lightgreen", "Correct", pch=15, cex=0.7, bty="n")
axis(1,c(0:5),line=-0.7)

hist(rt[accuracy==0], ann=F, axes=F, breaks=50, col="indianred1",
     border=NA)
mtext("Distribution of Reaction Times", 3, cex=0.65, line=0.5)
mtext("Reaction Time", 1, cex=0.6, line=2)
mtext("Count", 2, cex=0.6, line=1)
legend("topright", col="indianred1", "Incorrect", pch=15, cex=0.7, bty="n")
axis(1,c(0:5),line=-0.7)

barplot(c(sum(1-accuracy),sum(accuracy)), ann=F, axes=F,
        col=c("red3","springgreen3"))
mtext("Accuracy Histogram", 3, cex=0.65, line=0.5)
mtext("Count", 2, cex=0.6, line=1)
axis(1,c(0.7,1.9),c("Incorrect","Correct"))