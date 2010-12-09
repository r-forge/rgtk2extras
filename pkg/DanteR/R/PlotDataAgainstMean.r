
#MSdat <- log(Eset)


PlotDataAgainstMean <- function(Data, Columns, use.sampling=TRUE, sample.number=1000, DoPdf=FALSE, ShortenTitle=10){
library(MASS)
# plot the data against means
SAMPLE_NUMBER <- sample.number
means <- apply(Data, 1, mean, na.rm=T)
MSdat <- Data[,Columns,drop=F]
#means[rowSums(!is.na(MSdat)) < 2] <- NA

J <- ncol(MSdat)
I <- nrow(MSdat)
rows <- ceiling(J/ceiling(sqrt(J)))
cols <- ceiling(sqrt(J))

if(DoPdf) pdf("output.pdf", height = 3*rows, width=3*cols)

xlim <- range(means, na.rm=T)
ylim <- range(MSdat, na.rm=T)
xtext <- xlim[1] + diff(xlim)/4
ytext <- ylim[2] - diff(xlim)/4
dataset.names <- abbreviate(colnames(MSdat), ShortenTitle)

par(mfrow= c(rows, cols))
par(mar=c(0,0,0,0), oma=c(6,4,6,4), xaxt="n")

my.sample <- 1:I

for(i in 1:J){
  if(i > 1)
    par(yaxt="n")
  if (i%%cols == 1) par(yaxt="s")
  if(use.sampling) my.sample <- sample(1:I, SAMPLE_NUMBER, replace=TRUE)

  plot(means[my.sample], MSdat[my.sample,i], pch=20, cex=0.5, ylab="", xlab="", main="",     xlim=xlim, ylim=ylim)
  abline(c(0, 1))
  abline(rlm(MSdat[my.sample,i] ~ means[my.sample]), lty=3)
  text(xtext,ytext,parse(text=paste("r^{2}", "==",sprintf("%.2f", cor(means, MSdat[,i], use="complete")), sep="")), cex=1.3)
  mtext(dataset.names[i], side=3, line=-1.5, cex=0.7)
}
mtext("Log-intensity of data vs. mean", side=4, line=0.5, outer=T)

mtext("Log-intensity of data", side=3, line=3, outer=T, cex=1)
mtext("Observed log-intensity", side=2, line=2.5, outer=T, cex=1)
mtext("Mean log-intensity across experiments", side=1, line=3, outer=T, cex=1)

 if(DoPdf){
  dev.off()
  system(paste(getOption('pdfviewer'),"output.pdf"))
 } else {
    my.plot <- recordPlot()
    return(my.plot)
  }

}




