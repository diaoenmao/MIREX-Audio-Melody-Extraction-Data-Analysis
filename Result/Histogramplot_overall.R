Histogramplot_overall <- function() {
  mir09_0db<-DataOverallAccuracy("mir09_0db")
  adc04<-DataOverallAccuracy("adc04")
  mir05<-DataOverallAccuracy("mir05")
  ind08<-DataOverallAccuracy("ind08")
  mir09_m5db<-DataOverallAccuracy("mir09_m5db")
  mir09_p5db<-DataOverallAccuracy("mir09_p5db")
  Length<-max(length(mir09_0db), length(adc04), length(mir05), length(ind08),
              length(mir09_m5db), length(mir09_p5db))
  names<-c("MIREX2009 0db", "ADC04",
           "MIREX2005","MIREX2008",
           "MIREX2009 +5db", "MIREX2009 -5db")
  out<-data.frame()
  out<-data.frame(matrix(ncol = length(names), nrow = Length))
  colnames(out)<-names
  out[1:length(mir09_0db),1]<-mir09_0db
  out[1:length(adc04),2]<-adc04
  out[1:length(mir05),3]<-mir05
  out[1:length(ind08),4]<-ind08
  out[1:length(mir09_m5db),5]<-mir09_m5db
  out[1:length(mir09_p5db),6]<-mir09_p5db
  out<-out*100
  par(mfrow=c(3,2))
  
  x <- mir09_0db
  h<-hist(x, breaks=10, xlab="Overall Accuracy/%", 
          main="MIREX09 0db", ylim=c(0,8)) 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, lwd=1)
  grid()
  
  x <- adc04
  h<-hist(x, breaks=10, xlab="Overall Accuracy/%", 
          main="ADC04", ylim=c(0,8)) 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, lwd=1)
  grid()
  
  x <- mir05
  h<-hist(x, breaks=10, xlab="Overall Accuracy/%", 
          main="MIREX05", ylim=c(0,10)) 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, lwd=1)
  grid() 
  
  x <- ind08
  h<-hist(x, breaks=10, xlab="Overall Accuracy/%", 
          main="MIREX08", ylim=c(0,8)) 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, lwd=1)
  grid()
  
  x <- mir09_m5db
  h<-hist(x, breaks=10, xlab="Overall Accuracy/%", 
          main="MIREX09 -5db", ylim=c(0,8)) 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, lwd=1)
  grid()
  
  x <- mir09_p5db
  h<-hist(x, breaks=10, xlab="Overall Accuracy/%", 
          main="MIREX09 +5db", ylim=c(0,10)) 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, lwd=1)
  grid()
}