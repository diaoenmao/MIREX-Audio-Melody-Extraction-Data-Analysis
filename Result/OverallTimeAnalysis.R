OverallTimeAnalysis <- function() {
  Time2009<-TimeAnalysis("mir09_0db")
  Time2010<-TimeAnalysis("adc04")
  Time2011<-TimeAnalysis("mir05")
  Time2012<-TimeAnalysis("ind08")
  Time2013<-TimeAnalysis("mir09_m5db")
  Time2014<-TimeAnalysis("mir09_p5db")
  list<-list(Time2009, Time2010, Time2011, Time2012, Time2013, Time2014)
  Time<-c(2009, 2010, 2011, 2012, 2013, 2014)
  temp<-c()
  for(i in 1:length(list)) {
    temp<-rbind(temp, list[[i]][1,])
  }
  MaxOverallAccuracy_VD_MeanForAllSets<-colMeans(temp)
  jpeg(paste("./Output/Plot/TimevsOverallForAllSetsplot.jpg", sep=""))
  plot(Time,MaxOverallAccuracy_VD_MeanForAllSets, ylim=c(0, 1), type="l")
  grid()
  dev.off()
  out<-data.frame()
  out<-data.frame(matrix(ncol = length(Time), nrow = 1))
  rownames(out)<-"MaxOverallAccuracy_VD_MeanForAllSets"
  colnames(out)<-Time
  out[1,]<-MaxOverallAccuracy_VD_MeanForAllSets
  
  return(out)
}