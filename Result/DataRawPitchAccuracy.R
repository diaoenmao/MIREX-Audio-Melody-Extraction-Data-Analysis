DataRawPitchAccuracy <- function(dataset = c("mir09_0db", "adc04","mir05", "ind08", "mir09_m5db","mir09_p5db")) {
  data2009mir09_0db<-read.csv(paste("./",dataset,"/2009",dataset,".csv", sep=""))
  data2010mir09_0db<-read.csv(paste("./",dataset,"/2010",dataset,".csv", sep=""))
  data2011mir09_0db<-read.csv(paste("./",dataset,"/2011",dataset,".csv", sep=""))
  data2012mir09_0db<-read.csv(paste("./",dataset,"/2012",dataset,".csv", sep=""))
  data2013mir09_0db<-read.csv(paste("./",dataset,"/2013",dataset,".csv", sep=""))
  data2014mir09_0db<-read.csv(paste("./",dataset,"/2014",dataset,".csv", sep=""))
  data<-rbind(data2009mir09_0db, data2010mir09_0db, data2011mir09_0db,
              data2012mir09_0db, data2013mir09_0db, data2014mir09_0db)
  row.names(data)<-1:nrow(data)
  
  tempOut<-CleanDupAndVD(data)
  data<-tempOut[[1]]
  data_VD<-tempOut[[2]]
  data_WithoutVD<-tempOut[[3]]
  out<-data$Raw.Pitch.Accuracy
  return (out)
}