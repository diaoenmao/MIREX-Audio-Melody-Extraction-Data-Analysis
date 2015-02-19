TimeAnalysis <- function(dataset = c("mir09_0db", "adc04","mir05", "ind08", "mir09_m5db","mir09_p5db")) {
  data2009mir09_0db<-read.csv(paste("./",dataset,"/2009",dataset,".csv", sep=""))
  data2010mir09_0db<-read.csv(paste("./",dataset,"/2010",dataset,".csv", sep=""))
  data2011mir09_0db<-read.csv(paste("./",dataset,"/2011",dataset,".csv", sep=""))
  data2012mir09_0db<-read.csv(paste("./",dataset,"/2012",dataset,".csv", sep=""))
  data2013mir09_0db<-read.csv(paste("./",dataset,"/2013",dataset,".csv", sep=""))
  data2014mir09_0db<-read.csv(paste("./",dataset,"/2014",dataset,".csv", sep=""))
  list<-list(data2009mir09_0db, data2010mir09_0db, data2011mir09_0db, data2012mir09_0db,
             data2013mir09_0db, data2014mir09_0db)
  Time<-c(2009, 2010, 2011, 2012, 2013, 2014)
  MaxOverallAccuracy_Time<-c()
  MaxRawPitchAccuracy_Time<-c()
  MaxRawChromaAccuracy_Time<-c()
  MeanOverallAccuracy_Time<-c()
  MeanRawPitchAccuracy_Time<-c()
  MeanRawChromaAccuracy_Time<-c()
  for(i in 1:length(list)) {
    tempOut<-CleanDupAndVD(list[[i]])
    data<-tempOut[[1]]
    data_VD<-tempOut[[2]]
    data_WithoutVD<-tempOut[[3]]
    MaxOverallAccuracy_VD<-max(data_VD$Overall.Accuracy)
    MaxRawPitchAccuracy<-max(data$Raw.Pitch.Accuracy)
    MaxRawChromaAccuracy<-max(data$Raw.Chroma.Accuracy)
    MeanOverallAccuracy_VD<-mean(data_VD$Overall.Accuracy)
    MeanRawPitchAccuracy<-mean(data$Raw.Pitch.Accuracy)
    MeanRawChromaAccuracy<-mean(data$Raw.Chroma.Accuracy)
    
    MaxOverallAccuracy_Time<-c(MaxOverallAccuracy_Time, MaxOverallAccuracy_VD)
    MaxRawPitchAccuracy_Time<-c(MaxRawPitchAccuracy_Time, MaxRawPitchAccuracy)
    MaxRawChromaAccuracy_Time<-c(MaxRawChromaAccuracy_Time, MaxRawChromaAccuracy)
    MeanOverallAccuracy_Time<-c(MeanOverallAccuracy_Time, MeanOverallAccuracy_VD)
    MeanRawPitchAccuracy_Time<-c(MeanRawPitchAccuracy_Time, MeanRawPitchAccuracy)
    MeanRawChromaAccuracy_Time<-c(MeanRawChromaAccuracy_Time, MeanRawChromaAccuracy)
  }
  jpeg(paste("./Output/Plot/",dataset,"_TimevsMaxOverallplot.jpg", sep=""))
  plot(Time,MaxOverallAccuracy_Time*100, ylim=c(0, 100), type="l", ylab="Max Overall Accuracy/%")
  grid()
  dev.off()
  jpeg(paste("./Output/Plot/",dataset,"_TimevsMeanOverallplot.jpg", sep=""))
  plot(Time,MeanOverallAccuracy_Time*100, ylim=c(0, 100), type="l", ylab="Average Overall Accuracy/%")
  grid()
  dev.off()
 names<-c('MaxOverallAccuracy_VD', 'MaxRawPitchAccuracy',
          'MaxRawChromaAccuracy','MeanOverallAccuracy_VD',
          'MeanRawPitchAccuracy', 'MeanRawChromaAccuracy')
 out<-data.frame()
 out<-data.frame(matrix(ncol = length(Time), nrow = length(names)))
 
 rownames(out)<-names
 colnames(out)<-Time

 out[1,]<-MaxOverallAccuracy_Time
 out[2,]<-MaxRawPitchAccuracy_Time
 out[3,]<-MaxRawChromaAccuracy_Time
 out[4,]<-MeanOverallAccuracy_Time
 out[5,]<-MeanRawPitchAccuracy_Time
 out[6,]<-MeanRawChromaAccuracy_Time
 write.csv(out, file=paste("./Output/Spreadsheet/",dataset,"_TimeAnalysisOutput.csv", sep="")) 
 
 return(out)
}