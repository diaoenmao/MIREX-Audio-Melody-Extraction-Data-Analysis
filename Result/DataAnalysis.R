DataAnalysis <- function(dataset = c("mir09_0db", "adc04","mir05", "ind08", "mir09_m5db","mir09_p5db")) {
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
  
  MaxOverallAccuracy_VD<-max(data_VD$Overall.Accuracy)
  MaxOverallAccuracyAuthor_VD<-as.character(data_VD$Author[[which.max(data_VD$Overall.Accuracy)]])
  
  MaxRawPitchAccuracy<-max(data$Raw.Pitch.Accuracy)
  MaxRawPitchAccuracyAuthor<-as.character(data$Author[[which.max(data$Raw.Pitch.Accuracy)]])
  
  MaxRawChromaAccuracy<-max(data$Raw.Chroma.Accuracy)
  MaxRawChromaAccuracyAuthor<-as.character(data$Author[[which.max(data$Raw.Chroma.Accuracy)]])
  
  MaxVoicingRecallRate_VD<-max(data_VD$Voicing.Recall.Rate)
  MaxVoicingRecallRateAuthor_VD<-as.character(data_VD$Author[[which.max(data_VD$Voicing.Recall.Rate)]])
  
  MinVoicingReFalseAlarmRate_VD<-min(data_VD$Voicing.False.Alarm.Rate)
  MinVoicingReFalseAlarmRateAuthor_VD<-as.character(data_VD$Author[[which.min(data_VD$Voicing.False.Alarm.Rate)]])
  
  dPrime<-qnorm(data_VD$Voicing.Recall.Rate) - qnorm(data_VD$Voicing.False.Alarm.Rate)
  BestVoicingRecallRate_VD<-max(dPrime)
  BestVoicingRecallRateAuthor_VD<-as.character(data_VD$Author[[which.max(dPrime)]])
  
  jpeg(paste("./Output/Plot/",dataset,"_boxplot.jpg", sep=""))
  boxplot(data_VD$Overall.Accuracy, data=data_VD)
  grid()
  dev.off()
  jpeg(paste("./Output/Plot/",dataset,"_histplot.jpg", sep=""))
  x <- data_VD$Overall.Accuracy 
  h<-hist(x, breaks=10, xlab="Overall Accurcy", 
          main="Histogram with Normal Curve") 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, lwd=1)
  grid()
  dev.off()
  
  jpeg(paste("./Output/Plot/",dataset,"_densityplot.jpg", sep=""))
  d <- density(data_VD$Overall.Accuracy) # returns the density data 
  plot(d) # plots the results
  grid()
  dev.off()
  outliers = boxplot(data_VD$Overall.Accuracy, plot=FALSE)$out
  
  names<-c('MaxOverallAccuracy_VD','MaxOverallAccuracyAuthor_VD', 'MaxRawPitchAccuracy','MaxRawPitchAccuracyAuthor',
           'MaxRawChromaAccuracy','MaxRawChromaAccuracyAuthor', 'MaxVoicingRecallRate_VD', 'MaxVoicingRecallRateAuthor_VD',
           'MinVoicingReFalseAlarmRate_VD', 'MinVoicingReFalseAlarmRateAuthor_VD', 'BestVoicingRecallRate_VD(d-prime)',
           'BestVoicingRecallRateAuthor_VD')
  dataout<-c(MaxOverallAccuracy_VD, MaxOverallAccuracyAuthor_VD, MaxRawPitchAccuracy, MaxRawPitchAccuracyAuthor,
             MaxRawChromaAccuracy, MaxRawChromaAccuracyAuthor, MaxVoicingRecallRate_VD, MaxVoicingRecallRateAuthor_VD,
             MinVoicingReFalseAlarmRate_VD, MinVoicingReFalseAlarmRateAuthor_VD, BestVoicingRecallRate_VD,
             BestVoicingRecallRateAuthor_VD)
  out<-data.frame()
  out<-data.frame(t(rep(NA,length(names))))
  colnames(out)<-names
  out[1,]<-dataout
  write.csv(out, file=paste("./Output/Spreadsheet/",dataset,"_DataAnalysisOutput.csv", sep=""))

  return(out)
}