dataset<-"mir09_0db"
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
MaxdPrime_Time<-c()
MeandPrime_Time<-c()
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
  MaxdPrime<-max(qnorm(data_VD$Voicing.Recall.Rate) - qnorm(data_VD$Voicing.False.Alarm.Rate))
  MeandPrime<-mean(qnorm(data_VD$Voicing.Recall.Rate) - qnorm(data_VD$Voicing.False.Alarm.Rate))
  
  MaxOverallAccuracy_Time<-c(MaxOverallAccuracy_Time, MaxOverallAccuracy_VD)
  MaxRawPitchAccuracy_Time<-c(MaxRawPitchAccuracy_Time, MaxRawPitchAccuracy)
  MaxRawChromaAccuracy_Time<-c(MaxRawChromaAccuracy_Time, MaxRawChromaAccuracy)
  MeanOverallAccuracy_Time<-c(MeanOverallAccuracy_Time, MeanOverallAccuracy_VD)
  MeanRawPitchAccuracy_Time<-c(MeanRawPitchAccuracy_Time, MeanRawPitchAccuracy)
  MeanRawChromaAccuracy_Time<-c(MeanRawChromaAccuracy_Time, MeanRawChromaAccuracy)
  MaxdPrime_Time<-c(MaxdPrime_Time, MaxdPrime)
  MeandPrime_Time<-c(MeandPrime_Time, MeandPrime)
}
plot(Time,MaxOverallAccuracy_Time*100, ylim=c(0, 100), type="l", ylab="Accuracy/%"
     , main ="MIREX09 0db dataset Accuracy throughout 2009 to 2014", col = "red")

lines(Time,MeanOverallAccuracy_Time*100, ylim=c(0, 100), type="l", col = "blue")

lines(Time,MaxRawPitchAccuracy_Time*100, ylim=c(0, 100), type="l", col = "green")

lines(Time,MeanRawPitchAccuracy_Time*100, ylim=c(0, 100), type="l", col = "black")
grid()
legend(2012,30,c("Max Overall Accuracy","Average Overall Accuracy", "Max Raw Pitch Accuracy"
                 ,"Mean Raw Pitch Accuracy"), lty=c(1,1), lwd=c(2.5,2.5),col=c("red","blue","green", "black"))
x11()
plot(Time,MaxdPrime_Time, ylim=c(0, 2), type="l", ylab="d-prime/%"
     , main ="MIREX09 0db dataset d-prime throughout 2009 to 2014",col = "red")

lines(Time,MeandPrime_Time, ylim=c(0, 2), type="l", main ="MIREX09 0db",col = "blue")
grid()
legend(2011,0.5,c("Max d-prime","Mean d-prime"), lty=c(1,1), lwd=c(2.5,2.5),col=c("red","blue"))
