Boxplot_RPA <- function() {
  mir09_0db<-DataRawPitchAccuracy("mir09_0db")
  adc04<-DataRawPitchAccuracy("adc04")
  mir05<-DataRawPitchAccuracy("mir05")
  ind08<-DataRawPitchAccuracy("ind08")
  mir09_m5db<-DataRawPitchAccuracy("mir09_m5db")
  mir09_p5db<-DataRawPitchAccuracy("mir09_p5db")
  Length<-max(length(mir09_0db), length(adc04), length(mir05), length(ind08),
              length(mir09_m5db), length(mir09_p5db))
  names<-c("MIREX2009_0db", "ADC04",
           "MIREX2005","MIREX2008",
           "MIREX2009_m5db", "MIREX2009_p5db")
  out<-data.frame()
  out<-data.frame(matrix(ncol = length(names), nrow = Length))
  colnames(out)<-names
  names<-c("MIREX09 0db", "ADC04",
           "MIREX05","MIREX08",
           "MIREX09 -5db", "MIREX09 +5db")
  out[1:length(mir09_0db),1]<-mir09_0db
  out[1:length(adc04),2]<-adc04
  out[1:length(mir05),3]<-mir05
  out[1:length(ind08),4]<-ind08
  out[1:length(mir09_m5db),5]<-mir09_m5db
  out[1:length(mir09_p5db),6]<-mir09_p5db
  out<-out*100
  boxplot(out$MIREX2009_0db, out$ADC04, out$MIREX2005, out$MIREX2008, out$MIREX2009_m5db, out$MIREX2009_p5db, main="Raw Pitch Accuracy across all datasets from 2009 to 2014", 
          names=names, xlab="Datasets", ylab="Raw Pitch Accuracy/%", ylim=c(0, 100))
  grid()
}
