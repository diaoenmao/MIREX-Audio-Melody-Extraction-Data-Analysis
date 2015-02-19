Densityplot_RPA <- function() {
  mir09_0db<-DataRawPitchAccuracy("mir09_0db")
  adc04<-DataRawPitchAccuracy("adc04")
  mir05<-DataRawPitchAccuracy("mir05")
  ind08<-DataRawPitchAccuracy("ind08")
  mir09_m5db<-DataRawPitchAccuracy("mir09_m5db")
  mir09_p5db<-DataRawPitchAccuracy("mir09_p5db")
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
  plot(density(mir09_0db*100, na.rm = TRUE),xlab="Raw Pitch Accuracy/%",
       ylab="Density", main="MIREX09 0db", ylim=c(0,0.07))
  grid()
  plot(density(adc04*100, na.rm = TRUE),xlab="Raw Pitch Accuracy/%",
       ylab="Density", main="ADC04", ylim=c(0,0.07))
  grid()
  plot(density(mir05*100, na.rm = TRUE),xlab="Raw Pitch Accuracy/%",
       ylab="Density", main="MIREX05", ylim=c(0,0.07))
  grid()
  plot(density(ind08*100, na.rm = TRUE),xlab="Raw Pitch Accuracy/%",
       ylab="Density", main="MIREX08", ylim=c(0,0.07))
  grid()
  plot(density(mir09_m5db*100, na.rm = TRUE),xlab="Raw Pitch Accuracy/%",
       ylab="Density", main="MIREX09 -5db", ylim=c(0,0.07))
  grid()
  plot(density(mir09_p5db*100, na.rm = TRUE),xlab="Raw Pitch Accuracy/%",
       ylab="Density", main="MIREX09 +5db", ylim=c(0,0.07))
  grid()
}