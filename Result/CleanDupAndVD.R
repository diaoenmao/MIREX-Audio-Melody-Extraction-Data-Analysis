CleanDupAndVD <- function(data) {
  duplicated<-(duplicated(data$Overall.Accuracy)) & (duplicated(data$Raw.Pitch.Accuracy)) &
    (duplicated(data$Raw.Chroma.Accuracy)) & (duplicated(data$Voicing.Recall.Rate)) &
    (duplicated(data$Voicing.False.Alarm.Rate))
  data<-data[!duplicated,]
  withoutVD<-((data$Voicing.Recall.Rate >= 0.9) & (data$Voicing.False.Alarm.Rate >= 0.9))
  data_VD<-data[!withoutVD,]
  data_WithoutVD<-data[withoutVD,]
  out<-list(data, data_VD, data_WithoutVD)
  return(out)
}