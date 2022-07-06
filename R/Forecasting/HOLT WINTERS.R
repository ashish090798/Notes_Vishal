df<-read.csv("C:\\Users\\vish\\Documents\\df_a.csv")
head(df)


hw_model<-function(ts,h){
  library(forecast)
  model<-hw(y=ts,h=h,lambda="auto")
  model_forecast=forecast(model,h)
  mape<-mean(abs(ts-model$fitted)/ts)*100
  return (list(model_forecast$mean, model$fitted, mape,"HoltWinters"))
}

df$ds<-as.Date(df$ds)
startdate<-df[,2][1]
startdate

pos.date<-as.POSIXlt(startdate)
month<-pos.date$mon
year<-pos.date$year+1900
head(df)


ts<-ts(df$y.target.,start=c(year,month),frequency=12)
h=6
model_result<-hw_model(ts,h)
forecasted<-unlist(model_result[1])
fitted<-unlist(model_result[2])
mape<-unlist(model_result[3])
model<-unlist(model_result[4])
total_predictions=data.frame(forecasted)
total_predictions$mape=mape
total_predictions$model=model
head(total_predictions)
