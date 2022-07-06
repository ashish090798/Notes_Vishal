series=read.csv('https://raw.githubusercontent.com/jbrownlee/Datasets/master/shampoo.csv',row.names="Month")
timeseries=series[['Sales']]
plot.ts(timeseries)
ts_object=ts(series[['Sales']],frequency=12)
plot.ts(ts_object)

#Moving average

ma_model<-function(timeseries,h){
    library(smooth)
    ma=sma(timeseries,order=6,h=h)
    mape_ma=mean(abs(timeseries-ma$fitted)/timeseries)*100
    return(list(mape_ma, ma$forecast))
}

ma_results=ma_model(timeseries,6)
print(ma_results[1])
print(ma_results[2])

#Holt winter

h_w_model<-function(timeseries,h){
  library(forecast)
  h_w=hw(ts_object)
  mape_h_w=mean(abs(ts_object-h_w$fitted)/ts_object)*100
  h_w_forecast <- forecast(h_w, h=6,level=c(80,95))
  return(list(mape_h_w, h_w_forecast[2]))
}

h_w_results=h_w_model(ts_object,6)
print(h_w_results[1])
print(h_w_results[2])



