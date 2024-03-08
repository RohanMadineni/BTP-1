library("EMD")
#0.1^2, max.shift=30,boundary="periodic",max.imf=25
EMD<- function(ts,ms,bndry,mi)
{
  mraout <-emd(ts,tol=sd(ts)*0.1^2, max.sift=ms, stoprule="type1",
               boundary=bndry, max.imf=mi)
  dSeries <- cbind(mraout$imf,mraout$residue)
  return(list(dSeries=dSeries,dvSeries=mraout))
}
WaveletFittingnar<- function(ts,MaxARParam,NForecast, ms, bndry, mi)
  
{
  WS <- EMD(ts=ts, ms, bndry, mi)$dSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL
  
  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts), p = MaxARParam, repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit, h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}
ewnet <- function(ts,MaxARParam,NForecast, PI =FALSE, ms, bndry, mi){ 
  n_test = NForecast 
  fit_ewnet = WaveletFittingnar(ts(ts), MaxARParam,NForecast, ms, bndry, mi)
  if (isTRUE(PI)){
    upper = fit_ewnet$Finalforecast + 1.5*sd(fit_ewnet$Finalforecast)
    lower = fit_ewnet$Finalforecast - 1.5*sd(fit_ewnet$Finalforecast)
    forecast = data.frame("Forecast" = fit_ewnet$Finalforecast, 
                          "Lower Interval" = lower,
                          "Upper Interval" = upper)
  }else{
    forecast = data.frame("Forecast" = fit_ewnet$Finalforecast)
  }
  
  return(forecast)
}
#training_data <- Philippines_Dengue$Cases[1:96]
#testing_data <- Philippines_Dengue[97:nrow(Philippines_Dengue), ]
#plot(97:nrow(Philippines_Dengue),testing_data$Cases,type="l",col="blue",ylab="Cases",xlab="Months",main="Philippines_Dengue",ylim=c(0,200))
#lines(97:nrow(Philippines_Dengue),result$Forecast, col = "red",ylim=c(0,200))

#training_data <- Australia_Inflluenza$Cases[1:924]
#testing_data <- Australia_Inflluenza[925:nrow(Australia_Inflluenza), ]
#result<-ewnet(training_data, MaxARParam = 4, NForecast = nrow(testing_data), ms = 19, bndry = "periodic", mi = 28) 
#plot(925:nrow(Australia_Inflluenza),testing_data$Cases,type="l",col="blue",ylab="Cases",xlab="Weeks",main="Australia_Inflluenza",ylim=c(0,250))
#lines(925:nrow(Australia_Inflluenza),result$Forecast, col = "red",ylim=c(0,250))

training_data <- Ahmedabad_Dengue$Cases[1:385]
testing_data <- Ahmedabad_Dengue[386:nrow(Ahmedabad_Dengue), ]
result<-ewnet(training_data, MaxARParam=15, NForecast = nrow(testing_data), ms = 25, bndry = "periodic", mi=27) 
plot(386:nrow(Ahmedabad_Dengue),testing_data$Cases,type="l",col="blue",ylab="Ahmedabad_Dengue",xlab="Weeks",main="Ahmedabad_Dengue",ylim=c(0,50))
lines(386:nrow(Ahmedabad_Dengue),result$Forecast, col = "red",ylim=c(0,50))

#training_data <- Iquitos_Dengue$Cases[1:546]
#testing_data <- Iquitos_Dengue[547:nrow(Iquitos_Dengue), ]
#result<-ewnet(training_data, MaxARParam=5, NForecast = nrow(testing_data), ms = 30, bndry = "periodic", mi = 25) 
#plot(547:nrow(Iquitos_Dengue),testing_data$Cases,type="l",col="blue",ylab="Iquitos_Dengue",xlab="Weeks",main="Iquitos_Dengue",ylim=c(0,50))
#lines(547:nrow(Iquitos_Dengue),result$Forecast, col = "red",ylim=c(0,50))
