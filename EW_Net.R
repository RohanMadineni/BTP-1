
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='haar', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,MaxARParam,boundary,FastFlag,NForecast)
  
{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
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
ewnet <- function(ts,Waveletlevels,MaxARParam,boundary,FastFlag,NForecast, PI =FALSE){ 
  n_test = NForecast 
  fit_ewnet = WaveletFittingnar(ts(ts), Waveletlevels = floor(log(length(ts))), boundary = "periodic", 
                                FastFlag = TRUE, MaxARParam, NForecast)
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

#training_data <- Australia_Inflluenza$Cases[1:922]
#testing_data <- Australia_Inflluenza[923:nrow(Australia_Inflluenza), ]
#result<-ewnet(training_data, MaxARParam = 4, NForecast = nrow(testing_data)) 
#plot(923:nrow(Australia_Inflluenza),testing_data$Cases,type="l",col="blue",ylab="Cases",xlab="Weeks",main="Australia_Inflluenza",ylim=c(0,250))
#lines(923:nrow(Australia_Inflluenza),result$Forecast, col = "red",ylim=c(0,250))

training_data <- Ahmedabad_Dengue$Cases[1:385]
testing_data <- Ahmedabad_Dengue[386:nrow(Ahmedabad_Dengue), ]
result<-ewnet(training_data, MaxARParam = 15, NForecast = nrow(testing_data)) 
plot(386:nrow(Ahmedabad_Dengue),testing_data$Cases,type="l",col="blue",ylab="Cases",xlab="Weeks",main="Ahmedabad_Dengue",ylim=c(0,50))
lines(386:nrow(Ahmedabad_Dengue),result$Forecast, col = "red",ylim=c(0,50))

#training_data <- Iquitos_Dengue$Cases[1:546]
#testing_data <- Iquitos_Dengue[547:nrow(Iquitos_Dengue), ]
#result<-ewnet(training_data, MaxARParam=5, NForecast = nrow(testing_data)) 
#plot(547:nrow(Iquitos_Dengue),testing_data$Cases,type="l",col="blue",ylab="Iquitos_Dengue",xlab="Weeks",main="Iquitos_Dengue",ylim=c(0,50))
#lines(547:nrow(Iquitos_Dengue),result$Forecast, col = "red",ylim=c(0,50))

