weatherLBM<-function (weather, EmergDate, EndEpidDate, RHthreshold, is.prec.data = FALSE) 
  {

  if (!require("dplyr")) stop("The package 'dplyr' was not installed")
  if (!require("lubridate")) stop("The package 'lubridate' was not installed")
  
  # Checking weather-data names
  if(!("datetime" %in% colnames(weather))){stop("There is no 'datetime' data")}
  if(!("temp" %in% colnames(weather))){stop("There is no 'temp' data")}
  if(!("rhum" %in% colnames(weather))){stop("There is no 'rhum' data")}
  
  if(is.prec.data){
    if(!("prec" %in% colnames(weather))){stop("There is no 'prec' data")}
  } else {
    weather$prec <- 0
  }
  
  weather$date <- date(weather$datetime)
  weather$hour <- hour(weather$datetime)
  weather$hour_char <- ifelse(weather$hour <= 9, paste0("0", as.character(weather$hour)), as.character(weather$hour))
  weather$group <- paste0(weather$date, " ", weather$hour_char)
  
  # Summary by date-time (hourly data)
  smr <- weather %>%
    group_by(group) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
    
  smr <- as.data.frame(smr)
  smr$date <- as.Date(substr(smr$group,1,10))
  
  df <- smr[,c("date", "hour", "temp", "rhum", "prec")]
  df$rhum_count <- NA
  df$rhum_temp <- NA
  
  Wfile <- subset(df, ((df$date >= EmergDate) & (df$date <= EndEpidDate)))

  for(i in 1:nrow(Wfile)){
    if(!is.na(Wfile$rhum[i])){
      if(Wfile$rhum[i] > 100) 
        Wfile$rhum[i] = 100
      if(Wfile$rhum[i] > RHthreshold)
        Wfile$rhum_count[i] = 1
    }
    if (!is.na(Wfile$rhum_count[i])) {
      if (Wfile$rhum_count[i] == 1) 
        Wfile$rhum_temp[i] <- Wfile$temp[i]
    }
  }
  
  
  Rainfall <- round(as.matrix(by(Wfile$prec, Wfile$date, function(x) sum(x, na.rm = TRUE))), 1)
  Date <- as.Date(as.character(row.names(Rainfall)))
  Tmp <- as.matrix(by(Wfile$temp, Wfile$date, function(x) mean(x, na.rm = TRUE)))
  HumidHrs <- as.matrix(by(Wfile$rhum_count, Wfile$date, function(x) sum(x, na.rm = TRUE)))
  humidtmp <- as.matrix(by(Wfile$rhum_temp, Wfile$date, function(x) mean(x, na.rm = TRUE)))

  Wfile <- data.frame(Date, Rainfall, Tmp, HumidHrs, humidtmp)
  return(list(Wfile = Wfile, EmergDate = EmergDate, EndEpidDate = EndEpidDate))
}

