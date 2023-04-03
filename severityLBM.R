severityLBM<-function (severity, dates, EmergDate, Cultivar, ApplSys) 
{
  
  if(!("Cultivar" %in% colnames(severity))){stop("There is no 'Cultivar' data")}
  if(!("ApplSys" %in% colnames(severity))){stop("There is no 'ApplSys' data")}
  
  
  df = severity[severity$Cultivar==Cultivar & severity$ApplSys==ApplSys,]
  nday = as.numeric(dates - EmergDate)
  
  n = ncol(df)
  MeanSeverity = apply(df[,4:n], 2, function(x) mean(x, na.rm = TRUE))
  StDevSeverity = apply(df[, 4:n], 2, function(x) sd(x, na.rm = TRUE))
  nd = apply(df[, 4:n], 2, function(x) length(x[!is.na(x)]))
  
  Sfile = data.frame(Cultivar, ApplSys, dates, nday, MeanSeverity, StDevSeverity, nd)

  return(list(Sfile = Sfile))
}


