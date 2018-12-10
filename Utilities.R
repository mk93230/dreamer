# String to data function
stringToDate<-function(x){
  mdy_hm(x)
}

getYear <- function(x){
  date_value=mdy_hm(x)
  return(year(date_value))
}
