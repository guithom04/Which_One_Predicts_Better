dfexpec <- function(){
  # transformar Q em M, com interpolação.(retirado de NMECSYS - NOWCASTING)
  qtr2month <- function(x, reference_month = 3, interpolation = FALSE){
    
    if(!reference_month %in% c(1,2,3)){
      stop("The reference_month should be 1,2 or 3")
    }
    
    if(!is.ts(x)){
      stop("x should be a ts object")
    }
    
    if(!is.null(dim(x))){
      stop("x should be a single ts object")
    }
    
    data_q <- zoo::as.Date(x)
    data_m <- seq(data_q[1], data_q[length(data_q)], by = 'months')
    out_x <- ts(rep(NA,length(data_m)),
                start =  as.numeric(c(substr(data_q[1],1,4), substr(data_q[1],6,7))),
                frequency = 12)
    out_x[data_m %in% data_q] <- x
    
    if(reference_month %in% c(2,3)){
      out_x <- stats::lag(out_x, -(reference_month-1))
      data_q <- zoo::as.Date(out_x)
      data_m <- seq(data_q[1], data_q[length(data_q)], by = 'months')
    }
    
    if(interpolation){
      xout <- zoo::as.yearmon(data_m)
      out_x <- stats::approx(out_x, xout = xout, method = "linear")
      out_x <- ts(out_x$y, start = out_x$x[1], end = out_x$x[length(out_x$x)], frequency = 12)
    }
    
    # output
    return(out_x)
  }
  # transformar nível em yoy
  yoy = function(series){
    index = 1:length(series)
    for(i in index){
      YoY <- series[12+index]/series[index]-1
    }
    return(ts(na.omit(YoY),start = c(start(series)[1]+1,start(series)[2]),frequency = 12))
  }
  # pacotes usados extração de séries
  library(zoo)
  library(meedr)
  library(lubridate)
  trihoje = quarter(Sys.Date())
  # gets expectative given reference
  exp <- function(ref){
    meedr::get_quarterly(indicator = "PIB Total",
                         first_date = Sys.Date()-30*365,
                         reference_date = ref)
  }
  # gets today expectative
  GDP.now.expec <- function(trihoje){
    if (trihoje == "1") {
      exp <- function(ref){
        meedr::get_quarterly(indicator = "PIB Total",
                             first_date = Sys.Date()-30*365,
                             reference_date = ref)
      }
      PIB.Expect <- c()
      for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
        for (i in 1:4) {
          gdp_val <- exp(paste0(i,"/",j))$median[1]
          PIB.Expect[paste0(i,"/",j)] <- gdp_val
        }
      }
      EXPEC.PIB.HOJE <- PIB.Expect[1:(length(PIB.Expect)-3)]
      
    }
    if (trihoje == "2") {
      exp <- function(ref){
        meedr::get_quarterly(indicator = "PIB Total",
                             first_date = Sys.Date()-30*365,
                             reference_date = ref)
      }
      PIB.Expect <- c()
      for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
        for (i in 1:4) {
          gdp_val <- exp(paste0(i,"/",j))$median[1]
          PIB.Expect[paste0(i,"/",j)] <- gdp_val
        }
      }
      EXPEC.PIB.HOJE <- PIB.Expect[1:(length(PIB.Expect)-2)]
    }
    if (trihoje == "3") {
      exp <- function(ref){
        meedr::get_quarterly(indicator = "PIB Total",
                             first_date = Sys.Date()-30*365,
                             reference_date = ref)
      }
      PIB.Expect <- c()
      for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
        for (i in 1:4) {
          gdp_val <- exp(paste0(i,"/",j))$median[1]
          PIB.Expect[paste0(i,"/",j)] <- gdp_val
        }
      }
      EXPEC.PIB.HOJE <- PIB.Expect[1:(length(PIB.Expect)-1)]
    }
    if (trihoje == "4") {
      exp <- function(ref){
        meedr::get_quarterly(indicator = "PIB Total",
                             first_date = Sys.Date()-30*365,
                             reference_date = ref)
      }
      PIB.Expect <- c()
      for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
        for (i in 1:4) {
          gdp_val <- exp(paste0(i,"/",j))$median[1]
          PIB.Expect[paste0(i,"/",j)] <- gdp_val
        }
      }
      EXPEC.PIB.HOJE <- PIB.Expect
    }
    return(EXPEC.PIB.HOJE)
  }
  gdp.expec = GDP.now.expec(trihoje)
  gdp.expec.ts = round(ts(gdp.expec,start = c(2002,1), frequency = 4), digits = 2)
  monthly.gdp.expec.ts = qtr2month(gdp.expec.ts, reference_month = 3, interpolation = TRUE)
  time.M.expec = as.Date(time(monthly.gdp.expec.ts))
  plot(monthly.gdp.expec.ts)
  expec = data.frame(date = time.M.expec, expec = monthly.gdp.expec.ts)
  return(expec)}
