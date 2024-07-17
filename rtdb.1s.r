rtdb.1s <- function(df){
  library(lubridate)
  library(dplyr)
  mgdp <- function(){
    library(sidrar)
    library(zoo)
    library(lubridate)
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
    PIB = get_sidra(api = "/t/5932/n1/all/v/6561/p/all/c11255/90707/d/v6561%201")
    pib = ts(PIB$Valor, start = c(1996,01), frequency = 4)
    datas.pib = PIB$`Trimestre (Código)`
    pib.m = qtr2month(pib, reference_month = 3, interpolation = TRUE)
    PIB.M = ts(c(pib.m, rep(tail(pib.m)[6],2)), start = c(1996,01), frequency = 12)
    df_pib <- data.frame(date = as.Date(time(PIB.M)), pib = PIB.M)
    return(df_pib)
  }
  m.gdp = mgdp()
  year = year(m.gdp[nrow(m.gdp),][[1]])
  month = month(m.gdp[nrow(m.gdp),][[1]])
  filtered.in  <-   df %>%
    filter(date <= paste0(year,"-",month, "-01"))
  filtered.out <-   df %>%
    filter(date > paste0(year,"-",month, "-01"))
  filtered.out <- filtered.out[1:3,]
  df = list(df.in  = filtered.in,
            df.out = filtered.out)  
  return(df)
}
