###########################################################################
###                NOWCASTING: WHICH ONE PREDICTS BETTER                ###
###                     LARGE FUNCTION 5: prtdb                         ###
###########################################################################
df = upperbalance(df)
prtdb <- function(df, y, q){
  # count NAs
  library(dplyr)
  library(lubridate)
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
  actual.nowcast = m.gdp[nrow(m.gdp),][[1]]+60
  year = year(actual.nowcast)
  quarter = quarter(actual.nowcast)
  get_last_month_of_quarter <- function(year, quarter) {
    if (quarter >= 1 && quarter <= 4) {
      # Create a date for the first day of the quarter
      start_date <- ymd(paste(year, (quarter - 1) * 3 + 1, 1, sep = "-"))
      # Get the last day of the quarter by adding 3 months and subtracting one day
      end_date <- start_date %m+% months(3) - days(1)
      # Extract the month from the end date
      last_month <- month(end_date)
      return(list(year = year, last_month = last_month))
    } else {
      stop("Quarter must be between 1 and 4")
    }
  }
  file = get_last_month_of_quarter(year, quarter) 
  df1 <- df %>%
    filter(date <= paste0(file$year, "-",file$last_month,"-01"))
  na_counts <- sapply(df1, function(x) sum(is.na(x)))
  
  # filter 
  file1 = get_last_month_of_quarter(y, q) 
  df2 <- df1 %>%
    filter(date <= paste0(file1$year, "-",file1$last_month,"-01"))
  
  # Apply NA counts to df2 to mimic the unbalancing structure of df1
  for (col in names(na_counts)) {
    if (na_counts[col] > 0) {
      n <- na_counts[col]
      df2[(nrow(df2) - n + 1):nrow(df2), col] <- NA
    }
  }
  return(df2)
}

# prtdb = prtdb(df, 2023, 2)
# tail(prtdb)
