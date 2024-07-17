sidrarGET <- function(){
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
    library(sidrar)
    library(zoo)
    library(lubridate)
    #BR industrial production
    PIM = get_sidra(api = "/t/8888/n1/all/v/11602/p/all/c544/129314/d/v11602%201")
    pim = ts(c(NA,diff(PIM$Valor)), start = c(2002,01), frequency = 12)
    df_pim = data.frame(date = as.Date(time(pim)), pim = pim)
    #Capital Goods
    PIM_BK = get_sidra(api = "/t/8887/n1/all/v/11602/p/all/c543/129278/d/v11602%201")
    pim_bk = ts(PIM_BK$Valor, start = c(2002,01),frequency = 12)
    df_pim_bk = data.frame(date = as.Date(time(pim_bk)), pim_bk = pim_bk)
    #Intermediary goods 
    PIM_BI = get_sidra(api = "/t/8887/n1/all/v/11602/p/all/c543/129283/d/v11602%201")
    pim_bi = ts(PIM_BI$Valor, start = c(2002,01), frequency = 12)
    df_pim_bi = data.frame(date = as.Date(time(pim_bi)), pim_bi = pim_bi)
    #BR Retail Sales
    PMCA = get_sidra(api = "/t/8881/n1/all/v/11709/p/all/c11046/56736/d/v11709%201")
    pmca = ts(c(NA,diff(PMCA$Valor)), start = c(2003,01), frequency = 12)
    df_pmca = data.frame(date = as.Date(time(pmca)), pmca = pmca)
    #Área colhida agricultura
    AREA_COLHIDA <- sidrar::get_sidra(api = "/t/6588/n1/all/v/216/p/all/c48/0")
    area_colhida <- ts(yoy(c(NA,diff(AREA_COLHIDA$Valor))), start = c(2007,10), frequency = 12)
    df_area <- data.frame(date = as.Date(time(area_colhida)), area = area_colhida)
    # Custo de Habitação
    HOUSING <- sidrar::get_sidra(api = "/t/647/n3/11/v/all/p/all/c314/7087/c41/787/d/v51%202")
    housing <- ts(yoy(c(NA,NA,diff(HOUSING$Valor,2))), start = c(2000,02), frequency = 12)
    df_housing <- data.frame(date = as.Date(time(housing)), housing = housing)
    # unified dataframe
    df <- Reduce(function(df1, df2) merge(df1, df2, by = "date", all = TRUE),
                     list(df_pim, 
                          df_pim_bk,
                          df_pim_bi,
                          df_pmca,
                          df_area,
                          df_housing))
  }
  sidrar <- sidrarGET()
