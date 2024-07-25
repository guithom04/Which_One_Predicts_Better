midas_simple_base <- function(){
  # data
  library(midasr)
  library(forecast)
  library(rbcb)
  library(GetBCBData)
  library(lubridate)
  library(dplyr)
  # functions used inside
  upperbalance <- function(df) {
    df <- df[, colSums(is.na(df)) < nrow(df)]
    first_valid_indices <- apply(df, 2, function(col) which(!is.na(col))[1])
    latest_index <- max(first_valid_indices, na.rm = TRUE)
    df_cut <- df[latest_index:nrow(df), ]
    return(df_cut)
  }
  yoy = function(series){
    index = 1:length(series)
    for(i in index){
      YoY <- series[12+index]/series[index]-1
    }
    return(ts(na.omit(YoY),start = c(start(series)[1]+1,start(series)[2]),frequency = 12))
  }
  balance_panel <- function(df, method = c("INTERPOLATION", "NN")) {
    # FUNCTIONS
    upperbalance <- function(df) {
      df <- df[, colSums(is.na(df)) < nrow(df)]
      first_valid_indices <- apply(df, 2, function(col) which(!is.na(col))[1])
      latest_index <- max(first_valid_indices, na.rm = TRUE)
      df_cut <- df[latest_index:nrow(df), ]
      return(df_cut)
    }
    balanced <- function(df){
      df %>% 
        mutate_all(function(col) {
          if(all(is.na(col))) {
            return(col)
          } else if(any(is.na(col))) {
            col <- na.approx(col, na.rm = FALSE) # interpola as séries
            fit <- auto.arima(na.omit(col)) # omite NA's antes de fitar
            col[is.na(col)] <- forecast(fit, h = sum(is.na(col)))$mean
          }
          return(col)
        })
    }
    balanced.NN <- function(df) {
      library(tsfgrnn)
      library(zoo)
      library(dplyr)
      
      # Ensure the 'date' column is in Date format
      df$date <- as.Date(df$date)
      
      # Function to process each column
      process_column <- function(col, col_name) {
        if (all(is.na(col))) {
          message("Column '", col_name, "' has all NA values.")
          return(col)
        } else if (any(is.na(col))) {
          # Ensure the column is a time series object
          if (!is.ts(col)) {
            # Try to infer start and frequency if not already a ts object
            start_date <- as.Date(df$date[!is.na(df$date)][1])
            start_year <- as.numeric(format(start_date, "%Y"))
            start_month <- as.numeric(format(start_date, "%m"))
            col <- ts(col, start = c(start_year, start_month), frequency = 12)
          }
          col <- na.approx(col, na.rm = FALSE) # Interpolate missing values
          
          # Check if there is enough data for forecasting
          if (length(na.omit(col)) < 2) {
            message("Not enough data to forecast for column: ", col_name)
            return(col) # Not enough data to forecast
          }
          
          fit <- tryCatch({
            tsfgrnn::grnn_forecasting(na.omit(col), h = sum(is.na(col)))
          }, error = function(e) {
            message("Forecasting failed for column: ", col_name, " with error: ", e$message)
            return(NA) # Return NA if forecasting fails
          })
          
          if (length(fit$prediction) == sum(is.na(col))) {
            col[is.na(col)] <- fit$prediction
          } else {
            # Fallback to linear interpolation if forecasting fails
            col <- na.approx(col, na.rm = FALSE)
          }
        }
        return(col)
      }
      
      # Apply the process_column function to each column
      df_result <- df %>%
        mutate(across(-date, ~process_column(., cur_column())))
      
      return(df_result)
    }
    method <- match.arg(method)
    
    # Apply upperbalance to balance the header
    df_balanced_header <- upperbalance(df)
    
    # Balance the panel based on the chosen method
    if (method == "INTERPOLATION") {
      df_balanced <- as_tibble(balanced(df_balanced_header))
    } else if (method == "NN") {
      df_balanced <- as_tibble(balanced.NN(df_balanced_header))
    }
    
    return(df_balanced)
  }
  final_expec <- function() {
    trihoje <- quarter(Sys.Date())
    exp <- function(ref) {
      meedr::get_quarterly(indicator = "PIB Total", first_date = Sys.Date()-30*365, reference_date = ref)
    }
    GDP.now.expec <- function(trihoje) {
      PIB.Expect <- c()
      for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
        for (i in 1:4) {
          gdp_val <- exp(paste0(i,"/",j))$median[1]
          PIB.Expect[paste0(i,"/",j)] <- gdp_val
        }
      }
      if (trihoje == "1") {
        return(PIB.Expect[1:(length(PIB.Expect)-3)])
      }
      if (trihoje == "2") {
        return(PIB.Expect[1:(length(PIB.Expect)-2)])
      }
      if (trihoje == "3") {
        return(PIB.Expect[1:(length(PIB.Expect)-1)])
      }
      return(PIB.Expect)
    }
    PIB.Expect <- c()
    for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
      for (i in 1:4) {
        gdp_val <- exp(paste0(i,"/",j))$median[1]
        PIB.Expect[paste0(i,"/",j)] <- gdp_val
      }
    }
    gdp.expec <- GDP.now.expec(trihoje)
    gdp.expec.ts <- round(ts(gdp.expec, start = c(2002, 1), frequency = 4), digits = 2)
    monthly.gdp.expec.ts <- qtr2month(gdp.expec.ts, reference_month = 3, interpolation = TRUE)
    time.M.expec <- as.Date(time(monthly.gdp.expec.ts))
    df_expec <- data.frame(date = time.M.expec, expec = monthly.gdp.expec.ts)
    return(df_expec)
  }
  # assemble series for the full database
  # pib
  table.pib = get_sidra(api = "/t/5932/n1/all/v/6561/p/all/c11255/90707/d/v6561%201")
  pib.ts = ts(table.pib$Valor, start = c(1996,01), frequency = 4)
  # pim
  table.pim = get_sidra(api = "/t/8888/n1/all/v/11602/p/all/c544/129314/d/v11602%201")
  pim.ts = ts(table.pim$Valor, start = c(2002,01), frequency = 12)
  # pim diff
  pim.d = ts(c(NA,diff(pim.ts)), start = c(2002,01), frequency = 12)
  # pmca
  table.pmca = get_sidra(api = "/t/8881/n1/all/v/11709/p/all/c11046/56736/d/v11709%201")
  pmca.ts = ts(table.pmca$Valor, start = c(2003,01), frequency = 12)
  # pmca diff
  pmca.d = ts(c(NA,diff(pmca.ts)), start = c(2003,01), frequency = 12)
  # ibc br
  IBC_BR = rbcb::get_series(c(IBC_BR = 24363), start_date = "01-01-2003")
  ibc_br = ts(IBC_BR$IBC_BR, start = c(2003,01), frequency = 12)
  ibc.br.yoy = yoy(ibc_br)
  # make dataframes
  dfpim <- data.frame(date = as.Date(time(pim.ts)), pim = pim.ts)
  dfpimd <- data.frame(date = as.Date(time(pim.d)), pimd = pim.d)
  dfpmc <- data.frame(date = as.Date(time(pmca.ts)), pmca = pmca.ts)
  dfpmcd <- data.frame(date = as.Date(time(pmca.d)), pmcad = pmca.d)
  dfibcbr <- data.frame(date = as.Date(time(ibc.br.yoy)), ibc = ibc.br.yoy)
  df_finalexpec <- final_expec()
  # merge
  df_all <- Reduce(function(df1, df2) merge(df1, df2, by = "date", all = TRUE),
                   list(dfpim,
                        dfpimd,
                        dfpmc,
                        dfpmcd,
                        dfibcbr,
                        df_finalexpec))
  df <- upperbalance(df_all)
  return(df)
}
df_all = midas_simple_base()
tail(df_all)
upperbalance <- function(df) {
  df <- df[, colSums(is.na(df)) < nrow(df)]
  first_valid_indices <- apply(df, 2, function(col) which(!is.na(col))[1])
  latest_index <- max(first_valid_indices, na.rm = TRUE)
  df_cut <- df[latest_index:nrow(df), ]
  return(df_cut)
}
df_all <- upperbalance(df_all)


vintages_midas <- function(df, year, month){
  
  library(lubridate)
  library(dplyr)
  
  emonth = quarter(paste0(year,"-",month,"-01"))*3
  
  df_1 <- df_all %>%
    filter(date <= paste0(year,"-",month,"-01")) %>%
    select(-c(expec))
  
  df_2 <- df_all %>%
    select(date,expec) %>%
    filter(date <= paste0(year,"-",emonth,"-01")) 
  
  df_3 <- Reduce(function(df1, df2) merge(df1, df2, by = "date", all = TRUE),
                   list(df_1,
                        df_2))
  return(df_3)
}

v1_midas = vintages_midas(df_all, 2023, 01)
tail(v1_midas)
