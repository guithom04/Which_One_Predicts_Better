###########################################################################
###                NOWCASTING: WHICH ONE PREDICTS BETTER                ###
###                    LARGE FUNCTION 8:process_DFM                     ###
###########################################################################
library(dfms)
library(forecast)
library(zoo)
library(dplyr)

# Define the function
process_DFM <- function(type = c("1s", "ts"),
                        split_prtdb.1s = NULL,
                        split_prtdb.ts = NULL) {
  type <- match.arg(type)
  
  if (type == "1s") {
    # Full dataset for "1s"
    full <- bind_rows(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
    pib <- ts(full[, 2], start = c(2007, 10), frequency = 12)
    
    # Fit the DFM model
    mod <- DFM(full[,-1], r = 2, p = 2)
    
    # Extract factors
    F1 <- ts(mod$F_pca[, 1], start = start(pib), frequency = frequency(pib))
    F2 <- ts(mod$F_pca[, 2], start = start(pib), frequency = frequency(pib))
    
    # Bridge equation
    fit1 <- lm(pib ~ 1 + F1 + F2)
    
    # Prepare new data for prediction
    out.1s <- cbind.data.frame(F1 = tail(F1, 3), F2 = tail(F2, 3))
    
    # Make prediction and return the mean
    return(mean(predict(fit1, newdata = out.1s)))
    
  } else if (type == "ts") {
    # In-sample and out-of-sample datasets for "ts"
    x.in.ts <- split_prtdb.ts$df_in[, -c(1:2)]
    x.out.ts <- split_prtdb.ts$df_out[, -c(1:2)]
    pib.ts <- ts(split_prtdb.ts$df_in[, 2], start = c(2007, 10), frequency = 12)
    pib.ts.out <- ts(split_prtdb.ts$df_out[, 2], start = c(2020, 01), frequency = 12)
    
    # Fit the DFM model
    mod.ts <- DFM(x.in.ts, r = 2, p = 2)
    
    # Extract factors
    F1ts <- ts(mod.ts$F_pca[, 1], start = start(pib.ts), frequency = frequency(pib.ts))
    F2ts <- ts(mod.ts$F_pca[, 2], start = start(pib.ts), frequency = frequency(pib.ts))
    
    # Bridge equation
    fit.ts <- lm(pib.ts ~ 1 + F1ts + F2ts)
    
    # Fit the out-of-sample DFM model
    mod.ts.out <- DFM(x.out.ts, r = 2, p = 2)
    
    # Extract factors for out-of-sample
    F1ts.out <- ts(mod.ts.out$F_pca[, 1], start = start(pib.ts.out), frequency = frequency(pib.ts.out))
    F2ts.out <- ts(mod.ts.out$F_pca[, 2], start = start(pib.ts.out), frequency = frequency(pib.ts.out))
    
    # Align lengths if necessary
    min_length <- min(length(F1ts.out), length(F2ts.out))
    F1ts.out <- F1ts.out[1:min_length]
    F2ts.out <- F2ts.out[1:min_length]
    
    # Prepare the new data for prediction
    out.ts <- data.frame(F1ts = F1ts.out, F2ts = F2ts.out)
    
    # Make predictions and return the time series
    ts_pred <- ts(predict(fit.ts, newdata = out.ts), start = c(2020, 01), frequency = 12)
    
    return(ts_pred)
  }
}

# Example usage for "1s"
ts_value <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
print(ts_value)
# Example usage for "ts"
ts_series <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
glimpse(ts_series)
