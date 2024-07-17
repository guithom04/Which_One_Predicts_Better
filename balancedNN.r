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
balanced_NN <- as_tibble(balanced.NN(head))
tail(balanced_NN)
