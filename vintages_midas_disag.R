# midas disaggregated model
# vintage disag
disag.base <- function(df, year, month) {
  library(dplyr)
  
  # Convert month to numeric to determine the quarter
  month <- as.numeric(month)
  
  # Calculate the number of NA rows to add based on the month
  if (month %% 3 == 1) {
    # First month of the quarter, add two rows of NAs
    na_rows <- 2
    add_dates <- seq(as.Date(paste0(year, "-", sprintf("%02d", month + 1), "-01")),
                     by = "month", length.out = 2)
  } else if (month %% 3 == 2) {
    # Second month of the quarter, add one row of NAs
    na_rows <- 1
    add_dates <- as.Date(paste0(year, "-", sprintf("%02d", month + 1), "-01"))
  } else {
    # Third month of the quarter, add no rows of NAs
    na_rows <- 0
  }
  
  # Filter the dataframe based on the date
  df <- df %>%
    filter(date <= paste0(year, "-", sprintf("%02d", month), "-01"))
  
  # Add rows of NAs if necessary
  if (na_rows > 0) {
    # Create a data frame with NA rows for non-date columns
    na_df <- as.data.frame(matrix(NA, nrow = na_rows, ncol = ncol(df) - 1))
    names(na_df) <- names(df)[-1]
    
    # Create a data frame for the 'date' column with the correct number of rows
    na_dates <- data.frame(date = add_dates)
    
    # Combine the 'date' column with the NA rows for other columns
    na_df <- cbind(na_dates, na_df)
    
    # Combine the original data frame with the new rows of NAs
    df <- bind_rows(df, na_df)
  }
  
  return(df)
}
# user guide
result <- disag.base(df, 2022, 3)
tail(result)
