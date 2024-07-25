# generate monthly vintages

library(dplyr)
library(lubridate)

# Define a function to generate the dataframe for a specific quarter and month in the quarter
generate_quarter_df <- function(df, quarter_start, quarter_end, month_in_quarter) {
  # Break the dataframe into two parts
  df_aux1 <- df %>% select(date, pib, expec)
  df_aux2 <- df %>% select(-c(pib, expec))
  
  # Filter df_aux1 to the end of the specified quarter
  df_aux1_filtered <- df_aux1 %>%
    filter(date <= quarter_end)
  
  # Filter df_aux2 to the end of the specified quarter
  df_aux2_filtered <- df_aux2 %>%
    filter(date <= quarter_end) %>%
    mutate(across(-date, ~ ifelse(date >= quarter_start & month(date) > month(quarter_start) + (month_in_quarter - 1), NA, .)))
  
  # Bind the two dataframes by date
  combined_df <- cbind(df_aux1_filtered, df_aux2_filtered %>% select(-date))
  
  return(combined_df)
}

# Generate the list of start and end dates for each quarter from Q1 2020 to Q1 2024
quarter_start_dates <- seq(from = as.Date("2020-01-01"), to = as.Date("2024-01-01"), by = "quarter")
quarter_end_dates <- seq(from = as.Date("2020-03-01"), to = as.Date("2024-03-01"), by = "quarter")

# Initialize an empty list to store the dataframes
df_list <- list()

# Loop through each pair of start and end dates and generate the corresponding dataframe for M1, M2, and M3
for (i in seq_along(quarter_start_dates)) {
  quarter_start <- quarter_start_dates[i]
  quarter_end <- quarter_end_dates[i]
  
  # Generate dataframes for M1, M2, and M3
  for (month_in_quarter in 1:3) {
    df_generated <- generate_quarter_df(df, quarter_start, quarter_end, month_in_quarter)
    
    # Create a name for the dataframe
    year <- year(quarter_start)
    quarter <- paste0("Q", quarter(quarter_start))
    name <- paste0("v1_", year, "_", quarter, "_M", month_in_quarter)
    
    # Assign the dataframe to the list with the created name
    df_list[[name]] <- df_generated
  }
}
