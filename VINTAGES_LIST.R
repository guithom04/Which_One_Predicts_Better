library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)

# Ensure to run and define the final_prtdb and daily_to_monthly functions before this

# Define the function to generate a list of monthly dataframes for each date
generate_vintages <- function(start_year, start_month, end_year, end_month, day_list) {
  vintage_list <- list()
  
  for (year in start_year:end_year) {
    for (month in 1:12) {
      if (year == start_year && month < start_month) next
      if (year == end_year && month > end_month) break
      
      for (day in day_list) {
        # Ensure the day is valid for the given month and year
        if (day <= days_in_month(ymd(paste(year, month, 1, sep = "-")))) {
          date <- as.Date(paste(year, month, day, sep = "-"))
          vintage_name <- paste0("vintage_", format(date, "%Y_%m_%d"))
          vintage_list[[vintage_name]] <- tryCatch(
            daily_to_monthly(final_prtdb(year, month, day)),
            error = function(e) {
              message(paste("Error processing date:", date, "-", e))
              return(NULL)
            }
          )
        }
      }
    }
  }
  
  # Remove any NULL entries caused by errors
  vintage_list <- Filter(Negate(is.null), vintage_list)
  
  return(vintage_list)
}

# Define the parameters
start_year <- 2020
start_month <- 1
end_year <- 2024
end_month <- 3
day_list <- c(15, 28, 30)  # Include both 28 and 30 to cover month-end cases

# Generate the vintage list
vintages <- generate_vintages(start_year, start_month, end_year, end_month, day_list)

save(vintages, file = "vintages_list.RData")
