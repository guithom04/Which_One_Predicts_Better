vmidas <- function(df, y, m) {
  calculate_errors <- function(df) {
    # Extract the forecasts and the true value
    forecasts <- as.numeric(df[1, 1:(ncol(df)-1)])
    true_value <- df$pib
    
    # Calculate errors
    rmse_values <- sapply(forecasts, function(x) rmse(true_value, x))
    mape_values <- sapply(forecasts, function(x) mape(true_value, x))
    mae_values <- sapply(forecasts, function(x) mae(true_value, x))
    mse_values <- sapply(forecasts, function(x) mse(true_value, x))
    
    # Calculate averages
    average_rmse <- mean(rmse_values)
    average_mape <- mean(mape_values)
    average_mae <- mean(mae_values)
    average_mse <- mean(mse_values)
    
    # Create results dataframe
    results <- data.frame(
      Method = c("RMSE", "MAPE", "MAE", "MSE"),
      midas1 = c(rmse_values[1], mape_values[1], mae_values[1], mse_values[1]),
      midas2 = c(rmse_values[2], mape_values[2], mae_values[2], mse_values[2]),
      midas3 = c(rmse_values[3], mape_values[3], mae_values[3], mse_values[3]),
      midas4 = c(rmse_values[4], mape_values[4], mae_values[4], mse_values[4]),
      midas5 = c(rmse_values[5], mape_values[5], mae_values[5], mse_values[5]),
      midas6 = c(rmse_values[6], mape_values[6], mae_values[6], mse_values[6]),
      Average = c(average_rmse, average_mape, average_mae, average_mse)
    )
    
    return(results)
  }
  library(zoo)
  library(lubridate)
  library(sidrar)

  # # test
  # y = 2020
  # m = 2
  # df = df_all
  
  # Assuming the necessary functions `vintages_midas` and `balance_panel` are defined elsewhere
  
  v_midas = vintages_midas(df, y, m)
  bal.v.midas = balance_panel(v_midas, method = "NN")
  
  # Get the year and last month of the last quarter
  result <- get_last_quarter(y, m)
  
  # Get the last month of the current quarter
  result1 <- get_last_month_of_current_quarter(y, m)
  
  # Calculate the quarter values
  qt.old <- quarter(ymd(paste0(result$year, "-", result$last_month, "-01")))
  yr.old <- year(ymd(paste0(result$year, "-", result$last_month, "-01")))
  q <- quarter(ymd(paste0(y, "-", m, "-01")))
  
  # PIB
  table.pib <- get_sidra(api = "/t/5932/n1/all/v/6561/p/all/c11255/90707/d/v6561%201")
  pib.ts <- ts(table.pib$Valor, start = c(1996, 01), frequency = 4)
  pib.full <- window(pib.ts, start = c(2004, 02), end = c(y, q))
  pib.in <- window(pib.ts, start = c(2004, 02), end = c(result$year, qt.old))
  
  # PIM
  pim <- ts(bal.v.midas$pim, start = c(2004, 02), frequency = 12)
  pim.full <- window(pim, start = c(2004, 04), end = c(result1$year, result1$last_month))
  pim.in <- window(pim, start = c(2004, 04), end = c(result$year, result$last_month))
  
  # PIMD
  pimd <- ts(bal.v.midas$pimd, start = c(2004, 02), frequency = 12)
  pimd.full <- window(pimd, start = c(2004, 04), end = c(result1$year, result1$last_month))
  pimd.in <- window(pimd, start = c(2004, 04), end = c(result$year, result$last_month))
  
  # PMCA
  pmca <- ts(bal.v.midas$pmca, start = c(2004, 02), frequency = 12)
  pmca.full <- window(pmca, start = c(2004, 04), end = c(result1$year, result1$last_month))
  pmca.in <- window(pmca, start = c(2004, 04), end = c(result$year, result$last_month))
  
  # PMCAD
  pmcad <- ts(bal.v.midas$pmcad, start = c(2004, 02), frequency = 12)
  pmcad.full <- window(pmcad, start = c(2004, 04), end = c(result1$year, result1$last_month))
  pmcad.in <- window(pmcad, start = c(2004, 04), end = c(result$year, result$last_month))
  
  # IBC
  ibc <- ts(bal.v.midas$ibc, start = c(2004, 02), frequency = 12)
  ibc.full <- window(ibc, start = c(2004, 04), end = c(result1$year, result1$last_month))
  ibc.in <- window(ibc, start = c(2004, 04), end = c(result$year, result$last_month))
  
  # EXPEC
  expec <- ts(bal.v.midas$expec, start = c(2004, 02), frequency = 12)
  expec.full <- window(expec, start = c(2004, 04), end = c(result1$year, result1$last_month))
  expec.in <- window(expec, start = c(2004, 04), end = c(result$year, result$last_month))
  
  # Create the intersections
  in_series <- ts.intersect(pim.in, pimd.in, pmca.in, pmcad.in, ibc.in, expec.in)
  full_series <- ts.intersect(pim.full, pimd.full, pmca.full, pmcad.full, ibc.full, expec.full)
  
  # Create the list of series
  result_list <- list(pib_in = pib.in, pib_full = pib.full, in_series = in_series, full_series = full_series)
  
  # midas1
  midas_final1 = midas_r(formula = pib.in ~
                           mls(pim.in,1,3)+
                           mls(pmca.in,1,3)+
                           mls(ibc.in,1,3)+
                           mls(expec.in,1,3), start = NULL)
  # forecast
  fulldata1 <- list(pim.in = pim.full,
                    pmca.in = pmca.full,
                    ibc.in = ibc.full,
                    expec.in = expec.full,
                    pib.in = pib.full)

  insample1 <- 1:length(pib.in)
  outsample1 <- (1:length(fulldata1$pib.in))[-insample1]
  avgf1 = average_forecast(list(midas_final1),
                           data = fulldata1,
                           insample = insample1,
                           outsample = outsample1)
  avgf1$forecast
  
  # midas2
  midas_final2 = midas_r(pib.in~ mls(pim.in, 1:3, 3)
                         + mls(pmca.in, 1:3, 3)
                         + mls(ibc.in, 1:3, 3), start = list(pim.in = rep(0,3),
                                                             pmca.in = rep(0,3),
                                                             ibc.in = rep(0,3)))
  # forecast
  fulldata2 <- list(pim.in = pim.full,
                    pmca.in = pmca.full,
                    ibc.in = ibc.full,
                    pib.in = pib.full)


  insample2 <- 1:length(pib.in)
  outsample2 <- (1:length(fulldata2$pib.in))[-insample2]
  avgf2 = average_forecast(list(midas_final2), data = fulldata2, insample = insample2, outsample = outsample2)
  avgf2$forecast

  # midas 3
  midas_final3 = midas_r(pib.in~ mls(pim.in, 1:3, 3)
                         + mls(ibc.in, 1:3, 3), start = list(pim.in = rep(0,3),
                                                             ibc.in = rep(0,3)))
  # forecast


  fulldata3 <- list(pim.in = pim.full,
                    ibc.in = ibc.full,
                    pib.in = pib.full)

  insample3 <- 1:length(pib.in)
  outsample3 <- (1:length(fulldata3$pib.in))[-insample3]
  avgf3 = average_forecast(list(midas_final3), data = fulldata3, insample = insample3, outsample = outsample3)
  avgf3$forecast

  # midas 4
  midas_final4 = midas_r(pib.in~ mls(expec.in, 1, 3)
                         + mls(ibc.in, 1, 3), start = NULL)
  # forecast
  fulldata4 <- list(expec.in = expec.full,
                    ibc.in = ibc.full,
                    pib.in = pib.full)

  insample4 <- 1:length(pib.in)
  outsample4 <- (1:length(fulldata4$pib.in))[-insample4]
  avgf4 = average_forecast(list(midas_final4), data = fulldata4, insample = insample4, outsample = outsample4)
  avgf4$forecast

  # midas 5
  midas_final5 = midas_r(pib.in~mls(pimd.in, 1, 3)+mls(ibc.in,1,3)+mls(expec.in,1,3), start = NULL)
  summary(midas_final5)

  # forecast
  fulldata5 <- list(pimd.in = pimd.full,
                    ibc.in = ibc.full,
                    expec.in = expec.full,
                    pib.in = pib.full)

  insample5 <- 1:length(pib.in)
  outsample5 <- (1:length(fulldata5$pib.in))[-insample5]
  avgf5 = average_forecast(list(midas_final5), data = fulldata5, insample = insample5, outsample = outsample5)
  avgf5$forecast

  # midas 6
  midas_final6 = midas_r(pib.in~mls(pimd.in, 1, 3)+mls(pmcad.in, 1, 3)+mls(ibc.in,1,3)+mls(expec.in,1,3), start = NULL)
  summary(midas_final6)

  # forecast
  fulldata6 <- list(pimd.in = pimd.full,
                    pmcad.in = pmcad.full,
                    ibc.in = ibc.full,
                    expec.in = expec.full,
                    pib.in = pib.full)

  insample6 <- 1:length(pib.in)
  outsample6 <- (1:length(fulldata6$pib.in))[-insample6]
  avgf6 = average_forecast(list(midas_final6), data = fulldata6, insample = insample6, outsample = outsample6)
  avgf6$forecast


  midas.1s <- data.frame(midas1 = avgf1$forecast,
                         midas2 = avgf2$forecast,
                         midas3 = avgf3$forecast,
                         midas4 = avgf4$forecast,
                         midas5 = avgf5$forecast,
                         midas6 = avgf6$forecast,
                         pib = pib.full[length(pib.full)])

  error_results <- calculate_errors(midas.1s)

  midas_full = list(midas1s = midas.1s,
                    errors  = error_results)

  return(midas_full)
  
}

# Initialize an empty list to store results
results_list <- list()

# Loop from January 2020 to March 2024
start_year <- 2020
end_year <- 2024

for (year in start_year:end_year) {
  for (month in 1:12) {
    # Break the loop if the current year and month exceed March 2024
    if (year == end_year && month > 3) {
      break
    }
    
    # Print current year and month to track progress
    cat("Processing year:", year, "month:", month, "\n")
    
    # Call the vmidas function and store the result in the list
    result <- tryCatch({
      vmidas(df_all, year, month)
    }, error = function(e) {
      cat("Error processing year:", year, "month:", month, "\n")
      return(NULL)
    })
    
    results_list[[paste(year, month, sep = "_")]] <- result
  }
}

save(results_list, file = "results_list.RData")
