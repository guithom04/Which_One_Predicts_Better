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



library(Metrics)
# declare functions used in the exercise
compute_errors <- function(predictions, actual) {
  rmse_val <- rmse(actual, predictions)
  mape_val <- mape(actual, predictions)
  mae_val <- mae(actual, predictions)
  mse_val <- mse(actual, predictions)
  return(c(RMSE = rmse_val, MAPE = mape_val, MAE = mae_val, MSE = mse_val))
}
compute_all_errors <- function(df) {
  # Initialize an empty dataframe to store the results
  results <- data.frame(
    Model = character(),
    RMSE = numeric(),
    MAPE = numeric(),
    MAE = numeric(),
    MSE = numeric(),
    Mean_Error = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Get the list of model columns (excluding 'date' and 'pib')
  model_columns <- colnames(df)[!colnames(df) %in% c("date", "pib")]
  
  # Compute errors for each model
  for (model in model_columns) {
    errors <- compute_errors(df[[model]], df$pib)
    mean_error <- mean(errors)
    results <- rbind(results, data.frame(Model = model, t(errors), Mean_Error = mean_error))
  }
  
  # Compute mean of all metrics and overall mean
  mean_errors <- colMeans(results[, -1])
  mean_errors["Mean_Error"] <- mean(mean_errors)
  results <- rbind(results, c("Mean of All Models", mean_errors))
  
  return(results)
}
calculate_errors.ts <- function(forecast_df) {
  true_values <- forecast_df$pib
  forecast_values <- forecast_df[ , -c(1, ncol(forecast_df))]  # exclude date and true_values columns
  
  rmse_values <- sapply(forecast_values, function(forecast) rmse(true_values, forecast))
  mape_values <- sapply(forecast_values, function(forecast) mape(true_values, forecast))
  mae_values <- sapply(forecast_values, function(forecast) mae(true_values, forecast))
  mse_values <- sapply(forecast_values, function(forecast) mse(true_values, forecast))
  
  results <- data.frame(
    Method = c("RMSE", "MAPE", "MAE", "MSE"),
    midas1 = c(rmse_values[1], mape_values[1], mae_values[1], mse_values[1]),
    midas2 = c(rmse_values[2], mape_values[2], mae_values[2], mse_values[2]),
    midas3 = c(rmse_values[3], mape_values[3], mae_values[3], mse_values[3]),
    midas4 = c(rmse_values[4], mape_values[4], mae_values[4], mse_values[4]),
    midas5 = c(rmse_values[5], mape_values[5], mae_values[5], mse_values[5]),
    midas6 = c(rmse_values[6], mape_values[6], mae_values[6], mse_values[6]),
    Average = c(mean(rmse_values), mean(mape_values), mean(mae_values), mean(mse_values))
  )
  
  return(results)
}
machine_learning <- function(df, mode = c("1s","ts")){
  # packages
  library(sidrar)
  library(glmnet)
  library(neuralnet)
  library(e1071)
  library(randomForest)
  library(gbm)
  library(scales)
  library(lubridate)
  if (mode == "1s") {
    
    # treino e teste
    treinox <- split_prtdb.1s$df_in[,-c(1:2)]
    testex <- split_prtdb.1s$df_out[,-c(1:2)]
    treinoy <- split_prtdb.1s$df_in[,c(2)]$pib
    testey <- split_prtdb.1s$df_out[,c(2)]$pib
    
    # dataframe for neuralnet
    train_data <- data.frame(treinox, treinoy = treinoy)
    
    # LASSO
    LASSO.expec <- cv.glmnet(as.matrix(treinox), treinoy, alpha = 1)
    predict.lasso.EXPEC <- predict(LASSO.expec, newx = as.matrix(testex), s = "lambda.min")
    lasso <- mean(predict.lasso.EXPEC)
    
    # adaLASSO
    tau <- 1
    first.step.coef <- coef(LASSO.expec, s = "lambda.min")[-1]
    penalty.factor <- abs(first.step.coef + 1/sqrt(nrow(testex)))^(-tau)
    adalasso.expec <- cv.glmnet(as.matrix(treinox), treinoy, alpha = 1, penalty.factor = penalty.factor)
    pred.adalasso.expec <- predict(adalasso.expec, newx = as.matrix(testex), s = "lambda.min")
    adaLASSO <- mean(pred.adalasso.expec)
    
    # Ridge Regression
    ridge.fit.expec <- cv.glmnet(as.matrix(treinox), treinoy, alpha = 0)
    ridge.predict.expec <- predict(ridge.fit.expec, s = "lambda.min", newx = as.matrix(testex))
    ridge <- mean(ridge.predict.expec)
    
    # Elastic Net
    en.fit.expec <- cv.glmnet(as.matrix(treinox), treinoy, alpha = 0.5)
    en.predicted.expec <- predict(en.fit.expec, newx = as.matrix(testex), s = "lambda.min")
    elastic_net <- mean(en.predicted.expec)
    
    # Neural Networks 1
    NN1.fit.expec <- neuralnet(
      treinoy ~ ., 
      data = train_data, 
      hidden = c(3, 2), 
      err.fct = "sse", 
      linear.output = TRUE, 
      stepmax = 1e6,          # Increase the maximum steps
      learningrate = 0.1,    # Adjust the learning rate
      lifesign = "minimal",   # Reduce verbosity
      rep = 1,               # Increase the number of repetitions
      threshold = 0.3
    )
    
    predict.NN1.expec <- neuralnet::compute(NN1.fit.expec, as.data.frame(testex))$net.result
    nn1 <- mean(predict.NN1.expec)
    
    # Neural Network 2
    NN2.fit.expec <- neuralnet(
      treinoy ~ ., 
      data = train_data, 
      hidden = 3, 
      err.fct = "sse", 
      threshold = 0.3,
      learningrate = 0.1,
      linear.output = TRUE)
    
    predict.NN2.expec <- neuralnet::compute(NN2.fit.expec, as.data.frame(testex))$net.result
    nn2 <- mean(predict.NN2.expec)
    
    # Neural Network 3
    NN3.fit.expec <- neuralnet(
      treinoy ~ ., 
      data = train_data, 
      hidden = c(5, 3, 2), 
      err.fct = "sse",
      threshold = 0.3,
      learningrate = 0.5,
      linear.output = TRUE)
    
    predict.NN3.expec <- neuralnet::compute(NN3.fit.expec, as.data.frame(testex))$net.result
    nn3 <- mean(predict.NN3.expec)
    
    # Support Vector Regression (SVR)
    svr.fit.expec <- svm(treinoy ~ ., data = as.data.frame(treinox), kernel = "radial")
    svr.predict.expec <- predict(svr.fit.expec, newdata = as.data.frame(testex))
    svr <- mean(svr.predict.expec)
    
    # Random Forest
    rf.fit.expec <- randomForest(treinoy ~ ., data = as.data.frame(treinox), ntree = 500)
    rf.predict.expec <- predict(rf.fit.expec, newdata = as.data.frame(testex))
    rf <- mean(rf.predict.expec)
    
    # Gradient Boosting
    gbm.fit.expec <- gbm(treinoy ~ ., data = as.data.frame(treinox), distribution = "gaussian", n.trees = 1000, interaction.depth = 3, cv.folds = 5)
    best.iter.expec <- gbm.perf(gbm.fit.expec, method = "cv")
    gbm.predict.expec <- predict(gbm.fit.expec, newdata = as.data.frame(testex), n.trees = best.iter.expec)
    gbm <- mean(gbm.predict.expec)
    
    # make dataframe of nowcasts 1s - date has to be extracted from prtdb inputs
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
    # date
    date = split_prtdb.1s$df_out[,c(1)]
    
    # dataframe storing
    
    nowcast_df.1s <- data.frame(
      date = date,
      LASSO = lasso,
      adaLASSO = adaLASSO,
      Ridge = ridge,
      Elastic_Net = elastic_net,
      NN1 = nn1,
      NN2 = nn2,
      NN3 = nn3,
      SVR = svr,
      Random_Forest = rf,
      Gradient_Boosting = gbm,
      pib = testey
    )
    
    return(nowcast_df.1s)
  }
  if (mode == "ts") {
    
    # treino e teste
    treinox <- split_prtdb.ts$df_in[,-c(1:2)]
    testex <- split_prtdb.ts$df_out[,-c(1:2)]
    treinoy <- split_prtdb.ts$df_in[,c(2)]$pib
    testey <- split_prtdb.ts$df_out[,c(2)]$pib
    
    # dataframe for neuralnet
    train_data <- data.frame(treinox, treinoy = treinoy)
    
    # ML models estimated functions
    models <- function(){
      
      # LASSO
      LASSO.expec <- cv.glmnet(as.matrix(treinox), treinoy, alpha = 1)
      predict.lasso.EXPEC <- predict(LASSO.expec, newx = as.matrix(testex), s = "lambda.min")
      
      # adaLASSO
      tau <- 1
      first.step.coef <- coef(LASSO.expec, s = "lambda.min")[-1]
      penalty.factor <- abs(first.step.coef + 1/sqrt(nrow(testex)))^(-tau)
      adalasso.expec <- cv.glmnet(as.matrix(treinox), treinoy, alpha = 1, penalty.factor = penalty.factor)
      pred.adalasso.expec <- predict(adalasso.expec, newx = as.matrix(testex), s = "lambda.min")
      
      # Ridge Regression
      ridge.fit.expec <- cv.glmnet(as.matrix(treinox), treinoy, alpha = 0)
      ridge.predict.expec <- predict(ridge.fit.expec, s = "lambda.min", newx = as.matrix(testex))
      
      # Elastic Net
      en.fit.expec <- cv.glmnet(as.matrix(treinox), treinoy, alpha = 0.5)
      en.predicted.expec <- predict(en.fit.expec, newx = as.matrix(testex), s = "lambda.min")
      
      
      # Neural Networks 1
      NN1.fit.expec <- neuralnet(
        treinoy ~ ., 
        data = train_data, 
        hidden = c(3, 2), 
        err.fct = "sse", 
        linear.output = TRUE, 
        stepmax = 1e6,          # Increase the maximum steps
        learningrate = 0.1,    # Adjust the learning rate
        lifesign = "minimal",   # Reduce verbosity
        rep = 1,               # Increase the number of repetitions
        threshold = 0.3
      )
      predict.NN1.expec <- neuralnet::compute(NN1.fit.expec, as.data.frame(testex))$net.result
      
      
      # Neural Network 2
      NN2.fit.expec <- neuralnet(
        treinoy ~ ., 
        data = train_data, 
        hidden = 3, 
        err.fct = "sse", 
        threshold = 0.3,
        learningrate = 0.1,
        linear.output = TRUE)
      predict.NN2.expec <- neuralnet::compute(NN2.fit.expec, as.data.frame(testex))$net.result
      
      # Neural Network 3
      NN3.fit.expec <- neuralnet(
        treinoy ~ ., 
        data = train_data, 
        hidden = c(5, 3, 2), 
        err.fct = "sse",
        threshold = 0.3,
        learningrate = 0.5,
        linear.output = TRUE)
      
      predict.NN3.expec <- neuralnet::compute(NN3.fit.expec, as.data.frame(testex))$net.result
      
      # Support Vector Regression (SVR)
      svr.fit.expec <- svm(treinoy ~ ., data = as.data.frame(treinox), kernel = "radial")
      svr.predict.expec <- predict(svr.fit.expec, newdata = as.data.frame(testex))
      
      
      # Random Forest
      rf.fit.expec <- randomForest(treinoy ~ ., data = as.data.frame(treinox), ntree = 500)
      rf.predict.expec <- predict(rf.fit.expec, newdata = as.data.frame(testex))
      
      
      
      # Gradient Boosting
      gbm.fit.expec <- gbm(treinoy ~ ., data = as.data.frame(treinox), distribution = "gaussian", n.trees = 1000, interaction.depth = 3, cv.folds = 5)
      best.iter.expec <- gbm.perf(gbm.fit.expec, method = "cv")
      gbm.predict.expec <- predict(gbm.fit.expec, newdata = as.data.frame(testex), n.trees = best.iter.expec)
      
      
      # Define the start date and create a sequence of dates
      
      dates <- split_prtdb.ts$df_out[,c(1)]
      
      
      
      
      # df nowcasting ts
      nowcast_df.ts <- data.frame(
        Date = dates,
        lasso = predict.lasso.EXPEC,
        adaLASSO = pred.adalasso.expec,
        ridge = ridge.predict.expec,
        elastic_net = en.predicted.expec,
        NN1 = predict.NN1.expec,
        NN2 = predict.NN2.expec,
        NN3 = predict.NN3.expec,
        SVR = svr.predict.expec,
        Random_Forest = rf.predict.expec,
        Gradient_Boosting = gbm.predict.expec,
        pib = testey
      )
      
      colnames(nowcast_df.ts)[2] <- c("lasso")
      colnames(nowcast_df.ts)[3] <- c("adaLASSO")
      colnames(nowcast_df.ts)[4] <- c("ridge")
      colnames(nowcast_df.ts)[5] <- c("elastic_net")
      
      return(nowcast_df.ts)
      
      
    }
    
    # call
    models_estimated <- models()
    
    return(models_estimated)
  }
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
split.prtdb <- function(df, mode = c("1s","ts")){
  library(dplyr)
  # balance panel
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
  df.b <- balanced(df)
  if (mode == "1s") {
    filtered.in  = df.b[1:(nrow(df.b)-3),]
    filtered.out = df.b[-c(1:(nrow(df.b)-3)),]
  }
  if (mode == "ts") {
    filtered.in  = df.b %>%
      filter(date <= "2019-12-01")
    filtered.out  = df.b %>%
      filter(date > "2019-12-01")
  }
  dfs <- list(df_in  = filtered.in,
              df_out = filtered.out)
  return(dfs)
}
process_DFM <- function(type = c("1s", "ts"),
                        split_prtdb.1s = NULL,
                        split_prtdb.ts = NULL) {
  library(dfms)
  library(forecast)
  library(zoo)
  library(dplyr)
  
  type <- match.arg(type)
  
  if (type == "1s") {
    # Full dataset for "1s"
    full <- bind_rows(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
    pib <- ts(full[, 2], start = c(2007, 10), frequency = 12)
    
    # Fit the DFM model with r = 3 and p = 3
    mod <- DFM(full[,-1], r = 3, p = 3)
    
    # Extract factors
    F1 <- ts(mod$F_pca[, 1], start = start(pib), frequency = frequency(pib))
    F2 <- ts(mod$F_pca[, 2], start = start(pib), frequency = frequency(pib))
    F3 <- ts(mod$F_pca[, 3], start = start(pib), frequency = frequency(pib))
    
    # Bridge equation
    fit1 <- lm(pib ~ 1 + F1 + F2 + F3)
    
    # Prepare new data for prediction
    out.1s <- cbind.data.frame(F1 = tail(F1, 3), F2 = tail(F2, 3), F3 = tail(F3, 3))
    
    predict.df = data.frame(date = split_prtdb.1s$df_out$date,
                            dfm1 = predict(fit1, newdata = out.1s),
                            pib = split_prtdb.1s$df_out$pib)
    # Make prediction and return the mean
    # return(mean(predict(fit1, newdata = out.1s)))
    return(predict.df)
  } else if (type == "ts") {
    # In-sample and out-of-sample datasets for "ts"
    x.in.ts <- split_prtdb.ts$df_in[, -c(1:2)]
    x.out.ts <- split_prtdb.ts$df_out[, -c(1:2)]
    pib.ts <- ts(split_prtdb.ts$df_in[, 2], start = c(2007, 10), frequency = 12)
    pib.ts.out <- ts(split_prtdb.ts$df_out[, 2], start = c(2020, 01), frequency = 12)
    
    # Fit the DFM model with r = 3 and p = 3
    mod.ts <- DFM(x.in.ts, r = 3, p = 3)
    
    # Extract factors
    F1ts <- ts(mod.ts$F_pca[, 1], start = start(pib.ts), frequency = frequency(pib.ts))
    F2ts <- ts(mod.ts$F_pca[, 2], start = start(pib.ts), frequency = frequency(pib.ts))
    F3ts <- ts(mod.ts$F_pca[, 3], start = start(pib.ts), frequency = frequency(pib.ts))
    
    # Bridge equation
    fit.ts <- lm(pib.ts ~ 1 + F1ts + F2ts + F3ts)
    
    # Fit the out-of-sample DFM model
    mod.ts.out <- DFM(x.out.ts, r = 3, p = 3)
    
    # Extract factors for out-of-sample
    F1ts.out <- ts(mod.ts.out$F_pca[, 1], start = start(pib.ts.out), frequency = frequency(pib.ts.out))
    F2ts.out <- ts(mod.ts.out$F_pca[, 2], start = start(pib.ts.out), frequency = frequency(pib.ts.out))
    F3ts.out <- ts(mod.ts.out$F_pca[, 3], start = start(pib.ts.out), frequency = frequency(pib.ts.out))
    
    # Align lengths if necessary
    min_length <- min(length(F1ts.out), length(F2ts.out), length(F3ts.out))
    F1ts.out <- F1ts.out[1:min_length]
    F2ts.out <- F2ts.out[1:min_length]
    F3ts.out <- F3ts.out[1:min_length]
    
    # Prepare the new data for prediction
    out.ts <- data.frame(F1ts = F1ts.out, F2ts = F2ts.out, F3ts = F3ts.out)
    
    # Make predictions and return the time series
    ts_pred <- ts(predict(fit.ts, newdata = out.ts), start = c(2020, 01), frequency = 12)
    
    # Plotting the actual vs forecasted values
    plot(pib.ts.out, type = "l", col = "red", lwd = 2,
         main = "Actual vs Forecasted GDP",
         xlab = "Time", ylab = "GDP",
         ylim = range(c(pib.ts.out, ts_pred)))
    lines(ts_pred, col = "blue", lwd = 2)
    legend("topright", legend = c("Actual GDP", "Forecasted GDP"),
           col = c("red", "blue"), lwd = 2)
    
    df.test.dfm = data.frame(date = split_prtdb.ts$df_out$date,
               dfm1 = ts_pred,
               pib =  split_prtdb.ts$df_out$pib)
    return(df.test.dfm)
  }
}


# run machine learning models, 1s and ts - v1
v1 = df_list$v1_2020_Q1_M1
balanced.v1 = balance_panel(v1, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v1, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v1, mode = "ts")
ml.v1.1s <- machine_learning(balanced.v1, mode = "1s")
errors.ml.v1.1s <- compute_all_errors(ml.v1.1s)
ml.v1.ts <- machine_learning(balanced.v1, mode = "ts")
errors.ml.v1.ts <- compute_all_errors(ml.v1.ts)


# run machine learning models, 1s and ts - v2
v2 = df_list$v1_2020_Q1_M2
balanced.v2 = balance_panel(v2, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v2, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v2, mode = "ts")
ml.v2.1s <- machine_learning(balanced.v2, mode = "1s")
errors.ml.v2.1s <- compute_all_errors(ml.v2.1s)
ml.v2.ts <- machine_learning(balanced.v2, mode = "ts")
errors.ml.v2.ts <- compute_all_errors(ml.v2.ts)


# run machine learning models, 1s and ts - v3
v3 = df_list$v1_2020_Q1_M3
balanced.v3 = balance_panel(v3, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v3, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v3, mode = "ts")
ml.v3.1s <- machine_learning(balanced.v3, mode = "1s")
errors.ml.v3.1s <- compute_all_errors(ml.v3.1s)
ml.v3.ts <- machine_learning(balanced.v3, mode = "ts")
errors.ml.v3.ts <- compute_all_errors(ml.v3.ts)


# Q2 2020 M1
v4 = df_list$v1_2020_Q2_M1
balanced.v4 = balance_panel(v4, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v4, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v4, mode = "ts")
ml.v4.1s <- machine_learning(balanced.v4, mode = "1s")
errors.ml.v4.1s <- compute_all_errors(ml.v4.1s)
ml.v4.ts <- machine_learning(balanced.v4, mode = "ts")
errors.ml.v4.ts <- compute_all_errors(ml.v4.ts)

# Q2 2020 M2
v5 = df_list$v1_2020_Q2_M2
balanced.v5 = balance_panel(v5, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v5, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v5, mode = "ts")
ml.v5.1s <- machine_learning(balanced.v5, mode = "1s")
errors.ml.v5.1s <- compute_all_errors(ml.v5.1s)
ml.v5.ts <- machine_learning(balanced.v5, mode = "ts")
errors.ml.v5.ts <- compute_all_errors(ml.v5.ts)

# Q2 2020 M3
v6 = df_list$v1_2020_Q2_M3
balanced.v6 = balance_panel(v6, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v6, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v6, mode = "ts")
ml.v6.1s <- machine_learning(balanced.v6, mode = "1s")
errors.ml.v6.1s <- compute_all_errors(ml.v6.1s)
ml.v6.ts <- machine_learning(balanced.v6, mode = "ts")
errors.ml.v6.ts <- compute_all_errors(ml.v6.ts)


# Q3 2020 M1
v7 = df_list$v1_2020_Q3_M1
balanced.v7 = balance_panel(v7, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v7, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v7, mode = "ts")
ml.v7.1s <- machine_learning(balanced.v7, mode = "1s")
errors.ml.v7.1s <- compute_all_errors(ml.v7.1s)
ml.v7.ts <- machine_learning(balanced.v7, mode = "ts")
errors.ml.v7.ts <- compute_all_errors(ml.v7.ts)

# Q3 2020 M2
v8 = df_list$v1_2020_Q3_M2
balanced.v8 = balance_panel(v8, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v8, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v8, mode = "ts")
ml.v8.1s <- machine_learning(balanced.v8, mode = "1s")
errors.ml.v8.1s <- compute_all_errors(ml.v8.1s)
ml.v8.ts <- machine_learning(balanced.v8, mode = "ts")
errors.ml.v8.ts <- compute_all_errors(ml.v8.ts)

# Q3 2020 M3
v9 = df_list$v1_2020_Q3_M3
balanced.v9 = balance_panel(v9, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v9, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v9, mode = "ts")
ml.v9.1s <- machine_learning(balanced.v9, mode = "1s")
errors.ml.v9.1s <- compute_all_errors(ml.v9.1s)
ml.v9.ts <- machine_learning(balanced.v9, mode = "ts")
errors.ml.v9.ts <- compute_all_errors(ml.v9.ts)


# Q4 2020 M1
v10 = df_list$v1_2020_Q4_M1
balanced.v10 = balance_panel(v10, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v10, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v10, mode = "ts")
ml.v10.1s <- machine_learning(balanced.v10, mode = "1s")
errors.ml.v10.1s <- compute_all_errors(ml.v10.1s)
ml.v10.ts <- machine_learning(balanced.v10, mode = "ts")
errors.ml.v10.ts <- compute_all_errors(ml.v10.ts)

# Q4 2020 M2
v11 = df_list$v1_2020_Q4_M2
balanced.v11 = balance_panel(v11, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v11, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v11, mode = "ts")
ml.v11.1s <- machine_learning(balanced.v11, mode = "1s")
errors.ml.v11.1s <- compute_all_errors(ml.v11.1s)
ml.v11.ts <- machine_learning(balanced.v11, mode = "ts")
errors.ml.v11.ts <- compute_all_errors(ml.v11.ts)

# Q4 2020 M3
v12 = df_list$v1_2020_Q4_M3
balanced.v12 = balance_panel(v12, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v12, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v12, mode = "ts")
ml.v12.1s <- machine_learning(balanced.v12, mode = "1s")
errors.ml.v12.1s <- compute_all_errors(ml.v12.1s)
ml.v12.ts <- machine_learning(balanced.v12, mode = "ts")
errors.ml.v12.ts <- compute_all_errors(ml.v12.ts)

# Q1 2021 M1
v13 = df_list$v1_2021_Q1_M1
balanced.v13 = balance_panel(v13, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v13, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v13, mode = "ts")
ml.v13.1s <- machine_learning(balanced.v13, mode = "1s")
errors.ml.v13.1s <- compute_all_errors(ml.v13.1s)
ml.v13.ts <- machine_learning(balanced.v13, mode = "ts")
errors.ml.v13.ts <- compute_all_errors(ml.v13.ts)

# Q1 2021 M2
v14 = df_list$v1_2021_Q1_M2
balanced.v14 = balance_panel(v14, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v14, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v14, mode = "ts")
ml.v14.1s <- machine_learning(balanced.v14, mode = "1s")
errors.ml.v14.1s <- compute_all_errors(ml.v14.1s)
ml.v14.ts <- machine_learning(balanced.v14, mode = "ts")
errors.ml.v14.ts <- compute_all_errors(ml.v14.ts)

# Q1 2021 M3
v15 = df_list$v1_2021_Q1_M3
balanced.v15 = balance_panel(v15, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v15, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v15, mode = "ts")
ml.v15.1s <- machine_learning(balanced.v15, mode = "1s")
errors.ml.v15.1s <- compute_all_errors(ml.v15.1s)
ml.v15.ts <- machine_learning(balanced.v15, mode = "ts")
errors.ml.v15.ts <- compute_all_errors(ml.v15.ts)

# Q2 2021 M1
v16 = df_list$v1_2021_Q2_M1
balanced.v16 = balance_panel(v16, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v16, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v16, mode = "ts")
ml.v16.1s <- machine_learning(balanced.v16, mode = "1s")
errors.ml.v16.1s <- compute_all_errors(ml.v16.1s)
ml.v16.ts <- machine_learning(balanced.v16, mode = "ts")
errors.ml.v16.ts <- compute_all_errors(ml.v16.ts)

# Q2 2021 M2
v17 = df_list$v1_2021_Q2_M2
balanced.v17 = balance_panel(v17, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v17, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v17, mode = "ts")
ml.v17.1s <- machine_learning(balanced.v17, mode = "1s")
errors.ml.v17.1s <- compute_all_errors(ml.v17.1s)
ml.v17.ts <- machine_learning(balanced.v17, mode = "ts")
errors.ml.v17.ts <- compute_all_errors(ml.v17.ts)

# Q2 2021 M3
v18 = df_list$v1_2021_Q2_M3
balanced.v18 = balance_panel(v18, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v18, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v18, mode = "ts")
ml.v18.1s <- machine_learning(balanced.v18, mode = "1s")
errors.ml.v18.1s <- compute_all_errors(ml.v18.1s)
ml.v18.ts <- machine_learning(balanced.v18, mode = "ts")
errors.ml.v18.ts <- compute_all_errors(ml.v18.ts)

# Q3 2021 M1
v19 = df_list$v1_2021_Q3_M1
balanced.v19 = balance_panel(v19, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v19, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v19, mode = "ts")
ml.v19.1s <- machine_learning(balanced.v19, mode = "1s")
errors.ml.v19.1s <- compute_all_errors(ml.v19.1s)
ml.v19.ts <- machine_learning(balanced.v19, mode = "ts")
errors.ml.v19.ts <- compute_all_errors(ml.v19.ts)

# Q3 2021 M2
v20 = df_list$v1_2021_Q3_M2
balanced.v20 = balance_panel(v20, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v20, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v20, mode = "ts")
ml.v20.1s <- machine_learning(balanced.v20, mode = "1s")
errors.ml.v20.1s <- compute_all_errors(ml.v20.1s)
ml.v20.ts <- machine_learning(balanced.v20, mode = "ts")
errors.ml.v20.ts <- compute_all_errors(ml.v20.ts)

# Q3 2021 M3
v21 = df_list$v1_2021_Q3_M3
balanced.v21 = balance_panel(v21, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v21, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v21, mode = "ts")
ml.v21.1s <- machine_learning(balanced.v21, mode = "1s")
errors.ml.v21.1s <- compute_all_errors(ml.v21.1s)
ml.v21.ts <- machine_learning(balanced.v21, mode = "ts")
errors.ml.v21.ts <- compute_all_errors(ml.v21.ts)

# Q4 2021 M1
v22 = df_list$v1_2021_Q4_M1
balanced.v22 = balance_panel(v22, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v22, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v22, mode = "ts")
ml.v22.1s <- machine_learning(balanced.v22, mode = "1s")
errors.ml.v22.1s <- compute_all_errors(ml.v22.1s)
ml.v22.ts <- machine_learning(balanced.v22, mode = "ts")
errors.ml.v22.ts <- compute_all_errors(ml.v22.ts)

# Q4 2021 M2
v23 = df_list$v1_2021_Q4_M2
balanced.v23 = balance_panel(v23, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v23, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v23, mode = "ts")
ml.v23.1s <- machine_learning(balanced.v23, mode = "1s")
errors.ml.v23.1s <- compute_all_errors(ml.v23.1s)
ml.v23.ts <- machine_learning(balanced.v23, mode = "ts")
errors.ml.v23.ts <- compute_all_errors(ml.v23.ts)

# Q4 2021 M3
v24 = df_list$v1_2021_Q4_M3
balanced.v24 = balance_panel(v24, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v24, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v24, mode = "ts")
ml.v24.1s <- machine_learning(balanced.v24, mode = "1s")
errors.ml.v24.1s <- compute_all_errors(ml.v24.1s)
ml.v24.ts <- machine_learning(balanced.v24, mode = "ts")
errors.ml.v24.ts <- compute_all_errors(ml.v24.ts)

# Q1 2022 M1
v25 = df_list$v1_2022_Q1_M1
balanced.v25 = balance_panel(v25, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v25, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v25, mode = "ts")
ml.v25.1s <- machine_learning(balanced.v25, mode = "1s")
errors.ml.v25.1s <- compute_all_errors(ml.v25.1s)
ml.v25.ts <- machine_learning(balanced.v25, mode = "ts")
errors.ml.v25.ts <- compute_all_errors(ml.v25.ts)

# Q1 2022 M2
v26 = df_list$v1_2022_Q1_M2
balanced.v26 = balance_panel(v26, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v26, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v26, mode = "ts")
ml.v26.1s <- machine_learning(balanced.v26, mode = "1s")
errors.ml.v26.1s <- compute_all_errors(ml.v26.1s)
ml.v26.ts <- machine_learning(balanced.v26, mode = "ts")
errors.ml.v26.ts <- compute_all_errors(ml.v26.ts)

# Q1 2022 M3
v27 = df_list$v1_2022_Q1_M3
balanced.v27 = balance_panel(v27, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v27, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v27, mode = "ts")
ml.v27.1s <- machine_learning(balanced.v27, mode = "1s")
errors.ml.v27.1s <- compute_all_errors(ml.v27.1s)
ml.v27.ts <- machine_learning(balanced.v27, mode = "ts")
errors.ml.v27.ts <- compute_all_errors(ml.v27.ts)

# Q2 2022 M1
v28 = df_list$v1_2022_Q2_M1
balanced.v28 = balance_panel(v28, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v28, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v28, mode = "ts")
ml.v28.1s <- machine_learning(balanced.v28, mode = "1s")
errors.ml.v28.1s <- compute_all_errors(ml.v28.1s)
ml.v28.ts <- machine_learning(balanced.v28, mode = "ts")
errors.ml.v28.ts <- compute_all_errors(ml.v28.ts)

# Q2 2022 M2
v29 = df_list$v1_2022_Q2_M2
balanced.v29 = balance_panel(v29, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v29, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v29, mode = "ts")
ml.v29.1s <- machine_learning(balanced.v29, mode = "1s")
errors.ml.v29.1s <- compute_all_errors(ml.v29.1s)
ml.v29.ts <- machine_learning(balanced.v29, mode = "ts")
errors.ml.v29.ts <- compute_all_errors(ml.v29.ts)

# Q2 2022 M3
v30 = df_list$v1_2022_Q2_M3
balanced.v30 = balance_panel(v30, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v30, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v30, mode = "ts")
ml.v30.1s <- machine_learning(balanced.v30, mode = "1s")
errors.ml.v30.1s <- compute_all_errors(ml.v30.1s)
ml.v30.ts <- machine_learning(balanced.v30, mode = "ts")
errors.ml.v30.ts <- compute_all_errors(ml.v30.ts)

# Q3 2022 M1
v31 = df_list$v1_2022_Q3_M1
balanced.v31 = balance_panel(v31, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v31, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v31, mode = "ts")
ml.v31.1s <- machine_learning(balanced.v31, mode = "1s")
errors.ml.v31.1s <- compute_all_errors(ml.v31.1s)
ml.v31.ts <- machine_learning(balanced.v31, mode = "ts")
errors.ml.v31.ts <- compute_all_errors(ml.v31.ts)

# Q3 2022 M2
v32 = df_list$v1_2022_Q3_M2
balanced.v32 = balance_panel(v32, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v32, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v32, mode = "ts")
ml.v32.1s <- machine_learning(balanced.v32, mode = "1s")
errors.ml.v32.1s <- compute_all_errors(ml.v32.1s)
ml.v32.ts <- machine_learning(balanced.v32, mode = "ts")
errors.ml.v32.ts <- compute_all_errors(ml.v32.ts)

# Q3 2022 M3
v33 = df_list$v1_2022_Q3_M3
balanced.v33 = balance_panel(v33, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v33, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v33, mode = "ts")
ml.v33.1s <- machine_learning(balanced.v33, mode = "1s")
errors.ml.v33.1s <- compute_all_errors(ml.v33.1s)
ml.v33.ts <- machine_learning(balanced.v33, mode = "ts")
errors.ml.v33.ts <- compute_all_errors(ml.v33.ts)

# Q4 2022 M1
v34 = df_list$v1_2022_Q4_M1
balanced.v34 = balance_panel(v34, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v34, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v34, mode = "ts")
ml.v34.1s <- machine_learning(balanced.v34, mode = "1s")
errors.ml.v34.1s <- compute_all_errors(ml.v34.1s)
ml.v34.ts <- machine_learning(balanced.v34, mode = "ts")
errors.ml.v34.ts <- compute_all_errors(ml.v34.ts)

# Q4 2022 M2
v35 = df_list$v1_2022_Q4_M2
balanced.v35 = balance_panel(v35, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v35, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v35, mode = "ts")
ml.v35.1s <- machine_learning(balanced.v35, mode = "1s")
errors.ml.v35.1s <- compute_all_errors(ml.v35.1s)
ml.v35.ts <- machine_learning(balanced.v35, mode = "ts")
errors.ml.v35.ts <- compute_all_errors(ml.v35.ts)

# Q4 2022 M3
v36 = df_list$v1_2022_Q4_M3
balanced.v36 = balance_panel(v36, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v36, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v36, mode = "ts")
ml.v36.1s <- machine_learning(balanced.v36, mode = "1s")
errors.ml.v36.1s <- compute_all_errors(ml.v36.1s)
ml.v36.ts <- machine_learning(balanced.v36, mode = "ts")
errors.ml.v36.ts <- compute_all_errors(ml.v36.ts)

# Q1 2023 M1
v37 = df_list$v1_2023_Q1_M1
balanced.v37 = balance_panel(v37, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v37, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v37, mode = "ts")
ml.v37.1s <- machine_learning(balanced.v37, mode = "1s")
errors.ml.v37.1s <- compute_all_errors(ml.v37.1s)
ml.v37.ts <- machine_learning(balanced.v37, mode = "ts")
errors.ml.v37.ts <- compute_all_errors(ml.v37.ts)

# Q1 2023 M2
v38 = df_list$v1_2023_Q1_M2
balanced.v38 = balance_panel(v38, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v38, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v38, mode = "ts")
ml.v38.1s <- machine_learning(balanced.v38, mode = "1s")
errors.ml.v38.1s <- compute_all_errors(ml.v38.1s)
ml.v38.ts <- machine_learning(balanced.v38, mode = "ts")
errors.ml.v38.ts <- compute_all_errors(ml.v38.ts)

# Q1 2023 M3
v39 = df_list$v1_2023_Q1_M3
balanced.v39 = balance_panel(v39, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v39, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v39, mode = "ts")
ml.v39.1s <- machine_learning(balanced.v39, mode = "1s")
errors.ml.v39.1s <- compute_all_errors(ml.v39.1s)
ml.v39.ts <- machine_learning(balanced.v39, mode = "ts")
errors.ml.v39.ts <- compute_all_errors(ml.v39.ts)

# Q2 2023 M1
v40 = df_list$v1_2023_Q2_M1
balanced.v40 = balance_panel(v40, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v40, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v40, mode = "ts")
ml.v40.1s <- machine_learning(balanced.v40, mode = "1s")
errors.ml.v40.1s <- compute_all_errors(ml.v40.1s)
ml.v40.ts <- machine_learning(balanced.v40, mode = "ts")
errors.ml.v40.ts <- compute_all_errors(ml.v40.ts)

# Q2 2023 M2
v41 = df_list$v1_2023_Q2_M2
balanced.v41 = balance_panel(v41, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v41, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v41, mode = "ts")
ml.v41.1s <- machine_learning(balanced.v41, mode = "1s")
errors.ml.v41.1s <- compute_all_errors(ml.v41.1s)
ml.v41.ts <- machine_learning(balanced.v41, mode = "ts")
errors.ml.v41.ts <- compute_all_errors(ml.v41.ts)

# Q2 2023 M3
v42 = df_list$v1_2023_Q2_M3
balanced.v42 = balance_panel(v42, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v42, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v42, mode = "ts")
ml.v42.1s <- machine_learning(balanced.v42, mode = "1s")
errors.ml.v42.1s <- compute_all_errors(ml.v42.1s)
ml.v42.ts <- machine_learning(balanced.v42, mode = "ts")
errors.ml.v42.ts <- compute_all_errors(ml.v42.ts)

# Q3 2023 M1
v43 = df_list$v1_2023_Q3_M1
balanced.v43 = balance_panel(v43, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v43, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v43, mode = "ts")
ml.v43.1s <- machine_learning(balanced.v43, mode = "1s")
errors.ml.v43.1s <- compute_all_errors(ml.v43.1s)
ml.v43.ts <- machine_learning(balanced.v43, mode = "ts")
errors.ml.v43.ts <- compute_all_errors(ml.v43.ts)

# Q3 2023 M2
v44 = df_list$v1_2023_Q3_M2
balanced.v44 = balance_panel(v44, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v44, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v44, mode = "ts")
ml.v44.1s <- machine_learning(balanced.v44, mode = "1s")
errors.ml.v44.1s <- compute_all_errors(ml.v44.1s)
ml.v44.ts <- machine_learning(balanced.v44, mode = "ts")
errors.ml.v44.ts <- compute_all_errors(ml.v44.ts)

# Q3 2023 M3
v45 = df_list$v1_2023_Q3_M3
balanced.v45 = balance_panel(v45, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v45, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v45, mode = "ts")
ml.v45.1s <- machine_learning(balanced.v45, mode = "1s")
errors.ml.v45.1s <- compute_all_errors(ml.v45.1s)
ml.v45.ts <- machine_learning(balanced.v45, mode = "ts")
errors.ml.v45.ts <- compute_all_errors(ml.v45.ts)

# Q4 2023 M1
v46 = df_list$v1_2023_Q4_M1
balanced.v46 = balance_panel(v46, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v46, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v46, mode = "ts")
ml.v46.1s <- machine_learning(balanced.v46, mode = "1s")
errors.ml.v46.1s <- compute_all_errors(ml.v46.1s)
ml.v46.ts <- machine_learning(balanced.v46, mode = "ts")
errors.ml.v46.ts <- compute_all_errors(ml.v46.ts)

# Q4 2023 M2
v47 = df_list$v1_2023_Q4_M2
balanced.v47 = balance_panel(v47, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v47, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v47, mode = "ts")
ml.v47.1s <- machine_learning(balanced.v47, mode = "1s")
errors.ml.v47.1s <- compute_all_errors(ml.v47.1s)
ml.v47.ts <- machine_learning(balanced.v47, mode = "ts")
errors.ml.v47.ts <- compute_all_errors(ml.v47.ts)

# Q4 2023 M3
v48 = df_list$v1_2023_Q4_M3
balanced.v48 = balance_panel(v48, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v48, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v48, mode = "ts")
ml.v48.1s <- machine_learning(balanced.v48, mode = "1s")
errors.ml.v48.1s <- compute_all_errors(ml.v48.1s)
ml.v48.ts <- machine_learning(balanced.v48, mode = "ts")
errors.ml.v48.ts <- compute_all_errors(ml.v48.ts)

# Q1 2024 M1
v49 = df_list$v1_2024_Q1_M1
balanced.v49 = balance_panel(v49, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v49, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v49, mode = "ts")
ml.v49.1s <- machine_learning(balanced.v49, mode = "1s")
errors.ml.v49.1s <- compute_all_errors(ml.v49.1s)
ml.v49.ts <- machine_learning(balanced.v49, mode = "ts")
errors.ml.v49.ts <- compute_all_errors(ml.v49.ts)

# Q1 2024 M2
v50 = df_list$v1_2024_Q1_M2
balanced.v50 = balance_panel(v50, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v50, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v50, mode = "ts")
ml.v50.1s <- machine_learning(balanced.v50, mode = "1s")
errors.ml.v50.1s <- compute_all_errors(ml.v50.1s)
ml.v50.ts <- machine_learning(balanced.v50, mode = "ts")
errors.ml.v50.ts <- compute_all_errors(ml.v50.ts)

# Q1 2024 M3
v51 = df_list$v1_2024_Q1_M3
balanced.v51 = balance_panel(v51, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v51, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v51, mode = "ts")
ml.v51.1s <- machine_learning(balanced.v51, mode = "1s")
errors.ml.v51.1s <- compute_all_errors(ml.v51.1s)
ml.v51.ts <- machine_learning(balanced.v51, mode = "ts")
errors.ml.v51.ts <- compute_all_errors(ml.v51.ts)

plot(ml.v51.ts$pib, type = "l", col = "red")
lines(ml.v51.ts$Gradient_Boosting, col = "blue")
lines(ml.v51.ts$Random_Forest, col = "orange")
lines(ml.v51.ts$SVR, col = "green")
lines(ml.v51.ts$NN3, col = "purple")
lines(ml.v51.ts$NN2, col = "grey")
lines(ml.v51.ts$NN1, col = "black")
lines(ml.v51.ts$elastic_net, col = "darkblue")
lines(ml.v51.ts$ridge, col = "darkgreen")
lines(ml.v51.ts$adaLASSO, col = "pink")
lines(ml.v51.ts$lasso, col = "beige")


# perfeito, fazer agora modelos de redes neurais mais profundas
# tentar colocar xgboost

# deeper neural networks
# functions used in the exercise
compute_errors <- function(predictions, actual) {
  rmse_val <- rmse(actual, predictions)
  mape_val <- mape(actual, predictions)
  mae_val <- mae(actual, predictions)
  mse_val <- mse(actual, predictions)
  return(c(RMSE = rmse_val, MAPE = mape_val, MAE = mae_val, MSE = mse_val))
}
compute_all_errors <- function(df) {
  # Initialize an empty dataframe to store the results
  results <- data.frame(
    Model = character(),
    RMSE = numeric(),
    MAPE = numeric(),
    MAE = numeric(),
    MSE = numeric(),
    Mean_Error = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Get the list of model columns (excluding 'date' and 'pib')
  model_columns <- colnames(df)[!colnames(df) %in% c("Date", "PIB")]
  
  # Compute errors for each model
  for (model in model_columns) {
    errors <- compute_errors(df[[model]], df$PIB)
    mean_error <- mean(errors)
    results <- rbind(results, data.frame(Model = model, t(errors), Mean_Error = mean_error))
  }
  
  # Compute mean of all metrics and overall mean
  mean_errors <- colMeans(results[, -1])
  mean_errors["Mean_Error"] <- mean(mean_errors)
  results <- rbind(results, c("Mean of All Models", mean_errors))
  
  return(results)
}
forecast_models <- function(train, test) {
  library(keras)
  library(tensorflow)
  
  # Ensure reproducibility
  set.seed(42)
  
  # Preprocess data
  treinox <- train[,-c(1:2)]
  testex <- test[,-c(1:2)]
  treinoy <- train[,2]$pib # Assume PIB is the second column
  testey <- test[,2]$pib   # Assume PIB is the second column
  
  
  # Scale data
  scaler <- function(x) {
    scale(x, center = TRUE, scale = TRUE)
  }
  
  train_data <- scaler(treinox)
  test_data <- scaler(testex)
  
  # Reshape data for RNNs
  train_data <- array(train_data, dim = c(nrow(train_data), 1, ncol(train_data)))
  test_data <- array(test_data, dim = c(nrow(test_data), 1, ncol(test_data)))
  
  # Ensure response variable is numeric
  treinoy <- as.numeric(treinoy)
  testey <- as.numeric(testey)
  
  # Define RNN model
  model_rnn <- keras_model_sequential() %>%
    layer_simple_rnn(units = 50, input_shape = c(1, ncol(treinox)), return_sequences = TRUE) %>%
    layer_simple_rnn(units = 50) %>%
    layer_dense(units = 1, name = "pib")
  
  model_rnn %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam(),
    metrics = c("mean_absolute_error")
  )
  
  history_rnn <- model_rnn %>% fit(
    train_data, treinoy,
    epochs = 50,
    batch_size = 32,
    validation_split = 0.2,
    verbose = 1
  )
  
  predict_rnn <- model_rnn %>% predict(test_data)
  
  # Define LSTM model
  model_lstm <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(1, ncol(treinox)), return_sequences = TRUE) %>%
    layer_lstm(units = 50) %>%
    layer_dense(units = 1, name = "pib")
  
  model_lstm %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam(),
    metrics = c("mean_absolute_error")
  )
  
  history_lstm <- model_lstm %>% fit(
    train_data, treinoy,
    epochs = 50,
    batch_size = 32,
    validation_split = 0.2,
    verbose = 1
  )
  
  predict_lstm <- model_lstm %>% predict(test_data)
  
  # Define GRU model
  model_gru <- keras_model_sequential() %>%
    layer_gru(units = 50, input_shape = c(1, ncol(treinox)), return_sequences = TRUE) %>%
    layer_gru(units = 50) %>%
    layer_dense(units = 1, name = "pib")
  
  model_gru %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam(),
    metrics = c("mean_absolute_error")
  )
  
  history_gru <- model_gru %>% fit(
    train_data, treinoy,
    epochs = 50,
    batch_size = 32,
    validation_split = 0.2,
    verbose = 1
  )
  
  predict_gru <- model_gru %>% predict(test_data)
  
  # Define the start date and create a sequence of dates
  start_date <- as.Date("2020-01-01")
  end_date <- as.Date("2024-06-01")
  dates <- seq.Date(start_date, end_date, by = "month")
  
  # Create a dataframe with the forecasts
  forecast_df <- data.frame(
    Date = dates[1:length(predict_rnn)], # Ensure dates match the length of predictions
    PIB = as.numeric(testey),
    RNN = as.numeric(predict_rnn),
    LSTM = as.numeric(predict_lstm),
    GRU = as.numeric(predict_gru),
    AVG = rowMeans(cbind(predict_rnn, predict_lstm, predict_gru), na.rm = TRUE) # Handle NA values
  )
  
  plot(forecast_df$Date, forecast_df$PIB, col = "blue", type = "l", xlab = "Time", ylab = "Value", main = "Model Forecasts vs Actual Values", ylim = range(c(forecast_df$PIB, forecast_df$AVG)))
  lines(forecast_df$Date, forecast_df$RNN, col = "green", lty = 2)
  lines(forecast_df$Date, forecast_df$LSTM, col = "orange", lty = 2)
  lines(forecast_df$Date, forecast_df$GRU, col = "purple", lty = 2)
  lines(forecast_df$Date, forecast_df$AVG, col = "red", lwd = 2)
  legend("bottomright", legend = c("Actual", "RNN", "LSTM", "GRU", "Average"),
         col = c("blue", "green", "orange", "purple", "red"),
         lty = c(1, 2, 2, 2, 1), lwd = c(1, 1, 1, 1, 2),
         cex = 0.8, # Adjust text size
         inset = c(0, 0.04)) # Adjust position

  
  return(forecast_df)
}
forecast_models_one_step <- function(train, test) {
  library(keras)
  library(tensorflow)
  
  # Ensure reproducibility
  set.seed(42)
  
  # Preprocess data
  treinox <- train[,-c(1:2)]
  testex <- test[,-c(1:2)]
  treinoy <- train[,2]$pib # Assume PIB is the second column
  testey <- test[,2]$pib   # Assume PIB is the second column
  
  # Ensure response variable is numeric
  treinoy <- as.numeric(treinoy)
  testey <- as.numeric(testey)
  
  # Check for NA values in the response variables
  cat("Checking for NA values in response variables...\n")
  cat("NA values in treinoy:", sum(is.na(treinoy)), "\n")
  cat("NA values in testey:", sum(is.na(testey)), "\n")
  
  # Define scaler based on training data
  train_mean <- apply(treinox, 2, mean, na.rm = TRUE)
  train_sd <- apply(treinox, 2, sd, na.rm = TRUE)
  
  # Scale data using training set parameters
  scale_data <- function(data, mean, sd) {
    scaled_data <- sweep(data, 2, mean, "-")
    scaled_data <- sweep(scaled_data, 2, sd, "/")
    return(scaled_data)
  }
  
  train_data <- scale_data(treinox, train_mean, train_sd)
  test_data <- scale_data(testex, train_mean, train_sd)
  
  # Convert to numeric matrix
  train_data <- as.matrix(train_data)
  test_data <- as.matrix(test_data)
  
  # Check for NA values after scaling
  cat("Checking for NA values after scaling...\n")
  cat("NA values in train_data:", sum(is.na(train_data)), "\n")
  cat("NA values in test_data:", sum(is.na(test_data)), "\n")
  
  # Print shapes of data
  cat("Shapes of data:\n")
  cat("train_data:", dim(train_data), "\n")
  cat("test_data:", dim(test_data), "\n")
  cat("treinoy:", length(treinoy), "\n")
  cat("testey:", length(testey), "\n")
  
  # Reshape data for RNNs
  train_data <- array(train_data, dim = c(nrow(train_data), 1, ncol(train_data)))
  test_data <- array(test_data, dim = c(nrow(test_data), 1, ncol(test_data)))
  
  # Print shapes after reshaping
  cat("Shapes after reshaping:\n")
  cat("train_data:", dim(train_data), "\n")
  cat("test_data:", dim(test_data), "\n")
  
  # Define RNN model
  model_rnn <- keras_model_sequential() %>%
    layer_simple_rnn(units = 50, input_shape = c(1, ncol(treinox)), return_sequences = FALSE) %>%
    layer_dense(units = 1, name = "pib")
  
  model_rnn %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam(),
    metrics = c("mean_absolute_error")
  )
  
  history_rnn <- model_rnn %>% fit(
    train_data, treinoy,
    epochs = 50,
    batch_size = 32,
    validation_split = 0.2,
    verbose = 1
  )
  
  predict_rnn <- model_rnn %>% predict(test_data)
  
  # Define LSTM model
  model_lstm <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(1, ncol(treinox)), return_sequences = FALSE) %>%
    layer_dense(units = 1, name = "pib")
  
  model_lstm %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam(),
    metrics = c("mean_absolute_error")
  )
  
  history_lstm <- model_lstm %>% fit(
    train_data, treinoy,
    epochs = 50,
    batch_size = 32,
    validation_split = 0.2,
    verbose = 1
  )
  
  predict_lstm <- model_lstm %>% predict(test_data)
  
  # Define GRU model
  model_gru <- keras_model_sequential() %>%
    layer_gru(units = 50, input_shape = c(1, ncol(treinox)), return_sequences = FALSE) %>%
    layer_dense(units = 1, name = "pib")
  
  model_gru %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam(),
    metrics = c("mean_absolute_error")
  )
  
  history_gru <- model_gru %>% fit(
    train_data, treinoy,
    epochs = 50,
    batch_size = 32,
    validation_split = 0.2,
    verbose = 1
  )
  
  predict_gru <- model_gru %>% predict(test_data)
  
  # Print the first few predictions to check for NAs
  cat("First few predictions:\n")
  cat("RNN predictions:", head(predict_rnn), "\n")
  cat("LSTM predictions:", head(predict_lstm), "\n")
  cat("GRU predictions:", head(predict_gru), "\n")
  
  # Define the start date and create a sequence of dates
  start_date <- as.Date("2020-01-01")
  end_date <- as.Date("2024-06-01")
  dates <- seq.Date(start_date, end_date, by = "month")
  
  # Create a dataframe with the forecasts
  forecast_df <- data.frame(
    Date = dates[1:length(predict_rnn)], # Ensure dates match the length of predictions
    PIB = as.numeric(testey),
    RNN = as.numeric(predict_rnn),
    LSTM = as.numeric(predict_lstm),
    GRU = as.numeric(predict_gru),
    AVG = rowMeans(cbind(predict_rnn, predict_lstm, predict_gru), na.rm = TRUE) # Handle NA values
  )
  
  # Check the forecast dataframe for NAs
  cat("Checking for NA values in the forecast dataframe...\n")
  print(head(forecast_df))
  
  return(forecast_df)
}

# v51
v51 = df_list$v1_2024_Q1_M3
balanced.v51 = balance_panel(v51, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v51, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v51, mode = "ts")
forecast_df_v51.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v51.ts <- compute_all_errors(forecast_df_v51.ts)
forecast_df_v51.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v51.1s <- compute_all_errors(forecast_df_v51.ts)

# v50
v50 = df_list$v1_2024_Q1_M2
balanced.v50 = balance_panel(v50, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v50, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v50, mode = "ts")
forecast_df_v50.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v50.ts <- compute_all_errors(forecast_df_v50.ts)
forecast_df_v50.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v50.1s <- compute_all_errors(forecast_df_v50.ts)

# v49
v49 = df_list$v1_2024_Q1_M1
balanced.v49 = balance_panel(v49, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v49, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v49, mode = "ts")
forecast_df_v49.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v49.ts <- compute_all_errors(forecast_df_v49.ts)
forecast_df_v49.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v49.1s <- compute_all_errors(forecast_df_v49.ts)

# v48
v48 = df_list$v1_2023_Q4_M3
balanced.v48 = balance_panel(v48, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v48, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v48, mode = "ts")
forecast_df_v48.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v48.ts <- compute_all_errors(forecast_df_v48.ts)
forecast_df_v48.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v48.1s <- compute_all_errors(forecast_df_v48.ts)

# v47
v47 = df_list$v1_2023_Q4_M2
balanced.v47 = balance_panel(v47, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v47, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v47, mode = "ts")
forecast_df_v47.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v47.ts <- compute_all_errors(forecast_df_v47.ts)
forecast_df_v47.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v47.1s <- compute_all_errors(forecast_df_v47.ts)

# v46
v46 = df_list$v1_2023_Q4_M1
balanced.v46 = balance_panel(v46, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v46, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v46, mode = "ts")
forecast_df_v46.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v46.ts <- compute_all_errors(forecast_df_v46.ts)
forecast_df_v46.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v46.1s <- compute_all_errors(forecast_df_v46.ts)

# v45
v45 = df_list$v1_2023_Q3_M3
balanced.v45 = balance_panel(v45, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v45, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v45, mode = "ts")
forecast_df_v45.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v45.ts <- compute_all_errors(forecast_df_v45.ts)
forecast_df_v45.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v45.1s <- compute_all_errors(forecast_df_v45.ts)

# v44
v44 = df_list$v1_2023_Q3_M2
balanced.v44 = balance_panel(v44, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v44, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v44, mode = "ts")
forecast_df_v44.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v44.ts <- compute_all_errors(forecast_df_v44.ts)
forecast_df_v44.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v44.1s <- compute_all_errors(forecast_df_v44.ts)

# v43
v43 = df_list$v1_2023_Q3_M1
balanced.v43 = balance_panel(v43, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v43, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v43, mode = "ts")
forecast_df_v43.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v43.ts <- compute_all_errors(forecast_df_v43.ts)
forecast_df_v43.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v43.1s <- compute_all_errors(forecast_df_v43.ts)

# v42
v42 = df_list$v1_2023_Q2_M3
balanced.v42 = balance_panel(v42, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v42, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v42, mode = "ts")
forecast_df_v42.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v42.ts <- compute_all_errors(forecast_df_v42.ts)
forecast_df_v42.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v42.1s <- compute_all_errors(forecast_df_v42.ts)

# v41
v41 = df_list$v1_2023_Q2_M2
balanced.v41 = balance_panel(v41, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v41, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v41, mode = "ts")
forecast_df_v41.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v41.ts <- compute_all_errors(forecast_df_v41.ts)
forecast_df_v41.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v41.1s <- compute_all_errors(forecast_df_v41.ts)

# v40
v40 = df_list$v1_2023_Q2_M1
balanced.v40 = balance_panel(v40, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v40, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v40, mode = "ts")
forecast_df_v40.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v40.ts <- compute_all_errors(forecast_df_v40.ts)
forecast_df_v40.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v40.1s <- compute_all_errors(forecast_df_v40.ts)

# v39
v39 = df_list$v1_2023_Q1_M3
balanced.v39 = balance_panel(v39, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v39, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v39, mode = "ts")
forecast_df_v39.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v39.ts <- compute_all_errors(forecast_df_v39.ts)
forecast_df_v39.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v39.1s <- compute_all_errors(forecast_df_v39.ts)

# v38
v38 = df_list$v1_2023_Q1_M2
balanced.v38 = balance_panel(v38, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v38, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v38, mode = "ts")
forecast_df_v38.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v38.ts <- compute_all_errors(forecast_df_v38.ts)
forecast_df_v38.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v38.1s <- compute_all_errors(forecast_df_v38.ts)

# v37
v37 = df_list$v1_2023_Q1_M1
balanced.v37 = balance_panel(v37, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v37, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v37, mode = "ts")
forecast_df_v37.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v37.ts <- compute_all_errors(forecast_df_v37.ts)
forecast_df_v37.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v37.1s <- compute_all_errors(forecast_df_v37.ts)

# v36
v36 = df_list$v1_2022_Q4_M3
balanced.v36 = balance_panel(v36, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v36, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v36, mode = "ts")
forecast_df_v36.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v36.ts <- compute_all_errors(forecast_df_v36.ts)
forecast_df_v36.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v36.1s <- compute_all_errors(forecast_df_v36.ts)

# v35
v35 = df_list$v1_2022_Q4_M2
balanced.v35 = balance_panel(v35, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v35, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v35, mode = "ts")
forecast_df_v35.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v35.ts <- compute_all_errors(forecast_df_v35.ts)
forecast_df_v35.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v35.1s <- compute_all_errors(forecast_df_v35.ts)

# v34
v34 = df_list$v1_2022_Q4_M1
balanced.v34 = balance_panel(v34, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v34, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v34, mode = "ts")
forecast_df_v34.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v34.ts <- compute_all_errors(forecast_df_v34.ts)
forecast_df_v34.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v34.1s <- compute_all_errors(forecast_df_v34.ts)

# v33
v33 = df_list$v1_2022_Q3_M3
balanced.v33 = balance_panel(v33, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v33, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v33, mode = "ts")
forecast_df_v33.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v33.ts <- compute_all_errors(forecast_df_v33.ts)
forecast_df_v33.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v33.1s <- compute_all_errors(forecast_df_v33.ts)

# v32
v32 = df_list$v1_2022_Q3_M2
balanced.v32 = balance_panel(v32, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v32, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v32, mode = "ts")
forecast_df_v32.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v32.ts <- compute_all_errors(forecast_df_v32.ts)
forecast_df_v32.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v32.1s <- compute_all_errors(forecast_df_v32.ts)

# v31
v31 = df_list$v1_2022_Q3_M1
balanced.v31 = balance_panel(v31, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v31, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v31, mode = "ts")
forecast_df_v31.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v31.ts <- compute_all_errors(forecast_df_v31.ts)
forecast_df_v31.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v31.1s <- compute_all_errors(forecast_df_v31.ts)

# v30
v30 = df_list$v1_2022_Q2_M3
balanced.v30 = balance_panel(v30, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v30, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v30, mode = "ts")
forecast_df_v30.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v30.ts <- compute_all_errors(forecast_df_v30.ts)
forecast_df_v30.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v30.1s <- compute_all_errors(forecast_df_v30.ts)

# v29
v29 = df_list$v1_2022_Q2_M2
balanced.v29 = balance_panel(v29, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v29, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v29, mode = "ts")
forecast_df_v29.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v29.ts <- compute_all_errors(forecast_df_v29.ts)
forecast_df_v29.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v29.1s <- compute_all_errors(forecast_df_v29.ts)

# v28
v28 = df_list$v1_2022_Q2_M1
balanced.v28 = balance_panel(v28, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v28, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v28, mode = "ts")
forecast_df_v28.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v28.ts <- compute_all_errors(forecast_df_v28.ts)
forecast_df_v28.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v28.1s <- compute_all_errors(forecast_df_v28.ts)

# v27
v27 = df_list$v1_2022_Q1_M3
balanced.v27 = balance_panel(v27, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v27, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v27, mode = "ts")
forecast_df_v27.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v27.ts <- compute_all_errors(forecast_df_v27.ts)
forecast_df_v27.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v27.1s <- compute_all_errors(forecast_df_v27.ts)

# v26
v26 = df_list$v1_2022_Q1_M2
balanced.v26 = balance_panel(v26, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v26, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v26, mode = "ts")
forecast_df_v26.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v26.ts <- compute_all_errors(forecast_df_v26.ts)
forecast_df_v26.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v26.1s <- compute_all_errors(forecast_df_v26.ts)

# v25
v25 = df_list$v1_2022_Q1_M1
balanced.v25 = balance_panel(v25, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v25, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v25, mode = "ts")
forecast_df_v25.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v25.ts <- compute_all_errors(forecast_df_v25.ts)
forecast_df_v25.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v25.1s <- compute_all_errors(forecast_df_v25.ts)

# v24
v24 = df_list$v1_2021_Q4_M3
balanced.v24 = balance_panel(v24, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v24, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v24, mode = "ts")
forecast_df_v24.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v24.ts <- compute_all_errors(forecast_df_v24.ts)
forecast_df_v24.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v24.1s <- compute_all_errors(forecast_df_v24.ts)

# v23
v23 = df_list$v1_2021_Q4_M2
balanced.v23 = balance_panel(v23, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v23, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v23, mode = "ts")
forecast_df_v23.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v23.ts <- compute_all_errors(forecast_df_v23.ts)
forecast_df_v23.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v23.1s <- compute_all_errors(forecast_df_v23.ts)

# v22
v22 = df_list$v1_2021_Q4_M1
balanced.v22 = balance_panel(v22, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v22, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v22, mode = "ts")
forecast_df_v22.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v22.ts <- compute_all_errors(forecast_df_v22.ts)
forecast_df_v22.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v22.1s <- compute_all_errors(forecast_df_v22.ts)

# v21
v21 = df_list$v1_2021_Q3_M3
balanced.v21 = balance_panel(v21, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v21, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v21, mode = "ts")
forecast_df_v21.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v21.ts <- compute_all_errors(forecast_df_v21.ts)
forecast_df_v21.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v21.1s <- compute_all_errors(forecast_df_v21.ts)

# v20
v20 = df_list$v1_2021_Q3_M2
balanced.v20 = balance_panel(v20, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v20, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v20, mode = "ts")
forecast_df_v20.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v20.ts <- compute_all_errors(forecast_df_v20.ts)
forecast_df_v20.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v20.1s <- compute_all_errors(forecast_df_v20.ts)

# v19
v19 = df_list$v1_2021_Q3_M1
balanced.v19 = balance_panel(v19, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v19, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v19, mode = "ts")
forecast_df_v19.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v19.ts <- compute_all_errors(forecast_df_v19.ts)
forecast_df_v19.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v19.1s <- compute_all_errors(forecast_df_v19.ts)

# v18
v18 = df_list$v1_2021_Q2_M3
balanced.v18 = balance_panel(v18, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v18, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v18, mode = "ts")
forecast_df_v18.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v18.ts <- compute_all_errors(forecast_df_v18.ts)
forecast_df_v18.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v18.1s <- compute_all_errors(forecast_df_v18.ts)

# v17
v17 = df_list$v1_2021_Q2_M2
balanced.v17 = balance_panel(v17, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v17, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v17, mode = "ts")
forecast_df_v17.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v17.ts <- compute_all_errors(forecast_df_v17.ts)
forecast_df_v17.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v17.1s <- compute_all_errors(forecast_df_v17.ts)

# v16
v16 = df_list$v1_2021_Q2_M1
balanced.v16 = balance_panel(v16, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v16, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v16, mode = "ts")
forecast_df_v16.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v16.ts <- compute_all_errors(forecast_df_v16.ts)
forecast_df_v16.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v16.1s <- compute_all_errors(forecast_df_v16.ts)

# v15
v15 = df_list$v1_2021_Q1_M3
balanced.v15 = balance_panel(v15, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v15, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v15, mode = "ts")
forecast_df_v15.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v15.ts <- compute_all_errors(forecast_df_v15.ts)
forecast_df_v15.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v15.1s <- compute_all_errors(forecast_df_v15.ts)

# v14
v14 = df_list$v1_2021_Q1_M2
balanced.v14 = balance_panel(v14, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v14, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v14, mode = "ts")
forecast_df_v14.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v14.ts <- compute_all_errors(forecast_df_v14.ts)
forecast_df_v14.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v14.1s <- compute_all_errors(forecast_df_v14.ts)

# v13
v13 = df_list$v1_2021_Q1_M1
balanced.v13 = balance_panel(v13, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v13, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v13, mode = "ts")
forecast_df_v13.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v13.ts <- compute_all_errors(forecast_df_v13.ts)
forecast_df_v13.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v13.1s <- compute_all_errors(forecast_df_v13.ts)

# v12
v12 = df_list$v1_2020_Q4_M3
balanced.v12 = balance_panel(v12, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v12, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v12, mode = "ts")
forecast_df_v12.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v12.ts <- compute_all_errors(forecast_df_v12.ts)
forecast_df_v12.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v12.1s <- compute_all_errors(forecast_df_v12.ts)

# v11
v11 = df_list$v1_2020_Q4_M2
balanced.v11 = balance_panel(v11, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v11, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v11, mode = "ts")
forecast_df_v11.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v11.ts <- compute_all_errors(forecast_df_v11.ts)
forecast_df_v11.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v11.1s <- compute_all_errors(forecast_df_v11.ts)

# v10
v10 = df_list$v1_2020_Q4_M1
balanced.v10 = balance_panel(v10, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v10, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v10, mode = "ts")
forecast_df_v10.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v10.ts <- compute_all_errors(forecast_df_v10.ts)
forecast_df_v10.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v10.1s <- compute_all_errors(forecast_df_v10.ts)

# v9
v9 = df_list$v1_2020_Q3_M3
balanced.v9 = balance_panel(v9, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v9, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v9, mode = "ts")
forecast_df_v9.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v9.ts <- compute_all_errors(forecast_df_v9.ts)
forecast_df_v9.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v9.1s <- compute_all_errors(forecast_df_v9.ts)

# v8
v8 = df_list$v1_2020_Q3_M2
balanced.v8 = balance_panel(v8, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v8, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v8, mode = "ts")
forecast_df_v8.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v8.ts <- compute_all_errors(forecast_df_v8.ts)
forecast_df_v8.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v8.1s <- compute_all_errors(forecast_df_v8.ts)

# v7
v7 = df_list$v1_2020_Q3_M1
balanced.v7 = balance_panel(v7, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v7, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v7, mode = "ts")
forecast_df_v7.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v7.ts <- compute_all_errors(forecast_df_v7.ts)
forecast_df_v7.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v7.1s <- compute_all_errors(forecast_df_v7.ts)

# v6
v6 = df_list$v1_2020_Q2_M3
balanced.v6 = balance_panel(v6, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v6, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v6, mode = "ts")
forecast_df_v6.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v6.ts <- compute_all_errors(forecast_df_v6.ts)
forecast_df_v6.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v6.1s <- compute_all_errors(forecast_df_v6.ts)

# v5
v5 = df_list$v1_2020_Q2_M2
balanced.v5 = balance_panel(v5, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v5, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v5, mode = "ts")
forecast_df_v5.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v5.ts <- compute_all_errors(forecast_df_v5.ts)
forecast_df_v5.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v5.1s <- compute_all_errors(forecast_df_v5.ts)

# v4
v4 = df_list$v1_2020_Q2_M1
balanced.v4 = balance_panel(v4, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v4, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v4, mode = "ts")
forecast_df_v4.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v4.ts <- compute_all_errors(forecast_df_v4.ts)
forecast_df_v4.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v4.1s <- compute_all_errors(forecast_df_v4.ts)

# v3
v3 = df_list$v1_2020_Q1_M3
balanced.v3 = balance_panel(v3, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v3, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v3, mode = "ts")
forecast_df_v3.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v3.ts <- compute_all_errors(forecast_df_v3.ts)
forecast_df_v3.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v3.1s <- compute_all_errors(forecast_df_v3.ts)

# v2
v2 = df_list$v1_2020_Q1_M2
balanced.v2 = balance_panel(v2, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v2, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v2, mode = "ts")
forecast_df_v2.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v2.ts <- compute_all_errors(forecast_df_v2.ts)
forecast_df_v2.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v2.1s <- compute_all_errors(forecast_df_v2.ts)

# v1
v1 = df_list$v1_2020_Q1_M1
balanced.v1 = balance_panel(v1, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v1, mode = "1s")
split_prtdb.ts = split.prtdb(balanced.v1, mode = "ts")
forecast_df_v1.ts <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
errors.deep.v1.ts <- compute_all_errors(forecast_df_v1.ts)
forecast_df_v1.1s <- forecast_models_one_step(split_prtdb.1s$df_in, split_prtdb.1s$df_out)
errors.deep.v1.1s <- compute_all_errors(forecast_df_v1.ts)


# modelos clássicos
# midas
# Functions used in the exercise
# MIDAS 1S
# customized database midas
# functions used and preps
# Load necessary library
library(Metrics)
get_last_month_of_current_quarter <- function(year, month) {
  # Determine the quarter based on the month
  current_quarter <- ceiling(month / 3)
  
  # Determine the last month of the current quarter
  last_month <- current_quarter * 3
  
  # Return the year and last month of the current quarter
  return(list(year = year, last_month = last_month))
}
get_last_quarter <- function(year, month) {
  # Determine the quarter based on the month
  current_quarter <- ceiling(month / 3)
  
  # Calculate the last quarter
  last_quarter <- if (current_quarter == 1) {
    # If the current quarter is Q1, the last quarter is Q4 of the previous year
    year <- year - 1
    4
  } else {
    current_quarter - 1
  }
  
  # Determine the last month of the last quarter
  last_month <- last_quarter * 3
  
  # Return the year and last month of the last quarter
  return(list(year = year, last_month = last_month))
}
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
  # IBC_BR = rbcb::get_series(c(IBC_BR = 24363), start_date = "01-01-2003")
  IBC_BR = GetBCBData::gbcbd_get_series(id = 24363,first.date = "01-01-2003")
  ibc_br = ts(IBC_BR$value, start = c(2003,01), frequency = 12)
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
results_list$`2020_2`

# MIDAS TS
vmidas.ts <- function(df, y, m) {
  library(zoo)
  library(lubridate)
  library(sidrar)
  
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
  pib.in <- window(pib.ts, start = c(2004, 02), end = c(2019, 4))
  
  # PIM
  pim <- ts(bal.v.midas$pim, start = c(2004, 02), frequency = 12)
  pim.full <- window(pim, start = c(2004, 04), end = c(result1$year, result1$last_month))
  pim.in <- window(pim, start = c(2004, 04), end = c(2019, 12))
  
  # PIMD
  pimd <- ts(bal.v.midas$pimd, start = c(2004, 02), frequency = 12)
  pimd.full <- window(pimd, start = c(2004, 04), end = c(result1$year, result1$last_month))
  pimd.in <- window(pimd, start = c(2004, 04), end = c(2019, 12))
  
  # PMCA
  pmca <- ts(bal.v.midas$pmca, start = c(2004, 02), frequency = 12)
  pmca.full <- window(pmca, start = c(2004, 04), end = c(result1$year, result1$last_month))
  pmca.in <- window(pmca, start = c(2004, 04), end = c(2019, 12))
  
  # PMCAD
  pmcad <- ts(bal.v.midas$pmcad, start = c(2004, 02), frequency = 12)
  pmcad.full <- window(pmcad, start = c(2004, 04), end = c(result1$year, result1$last_month))
  pmcad.in <- window(pmcad, start = c(2004, 04), end = c(2019, 12))
  
  # IBC
  ibc <- ts(bal.v.midas$ibc, start = c(2004, 02), frequency = 12)
  ibc.full <- window(ibc, start = c(2004, 04), end = c(result1$year, result1$last_month))
  ibc.in <- window(ibc, start = c(2004, 04), end = c(2019, 12))
  
  # EXPEC
  expec <- ts(bal.v.midas$expec, start = c(2004, 02), frequency = 12)
  expec.full <- window(expec, start = c(2004, 04), end = c(result1$year, result1$last_month))
  expec.in <- window(expec, start = c(2004, 04), end = c(2019, 12))
  
  
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
  
  
  midas.ts <- data.frame(date   = as.Date(time(pib.full))[64:length(pib.full)],
                         midas1 = avgf1$forecast,
                         midas2 = avgf2$forecast,
                         midas3 = avgf3$forecast,
                         midas4 = avgf4$forecast,
                         midas5 = avgf5$forecast,
                         midas6 = avgf6$forecast,
                         pib = pib.full[64:length(pib.full)])
  
  error_results <- calculate_errors.ts(midas.ts)
  
  midas_full = list(midasts = midas.ts,
                    errors  = error_results)
  
  return(midas_full)
  
}
results_list_ts <- list()
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
      vmidas.ts(df_all, year, month)
    }, error = function(e) {
      cat("Error processing year:", year, "month:", month, "\n")
      return(NULL)
    })
    
    results_list_ts[[paste(year, month, sep = "_")]] <- result
  }
}
results_list_ts$`2024_3`
plot(results_list_ts$`2024_3`$midasts$pib, type = "l")
lines(results_list_ts$`2024_3`$midasts$midas1, col = "red")

# apenas dfms agora graças a deus
# must start at least 10 periods ahead because of invertible matrix
# dfm.1s

compute_error_1s.dfm <- function(df) {
  mae <- mae(df$pib, df$dfm1)
  mape <- mape(df$pib, df$dfm1)
  rmse <- rmse(df$pib, df$dfm1)
  
  return(list(MAE = mae, MAPE = mape, RMSE = rmse))
}
compute_error_ms <- function(df) {
  mae <- mae(df$pib, df$dfm1)
  mape <- mape(df$pib, df$dfm1)
  rmse <- rmse(df$pib, df$dfm1)
  
  return(list(MAE = mae, MAPE = mape, RMSE = rmse))
}

# v1
v1 = df_list$v1_2020_Q1_M1
balanced.v1 = balance_panel(v1, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v1, mode = "1s")
dfm.v1.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v1.1s)

# v2
v2 = df_list$v1_2020_Q1_M2
balanced.v2 = balance_panel(v2, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v2, mode = "1s")
dfm.v2.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v2.1s)

# v3
v3 = df_list$v1_2020_Q1_M3
balanced.v3 = balance_panel(v3, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v3, mode = "1s")
dfm.v3.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v3.1s)

# v4
v4 = df_list$v1_2020_Q2_M1
balanced.v4 = balance_panel(v4, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v4, mode = "1s")
dfm.v4.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v4.1s)

# v5
v5 = df_list$v1_2020_Q2_M2
balanced.v5 = balance_panel(v5, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v5, mode = "1s")
dfm.v5.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v5.1s)

# v6
v6 = df_list$v1_2020_Q2_M3
balanced.v6 = balance_panel(v6, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v6, mode = "1s")
dfm.v6.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v6.1s)

# v7
v7 = df_list$v1_2020_Q3_M1
balanced.v7 = balance_panel(v7, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v7, mode = "1s")
dfm.v7.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v7.1s)

# v8
v8 = df_list$v1_2020_Q3_M2
balanced.v8 = balance_panel(v8, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v8, mode = "1s")
dfm.v8.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v8.1s)

# v9
v9 = df_list$v1_2020_Q3_M3
balanced.v9 = balance_panel(v9, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v9, mode = "1s")
dfm.v9.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v9.1s)

# v10
v10 = df_list$v1_2020_Q4_M1
balanced.v10 = balance_panel(v10, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v10, mode = "1s")
dfm.v10.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v10.1s)

# v11
v11 = df_list$v1_2020_Q4_M2
balanced.v11 = balance_panel(v11, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v11, mode = "1s")
dfm.v11.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v11.1s)

v12
v12 = df_list$v1_2020_Q4_M3
balanced.v12 = balance_panel(v12, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v12, mode = "1s")
dfm.v12.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v12.1s)

# v13
v13 = df_list$v1_2021_Q1_M1
balanced.v13 = balance_panel(v13, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v13, mode = "1s")
dfm.v13.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v13.1s)

# v14
v14 = df_list$v1_2021_Q1_M2
balanced.v14 = balance_panel(v14, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v14, mode = "1s")
dfm.v14.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v14.1s)

# v15
v15 = df_list$v1_2021_Q1_M3
balanced.v15 = balance_panel(v15, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v15, mode = "1s")
dfm.v15.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v15.1s)

# v16
v16 = df_list$v1_2021_Q2_M1
balanced.v16 = balance_panel(v16, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v16, mode = "1s")
dfm.v16.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v16.1s)

# v17
v17 = df_list$v1_2021_Q2_M2
balanced.v17 = balance_panel(v17, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v17, mode = "1s")
dfm.v17.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v17.1s)

# v18
v18 = df_list$v1_2021_Q2_M3
balanced.v18 = balance_panel(v18, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v18, mode = "1s")
dfm.v18.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v18.1s)

# v19
v19 = df_list$v1_2021_Q3_M1
balanced.v19 = balance_panel(v19, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v19, mode = "1s")
dfm.v19.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v19.1s)

# v20
v20 = df_list$v1_2021_Q3_M2
balanced.v20 = balance_panel(v20, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v20, mode = "1s")
dfm.v20.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v20.1s)

# v21
v21 = df_list$v1_2021_Q3_M3
balanced.v21 = balance_panel(v21, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v21, mode = "1s")
dfm.v21.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v21.1s)

# v22
v22 = df_list$v1_2021_Q4_M1
balanced.v22 = balance_panel(v22, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v22, mode = "1s")
dfm.v22.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v22.1s)

# v23
v23 = df_list$v1_2021_Q4_M2
balanced.v23 = balance_panel(v23, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v23, mode = "1s")
dfm.v23.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v23.1s)

# v24
v24 = df_list$v1_2021_Q4_M3
balanced.v24 = balance_panel(v24, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v24, mode = "1s")
dfm.v24.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v23.1s)

# v25
v25 = df_list$v1_2022_Q1_M1
balanced.v25 = balance_panel(v25, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v25, mode = "1s")
dfm.v25.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v25.1s)

# v26
v26 = df_list$v1_2022_Q1_M2
balanced.v26 = balance_panel(v26, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v26, mode = "1s")
dfm.v26.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v26.1s)

# v27
v27 = df_list$v1_2022_Q1_M3
balanced.v27 = balance_panel(v27, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v27, mode = "1s")
dfm.v27.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v27.1s)

# v28
v28 = df_list$v1_2022_Q2_M1
balanced.v28 = balance_panel(v28, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v28, mode = "1s")
dfm.v28.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v28.1s)

# v29
v29 = df_list$v1_2022_Q2_M2
balanced.v29 = balance_panel(v29, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v29, mode = "1s")
dfm.v29.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v29.1s)

# v30
v30 = df_list$v1_2022_Q2_M3
balanced.v30 = balance_panel(v30, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v30, mode = "1s")
dfm.v30.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v30.1s)

# v31
v31 = df_list$v1_2022_Q3_M1
balanced.v31 = balance_panel(v31, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v31, mode = "1s")
dfm.v31.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v31.1s)

# v32
v32 = df_list$v1_2022_Q3_M2
balanced.v32 = balance_panel(v32, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v32, mode = "1s")
dfm.v32.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v32.1s)

# v33
v33 = df_list$v1_2022_Q3_M3
balanced.v33 = balance_panel(v33, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v33, mode = "1s")
dfm.v33.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v33.1s)

# v34
v34 = df_list$v1_2022_Q4_M1
balanced.v34 = balance_panel(v34, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v34, mode = "1s")
dfm.v34.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v34.1s)

# v35
v35 = df_list$v1_2022_Q4_M2
balanced.v35 = balance_panel(v35, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v35, mode = "1s")
dfm.v35.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v35.1s)

# v36
v36 = df_list$v1_2022_Q4_M3
balanced.v36 = balance_panel(v36, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v36, mode = "1s")
dfm.v36.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v36.1s)

# v37
v37 = df_list$v1_2023_Q1_M1
balanced.v37 = balance_panel(v37, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v37, mode = "1s")
dfm.v37.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v37.1s)

# v38
v38 = df_list$v1_2023_Q1_M2
balanced.v38 = balance_panel(v38, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v38, mode = "1s")
dfm.v38.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v38.1s)

# v39
v39 = df_list$v1_2023_Q1_M3
balanced.v39 = balance_panel(v39, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v39, mode = "1s")
dfm.v39.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v39.1s)

# v40
v40 = df_list$v1_2023_Q2_M1
balanced.v40 = balance_panel(v40, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v40, mode = "1s")
dfm.v40.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v40.1s)

# v41
v41 = df_list$v1_2023_Q2_M2
balanced.v41 = balance_panel(v41, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v41, mode = "1s")
dfm.v41.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v41.1s)

# v42
v42 = df_list$v1_2023_Q2_M3
balanced.v42 = balance_panel(v42, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v42, mode = "1s")
dfm.v42.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v42.1s)

# v43
v43 = df_list$v1_2023_Q3_M1
balanced.v43 = balance_panel(v43, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v43, mode = "1s")
dfm.v43.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v43.1s)

# v44
v44 = df_list$v1_2023_Q3_M2
balanced.v44 = balance_panel(v44, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v44, mode = "1s")
dfm.v44.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v44.1s)

# v45
v45 = df_list$v1_2023_Q3_M3
balanced.v45 = balance_panel(v45, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v45, mode = "1s")
dfm.v45.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v45.1s)

# v46
v46 = df_list$v1_2023_Q4_M1
balanced.v46 = balance_panel(v46, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v46, mode = "1s")
dfm.v46.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v46.1s)

# v47
v47 = df_list$v1_2023_Q4_M2
balanced.v47 = balance_panel(v47, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v47, mode = "1s")
dfm.v47.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v47.1s)

# v48
v48 = df_list$v1_2023_Q4_M3
balanced.v48 = balance_panel(v48, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v48, mode = "1s")
dfm.v48.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v48.1s)

# v49
v49 = df_list$v1_2024_Q1_M1
balanced.v49 = balance_panel(v49, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v49, mode = "1s")
dfm.v49.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v49.1s)

# v50
v50 = df_list$v1_2024_Q1_M2
balanced.v50 = balance_panel(v50, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v50, mode = "1s")
dfm.v50.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
compute_error_1s.dfm(dfm.v50.1s)

# v51
v51 = df_list$v1_2024_Q1_M3
balanced.v51 = balance_panel(v51, method = "NN")
split_prtdb.1s = split.prtdb(balanced.v51, mode = "1s")
dfm.v51.1s <- process_DFM(type = "1s", split_prtdb.1s = split_prtdb.1s)
errors_1s <- compute_error_1s.dfm(dfm.v51.1s)
compute_error_1s.dfm(dfm.v51.1s)

# ts
# v21
split_prtdb.ts = split.prtdb(balanced.v21, mode = "ts")
dfm.v21.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v21 = compute_error_ms(dfm.v21.ts)

# v22
split_prtdb.ts = split.prtdb(balanced.v22, mode = "ts")
dfm.v22.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v22 = compute_error_ms(dfm.v22.ts)

# v23
split_prtdb.ts = split.prtdb(balanced.v23, mode = "ts")
dfm.v23.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v23 = compute_error_ms(dfm.v23.ts)

# v24
split_prtdb.ts = split.prtdb(balanced.v24, mode = "ts")
dfm.v24.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v24 =compute_error_ms(dfm.v24.ts)

# v25
split_prtdb.ts = split.prtdb(balanced.v25, mode = "ts")
dfm.v25.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v25 = compute_error_ms(dfm.v25.ts)

# v26
split_prtdb.ts = split.prtdb(balanced.v26, mode = "ts")
dfm.v26.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v26 = compute_error_ms(dfm.v26.ts)

# v27
split_prtdb.ts = split.prtdb(balanced.v27, mode = "ts")
dfm.v27.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v27 = compute_error_ms(dfm.v27.ts)

# v28
split_prtdb.ts = split.prtdb(balanced.v28, mode = "ts")
dfm.v28.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v28 = compute_error_ms(dfm.v28.ts)

# v29
split_prtdb.ts = split.prtdb(balanced.v29, mode = "ts")
dfm.v29.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v29 = compute_error_ms(dfm.v29.ts)

# v30
split_prtdb.ts = split.prtdb(balanced.v30, mode = "ts")
dfm.v30.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v30 = compute_error_ms(dfm.v30.ts)

# v31
split_prtdb.ts = split.prtdb(balanced.v31, mode = "ts")
dfm.v31.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v31 = compute_error_ms(dfm.v31.ts)

# v32
split_prtdb.ts = split.prtdb(balanced.v32, mode = "ts")
dfm.v32.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v32 = compute_error_ms(dfm.v32.ts)

# v33
split_prtdb.ts = split.prtdb(balanced.v33, mode = "ts")
dfm.v33.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v33 = compute_error_ms(dfm.v33.ts)

# v34
split_prtdb.ts = split.prtdb(balanced.v34, mode = "ts")
dfm.v34.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v34 = compute_error_ms(dfm.v34.ts)

# v35
split_prtdb.ts = split.prtdb(balanced.v35, mode = "ts")
dfm.v35.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v35 = compute_error_ms(dfm.v35.ts)

# v36
split_prtdb.ts = split.prtdb(balanced.v36, mode = "ts")
dfm.v36.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v36 = compute_error_ms(dfm.v36.ts)

# v37
split_prtdb.ts = split.prtdb(balanced.v37, mode = "ts")
dfm.v37.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v37 = compute_error_ms(dfm.v37.ts)

# v38
split_prtdb.ts = split.prtdb(balanced.v38, mode = "ts")
dfm.v38.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v38 = compute_error_ms(dfm.v38.ts)

# v39
split_prtdb.ts = split.prtdb(balanced.v39, mode = "ts")
dfm.v39.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v39 = error.ts.dfm.v39 = compute_error_ms(dfm.v39.ts)

# v40
split_prtdb.ts = split.prtdb(balanced.v40, mode = "ts")
dfm.v40.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v40 = error.ts.dfm.v40 = compute_error_ms(dfm.v40.ts)

# v41
split_prtdb.ts = split.prtdb(balanced.v41, mode = "ts")
dfm.v41.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v41 = compute_error_ms(dfm.v41.ts)

# v42
split_prtdb.ts = split.prtdb(balanced.v42, mode = "ts")
dfm.v42.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v42 = compute_error_ms(dfm.v42.ts)

# v43
split_prtdb.ts = split.prtdb(balanced.v43, mode = "ts")
dfm.v43.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v43 = compute_error_ms(dfm.v43.ts)

# v44
split_prtdb.ts = split.prtdb(balanced.v44, mode = "ts")
dfm.v44.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v44 = compute_error_ms(dfm.v44.ts)

# v45
split_prtdb.ts = split.prtdb(balanced.v45, mode = "ts")
dfm.v45.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v45 = compute_error_ms(dfm.v45.ts)

# v46
split_prtdb.ts = split.prtdb(balanced.v46, mode = "ts")
dfm.v46.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v46 = compute_error_ms(dfm.v46.ts)

# v47
split_prtdb.ts = split.prtdb(balanced.v47, mode = "ts")
dfm.v47.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v47 = compute_error_ms(dfm.v47.ts)

# v48
split_prtdb.ts = split.prtdb(balanced.v48, mode = "ts")
dfm.v48.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v48 = compute_error_ms(dfm.v48.ts)

# v49
split_prtdb.ts = split.prtdb(balanced.v49, mode = "ts")
dfm.v49.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v49 = compute_error_ms(dfm.v49.ts)

# v50
split_prtdb.ts = split.prtdb(balanced.v50, mode = "ts")
dfm.v50.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v50 = compute_error_ms(dfm.v50.ts)

# v51
split_prtdb.ts = split.prtdb(balanced.v51, mode = "ts")
dfm.v51.ts <- process_DFM(type = "ts", split_prtdb.ts = split_prtdb.ts)
error.ts.dfm.v51 = compute_error_ms(dfm.v51.ts)




