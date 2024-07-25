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





