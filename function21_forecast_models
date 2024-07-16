###########################################################################
###                NOWCASTING: WHICH ONE PREDICTS BETTER                ###
###                   LARGE FUNCTION 11:forecast_models                 ###
###########################################################################
library(keras)
library(tensorflow)
# Funçao
forecast_models <- function(train, test) {
  
  # Preprocess data
  treinox <- train[,-c(1:2)]
  testex <- test[,-c(1:2)]
  treinoy <- train[,c(2)]
  testey <- test[,c(2)]
  
  # Scale data
  train_data <- scale(treinox)
  test_data <- scale(testex)
  
  # Reshape data for RNNs
  train_data <- array(train_data, dim = c(nrow(train_data), 1, ncol(train_data)))
  test_data <- array(test_data, dim = c(nrow(test_data), 1, ncol(test_data)))
  
  # RNN model
  model_rnn <- keras_model_sequential() %>%
    layer_simple_rnn(units = 50, input_shape = c(1, ncol(treinox)), return_sequences = TRUE) %>%
    layer_simple_rnn(units = 50) %>%
    layer_dense(units = 1)
  
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
  
  # LSTM model
  model_lstm <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(1, ncol(treinox)), return_sequences = TRUE) %>%
    layer_lstm(units = 50) %>%
    layer_dense(units = 1)
  
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
  
  # GRU model
  model_gru <- keras_model_sequential() %>%
    layer_gru(units = 50, input_shape = c(1, ncol(treinox)), return_sequences = TRUE) %>%
    layer_gru(units = 50) %>%
    layer_dense(units = 1)
  
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
    Date = dates,
    RNN = as.numeric(predict_rnn),
    LSTM = as.numeric(predict_lstm),
    GRU = as.numeric(predict_gru),
    AVG = rowMeans(cbind(predict_rnn, predict_lstm, predict_gru))
  )
  
  # Plot the results
  plot(dates, testey, col = "blue", type = "l", xlab = "Time", ylab = "Value", main = "Model Forecasts vs Actual Values", ylim = range(c(testey, forecast_df$AVG)))
  lines(dates, forecast_df$RNN, col = "green", lty = 2)
  lines(dates, forecast_df$LSTM, col = "orange", lty = 2)
  lines(dates, forecast_df$GRU, col = "purple", lty = 2)
  lines(dates, forecast_df$AVG, col = "red", lwd = 2)
  legend("topright", legend = c("Actual", "RNN", "LSTM", "GRU", "Average"), 
         col = c("blue", "green", "orange", "purple", "red"), 
         lty = c(1, 2, 2, 2, 1), lwd = c(1, 1, 1, 1, 2))
  
  return(forecast_df)
}
# Usage
forecast_df <- forecast_models(split_prtdb.ts$df_in, split_prtdb.ts$df_out)
print(forecast_df)
