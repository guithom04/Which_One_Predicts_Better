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
