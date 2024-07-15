###########################################################################
###                NOWCASTING: WHICH ONE PREDICTS BETTER                ###
###                  LARGE FUNCTION 7:machine_learning                  ###
###########################################################################
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
    treinoy <- split_prtdb.1s$df_in[,c(2)]
    testey <- split_prtdb.1s$df_out[,c(2)]
    
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
    year = 2024
    quarter = 2
    date = paste0(get_last_month_of_quarter(year, quarter)$year,"-",
                  get_last_month_of_quarter(year, quarter)$last_month,"-01")
    
    # dataframe storing
    
    nowcast_df.1s <- data.frame(
      Date = date,
      LASSO = lasso,
      adaLASSO = adaLASSO,
      Ridge = ridge,
      Elastic_Net = elastic_net,
      NN1 = nn1,
      NN2 = nn2,
      NN3 = nn3,
      SVR = svr,
      Random_Forest = rf,
      Gradient_Boosting = gbm
    )
    
    return(nowcast_df.1s)
  }
  if (mode == "ts") {
    
    # treino e teste
    treinox <- split_prtdb.ts$df_in[,-c(1:2)]
    testex <- split_prtdb.ts$df_out[,-c(1:2)]
    treinoy <- split_prtdb.ts$df_in[,c(2)]
    testey <- split_prtdb.ts$df_out[,c(2)]
    
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
      start_date <- as.Date("2020-01-01")
      num_dates <- length(testey)
      dates <- seq.Date(start_date, by = "month", length.out = num_dates)
      
      
      
      
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
        Gradient_Boosting = gbm.predict.expec
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
# Usage
# 1s
ml.1s = machine_learning(split_prtdb.1s, mode = "1s")
print(ml.1s)
# ts
ml.ts = machine_learning(split_prtdb.ts, mode = "ts")
tail(ml.ts)
