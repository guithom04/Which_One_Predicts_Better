###########################################################################
###                NOWCASTING: WHICH ONE PREDICTS BETTER                ###
###                     LARGE FUNCTION 10:midas_ts                      ###
###########################################################################
midas_ts <- function(year, quarter) {
  qtr2month <- function(x, reference_month = 3, interpolation = FALSE) {
    if (!reference_month %in% c(1, 2, 3)) {
      stop("The reference_month should be 1,2 or 3")
    }
    
    if (!is.ts(x)) {
      stop("x should be a ts object")
    }
    
    if (!is.null(dim(x))) {
      stop("x should be a single ts object")
    }
    
    data_q <- zoo::as.Date(x)
    data_m <- seq(data_q[1], data_q[length(data_q)], by = 'months')
    out_x <- ts(rep(NA, length(data_m)),
                start =  as.numeric(c(substr(data_q[1], 1, 4), substr(data_q[1], 6, 7))),
                frequency = 12)
    out_x[data_m %in% data_q] <- x
    
    if (reference_month %in% c(2, 3)) {
      out_x <- stats::lag(out_x, -(reference_month - 1))
      data_q <- zoo::as.Date(out_x)
      data_m <- seq(data_q[1], data_q[length(data_q)], by = 'months')
    }
    
    if (interpolation) {
      xout <- zoo::as.yearmon(data_m)
      out_x <- stats::approx(out_x, xout = xout, method = "linear")
      out_x <- ts(out_x$y, start = out_x$x[1], end = out_x$x[length(out_x$x)], frequency = 12)
    }
    
    return(out_x)
  }
  
  yoy <- function(series) {
    index <- 1:length(series)
    YoY <- rep(NA, length(series) - 12)
    for (i in index) {
      if (i > 12) {
        YoY[i - 12] <- series[i] / series[i - 12] - 1
      }
    }
    return(ts(na.omit(YoY), start = c(start(series)[1] + 1, start(series)[2]), frequency = 12))
  }
  
  library(rbcb)
  library(meedr)
  library(tidyipea)
  library(forecast)
  library(sidrar)
  library(midasr)
  library(lubridate)
  
  calculate_date_from_quarter <- function(year, quarter) {
    month <- (quarter) * 3 
    date <- as.Date(paste0(year, "-", month, "-01"))
    return(date)
  }
  
  date <- calculate_date_from_quarter(year, quarter)
  month <- month(date)
  year <- year(date)
  
  # pib
  table.pib <- get_sidra(api = "/t/5932/n1/all/v/6561/p/all/c11255/90707/d/v6561%201")
  pib.ts <- ts(table.pib$Valor, start = c(1996, 01), frequency = 4)
  pib.fit <- tsfgrnn::grnn_forecasting(na.omit(pib.ts), h = 1)
  FIT.PIB <- pib.fit$prediction
  pib.trim <- ts(c(table.pib$Valor, FIT.PIB), start = c(1996, 01), frequency = 4)
  pib.full <- window(pib.trim, start = c(2004, 01), end = c(year, quarter))
  pib.in <- window(pib.trim, start = c(2004, 01), end = c(2019, 04))
  
  # pim
  table.pim <- get_sidra(api = "/t/8888/n1/all/v/11602/p/all/c544/129314/d/v11602%201")
  pim.ts <- ts(table.pim$Valor, start = c(2002, 01), frequency = 12)
  pim.fit <- grnn_forecasting(na.omit(pim.ts), h = 2)
  FIT.PIM <- pim.fit$prediction
  pim <- ts(c(table.pim$Valor, FIT.PIM[1], FIT.PIM[2]), start = c(2002, 01), frequency = 12)
  pim.full <- window(pim, start = c(2004, 01), end = c(year, month))
  pim.in <- window(pim, start = c(2004, 01), end = c(2019, 12))
  
  # pim diff
  pim.d <- ts(diff(c(table.pim$Valor, FIT.PIM[1], FIT.PIM[2])), start = c(2002, 02), frequency = 12)
  pim.d.full <- window(pim.d, start = c(2004, 01), end = c(year, month))
  pim.d.in <- window(pim.d, start = c(2004, 01), end = c(2019, 12))
  
  # pmca
  table.pmca <- get_sidra(api = "/t/8881/n1/all/v/11709/p/all/c11046/56736/d/v11709%201")
  pmca.ts <- ts(table.pmca$Valor, start = c(2003, 01), frequency = 12)
  pmca.fit <- grnn_forecasting(na.omit(pmca.ts), h = 2)
  FIT.PMCA <- pmca.fit$prediction
  pmca <- ts(c(table.pmca$Valor, FIT.PMCA[1], FIT.PMCA[2]), start = c(2003, 01), frequency = 12)
  pmca.full <- window(pmca, start = c(2004, 01), end = c(year, month))
  pmca.in <- window(pmca, start = c(2004, 01), end = c(2019, 12))
  
  # pmca diff
  pmca.d <- ts(diff(c(table.pmca$Valor, FIT.PMCA[1], FIT.PMCA[2])), start = c(2003, 02), frequency = 12)
  pmca.d.full <- window(pmca.d, start = c(2004, 01), end = c(year, month))
  pmca.d.in <- window(pmca.d, start = c(2004, 01), end = c(2019, 12))
  
  # ibc br
  IBC_BR <- rbcb::get_series(c(IBC_BR = 24363), start_date = "01-01-2003")
  ibc_br <- ts(IBC_BR$IBC_BR, start = c(2003, 01), frequency = 12)
  ibc.br.yoy <- yoy(ibc_br)
  ibc.fit <- grnn_forecasting(na.omit(ibc.br.yoy), h = 2)
  FIT.IBC <- ibc.fit$prediction
  ibcibc <- ts(c(ibc.br.yoy, FIT.IBC[1], FIT.IBC[2]), start = c(2004, 01), frequency = 12)
  ibc.full <- window(ibcibc, start = c(2004, 01), end = c(year, month))
  ibc.in <- window(ibcibc, start = c(2004, 01), end = c(2019, 12))
  
  # expectations
  final_expec <- function() {
    trihoje <- quarter(Sys.Date())
    exp <- function(ref) {
      meedr::get_quarterly(indicator = "PIB Total", first_date = Sys.Date() - 30 * 365, reference_date = ref)
    }
    GDP.now.expec <- function(trihoje) {
      PIB.Expect <- c()
      for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
        for (i in 1:4) {
          gdp_val <- exp(paste0(i, "/", j))$median[1]
          PIB.Expect[paste0(i, "/", j)] <- gdp_val
        }
      }
      if (trihoje == "1") {
        return(PIB.Expect[1:(length(PIB.Expect) - 3)])
      }
      if (trihoje == "2") {
        return(PIB.Expect[1:(length(PIB.Expect) - 2)])
      }
      if (trihoje == "3") {
        return(PIB.Expect[1:(length(PIB.Expect) - 1)])
      }
      return(PIB.Expect)
    }
    PIB.Expect <- c()
    for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
      for (i in 1:4) {
        gdp_val <- exp(paste0(i, "/", j))$median[1]
        PIB.Expect[paste0(i, "/", j)] <- gdp_val
      }
    }
    gdp.expec <- GDP.now.expec(trihoje)
    gdp.expec.ts <- round(ts(gdp.expec, start = c(2002, 1), frequency = 4), digits = 2)
    monthly.gdp.expec.ts <- qtr2month(gdp.expec.ts, reference_month = 3, interpolation = TRUE)
    time.M.expec <- as.Date(time(monthly.gdp.expec.ts))
    df_expec <- data.frame(date = time.M.expec, expec = monthly.gdp.expec.ts)
    return(df_expec)
  }
  
  expec <- final_expec()
  expec <- ts(expec[, 2], start = c(2002, 03), frequency = 12)
  expec.full <- window(expec, start = c(2004, 01), end = c(year, month))
  expec.in <- window(expec, start = c(2004, 01), end = c(2019, 12))
  
  # nuci
  nuci <- rbcb::get_series(c(nuci = 24352), start_date = "01-01-2003")
  nuci.yoy <- yoy(nuci$nuci)
  nuci.fit <- grnn_forecasting(na.omit(nuci.yoy), h = 1)
  nuci.ts <- ts(c(nuci.yoy, nuci.fit$prediction[1]), start = c(2002, 01), frequency = 12)
  nuci.full <- window(nuci.ts, start = c(2004, 01), end = c(year, month))
  nuci.in <- window(nuci.ts, start = c(2004, 01), end = c(2019, 12))
  
  # midas1
  midas_final1 <- midas_r(formula = pib.in ~
                            mls(pim.in, 1, 3) +
                            mls(pmca.in, 1, 3) +
                            mls(ibc.in, 1, 3) +
                            mls(expec.in, 1, 3) +
                            mls(nuci.in, 1, 3), start = NULL)
  
  # forecast
  fulldata1 <- list(pim.in = pim.full,
                    pmca.in = pmca.full,
                    ibc.in = ibc.full,
                    expec.in = expec.full,
                    nuci.in = nuci.full,
                    pib.in = pib.full)
  
  insample1 <- 1:length(pib.in)
  outsample1 <- (1:length(fulldata1$pib.in))[-insample1]
  avgf1 <- average_forecast(list(midas_final1),
                            data = fulldata1,
                            insample = insample1,
                            outsample = outsample1)
  AVGF1 <- ts(avgf1$forecast, start = c(2020, 01), frequency = 4)
  
  # midas2
  midas_final2 <- midas_r(pib.in ~ mls(pim.in, 1:3, 3) +
                            mls(pmca.in, 1:3, 3) +
                            mls(ibc.in, 1:3, 3), start = list(pim.in = rep(0, 3),
                                                              pmca.in = rep(0, 3),
                                                              ibc.in = rep(0, 3)))
  
  # forecast
  fulldata2 <- list(pim.in = pim.full,
                    pmca.in = pmca.full,
                    ibc.in = ibc.full,
                    pib.in = pib.full)
  
  insample2 <- 1:length(pib.in)
  outsample2 <- (1:length(fulldata2$pib.in))[-insample2]
  avgf2 <- average_forecast(list(midas_final2), data = fulldata2, insample = insample2, outsample = outsample2)
  AVGF2 <- ts(avgf2$forecast, start = c(2020, 01), frequency = 4)
  
  # midas3
  midas_final3 <- midas_r(pib.in ~ mls(pim.in, 1:3, 3) +
                            mls(ibc.in, 1:3, 3), start = list(pim.in = rep(0, 3),
                                                              ibc.in = rep(0, 3)))
  
  # forecast
  fulldata3 <- list(pim.in = pim.full,
                    ibc.in = ibc.full,
                    pib.in = pib.full)
  
  insample3 <- 1:length(pib.in)
  outsample3 <- (1:length(fulldata3$pib.in))[-insample3]
  avgf3 <- average_forecast(list(midas_final3), data = fulldata3, insample = insample3, outsample = outsample3)
  AVGF3 <- ts(avgf3$forecast, start = c(2020, 01), frequency = 4)
  
  # midas4
  midas_final4 <- midas_r(pib.in ~ mls(expec.in, 1, 3) +
                            mls(ibc.in, 1, 3), start = NULL)
  
  # forecast
  fulldata4 <- list(expec.in = expec.full,
                    ibc.in = ibc.full,
                    pib.in = pib.full)
  
  insample4 <- 1:length(pib.in)
  outsample4 <- (1:length(fulldata4$pib.in))[-insample4]
  avgf4 <- average_forecast(list(midas_final4), data = fulldata4, insample = insample4, outsample = outsample4)
  AVGF4 <- ts(avgf4$forecast, start = c(2020, 01), frequency = 4)
  
  # midas5
  midas_final5 <- midas_r(pib.in ~ mls(pim.d.in, 1, 3) +
                            mls(ibc.in, 1, 3) +
                            mls(expec.in, 1, 3), start = NULL)
  
  summary(midas_final5)
  
  # forecast
  fulldata5 <- list(pim.d.in = pim.d.full,
                    ibc.in = ibc.full,
                    expec.in = expec.full,
                    pib.in = pib.full)
  
  insample5 <- 1:length(pib.in)
  outsample5 <- (1:length(fulldata5$pib.in))[-insample5]
  avgf5 <- average_forecast(list(midas_final5), data = fulldata5, insample = insample5, outsample = outsample5)
  AVGF5 <- ts(avgf5$forecast, start = c(2020, 01), frequency = 4)
  
  midas.nowcast.ts <- rowMeans(cbind(AVGF1, AVGF2, AVGF3, AVGF4, AVGF5))
  
  # Plotting the nowcast vs true values
  plot(pib.full[(length(pib.full)-17):length(pib.full)], type = "l", col = "red", lwd = 2,
       main = "MIDAS Nowcasting vs True GDP Values",
       xlab = "Time",
       ylab = "GDP",
       ylim = range(c(pib.full[(length(pib.full)-17):length(pib.full)], midas.nowcast.ts)))
  lines(midas.nowcast.ts, col = "blue", lwd = 2)
  legend("topright", legend = c("True GDP", "MIDAS Nowcast"),
         col = c("red", "blue"), lwd = 2)
  
  # Calculate residuals
  true_values <- pib.full[(length(pib.full) - 17):length(pib.full)]
  residuals <- true_values - midas.nowcast.ts
  
  # Plot histogram of residuals
  hist(residuals, main = "Histogram of Residuals", xlab = "Residual", breaks = 10)
  
  return(midas.nowcast.ts)
}

# Usage
midas_ts1 <- midas_ts(2024, 02)
