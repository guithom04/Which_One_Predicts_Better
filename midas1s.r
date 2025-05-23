###########################################################################
###                NOWCASTING: WHICH ONE PREDICTS BETTER                ###
###                      LARGE FUNCTION 9:midas_1s                      ###
###########################################################################
# MIDAS 1S
midas1s <- function(year, month, quarter){
  
  yoy = function(series){
    index = 1:length(series)
    for(i in index){
      YoY <- series[12+index]/series[index]-1
    }
    return(ts(na.omit(YoY),start = c(start(series)[1]+1,start(series)[2]),frequency = 12))
  }
  library(rbcb)
  library(tidyipea)
  library(forecast)
  library(midasr)
  
  relevant_date = as.Date(paste0(year,"-",month,"-01")) 
  old_date = relevant_date %m-% months(3)
  
  # pib
  table.pib = get_sidra(api = "/t/5932/n1/all/v/6561/p/all/c11255/90707/d/v6561%201")
  pib.ts = ts(table.pib$Valor, start = c(1996,01), frequency = 4)
  pib.fit = tsfgrnn::grnn_forecasting(na.omit(pib.ts), h = 1)
  FIT.PIB = pib.fit$prediction
  pib.trim = ts(c(table.pib$Valor,FIT.PIB), start = c(1996,01), frequency = 04)
  pib.full = window(pib.trim, start = c(2004,01), end = c(year(relevant_date),quarter(relevant_date)))
  pib.in = window(pib.trim, start = c(2004,01), end = c(year(old_date),quarter(old_date)))
  
  # pim
  table.pim = get_sidra(api = "/t/8888/n1/all/v/11602/p/all/c544/129314/d/v11602%201")
  pim.ts = ts(table.pim$Valor, start = c(2002,01), frequency = 12)
  pim.fit = grnn_forecasting(na.omit(pim.ts), h = 2)
  FIT.PIM = pim.fit$prediction
  pim = ts(c(table.pim$Valor, FIT.PIM[1], FIT.PIM[2]),start = c(2002,01), frequency = 12)
  pim.full = window(pim, start = c(2004,01), end = c(year(relevant_date),month(relevant_date)))
  pim.in = window(pim, start = c(2004,01), end = c(year(old_date),month(old_date)))
  
  # pim diff
  table.pim = get_sidra(api = "/t/8888/n1/all/v/11602/p/all/c544/129314/d/v11602%201")
  pim.ts = ts(table.pim$Valor, start = c(2002,01), frequency = 12)
  pim.fit = grnn_forecasting(na.omit(pim.ts), h = 2)
  FIT.PIM = pim.fit$prediction
  pim.d = ts(diff(c(table.pim$Valor, FIT.PIM[1], FIT.PIM[2])),start = c(2002,02), frequency = 12)
  pim.d.full = window(pim.d, start = c(2004,01), end = c(year(relevant_date),month(relevant_date)))
  pim.d.in = window(pim.d, start = c(2004,01), end = c(year(old_date),month(old_date)))
  
  # pmca
  table.pmca = get_sidra(api = "/t/8881/n1/all/v/11709/p/all/c11046/56736/d/v11709%201")
  pmca.ts = ts(table.pmca$Valor, start = c(2003,01), frequency = 12)
  pmca.fit = grnn_forecasting(na.omit(pmca.ts), h = 2)
  FIT.PMCA = pmca.fit$prediction
  pmca = ts(c(table.pmca$Valor, FIT.PMCA[1], FIT.PMCA[2]), start = c(2003,01), frequency = 12)
  pmca.full = window(pmca, start = c(2004,01), end = c(year(relevant_date),month(relevant_date)))
  pmca.in = window(pmca, start = c(2004,01), end = c(year(old_date),month(old_date)))
  
  # pmca diff
  pmca.d = ts(diff(c(table.pmca$Valor, FIT.PMCA[1], FIT.PMCA[2])), start = c(2003,02), frequency = 12)
  pmca.d.full = window(pmca.d, start = c(2004,01), end = c(year(relevant_date),month(relevant_date)))
  pmca.d.in = window(pmca.d, start = c(2004,01), end = c(year(old_date),month(old_date)))
  
  # ibc br
  IBC_BR = rbcb::get_series(c(IBC_BR = 24363), start_date = "01-01-2003")
  ibc_br = ts(IBC_BR$IBC_BR, start = c(2003,01), frequency = 12)
  ibc.br.yoy = yoy(ibc_br)
  ibc.fit = grnn_forecasting(na.omit(ibc.br.yoy), h = 2)
  FIT.IBC = ibc.fit$prediction
  ibcibc = ts(c(ibc.br.yoy, FIT.IBC[1], FIT.IBC[2]), start = c(2004,01), frequency = 12)
  ibc.full = window(ibcibc, start = c(2004,01), end =c(year(relevant_date),month(relevant_date)))
  ibc.in = window(ibcibc, start = c(2004,01), end = c(year(old_date),month(old_date)))
  
  # expectations
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
  expec = final_expec()
  expec = ts(expec[,2], start = c(2002,03), frequency = 12)
  expec.full = window(expec, start = c(2004,01), end = c(year(relevant_date),month(relevant_date)))
  expec.in = window(expec, start = c(2004,01), end = c(year(old_date),month(old_date)))
  
  # nuci
  nuci <- rbcb::get_series(c(nuci = 24352), start_date = "01-01-2003")
  nuci.yoy <- yoy(nuci$nuci)
  nuci.fit <- grnn_forecasting(na.omit(nuci.yoy), h = 1)
  nuci.ts <- ts(c(nuci.yoy, nuci.fit$prediction[1]), start = c(2002,01), frequency = 12)
  nuci.full <- window(nuci.ts, start = c(2004,01), end = c(year(relevant_date),month(relevant_date)))
  nuci.in = window(nuci.ts, start = c(2004,01), end = c(year(old_date),month(old_date)))
  
  # midas1
  midas_final1 = midas_r(formula = pib.in ~
                           mls(pim.in,1,3)+
                           mls(pmca.in,1,3)+
                           mls(ibc.in,1,3)+
                           mls(expec.in,1,3)+
                           mls(nuci.in,1,3), start = NULL)
  # forecast
  fulldata1 <- list(pim.in = pim.full,
                    pmca.in = pmca.full,
                    ibc.in = ibc.full,
                    expec.in = expec.full,
                    nuci.in = nuci.full,
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
  midas_final5 = midas_r(pib.in~mls(pim.d.in, 1, 3)+mls(ibc.in,1,3)+mls(expec.in,1,3), start = NULL)
  summary(midas_final5)
  
  # forecast
  fulldata5 <- list(pim.d.in = pim.d.full,
                    ibc.in = ibc.full,
                    expec.in = expec.full,
                    pib.in = pib.full)
  
  insample5 <- 1:length(pib.in) 
  outsample5 <- (1:length(fulldata5$pib.in))[-insample5]
  avgf5 = average_forecast(list(midas_final5), data = fulldata5, insample = insample5, outsample = outsample5)
  avgf5$forecast
  
  
  midas.nowcast.one.step <- mean(avgf1$forecast,
                                 avgf2$forecast,
                                 avgf3$forecast,
                                 avgf4$forecast,
                                 avgf5$forecast)
  
}
# usage
midas_1s <- midas1s(2022,12,04)
