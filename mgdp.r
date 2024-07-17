mgdp <- function(){
  library(sidrar)
  library(zoo)
  library(lubridate)
  PIB = get_sidra(api = "/t/5932/n1/all/v/6561/p/all/c11255/90707/d/v6561%201")
  pib = ts(PIB$Valor, start = c(1996,01), frequency = 4)
  datas.pib = PIB$`Trimestre (Código)`
  pib.m = qtr2month(pib, reference_month = 3, interpolation = TRUE)
  PIB.M = ts(c(pib.m, rep(tail(pib.m)[6],2)), start = c(1996,01), frequency = 12)
  df_pib <- data.frame(date = as.Date(time(PIB.M)), pib = PIB.M)
  return(df_pib)
}
mgdp <- mgdp()
  
