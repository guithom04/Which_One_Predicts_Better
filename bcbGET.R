bcbGET <- function(){
  library(rbcb)
  # IBCBr
  IBC_BR = rbcb::get_series(c(IBC_BR = 24363), start_date = "01-01-2003")
  # IBC_BR = GetBCBData::gbcbd_get_series(id = 24363, first.date = "01-01-2003")
  ibc.br.yoy = ts(yoy(IBC_BR$IBC_BR), start = c(2004,01), frequency = 12)
  df_ibc <- data.frame(date = as.Date(time(ibc.br.yoy)), ibc_br = diff(c(NA,ibc.br.yoy)))
  
  # NUCI
  nuci <- rbcb::get_series(c(nuci = 24352), start_date = "01-01-2003")
  df_nuci <- data.frame(date = nuci$date[13:length(nuci$date)], nuci = diff(c(NA,yoy(nuci$nuci))))
  df <- Reduce(function(df1, df2) merge(df1, df2, by = "date", all = TRUE),
               list(df_nuci, 
                    df_ibc))
  return(df)
}
bcb = bcbGET()
