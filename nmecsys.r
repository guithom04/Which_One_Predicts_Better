###########################################################################
###                NOWCASTING: WHICH ONE PREDICTS BETTER                ###
###                      LARGE FUNCTION 8:nmecsys                       ###
###########################################################################
nmecsys <- function(df){
  
  # pacotes
  library(nowcasting)
  library(dplyr)
  
  # base deve ser objeto split_prtdb.1s in sample
  full <- df
  full.ts = ts(full[,-1], start = c(2007,10), frequency = 12)
  pib <- ts(full[, 2], start = c(2007, 10), frequency = 12)
  
  
  # comando Bpanel configura o dataframe para nowcast
  data <- Bpanel(base = full.ts,
                 trans = rep(0,ncol(full.ts)),
                 aggregate = FALSE)
  
  # vetor de frequências, como já trabalhei as séries
  # todas frequência mensal.
  frequency <- c(rep(12, ncol(data)))
  
  # nowcast
  nowcast <- nowcast(formula = pib ~ .,
                     data = data,
                     r = 2,
                     p = 2,
                     q = 2, 
                     method = '2s_agg',
                     frequency = frequency)
  
  # y forecasts
  fit = nowcast$yfcst
  return(fit)
  
}
# usage
# df <- split_prtdb.1s$df_in
# nmec <- nmecsys(df)
