library(forecast)   # nnetar()
library(imputeTS)   # na.interp() para o chute inicial

interp_nnetar <- function(y,
                          p       = 12,   # nº de lags
                          repeats = 20,   # ensembles internos do nnetar
                          iter    = 3) {  # nº de iterações de refinamento
  
  stopifnot(is.ts(y))                # garante objeto ts
  na_pos <- which(is.na(y))          # posições faltantes
  if (length(na_pos) == 0) return(y) # nada a interpolar
  
  # 1) chute inicial (interp ETS/STL do imputeTS)
  y_filled <- na.interp(y)
  
  for (i in seq_len(iter)) {
    fit  <- nnetar(y_filled, p = p, repeats = repeats)
    y_hat        <- fitted(fit)      # valores in-sample
    y_filled[na_pos] <- y_hat[na_pos]
  }
  
  return(y_filled)
}


#uso

set.seed(1)
x <- ts(AirPassengers)           
x[c(20:25, 100, 110:112)] <- NA  # criar buraco artificial

x_imp <- interp_nnetar(x)

plot(cbind(original = x, imputed = x_imp),
     main = "Interpolação com nnetar")
