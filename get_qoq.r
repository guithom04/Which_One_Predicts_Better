# get QoQ from YoY Number
get_qoq <- function(yoy){
  library(sidrar)
  library(lubridate)
  library(stats)
  library(dplyr)
  library(seasonal)
  CNT = sidrar::get_sidra(api = "/t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%202")
  pib = CNT  %>% 
    dplyr::select(`Trimestre (Código)`, Valor)
  colnames(pib) <- c("trimestre", "valor")
  pib.ts = ts(pib$valor,
              start = c(as.numeric(substr(pib$trimestre,1,4))[1],
                        as.numeric(substr(pib$trimestre,5,6))[1]), 
              end = c(as.numeric(substr(pib$trimestre,1,4))[nrow(pib)],
                      as.numeric(substr(pib$trimestre,5,6))[nrow(pib)]), frequency = 4)
  cresce = (1+yoy/100)*pib.ts[length(pib.ts)-3]
  Fpib = c(pib.ts,round(cresce, digits = 2))
  Fpib.ts = ts(Fpib, start = c(1996,01), frequency = 4)
  Fpib.meu.dessaz = seas(x = Fpib.ts, 
                         outlier.types = "all",
                         pickmdl.method = "best",
                         pickmdl.identify = "all",
                         forecast.maxlead = 6,
                         forecast.maxback = 0,
                         forecast.print = "none",
                         estimate = "",
                         x11.savelog = "q",
                         automdl = NULL)
  round(100*(Fpib.meu.dessaz$data[,1][nrow(Fpib.meu.dessaz$data)]/Fpib.meu.dessaz$data[,1][nrow(Fpib.meu.dessaz$data)-1]-1), digits = 1)
}

# Usage
# QoQ% = get_qoq(YoY%)
