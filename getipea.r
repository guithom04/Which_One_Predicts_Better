getipea <- function(){
  library(ipeadatar)
  library(dplyr)
  library(forecast)
  # brazilian payroll (cumulative 12m)
  novo_caged = ipeadatar::ipeadata(code = "CAGED12_SALDON12")
  antigo_caged = ipeadatar::ipeadata(code = "CAGED12_SALDO12")
  CAGED <- bind_rows(antigo_caged,novo_caged)
  caged.a12 = CAGED %>%
    mutate(cumulative_12_months = rollapply(value, width = 12, FUN = sum, align = "right", fill = NA))
  caged = ts(caged.a12$cumulative_12_months, start = c(1999,05), frequency = 12)
  datas.caged = caged.a12$date
  df_caged <- data.frame(date = datas.caged, caged_a12 = caged/10000000)
  ABPO = ipeadatar::ipeadata(code = "ABPO12_PAPEL12")
  abpo = ts(ABPO$value, start = c(1980,01), frequency = 12)
  abpo.yoy = yoy(abpo)
  df_abpo <- data.frame(date = as.Date(time(abpo.yoy)), ABPO = abpo.yoy)
  fenabrave = ipeadatar::ipeadata(code = "FENABRAVE12_VENDVETOT12")
  fenabrave = ts(fenabrave$value, start = c(1990,01), frequency = 12)
  fenabrave = yoy(fenabrave)
  df_fenabrave <- data.frame(date = as.Date(time(fenabrave)), fenabrave = c(fenabrave))
  anp.gas.natural = ipeadatar::ipeadata(code = "ANP12_PDGASN12")
  anp.gas.nat =  ts(anp.gas.natural$value, start = c(1979,01), frequency = 12)
  anp.yoy = ts(c(NA,diff(yoy(anp.gas.nat))), start = c(1980,01), frequency = 12)
  df_anp <- data.frame(date = as.Date(time(anp.yoy)), anp = anp.yoy)
  ibovespa_fechamento = ipeadatar::ipeadata(code = "ANBIMA12_IBVSP12")
  ibovespa.fechamento = ts(c(NA,diff(ibovespa_fechamento$value)), start = c(1968,02), frequency = 12)
  df_ibovespa <- data.frame(date = as.Date(time(ibovespa.fechamento)), ibov = c(ibovespa.fechamento))
  anfavea = ipeadatar::ipeadata(code = "ANFAVE12_LICVETOT12")
  anfavea = ts(anfavea$value, start = c(1957,01), frequency = 12)
  anfavea = ts(c(NA,diff(yoy(anfavea))), start = c(1958,01), frequency = 12)
  df_anfavea <- data.frame(date = as.Date(time(anfavea)), anfavea = c(anfavea))
  cpi = ipeadatar::ipeadata(code = "BLS12_IPCEUA12")
  cpi = ts(cpi$value, start = c(1947,01), frequency = 12)
  yoy.cpi = ts(c(NA,NA,diff(diff(yoy(cpi)))), start = c(1948,01), frequency = 12)
  df_cpi <- data.frame(date = as.Date(time(yoy.cpi)), cpi = c(yoy.cpi))
  bp = ipeadatar::ipeadata(code = "BPAG12_BC12")
  bp = ts(bp$value, start = c(1995,01), frequency = 12)
  bp = yoy(bp)
  df_bp <- data.frame(date = as.Date(time(bp)), bp = c(bp))
  idp = ipeadatar::ipeadata(code = "BPAG12_IDPI12")
  idp = ts(idp$value, start = c(1995,01), frequency = 12)
  idp = ts(c(NA,diff(yoy(idp))), start = c(1996,01), frequency = 12)
  df_idp <- data.frame(date = as.Date(time(idp)), idp = c(idp))
  exp.inf = ipeadatar::ipeadata(code = "BM12_IPCAEXP1212")
  exp.inf = ts(c(NA, diff(exp.inf$value)), start = c(2001,07), frequency = 12)
  df_expinf <- data.frame(date = as.Date(time(exp.inf)), expinf = c(exp.inf))
  primario = ipeadatar::ipeadata(code = "BM12_NFGFPNS12")
  primario = ts(primario$value, start = c(1991,01), frequency = 12)
  primario = diff(yoy(primario))
  df_primario <- data.frame(date = as.Date(time(primario)), primario = c(primario))
  credito = ipeadatar::ipeadata(code = "BM12_CS12")
  yoy.credito = ts(yoy(credito$value), start = c(1990,01), frequency = 12)
  credito = ts(yoy.credito, start = c(1990,01), frequency = 12)
  df_credito <- data.frame(date = as.Date(time(credito)), credito = c(credito))
  # reduce
  df <- Reduce(function(df1, df2) merge(df1, df2, by = "date", all = TRUE),
               list(df_caged,
                    df_abpo,
                    df_fenabrave,
                    df_anp,
                    df_ibovespa,
                    df_anfavea,
                    df_cpi,
                    df_bp,
                    df_idp,
                    df_expinf,
                    df_primario,
                    df_credito))
}
ipea <- getipea()
tail(ipea)
