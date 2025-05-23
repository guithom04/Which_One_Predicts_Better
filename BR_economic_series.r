BR_economic_series <- function(){
  # function perfectly informed market expectations
  yoy = function(series){
    index = 1:length(series)
    for(i in index){
      YoY <- series[12+index]/series[index]-1
    }
    return(ts(na.omit(YoY),start = c(start(series)[1]+1,start(series)[2]),frequency = 12))
  }
  dfexpec <- function(){
    # transformar Q em M, com interpolação.(retirado de NMECSYS - NOWCASTING)
    qtr2month <- function(x, reference_month = 3, interpolation = FALSE){
      
      if(!reference_month %in% c(1,2,3)){
        stop("The reference_month should be 1,2 or 3")
      }
      
      if(!is.ts(x)){
        stop("x should be a ts object")
      }
      
      if(!is.null(dim(x))){
        stop("x should be a single ts object")
      }
      
      data_q <- zoo::as.Date(x)
      data_m <- seq(data_q[1], data_q[length(data_q)], by = 'months')
      out_x <- ts(rep(NA,length(data_m)),
                  start =  as.numeric(c(substr(data_q[1],1,4), substr(data_q[1],6,7))),
                  frequency = 12)
      out_x[data_m %in% data_q] <- x
      
      if(reference_month %in% c(2,3)){
        out_x <- stats::lag(out_x, -(reference_month-1))
        data_q <- zoo::as.Date(out_x)
        data_m <- seq(data_q[1], data_q[length(data_q)], by = 'months')
      }
      
      if(interpolation){
        xout <- zoo::as.yearmon(data_m)
        out_x <- stats::approx(out_x, xout = xout, method = "linear")
        out_x <- ts(out_x$y, start = out_x$x[1], end = out_x$x[length(out_x$x)], frequency = 12)
      }
      
      # output
      return(out_x)
    }
    # transformar nível em yoy
    yoy = function(series){
      index = 1:length(series)
      for(i in index){
        YoY <- series[12+index]/series[index]-1
      }
      return(ts(na.omit(YoY),start = c(start(series)[1]+1,start(series)[2]),frequency = 12))
    }
    # pacotes usados extração de séries
    library(zoo)
    library(meedr)
    library(lubridate)
    trihoje = quarter(Sys.Date())
    # gets expectative given reference
    exp <- function(ref){
      meedr::get_quarterly(indicator = "PIB Total",
                           first_date = Sys.Date()-30*365,
                           reference_date = ref)
    }
    # gets today expectative
    GDP.now.expec <- function(trihoje){
      if (trihoje == "1") {
        exp <- function(ref){
          meedr::get_quarterly(indicator = "PIB Total",
                               first_date = Sys.Date()-30*365,
                               reference_date = ref)
        }
        PIB.Expect <- c()
        for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
          for (i in 1:4) {
            gdp_val <- exp(paste0(i,"/",j))$median[1]
            PIB.Expect[paste0(i,"/",j)] <- gdp_val
          }
        }
        EXPEC.PIB.HOJE <- PIB.Expect[1:(length(PIB.Expect)-3)]
        
      }
      if (trihoje == "2") {
        exp <- function(ref){
          meedr::get_quarterly(indicator = "PIB Total",
                               first_date = Sys.Date()-30*365,
                               reference_date = ref)
        }
        PIB.Expect <- c()
        for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
          for (i in 1:4) {
            gdp_val <- exp(paste0(i,"/",j))$median[1]
            PIB.Expect[paste0(i,"/",j)] <- gdp_val
          }
        }
        EXPEC.PIB.HOJE <- PIB.Expect[1:(length(PIB.Expect)-2)]
      }
      if (trihoje == "3") {
        exp <- function(ref){
          meedr::get_quarterly(indicator = "PIB Total",
                               first_date = Sys.Date()-30*365,
                               reference_date = ref)
        }
        PIB.Expect <- c()
        for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
          for (i in 1:4) {
            gdp_val <- exp(paste0(i,"/",j))$median[1]
            PIB.Expect[paste0(i,"/",j)] <- gdp_val
          }
        }
        EXPEC.PIB.HOJE <- PIB.Expect[1:(length(PIB.Expect)-1)]
      }
      if (trihoje == "4") {
        exp <- function(ref){
          meedr::get_quarterly(indicator = "PIB Total",
                               first_date = Sys.Date()-30*365,
                               reference_date = ref)
        }
        PIB.Expect <- c()
        for (j in 2002:as.numeric(format(Sys.Date(), "%Y"))) {
          for (i in 1:4) {
            gdp_val <- exp(paste0(i,"/",j))$median[1]
            PIB.Expect[paste0(i,"/",j)] <- gdp_val
          }
        }
        EXPEC.PIB.HOJE <- PIB.Expect
      }
      return(EXPEC.PIB.HOJE)
    }
    gdp.expec = GDP.now.expec(trihoje)
    gdp.expec.ts = round(ts(gdp.expec,start = c(2002,1), frequency = 4), digits = 2)
    monthly.gdp.expec.ts = qtr2month(gdp.expec.ts, reference_month = 3, interpolation = TRUE)
    time.M.expec = as.Date(time(monthly.gdp.expec.ts))
    plot(monthly.gdp.expec.ts)
    expec = data.frame(date = time.M.expec, expec = monthly.gdp.expec.ts)
    return(expec)}
  exp = dfexpec()
  # function get yoy Qgdp to Mgdp
  mgdp <- function(){
    library(sidrar)
    library(zoo)
    library(lubridate)
    qtr2month <- function(x, reference_month = 3, interpolation = FALSE){
      
      if(!reference_month %in% c(1,2,3)){
        stop("The reference_month should be 1,2 or 3")
      }
      
      if(!is.ts(x)){
        stop("x should be a ts object")
      }
      
      if(!is.null(dim(x))){
        stop("x should be a single ts object")
      }
      
      data_q <- zoo::as.Date(x)
      data_m <- seq(data_q[1], data_q[length(data_q)], by = 'months')
      out_x <- ts(rep(NA,length(data_m)),
                  start =  as.numeric(c(substr(data_q[1],1,4), substr(data_q[1],6,7))),
                  frequency = 12)
      out_x[data_m %in% data_q] <- x
      
      if(reference_month %in% c(2,3)){
        out_x <- stats::lag(out_x, -(reference_month-1))
        data_q <- zoo::as.Date(out_x)
        data_m <- seq(data_q[1], data_q[length(data_q)], by = 'months')
      }
      
      if(interpolation){
        xout <- zoo::as.yearmon(data_m)
        out_x <- stats::approx(out_x, xout = xout, method = "linear")
        out_x <- ts(out_x$y, start = out_x$x[1], end = out_x$x[length(out_x$x)], frequency = 12)
      }
      
      # output
      return(out_x)
    }
    # transformar nível em yoy
    yoy = function(series){
      index = 1:length(series)
      for(i in index){
        YoY <- series[12+index]/series[index]-1
      }
      return(ts(na.omit(YoY),start = c(start(series)[1]+1,start(series)[2]),frequency = 12))
    }
    PIB = get_sidra(api = "/t/5932/n1/all/v/6561/p/all/c11255/90707/d/v6561%201")
    pib = ts(PIB$Valor, start = c(1996,01), frequency = 4)
    datas.pib = PIB$`Trimestre (Código)`
    pib.m = qtr2month(pib, reference_month = 3, interpolation = TRUE)
    PIB.M = ts(c(pib.m, rep(tail(pib.m)[6],2)), start = c(1996,01), frequency = 12)
    df_pib <- data.frame(date = as.Date(time(PIB.M)), pib = PIB.M)
    return(df_pib)
  }
  mgdp <- mgdp()
  # function to get real time database specified series from sidra
  sidrarGET <- function(){
    qtr2month <- function(x, reference_month = 3, interpolation = FALSE){
      
      if(!reference_month %in% c(1,2,3)){
        stop("The reference_month should be 1,2 or 3")
      }
      
      if(!is.ts(x)){
        stop("x should be a ts object")
      }
      
      if(!is.null(dim(x))){
        stop("x should be a single ts object")
      }
      
      data_q <- zoo::as.Date(x)
      data_m <- seq(data_q[1], data_q[length(data_q)], by = 'months')
      out_x <- ts(rep(NA,length(data_m)),
                  start =  as.numeric(c(substr(data_q[1],1,4), substr(data_q[1],6,7))),
                  frequency = 12)
      out_x[data_m %in% data_q] <- x
      
      if(reference_month %in% c(2,3)){
        out_x <- stats::lag(out_x, -(reference_month-1))
        data_q <- zoo::as.Date(out_x)
        data_m <- seq(data_q[1], data_q[length(data_q)], by = 'months')
      }
      
      if(interpolation){
        xout <- zoo::as.yearmon(data_m)
        out_x <- stats::approx(out_x, xout = xout, method = "linear")
        out_x <- ts(out_x$y, start = out_x$x[1], end = out_x$x[length(out_x$x)], frequency = 12)
      }
      
      # output
      return(out_x)
    }
    # transformar nível em yoy
    yoy = function(series){
      index = 1:length(series)
      for(i in index){
        YoY <- series[12+index]/series[index]-1
      }
      return(ts(na.omit(YoY),start = c(start(series)[1]+1,start(series)[2]),frequency = 12))
    }
    library(sidrar)
    library(zoo)
    library(lubridate)
    
    #BR industrial production
    PIM = get_sidra(api = "/t/8888/n1/all/v/11602/p/all/c544/129314/d/v11602%201")
    pim = ts(c(NA,diff(PIM$Valor)), start = c(2002,01), frequency = 12)
    df_pim = data.frame(date = as.Date(time(pim)), pim = pim)
    
    #Capital Goods
    PIM_BK = get_sidra(api = "/t/8887/n1/all/v/11602/p/all/c543/129278/d/v11602%201")
    pim_bk = ts(PIM_BK$Valor, start = c(2002,01),frequency = 12)
    df_pim_bk = data.frame(date = as.Date(time(pim_bk)), pim_bk = pim_bk)
    
    #Intermediary goods 
    PIM_BI = get_sidra(api = "/t/8887/n1/all/v/11602/p/all/c543/129283/d/v11602%201")
    pim_bi = ts(PIM_BI$Valor, start = c(2002,01), frequency = 12)
    df_pim_bi = data.frame(date = as.Date(time(pim_bi)), pim_bi = pim_bi)
    
    #BR Retail Sales
    PMCA = get_sidra(api = "/t/8881/n1/all/v/11709/p/all/c11046/56736/d/v11709%201")
    pmca = ts(c(NA,diff(PMCA$Valor)), start = c(2003,01), frequency = 12)
    df_pmca = data.frame(date = as.Date(time(pmca)), pmca = pmca)
    
    #Área colhida agricultura
    AREA_COLHIDA <- sidrar::get_sidra(api = "/t/6588/n1/all/v/216/p/all/c48/0")
    area_colhida <- ts(yoy(c(NA,diff(AREA_COLHIDA$Valor))), start = c(2007,10), frequency = 12)
    df_area <- data.frame(date = as.Date(time(area_colhida)), area = area_colhida)
    
    # Custo de Habitação
    HOUSING <- sidrar::get_sidra(api = "/t/647/n3/11/v/all/p/all/c314/7087/c41/787/d/v51%202")
    housing <- ts(yoy(c(NA,NA,diff(HOUSING$Valor,2))), start = c(2000,02), frequency = 12)
    df_housing <- data.frame(date = as.Date(time(housing)), housing = housing)
    
    # pmc veículos 
    PMC_VEICULOS <- sidrar::get_sidra(api = "/t/8884/n1/all/v/11709/p/all/c11046/56737/d/v11709%201")
    pmc_veiculos <- ts(PMC_VEICULOS$Valor, start = c(2000,01), frequency = 12)
    df_pmcv <- data.frame(date = as.Date(time(pmc_veiculos)), pmcv = pmc_veiculos)
    
    # pmc combustiveis e lubrificantes
    PMC_COMB <- sidrar::get_sidra(api = "/t/8883/n1/all/v/11709/p/all/c11046/56736/c85/90671/d/v11709%201")
    pmc_comb <- ts(PMC_COMB$Valor, start = c(2000,01), frequency = 12)
    df_pmc_comb <- data.frame(date = as.Date(time(pmc_comb)), pmc_comb = pmc_comb)
    
    # supermercado
    PMC_SUPER <- sidrar::get_sidra(api = "/t/8883/n1/all/v/11709/p/all/c11046/56736/c85/90672/d/v11709%201")
    pmc_super <- ts(PMC_SUPER$Valor, start = c(2000,01), frequency = 12)
    df_pmc_super <- data.frame(date = as.Date(time(pmc_super)), pmc_super = pmc_super)
    
    
    # tecidos
    TECIDOS <- sidrar::get_sidra(api = "/t/8883/n1/all/v/11709/p/all/c11046/56736/c85/90673/d/v11709%201")
    tecidos <- ts(TECIDOS$Valor, start = c(2000,01), frequency = 12)
    df_tecidos <- data.frame(date = as.Date(time(tecidos)), tecidos = tecidos)
    
    
    # material de contruç"ao
    
      
    # unified dataframe
    df <- Reduce(function(df1, df2) merge(df1, df2, by = "date", all = TRUE),
                 list(df_pim, 
                      df_pim_bk,
                      df_pim_bi,
                      df_pmca,
                      df_area,
                      df_housing,
                      df_pmcv,
                      df_pmc_comb,
                      df_pmc_super,
                      df_tecidos))
  }
  sidrar <- sidrarGET()
  # function to get real time database specified series from BCB APIs
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
  # function to get real time database specified series from IPEA API
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
    ETTJ_12M <- ipeadatar::ipeadata(code = "ANBIMA12_TJTLN1212")
    ettj_yoy <- yoy(ETTJ_12M$value)
    ettj_12m <- ts(ettj_yoy, start = c(2001,04), frequency = 12)
    df_ettj <- data.frame(date = as.Date(time(ettj_1m)), ettj = ettj_1m)
    CAMBIO_NOMINAL <- ipeadatar::ipeadata(code = "BM12_ERC12")
    cambio_yoy = yoy(CAMBIO_NOMINAL$value)
    cambio_nominal <- ts(c(NA, diff(cambio_yoy)), start = c(1954,01), frequency = 12)
    df_cambio <- data.frame(date = as.Date(time(cambio_nominal)), cambio = cambio_nominal)
    CDI_OVER <- ipeadatar::ipeadata(code = "BM12_TJCDI12")
    cdi_over <- ts(c(NA,diff(yoy(CDI_OVER$value))), start = c(1987,03), frequency = 12)
    df_cdi <- data.frame(date = as.Date(time(cdi_over)), cdi = cdi_over)
    ICC_FECOM = ipeadatar::ipeadata(code = "FCESP12_IIC12")
    icc_fecom_yoy <- yoy(ICC_FECOM$value)
    icc_fecom <- ts(icc_fecom_yoy,start = c(2000,12), frequency = 12)
    df_icc = data.frame(date = as.Date(time(icc_fecom)), icc = icc_fecom)
    M2 = ipeadatar::ipeadata(code = "BM12_TIPRINCNY12")
    m2 = ts(yoy(M2$value), start = c(2000,12), frequency = 12)
    df_m2 = data.frame(date = as.Date(time(m2)), m2 = m2)
    IMP = ipeadatar::ipeadata(code = "PAN12_MTV12")
    imp = ts(c(NA,yoy(IMP$value)), start = c(1996,01), frequency = 12)
    df_imp = data.frame(date = as.Date(time(imp)), imports = imp)
    CEE = ipeadatar::ipeadata(code = "ELETRO12_CEECOM12")
    cee = ts(c(NA,diff(yoy(CEE$value))), start = c(1976,01), frequency = 12)
    df_cee = data.frame(date = as.Date(time(cee)), Energia = cee)
    ICEA_FECOM = ipeadatar::ipeadata(code = "FCESP12_IICA12")
    icea_fecom = ts(yoy(ICEA_FECOM$value), start = c(1995,06), frequency = 12)
    df_icea = data.frame(date = as.Date(time(icea_fecom)), icea = icea_fecom)
    IEC_FECOM = ipeadatar::ipeadata(code = "FCESP12_IICF12")
    iec_fecom = ts(yoy(IEC_FECOM$value), start = c(1995,06), frequency = 12)
    df_iec = data.frame(date = as.Date(time(iec_fecom)), iec = iec_fecom)
    EXP = ipeadatar::ipeadata(code = "SECEX12_XBENCAPGCE12")
    exp = ts(yoy(EXP$value), start = c(1998,01), frequency = 12)
    df_exp = data.frame(date = as.Date(time(exp)),exports = exp)
    IOF = ipeadatar::ipeadata(code = "SRF12_IOF12")
    iof = ts(yoy(IOF$value), start = c(1987,01), frequency = 12)
    df_iof = data.frame(date = as.Date(time(iof)), iof = iof)
    NASDAQ = ipeadatar::ipeadata(code = "GM12_NASDAQ12")
    nasdaq = ts(yoy(NASDAQ$value), start = c(1972,03), frequency = 12)
    df_nasdaq = data.frame(date = as.Date(nasdaq), nasdaq = nasdaq)
    DOW_JONES = ipeadatar::ipeadata(code = "GM12_DOW12")
    dow_jones = ts(yoy(DOW_JONES$value), start = c(1923,01), frequency = 12)
    df_dowjones = data.frame(date = as.Date(dow_jones), dow_jones = dow_jones)
    
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
                      df_credito,
                      df_ettj,
                      df_cambio,
                      df_cdi,
                      df_m2,
                      df_imp,
                      df_cee,
                      df_icea,
                      df_iec,
                      df_exp,
                      df_iof,
                      df_nasdaq,
                      df_dowjones))
  }
  ipea <- getipea()
  # now merge the series into one only unbalanced panel.
  merged_df <- Reduce(function(df1, df2) merge(df1, df2, by = "date", all = TRUE),
                      list(mgdp, exp, sidrar, bcb, ipea))
  tail(merged_df)
  return(merged_df)
}
df = BR_economic_series()
tail(df)
