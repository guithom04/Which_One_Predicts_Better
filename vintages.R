##########################################################################
###                NOWCASTING: WHICH ONE PREDICTS BETTER                ###
###                            PSEUDO VINTAGES                          ###
###########################################################################
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
# real time dataframe
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
    # IBC_BR = rbcb::get_series(c(IBC_BR = 24363), start_date = "01-01-2003")
    IBC_BR = GetBCBData::gbcbd_get_series(id = 24363, first.date = "01-01-2003")
    # ibc.br.yoy = ts(yoy(IBC_BR$IBC_BR), start = c(2004,01), frequency = 12)
    ibc.br.yoy = ts(yoy(IBC_BR$value), start = c(2004,01), frequency = 12)
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
    df_ettj <- data.frame(date = as.Date(time(ettj_12m)), ettj = ettj_12m)
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

# upperbalance
upperbalance <- function(df) {
  df <- df[, colSums(is.na(df)) < nrow(df)]
  first_valid_indices <- apply(df, 2, function(col) which(!is.na(col))[1])
  latest_index <- max(first_valid_indices, na.rm = TRUE)
  df_cut <- df[latest_index:nrow(df), ]
  return(df_cut)
}
df = upperbalance(df)
df <- df %>%
  filter(date <= "2024-06-01")
tail(df)  

# Define the days, delays df
delays <- list(
  pib = c(1, 90),
  expec = c(1, 7),
  pim = c(4, 52),
  pim_bk = c(4, 52),
  pim_bi = c(4, 52),
  pmca = c(14, 52),
  area = c(13, 48),
  housing = c(12, 48),
  pmcv = c(14, 52),
  pmc_comb = c(14, 52),
  pmc_super = c(14, 52),
  tecidos = c(14, 52),
  nuci = c(2, 28),
  ibc_br = c(26, 56),
  caged_a12 = c(26, 54),
  ABPO = c(3, 28),
  fenabrave = c(25, 28),
  anp = c(16, 28),
  ibov = c(6, 87),
  anfavea = c(6, 31),
  bp = c(4, 28),
  idp = c(4, 28),
  expinf = c(5, 27),
  primario = c(28, 27),
  credito = c(26, 27),
  ettj = c(19, 27),
  cambio = c(5, 25),
  cdi = c(19, 54),
  m2 = c(4, 52),
  imports = c(4, 28),
  Energia = c(8, 145),
  icea = c(6, 63),
  iec = c(6, 62),
  exports = c(9, 28),
  iof = c(17, 32),
  nasdaq = c(4, 22),
  dow_jones = c(4, 22)
)

# Ensure the date column is in Date format
df$date <- as.Date(df$date)

# Create a complete sequence of daily dates
all_dates <- data.frame(date = seq(min(df$date, na.rm = TRUE), max(df$date, na.rm = TRUE), by = "day"))

# Function to apply delays to a series
apply_delays <- function(series_name, df, delays) {
  delay_info <- delays[[series_name]]
  release_day <- delay_info[1]
  delay_days <- delay_info[2]
  
  df_series <- df %>%
    filter(!is.na(!!sym(series_name))) %>%
    mutate(date = as.Date(date) + delay_days) %>%
    mutate(date = floor_date(date, unit = "month") + days(release_day - 1)) %>%
    distinct(date, .keep_all = TRUE)  # Ensure each date is unique
  
  return(df_series)
}

# Apply delays to each series and merge into one dataframe
df_adjusted <- all_dates

for (series_name in names(delays)) {
  df_series <- apply_delays(series_name, df, delays)
  df_adjusted <- df_adjusted %>%
    left_join(df_series %>% select(date, !!sym(series_name)), by = "date")
}

# Fill initial missing values with the first non-NA value from the original df, but only the first sequence of NAs
fill_first_na_sequence <- function(series_name, df_adjusted, df_original) {
  first_value <- df_original %>% filter(!is.na(!!sym(series_name))) %>% slice(1) %>% pull(!!sym(series_name))
  first_na_index <- which(is.na(df_adjusted[[series_name]]))[1]
  if (!is.na(first_na_index)) {
    na_run_length <- rle(is.na(df_adjusted[[series_name]]))
    run_lengths <- na_run_length$lengths
    run_values <- na_run_length$values
    first_na_sequence_length <- run_lengths[which(run_values == TRUE)[1]]
    df_adjusted[[series_name]][first_na_index:(first_na_index + first_na_sequence_length - 1)] <- first_value
  }
  return(df_adjusted)
}

for (series_name in names(delays)) {
  df_adjusted <- fill_first_na_sequence(series_name, df_adjusted, df)
}

# Fill remaining missing values downwards
df_adjusted <- df_adjusted %>%
  fill(everything(), .direction = "down")


# Save the dataframe as an R object
save(df_adjusted, file = "df_adjusted.RData")


# daily prtdb to monthly prtdb
final_prtdb <- function(year, month, day) {
  
  # Load necessary libraries
  library(dplyr)
  library(lubridate)
  
  # Load the dataframe
  load("df_adjusted.RData")
  View(df_adjusted)
  # Construct the date string and filter the dataframe
  target_date <- as.Date(paste(year, month, day, sep = "-"))
  df_modified <- df_adjusted %>%
    filter(date <= target_date)
  
  # Find the end of the current quarter
  end_of_quarter <- ceiling_date(target_date, "quarter") - days(1)
  extended_dates <- seq.Date(from = target_date + days(1), to = end_of_quarter, by = "day")
  
  # Define the release dates for each series
  delays <- list(
    pib = c(1, 90),
    expec = c(1, 7),
    pim = c(4, 52),
    pim_bk = c(4, 52),
    pim_bi = c(4, 52),
    pmca = c(14, 52),
    area = c(13, 48),
    housing = c(12, 48),
    pmcv = c(14, 52),
    pmc_comb = c(14, 52),
    pmc_super = c(14, 52),
    tecidos = c(14, 52),
    nuci = c(2, 28),
    ibc_br = c(26, 56),
    caged_a12 = c(26, 54),
    ABPO = c(3, 28),
    fenabrave = c(25, 28),
    anp = c(16, 28),
    ibov = c(6, 87),
    anfavea = c(6, 31),
    bp = c(4, 28),
    idp = c(4, 28),
    expinf = c(5, 27),
    primario = c(28, 27),
    credito = c(26, 27),
    ettj = c(19, 27),
    cambio = c(5, 25),
    cdi = c(19, 54),
    m2 = c(4, 52),
    imports = c(4, 28),
    Energia = c(8, 145),
    icea = c(6, 63),
    iec = c(6, 62),
    exports = c(9, 28),
    iof = c(17, 32),
    nasdaq = c(4, 22),
    dow_jones = c(4, 22)
  )
  
  # Create a dataframe with the extended dates
  extended_df <- data.frame(date = extended_dates)
  
  for (series_name in names(df_modified)[-1]) {
    if (series_name %in% names(delays)) {
      delay_info <- delays[[series_name]]
      release_day <- delay_info[1]
      delay_days <- delay_info[2]
      
      # Calculate the release date for the last month in the dataframe
      last_month <- floor_date(target_date, unit = "month")
      last_release_date <- last_month + days(release_day - 1)
      
      # Determine if the input date is before or after the release date
      if (target_date < last_release_date) {
        # Fill with NA from the start of the month until the end of the quarter
        series_data <- df_modified %>%
          filter(date < last_month) %>%
          slice_tail(n = 1) %>%
          pull(!!sym(series_name))
        
        extended_series <- c(rep(NA, length(extended_dates)))
        
        # Insert NA from the start of the month
        df_modified[df_modified$date >= last_month, series_name] <- NA
        extended_df[[series_name]] <- extended_series
      } else {
        # Repeat the last available value until the end of the current month
        series_data <- df_modified %>%
          filter(!is.na(!!sym(series_name))) %>%
          slice_tail(n = 1) %>%
          pull(!!sym(series_name))
        
        extended_series <- if (month(target_date) == month(end_of_quarter)) {
          rep(series_data, length(extended_dates))
        } else {
          # Repeat the value only for the days in the current month
          next_month_start <- ceiling_date(target_date, "month")
          days_in_current_month <- seq.Date(from = target_date + days(1), to = next_month_start - days(1), by = "day")
          c(rep(series_data, length(days_in_current_month)), rep(NA, length(extended_dates) - length(days_in_current_month)))
        }
        
        extended_df[[series_name]] <- extended_series
      }
    } else {
      extended_df[[series_name]] <- NA
    }
  }
  
  # Bind the extended dataframe to the filtered dataframe
  df_extended <- bind_rows(df_modified, extended_df)
  
  return(df_extended)
}
daily_to_monthly <- function(daily_df) {
  monthly_df <- daily_df %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(across(where(is.numeric), ~ ifelse(all(is.na(.)), NA, last(na.omit(.))))) %>%
    ungroup() %>%
    rename(date = month)
  monthly_df[nrow(monthly_df),2] <- NA
  monthly_df[nrow(monthly_df)-1,2] <- NA
  monthly_df[nrow(monthly_df)-2,2] <- NA
  return(monthly_df)
}

# usage
daily <- final_prtdb(2024, 2, 28)
monthly <- daily_to_monthly(daily)

# ASSEMBLE VINTAGES

# Ensure to run and define the final_prtdb and daily_to_monthly functions before this

# Define the function to generate a list of monthly dataframes for each date
generate_vintages <- function(start_year, start_month, end_year, end_month, day_list) {
  vintage_list <- list()
  
  for (year in start_year:end_year) {
    for (month in 1:12) {
      if (year == start_year && month < start_month) next
      if (year == end_year && month > end_month) break
      
      for (day in day_list) {
        # Ensure the day is valid for the given month and year
        if (day <= days_in_month(ymd(paste(year, month, 1, sep = "-")))) {
          date <- as.Date(paste(year, month, day, sep = "-"))
          vintage_name <- paste0("vintage_", format(date, "%Y_%m_%d"))
          vintage_list[[vintage_name]] <- tryCatch(
            daily_to_monthly(final_prtdb(year, month, day)),
            error = function(e) {
              message(paste("Error processing date:", date, "-", e))
              return(NULL)
            }
          )
        }
      }
    }
  }
  
  # Remove any NULL entries caused by errors
  vintage_list <- Filter(Negate(is.null), vintage_list)
  
  return(vintage_list)
}

# Define the parameters
start_year <- 2020
start_month <- 1
end_year <- 2024
end_month <- 3
day_list <- c(15, 28, 30)  # Include both 28 and 30 to cover month-end cases

# Generate the vintage list
vintages <- generate_vintages(start_year, start_month, end_year, end_month, day_list)

save(vintages, file = "vintages_list.RData")




# declare balancing function
balance_panel <- function(df, method = c("INTERPOLATION", "NN")) {
  # FUNCTIONS
  upperbalance <- function(df) {
    df <- df[, colSums(is.na(df)) < nrow(df)]
    first_valid_indices <- apply(df, 2, function(col) which(!is.na(col))[1])
    latest_index <- max(first_valid_indices, na.rm = TRUE)
    df_cut <- df[latest_index:nrow(df), ]
    return(df_cut)
  }
  balanced <- function(df){
    df %>% 
      mutate_all(function(col) {
        if(all(is.na(col))) {
          return(col)
        } else if(any(is.na(col))) {
          col <- na.approx(col, na.rm = FALSE) # interpola as séries
          fit <- auto.arima(na.omit(col)) # omite NA's antes de fitar
          col[is.na(col)] <- forecast(fit, h = sum(is.na(col)))$mean
        }
        return(col)
      })
  }
  balanced.NN <- function(df) {
    library(tsfgrnn)
    library(zoo)
    library(dplyr)
    
    # Ensure the 'date' column is in Date format
    df$date <- as.Date(df$date)
    
    # Function to process each column
    process_column <- function(col, col_name) {
      if (all(is.na(col))) {
        message("Column '", col_name, "' has all NA values.")
        return(col)
      } else if (any(is.na(col))) {
        # Ensure the column is a time series object
        if (!is.ts(col)) {
          # Try to infer start and frequency if not already a ts object
          start_date <- as.Date(df$date[!is.na(df$date)][1])
          start_year <- as.numeric(format(start_date, "%Y"))
          start_month <- as.numeric(format(start_date, "%m"))
          col <- ts(col, start = c(start_year, start_month), frequency = 12)
        }
        col <- na.approx(col, na.rm = FALSE) # Interpolate missing values
        
        # Check if there is enough data for forecasting
        if (length(na.omit(col)) < 2) {
          message("Not enough data to forecast for column: ", col_name)
          return(col) # Not enough data to forecast
        }
        
        fit <- tryCatch({
          tsfgrnn::grnn_forecasting(na.omit(col), h = sum(is.na(col)))
        }, error = function(e) {
          message("Forecasting failed for column: ", col_name, " with error: ", e$message)
          return(NA) # Return NA if forecasting fails
        })
        
        if (length(fit$prediction) == sum(is.na(col))) {
          col[is.na(col)] <- fit$prediction
        } else {
          # Fallback to linear interpolation if forecasting fails
          col <- na.approx(col, na.rm = FALSE)
        }
      }
      return(col)
    }
    
    # Apply the process_column function to each column
    df_result <- df %>%
      mutate(across(-date, ~process_column(., cur_column())))
    
    return(df_result)
  }
  method <- match.arg(method)
  
  # Apply upperbalance to balance the header
  df_balanced_header <- upperbalance(df)
  
  # Balance the panel based on the chosen method
  if (method == "INTERPOLATION") {
    df_balanced <- as_tibble(balanced(df_balanced_header))
  } else if (method == "NN") {
    df_balanced <- as_tibble(balanced.NN(df_balanced_header))
  }
  
  return(df_balanced)
}

# load vintages file
load("vintages_list.RData")

# Q1 2020
# 2020_01_15
v1 = vintages$vintage_2020_01_15
v1.balanced = balance_panel(v1, method = c("INTERPOLATION"))
# 2020_01_28
v2 = vintages$vintage_2020_01_28
v2.balanced = balance_panel(v2, method = c("INTERPOLATION"))
# 2020_01_30
v3 = vintages$vintage_2020_01_30
v3.balanced = balance_panel(v3, method = c("INTERPOLATION"))
# 2020_02_15
v4 = vintages$vintage_2020_02_15
v4.balanced = balance_panel(v4, method = c("INTERPOLATION"))
# 2020_02_28
v5 = vintages$vintage_2020_02_28
v5.balanced = balance_panel(v5, method = c("INTERPOLATION"))
# 2020_03_15
v6 = vintages$vintage_2020_03_15
v6.balanced = balance_panel(v6, method = c("INTERPOLATION"))
# 2020_03_28
v7 = vintages$vintage_2020_03_28
v7.balanced = balance_panel(v7, method = c("INTERPOLATION"))
# 2020_03_30
v8 = vintages$vintage_2020_03_30
v8.balanced = balance_panel(v8, method = c("INTERPOLATION"))

# Q2 2020
# 2020_04_15
v9 = vintages$vintage_2020_04_15
v9.balanced = balance_panel(v9, method = c("INTERPOLATION"))
# 2020_04_28
v10 = vintages$vintage_2020_04_28
v10.balanced = balance_panel(v10, method = c("INTERPOLATION"))
# 2020_04_30
v11 = vintages$vintage_2020_04_30
v11.balanced = balance_panel(v11, method = c("INTERPOLATION"))
# 2020_05_15
v12 = vintages$vintage_2020_05_15
v12.balanced = balance_panel(v12, method = c("INTERPOLATION"))
# 2020_05_28
v13 = vintages$vintage_2020_05_28
v13.balanced = balance_panel(v13, method = c("INTERPOLATION"))
# 2020_05_30
v14 = vintages$vintage_2020_05_30
v14.balanced = balance_panel(v14, method = c("INTERPOLATION"))
# 2020_06_15
v15 = vintages$vintage_2020_06_15
v15.balanced = balance_panel(v15, method = c("INTERPOLATION"))
# 2020_06_28
v16 = vintages$vintage_2020_06_28
v16.balanced = balance_panel(v16, method = c("INTERPOLATION"))
# 2020_06_30
v17 = vintages$vintage_2020_06_30
v17.balanced = balance_panel(v17, method = c("INTERPOLATION"))

# Q3 2020
# 2020_07_15
v18 = vintages$vintage_2020_07_15
v18.balanced = balance_panel(v18, method = c("INTERPOLATION"))
# 2020_07_28
v19 = vintages$vintage_2020_07_28
v19.balanced = balance_panel(v19, method = c("INTERPOLATION"))
# 2020_07_30
v20 = vintages$vintage_2020_07_30
v20.balanced = balance_panel(v20, method = c("INTERPOLATION"))
# 2020_08_15
v21 = vintages$vintage_2020_08_15
v21.balanced = balance_panel(v21, method = c("INTERPOLATION"))
# 2020_08_28
v22 = vintages$vintage_2020_08_28
v22.balanced = balance_panel(v22, method = c("INTERPOLATION"))
# 2020_08_30
v23 = vintages$vintage_2020_08_30
v23.balanced = balance_panel(v23, method = c("INTERPOLATION"))
# 2020_09_15
v24 = vintages$vintage_2020_09_15
v24.balanced = balance_panel(v24, method = c("INTERPOLATION"))
# 2020_09_28
v25 = vintages$vintage_2020_09_28
v25.balanced = balance_panel(v25, method = c("INTERPOLATION"))
# 2020_09_30
v26 = vintages$vintage_2020_09_30
v26.balanced = balance_panel(v26, method = c("INTERPOLATION"))

# Q4 2020
# 2020_10_15
v27 = vintages$vintage_2020_10_15
v27.balanced = balance_panel(v27, method = c("INTERPOLATION"))
# 2020_10_28
v28 = vintages$vintage_2020_10_28
v28.balanced = balance_panel(v28, method = c("INTERPOLATION"))
# 2020_10_30
v29 = vintages$vintage_2020_10_30
v29.balanced = balance_panel(v29, method = c("INTERPOLATION"))
# 2020_11_15
v30 = vintages$vintage_2020_11_15
v30.balanced = balance_panel(v30, method = c("INTERPOLATION"))
# 2020_11_28
v31 = vintages$vintage_2020_11_28
v31.balanced = balance_panel(v31, method = c("INTERPOLATION"))
# 2020_11_30
v32 = vintages$vintage_2020_11_30
v32.balanced = balance_panel(v32, method = c("INTERPOLATION"))
# 2020_12_15
v33 = vintages$vintage_2020_12_15
v33.balanced = balance_panel(v33, method = c("INTERPOLATION"))
# 2020_12_28
v34 = vintages$vintage_2020_12_28
v34.balanced = balance_panel(v34, method = c("INTERPOLATION"))
# 2020_12_30
v35 = vintages$vintage_2020_12_30
v35.balanced = balance_panel(v35, method = c("INTERPOLATION"))

# Q1 2021
# 2021_01_15
v36 = vintages$vintage_2021_01_15
v36.balanced = balance_panel(v36, method = c("INTERPOLATION"))
# 2021_01_28
v37 = vintages$vintage_2021_01_28
v37.balanced = balance_panel(v37, method = c("INTERPOLATION"))
# 2021_01_30
v38 = vintages$vintage_2021_01_30
v38.balanced = balance_panel(v38, method = c("INTERPOLATION"))
# 2021_02_15
v39 = vintages$vintage_2021_02_15
v39.balanced = balance_panel(v39, method = c("INTERPOLATION"))
# 2021_02_28
v40 = vintages$vintage_2021_02_28
v40.balanced = balance_panel(v40, method = c("INTERPOLATION"))
# 2021_03_15
v41 = vintages$vintage_2021_03_15
v41.balanced = balance_panel(v41, method = c("INTERPOLATION"))
# 2021_03_28
v42 = vintages$vintage_2021_03_28
v42.balanced = balance_panel(v42, method = c("INTERPOLATION"))
# 2021_03_30
v43 = vintages$vintage_2021_03_30
v43.balanced = balance_panel(v43, method = c("INTERPOLATION"))

# Q2 2021
# 2021_04_15
v44 = vintages$vintage_2021_04_15
v44.balanced = balance_panel(v44, method = c("INTERPOLATION"))
# 2021_04_28
v45 = vintages$vintage_2021_04_28
v45.balanced = balance_panel(v45, method = c("INTERPOLATION"))
# 2021_04_30
v46 = vintages$vintage_2021_04_30
v46.balanced = balance_panel(v46, method = c("INTERPOLATION"))
# 2021_05_15
v47 = vintages$vintage_2021_05_15
v47.balanced = balance_panel(v47, method = c("INTERPOLATION"))
# 2021_05_28
v48 = vintages$vintage_2021_05_28
v48.balanced = balance_panel(v48, method = c("INTERPOLATION"))
# 2021_05_30
v49 = vintages$vintage_2021_05_30
v49.balanced = balance_panel(v49, method = c("INTERPOLATION"))
# 2021_06_15
v50 = vintages$vintage_2021_06_15
v50.balanced = balance_panel(v50, method = c("INTERPOLATION"))
# 2021_06_28
v51 = vintages$vintage_2021_06_28
v51.balanced = balance_panel(v51, method = c("INTERPOLATION"))
# 2021_06_30
v52 = vintages$vintage_2021_06_30
v52.balanced = balance_panel(v52, method = c("INTERPOLATION"))

# Q3 2021
# 2021_07_15
v53 = vintages$vintage_2021_07_15
v53.balanced = balance_panel(v53, method = c("INTERPOLATION"))
# 2021_07_28
v54 = vintages$vintage_2021_07_28
v54.balanced = balance_panel(v54, method = c("INTERPOLATION"))
# 2021_07_30
v55 = vintages$vintage_2021_07_30
v55.balanced = balance_panel(v55, method = c("INTERPOLATION"))
# 2021_08_15
v56 = vintages$vintage_2021_08_15
v56.balanced = balance_panel(v56, method = c("INTERPOLATION"))
# 2021_08_28
v57 = vintages$vintage_2021_08_28
v57.balanced = balance_panel(v57, method = c("INTERPOLATION"))
# 2021_08_30
v58 = vintages$vintage_2021_08_30
v58.balanced = balance_panel(v58, method = c("INTERPOLATION"))
# 2021_09_15
v59 = vintages$vintage_2021_09_15
v59.balanced = balance_panel(v59, method = c("INTERPOLATION"))
# 2021_09_28
v60 = vintages$vintage_2021_09_28
v60.balanced = balance_panel(v60, method = c("INTERPOLATION"))
# 2021_09_30
v61 = vintages$vintage_2021_09_30
v61.balanced = balance_panel(v61, method = c("INTERPOLATION"))

# Q4 2021
# 2021_10_15
v62 = vintages$vintage_2021_10_15
v62.balanced = balance_panel(v62, method = c("INTERPOLATION"))
# 2021_10_28
v63 = vintages$vintage_2021_10_28
v63.balanced = balance_panel(v63, method = c("INTERPOLATION"))
# 2021_10_30
v64 = vintages$vintage_2021_10_30
v64.balanced = balance_panel(v64, method = c("INTERPOLATION"))
# 2021_11_15
v65 = vintages$vintage_2021_11_15
v65.balanced = balance_panel(v65, method = c("INTERPOLATION"))
# 2021_11_28
v66 = vintages$vintage_2021_11_28
v66.balanced = balance_panel(v66, method = c("INTERPOLATION"))
# 2021_11_30
v67 = vintages$vintage_2021_11_30
v67.balanced = balance_panel(v67, method = c("INTERPOLATION"))
# 2021_12_15
v68 = vintages$vintage_2021_12_15
v68.balanced = balance_panel(v68, method = c("INTERPOLATION"))
# 2021_12_28
v69 = vintages$vintage_2021_12_28
v69.balanced = balance_panel(v69, method = c("INTERPOLATION"))
# 2021_12_30
v70 = vintages$vintage_2021_12_30
v70.balanced = balance_panel(v70, method = c("INTERPOLATION"))

# Q1 2022
# 2022_01_15
v71 = vintages$vintage_2022_01_15
v71.balanced = balance_panel(v71, method = c("INTERPOLATION"))
# 2022_01_28
v72 = vintages$vintage_2022_01_28
v72.balanced = balance_panel(v72, method = c("INTERPOLATION"))
# 2022_01_30
v73 = vintages$vintage_2022_01_30
v73.balanced = balance_panel(v73, method = c("INTERPOLATION"))
# 2022_02_15
v74 = vintages$vintage_2022_02_15
v74.balanced = balance_panel(v74, method = c("INTERPOLATION"))
# 2022_02_28
v75 = vintages$vintage_2022_02_28
v75.balanced = balance_panel(v75, method = c("INTERPOLATION"))
# 2022_03_15
v76 = vintages$vintage_2022_03_15
v76.balanced = balance_panel(v76, method = c("INTERPOLATION"))
# 2022_03_28
v77 = vintages$vintage_2022_03_28
v77.balanced = balance_panel(v77, method = c("INTERPOLATION"))
# 2022_03_30
v78 = vintages$vintage_2022_03_30
v78.balanced = balance_panel(v78, method = c("INTERPOLATION"))

# Q2 2022
# 2022_04_15
v79 = vintages$vintage_2022_04_15
v79.balanced = balance_panel(v79, method = c("INTERPOLATION"))
# 2022_04_28
v80 = vintages$vintage_2022_04_28
v80.balanced = balance_panel(v80, method = c("INTERPOLATION"))
# 2022_04_30
v81 = vintages$vintage_2022_04_30
v81.balanced = balance_panel(v81, method = c("INTERPOLATION"))
# 2022_05_15
v82 = vintages$vintage_2022_05_15
v82.balanced = balance_panel(v82, method = c("INTERPOLATION"))
# 2022_05_28
v83 = vintages$vintage_2022_05_28
v83.balanced = balance_panel(v83, method = c("INTERPOLATION"))
# 2022_05_30
v84 = vintages$vintage_2022_05_30
v84.balanced = balance_panel(v84, method = c("INTERPOLATION"))
# 2022_06_15
v85 = vintages$vintage_2022_06_15
v85.balanced = balance_panel(v85, method = c("INTERPOLATION"))
# 2022_06_28
v86 = vintages$vintage_2022_06_28
v86.balanced = balance_panel(v86, method = c("INTERPOLATION"))
# 2022_06_30
v87 = vintages$vintage_2022_06_30
v87.balanced = balance_panel(v87, method = c("INTERPOLATION"))

# Q3 2022
# 2022_07_15
v88 = vintages$vintage_2022_07_15
v88.balanced = balance_panel(v88, method = c("INTERPOLATION"))
# 2022_07_28
v89 = vintages$vintage_2022_07_28
v89.balanced = balance_panel(v89, method = c("INTERPOLATION"))
# 2022_07_30
v90 = vintages$vintage_2022_07_30
v90.balanced = balance_panel(v90, method = c("INTERPOLATION"))
# 2022_08_15
v91 = vintages$vintage_2022_08_15
v91.balanced = balance_panel(v91, method = c("INTERPOLATION"))
# 2022_08_28
v92 = vintages$vintage_2022_08_28
v92.balanced = balance_panel(v92, method = c("INTERPOLATION"))
# 2022_08_30
v93 = vintages$vintage_2022_08_30
v93.balanced = balance_panel(v93, method = c("INTERPOLATION"))
# 2022_09_15
v94 = vintages$vintage_2022_09_15
v94.balanced = balance_panel(v94, method = c("INTERPOLATION"))
# 2022_09_28
v95 = vintages$vintage_2022_09_28
v95.balanced = balance_panel(v95, method = c("INTERPOLATION"))
# 2022_09_30
v96 = vintages$vintage_2022_09_30
v96.balanced = balance_panel(v96, method = c("INTERPOLATION"))

# Q4 2022
# 2022_10_15
v97 = vintages$vintage_2022_10_15
v97.balanced = balance_panel(v97, method = c("INTERPOLATION"))
# 2022_10_28
v98 = vintages$vintage_2022_10_28
v98.balanced = balance_panel(v98, method = c("INTERPOLATION"))
# 2022_10_30
v99 = vintages$vintage_2022_10_30
v99.balanced = balance_panel(v99, method = c("INTERPOLATION"))
# 2022_11_15
v100 = vintages$vintage_2022_11_15
v100.balanced = balance_panel(v100, method = c("INTERPOLATION"))
# 2022_11_28
v101 = vintages$vintage_2022_11_28
v101.balanced = balance_panel(v101, method = c("INTERPOLATION"))
# 2022_11_30
v102 = vintages$vintage_2022_11_30
v102.balanced = balance_panel(v102, method = c("INTERPOLATION"))
# 2022_12_15
v103 = vintages$vintage_2022_12_15
v103.balanced = balance_panel(v103, method = c("INTERPOLATION"))
# 2022_12_28
v104 = vintages$vintage_2022_12_28
v104.balanced = balance_panel(v104, method = c("INTERPOLATION"))
# 2022_12_30
v105 = vintages$vintage_2022_12_30
v105.balanced = balance_panel(v105, method = c("INTERPOLATION"))

# Q1 2023
# 2023_01_15
v106 = vintages$vintage_2023_01_15
v106.balanced = balance_panel(v106, method = c("INTERPOLATION"))
# 2023_01_28
v107 = vintages$vintage_2023_01_28
v107.balanced = balance_panel(v107, method = c("INTERPOLATION"))
# 2023_01_30
v108 = vintages$vintage_2023_01_30
v108.balanced = balance_panel(v108, method = c("INTERPOLATION"))
# 2023_02_15
v109 = vintages$vintage_2023_02_15
v109.balanced = balance_panel(v109, method = c("INTERPOLATION"))
# 2023_02_28
v110 = vintages$vintage_2023_02_28
v110.balanced = balance_panel(v110, method = c("INTERPOLATION"))
# 2023_03_15
v111 = vintages$vintage_2023_03_15
v111.balanced = balance_panel(v111, method = c("INTERPOLATION"))
# 2023_03_28
v112 = vintages$vintage_2023_03_28
v112.balanced = balance_panel(v112, method = c("INTERPOLATION"))
# 2023_03_30
v113 = vintages$vintage_2023_03_30
v113.balanced = balance_panel(v113, method = c("INTERPOLATION"))

# Q2 2023
# 2023_04_15
v114 = vintages$vintage_2023_04_15
v114.balanced = balance_panel(v114, method = c("INTERPOLATION"))
# 2023_04_28
v115 = vintages$vintage_2023_04_28
v115.balanced = balance_panel(v115, method = c("INTERPOLATION"))
# 2023_04_30
v116 = vintages$vintage_2023_04_30
v116.balanced = balance_panel(v116, method = c("INTERPOLATION"))
# 2023_05_15
v117 = vintages$vintage_2023_05_15
v117.balanced = balance_panel(v117, method = c("INTERPOLATION"))
# 2023_05_28
v118 = vintages$vintage_2023_05_28
v118.balanced = balance_panel(v118, method = c("INTERPOLATION"))
# 2023_05_30
v119 = vintages$vintage_2023_05_30
v119.balanced = balance_panel(v119, method = c("INTERPOLATION"))
# 2023_06_15
v120 = vintages$vintage_2023_06_15
v120.balanced = balance_panel(v120, method = c("INTERPOLATION"))
# 2023_06_28
v121 = vintages$vintage_2023_06_28
v121.balanced = balance_panel(v121, method = c("INTERPOLATION"))
# 2023_06_30
v122 = vintages$vintage_2023_06_30
v122.balanced = balance_panel(v122, method = c("INTERPOLATION"))

# Q3 2023
# 2023_07_15
v123 = vintages$vintage_2023_07_15
v123.balanced = balance_panel(v123, method = c("INTERPOLATION"))
# 2023_07_28
v124 = vintages$vintage_2023_07_28
v124.balanced = balance_panel(v124, method = c("INTERPOLATION"))
# 2023_07_30
v125 = vintages$vintage_2023_07_30
v125.balanced = balance_panel(v125, method = c("INTERPOLATION"))
# 2023_08_15
v126 = vintages$vintage_2023_08_15
v126.balanced = balance_panel(v126, method = c("INTERPOLATION"))
# 2023_08_28
v127 = vintages$vintage_2023_08_28
v127.balanced = balance_panel(v127, method = c("INTERPOLATION"))
# 2023_08_30
v128 = vintages$vintage_2023_08_30
v128.balanced = balance_panel(v128, method = c("INTERPOLATION"))
# 2023_09_15
v129 = vintages$vintage_2023_09_15
v129.balanced = balance_panel(v129, method = c("INTERPOLATION"))
# 2023_09_28
v130 = vintages$vintage_2023_09_28
v130.balanced = balance_panel(v130, method = c("INTERPOLATION"))
# 2023_09_30
v131 = vintages$vintage_2023_09_30
v131.balanced = balance_panel(v131, method = c("INTERPOLATION"))

# Q4 2023
# 2023_10_15
v132 = vintages$vintage_2023_10_15
v132.balanced = balance_panel(v132, method = c("INTERPOLATION"))
# 2023_10_28
v133 = vintages$vintage_2023_10_28
v133.balanced = balance_panel(v133, method = c("INTERPOLATION"))
# 2023_10_30
v134 = vintages$vintage_2023_10_30
v134.balanced = balance_panel(v134, method = c("INTERPOLATION"))
# 2023_11_15
v135 = vintages$vintage_2023_11_15
v135.balanced = balance_panel(v135, method = c("INTERPOLATION"))
# 2023_11_28
v136 = vintages$vintage_2023_11_28
v136.balanced = balance_panel(v136, method = c("INTERPOLATION"))
# 2023_11_30
v137 = vintages$vintage_2023_11_30
v137.balanced = balance_panel(v137, method = c("INTERPOLATION"))
# 2023_12_15
v138 = vintages$vintage_2023_12_15
v138.balanced = balance_panel(v138, method = c("INTERPOLATION"))
# 2023_12_28
v139 = vintages$vintage_2023_12_28
v139.balanced = balance_panel(v139, method = c("INTERPOLATION"))
# 2023_12_30
v140 = vintages$vintage_2023_12_30
v140.balanced = balance_panel(v140, method = c("INTERPOLATION"))

# Q1 2024
# 2024_01_15
v141 = vintages$vintage_2024_01_15
v141.balanced = balance_panel(v141, method = c("INTERPOLATION"))
# 2024_01_28
v142 = vintages$vintage_2024_01_28
v142.balanced = balance_panel(v142, method = c("INTERPOLATION"))
# 2024_01_30
v143 = vintages$vintage_2024_01_30
v143.balanced = balance_panel(v143, method = c("INTERPOLATION"))
# 2024_02_15
v144 = vintages$vintage_2024_02_15
v144.balanced = balance_panel(v144, method = c("INTERPOLATION"))
# 2024_02_28
v145 = vintages$vintage_2024_02_28
v145.balanced = balance_panel(v145, method = c("INTERPOLATION"))
# 2024_03_15
v146 = vintages$vintage_2024_03_15
v146.balanced = balance_panel(v146, method = c("INTERPOLATION"))
# 2024_03_28
v147 = vintages$vintage_2024_03_28
v147.balanced = balance_panel(v147, method = c("INTERPOLATION"))
# 2024_03_30
v148 = vintages$vintage_2024_03_30
v148.balanced = balance_panel(v148, method = c("INTERPOLATION"))
