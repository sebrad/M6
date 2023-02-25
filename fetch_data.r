# libs
library(quantmod)
library(data.table)
library(qs)

# fun
fetch_yahoo_data <- function(symbols, periodicity){
  # try to fetch each symbol in symbols
  # in case of an error, skip ii and proceed with ii+1
  tmp <- list()
  for(ii in seq_along(symbols)){
    symb <- symbols[ii]
    dat <- try(getSymbols(symb, src = "yahoo", auto.assign = FALSE, periodicity = periodicity))
    if(!is.xts(dat)) next
    dat <- as.data.frame(dat)
    dat$index <- rownames(dat)
    setDT(dat)
    setnames(dat, c("open", "high", "low", "close", "volume", "adjusted", "index"))
    dat[, symbol := symb]
    tmp[[length(tmp)+1]] <- dat
  }
  
  tmp <- rbindlist(tmp)
  return(tmp)
}

# symbols
m6_symbols <- c(
  "ABBV","ACN","AEP","AIZ","ALLE","AMAT","AMP","AMZN","AVB","AVY",   
  "AXP","BDX","BF-B","BMY","BR","CARR","CDW","CE","CHTR","CNC",   
  "CNP","COP","CTAS","CZR","DG","DPZ","DRE","DXC","META","FTV",   
  "GOOG","GPC","HIG","HST","JPM","KR","OGN","PG","PPL","PRU",   
  "PYPL","RE","ROL","ROST","UNH","URI","V","VRSK","WRK","XOM",   
  "IVV","IWM","EWU","EWG","EWL","EWQ","IEUS","EWJ","EWT","MCHI",  
  "INDA","EWY","EWA","EWH","EWZ","EWC","IEMG","LQD","HYG","SHY",  
  "IEF","TLT","SEGA.L","IEAA.L","HIGH.L","JPEA.L","IAU","SLV","GSG","REET",  
  "ICLN","IXN","IGF","IUVL.L","IUMO.L","SPMV.L","IEVL.L","IEFM.L","MVEU.L","XLK",   
  "XLF","XLV","XLE","XLY","XLI","XLC","XLU","XLP","XLB","VXX") 

extended_stock_symbols <- qs::qread("data/stocks_extended.qs")
extended_etf_symbols <- qs::qread("data/etf_extended.qs")

# load m6 univ daily and weekly data
tmp <- fetch_yahoo_data(symbols = m6_symbols, periodicity = "daily")
qs::qsave(tmp, "m6_daily_data.qs")  

tmp <- fetch_yahoo_data(symbols = m6_symbols, periodicity = "weekly")
qs::qsave(tmp, "m6_weekly_data.qs")  

# load extended stock weekly data
tmp <- fetch_yahoo_data(symbols = extended_stock_symbols, periodicity = "weekly")
qs::qsave(tmp, "stocks_weekly_data.qs")  

# load extended etf data
tmp <- fetch_yahoo_data(symbols = extended_etf_symbols, periodicity = "weekly")
qs::qsave(tmp, "etf_weekly_data.qs")  




