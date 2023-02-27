get_and_clean_weekly_data <- function(weekly_data_file, 
                                      weekly_data_files_extended,
                                      weekly_data_files_etf,
                                      nstep_ahead = 1, 
                                      test_index, simulate = FALSE){
  
  dt <- qs::qread(weekly_data_file)
  dt[, index := as.Date(index)]
  dt[, m6univ := 1]
  setorder(dt, symbol, index)
  
  dt_big <- qs::qread(weekly_data_files_extended)[,class := "Stock"]
  dt_big[, index := as.Date(index)]
  dt_big <- dt_big[index <= min(test_index)-10]
  
  dt_etf <- qs::qread(weekly_data_files_etf)[,class := "ETF"]
  dt_etf[, index := as.Date(index)]
  dt_etf <- dt_etf[index <= min(test_index)-10]
  
  dt <- rbindlist(list(dt, dt_big, dt_etf), fill = TRUE)
  dt[is.na(m6univ), m6univ := 0]
  
  # read symbol information
  m6univ <- fread("~/MEGA/M6_data/M6/01_first_quarter/01_raw_data1/M6_Universe_META.csv")
  dt[m6univ, on = 'symbol', ':=' (class = i.class)]
  
  # synchronize weekly index for all symbols
  dt[, isoweek := ISOweek(index)]
  
  # - only keep max index per symbol and isoweek
  dt[, maxindex := max(index), by = .(symbol, isoweek)]
  dt <- dt[index == maxindex][, maxindex := NULL]
  
  # - synchronize index for all symbols
  dt <- dt[dt[, .(max_index = max(index)), by = .(isoweek)], on = c("isoweek")]
  dt[, index := max_index]
  dt[, c("isoweek", "max_index") := NULL]
  
  # require at least 10 values per index
  dt <- dt[dt[,.N, index][N > 10], on = "index"][,N := NULL]
  
  # inflate weekly data
  grid <- expand.grid(index = dt[, .N, index]$index, symbol = dt[, .N, symbol]$symbol, stringsAsFactors = FALSE)
  setDT(grid)
  dt <- dt[grid, on = c("index", "symbol")]
  
  # trim all NAs before the first price
  setorder(dt, symbol, index)
  dt[, nacs := cumsum(!is.na(adjusted)), by = symbol]
  dt <- dt[nacs > 0][,nacs := NULL]
  
  # set volume for days with is.na(adjusted) to 0 
  dt[is.na(adjusted), volume := 0]
  
  # impute prices for days where no entries are available
  adjusted_nas <- dt[, any(is.na(adjusted))]
  iter <- 0
  while(adjusted_nas){
    iter <- iter +1
    #print(iter)
    dt[, adj_shift := shift(adjusted, n = 1L), by = symbol]
    dt[is.na(adjusted), adjusted := adj_shift]
    adjusted_nas <- dt[, any(is.na(adjusted))]
  }
  dt[, adj_shift := NULL]
  
  # drop nas
  dt <- na.omit(dt)
  
  # remove duplicate entries
  dt <- unique(dt)
  
  # remove duplicate symbol index-combinations
  dt[, i := 1:.N, by = .(index, symbol)]
  if(nrow(dt[i>1])>0) stop("duplicates")
  dt[,i := NULL]
  
  # ===========================================================
  # drop observations of predictions-month (if available)
  # and add dummy month
  # ===========================================================
  if(simulate == FALSE){
    yearweek_test <- ISOweek(test_index)
    dt[, yearweek := ISOweek(index)]
    dt <- dt[yearweek < min(yearweek_test)]
    
    yearweek_dt <- data.table(yearweek = yearweek_test, k = 1)
    symbol_class_dt <- dt[,logical(1),.(symbol, class, m6univ)][,k := 1]
    dt_test <- yearweek_dt[symbol_class_dt, on = "k", allow.cartesian = TRUE][, k := NULL]

    dt_test <- unique(dt[,.(symbol, class, m6univ)])[dt_test[,!"V1"], on = .(symbol, class, m6univ)]
    dt_test <- dt_test[data.table(yearweek = yearweek_test, index = test_index), on = "yearweek"]
    dt <- rbind(dt, dt_test, fill = TRUE)
    setorder(dt, symbol, yearweek)
  }
  
  # -----------------------------
  # calculate features for each symbol
  # -----------------------------
  
  # diffs
  dt[, log_close_diffs := c(NA, diff(log(adjusted))), by = symbol]
  dt[, log_volume_diffs := c(NA, diff(log(volume+1))), by = symbol]
  
  # lagged log diffs
  lags <- c(1:6,12,18,24)
  dt[, paste0("log_close_diffs_lag", lags) := shift(log_close_diffs, lags), by = symbol]
  dt[, paste0("log_volume_diffs_lag", lags) := shift(log_volume_diffs, lags), by = symbol]
  dt[, volume_lag1 := shift(volume), by = symbol]
  
  # scale volume given rolling mean
  lags <- c(1:6,12,18,24,52)
  dt[, paste0("volume_ma", lags) := frollmean(volume_lag1, n = lags), by = symbol]
  dt[, volume_scaled4 := volume_lag1/volume_ma4]
  dt[, volume_scaled12 := volume_lag1/volume_ma12]
  dt[, volume_scaled52 := volume_lag1/volume_ma52]
  
  # rolling log_close diffs mean und variance
  cat(format(Sys.time(), "%c"), "calculate mean variance rolling means", "\n")
  lags <- c(2,4,8,12,24,52,104,156)
  dt[, paste0("m", lags) := frollmean(log_close_diffs_lag1, n = lags), by = symbol]
  dt[, paste0("s", lags):= frollapply(log_close_diffs_lag1, n = lags, FUN = sd), by = symbol]
  
  # rolling log_close diffs mean und variance für lag2
  cat(format(Sys.time(), "%c"), "calculate mean variance rolling means", "\n")
  lags <- c(2,4,8,12,24,52,104,156)
  dt[, paste0("lag2_m", lags) := frollmean(log_close_diffs_lag2, n = lags), by = symbol]
  dt[, paste0("lag2_s", lags):= frollapply(log_close_diffs_lag2, n = lags, FUN = sd), by = symbol]
  
  
  # difference of rolling log_close diffs mean und variance
  cat(format(Sys.time(), "%c"), "calculate differences for mean variance rolling means", "\n")
  lags <- c(2,4,8,12,24,52,104,156)
  for(ll in 1:(length(lags)-1)){
    for(jj in (ll+1):length(lags)){
      dt[, paste0("m", lags[ll], "_", "m", lags[jj], "_diff") := get(paste0("m", lags[ll]))-get(paste0("m", lags[jj]))]
      dt[, paste0("s", lags[ll], "_", "s", lags[jj], "_diff") := get(paste0("s", lags[ll]))-get(paste0("s", lags[jj]))]
    }
  }
  
  # -----------------------------
  # calculate INDEX FEATURES 
  # -----------------------------
  dglob <- dt[class == "Stock", .(overall_adjusted = mean(adjusted), 
                                  overall_volume = mean(volume)), by = index]
  setorder(dglob, index)
  dglob[, o_log_close_diffs := c(NA, diff(log(overall_adjusted)))]
  dglob[, o_log_volume_diffs := c(NA, diff(log(overall_volume+1)))]
  
  # lagged log diffs
  lags <- c(1:6,12,18,24)
  dglob[, paste0("o_log_close_diffs_lag", lags) := shift(o_log_close_diffs, lags)]
  dglob[, o_log_volume_diffs_lag1 := shift(o_log_volume_diffs)]
  dglob[, overall_volume_lag1 := shift(overall_volume)]
  
  # scale volume given rolling mean
  dglob[, overall_volume_ma3 := frollmean(overall_volume_lag1, n = 3)]
  dglob[, overall_volume_scaled3 := overall_volume_lag1/overall_volume_ma3]
  
  # scale volume given rolling mean
  dglob[, overall_volume_ma12 := frollmean(overall_volume_lag1, n = 12)]
  dglob[, overall_volume_scaled12 := overall_volume_lag1/overall_volume_ma12]
  
  # scale volume given rolling mean
  dglob[, overall_volume_ma52 := frollmean(overall_volume_lag1, n = 52)]
  dglob[, overall_volume_scaled52 := overall_volume_lag1/overall_volume_ma52]
  
  # rolling log_close diffs mean und variance
  cat(format(Sys.time(), "%c"), "calculate index mean variance rolling means", "\n")
  lags <- c(2,4,8,12,24,52,104,156)
  dglob[, paste0("o_m", lags) := frollmean(o_log_close_diffs_lag1, n = lags)]
  dglob[, paste0("o_s", lags):= frollapply(o_log_close_diffs_lag1, n = lags, FUN = sd)]
  
  # rolling log_close diffs mean and variance with lag2
  cat(format(Sys.time(), "%c"), "calculate index mean variance rolling means with lag2", "\n")
  lags <- c(2,4,8,12,24,52,104,156)
  dglob[, paste0("o_lag2_m", lags) := frollmean(o_log_close_diffs_lag2, n = lags)]
  dglob[, paste0("o_lag2_s", lags):= frollapply(o_log_close_diffs_lag2, n = lags, FUN = sd)]
  
  # differences of rolling log close diffs and variances with lag2
  cat(format(Sys.time(), "%c"), "calculate differences for mean variance rolling means", "\n")
  lags <- c(2,4,8,12,24,52,104,156)
  for(ll in 1:(length(lags)-1)){
    for(jj in (ll+1):length(lags)){
      dglob[, paste0("o_lag2_m", lags[ll], "_", "o_lag2_m", lags[jj], "_diff") := get(paste0("o_lag2_m", lags[ll]))-get(paste0("o_lag2_m", lags[jj]))]
      dglob[, paste0("o_lag2_s", lags[ll], "_", "o_lag2_s", lags[jj], "_diff") := get(paste0("o_lag2_s", lags[ll]))-get(paste0("o_lag2_s", lags[jj]))]
    }
  }
  
  # cobine index-features and symbol-features
  dt <- dt[dglob, on = 'index']
  
  
  # -----------------------------
  # Calculate beta with log_close_diffs_lag2, o_log_close_diffs_lag_2 and o_lag2 
  # per symbol, iterate over rolling window and calculate beta
  # -----------------------------
  
  cat(format(Sys.time(), "%c"), "calculate beta features", "\n")
  window_size <- c(12,24)
  dt[, i := 1:.N, by = symbol]
  max_i <- dt[,max(i)]
  
  for(ww in window_size){
    for(iii in (ww+1):max_i){
      if(iii %% 100 == 0) cat("window_size:", ww, iii, "/", max_i, "\n")
      dt_sub <- na.omit(dt[i <= iii & i > iii-ww, .(symbol, o_log_close_diffs_lag2, log_close_diffs_lag2)])
      dt_sub <- dt_sub[dt_sub[, .N, symbol][N == ww], on = "symbol"][,N := NULL]
      if(nrow(dt_sub) == 0) next
      dt_beta <- dt_sub[, get_beta(x = o_log_close_diffs_lag2, y = log_close_diffs_lag2), by = symbol][, i:=iii]
      dt[dt_beta, on = c("symbol", "i"), ':=' (alpha = i.alpha, beta = i.beta)]
    }
    
    setnames(dt, "beta", paste0("lag2_beta_", ww))
    setnames(dt, "alpha", paste0("lag2_alpha_", ww))
    dt[, paste0("lag2_sml_diff_", ww) := get(paste0("lag2_beta_", ww))*get(paste0("o_lag2_m", ww))-get(paste0("lag2_m",ww))]
  }
  dt[, i := NULL]
  
  
  return(dt)
}


# =========================================
# FIT PREDICT N STEPS AHEAD
# =========================================

fit_predict_nstep_ahead <- function(data = copy(dt_mod), 
                                    nstep_ahead = 1:4, 
                                    lhs_formula = "log_close_diffs", 
                                    rhs_formula = rep("log_close_diffs_lag_1", 4), 
                                    test_index = "2022-05-02", 
                                    verbose = FALSE,
                                    extrapolate = FALSE){
  
  
  if(length(nstep_ahead) != length(test_index)) stop("nstep_ahead and test_index not same length")
  if(length(nstep_ahead) != length(rhs_formula)) stop("nstep_ahead and rhs_formula not same length")
  
  # create target variale for nstep ahead
  data[, paste0(lhs_formula, nstep_ahead) := shift(get(lhs_formula), n = nstep_ahead-1, type = "lead"), by = symbol]
  data[, paste0("yearweek", nstep_ahead) := shift(yearweek, n = nstep_ahead-1, type = "lead"), by = symbol]
  
  # add yearweek
  yearweek_test <- ISOweek(test_index)
  
  res <- list()
  for(ss in seq_along(nstep_ahead)){
    
    # create formula
    form <- formula(paste0(lhs_formula, nstep_ahead[ss], " ~ ", rhs_formula[ss]))
    
    # train test split
    dtrain <- data[get(paste0("yearweek",nstep_ahead[ss])) < yearweek_test[ss]]
    dtest <- data[get(paste0("yearweek",nstep_ahead[ss])) == yearweek_test[ss]]
    if(nrow(dtest) == 0) next
    
    # treat extrapolation
    if(!extrapolate){
      cnames <- colnames(dtrain)
      cnames_bool <- logical(length(cnames))
      for(ff in seq_along(cnames)){
        cnames_bool[ff] <- grepl(cnames[ff], rhs_formula[ss])
      }
      
      train_features <- grep("yearweek|index|symbol|m6univ|class|log_close_diffs|close", colnames(dtrain)[cnames_bool], invert = TRUE, value = TRUE)
      for(tt in train_features){
        rr <- range(dtrain[,get(tt)], na.rm = TRUE)
        dtest[, (tt) := pmax(rr[1], get(tt))]
        dtest[, (tt) := pmin(rr[2], get(tt))]
      }
    }
    
    # fit model & predict
    fit <- lm(form, data = na.omit(dtrain))
    if(verbose) print(summary(fit))
    dtest[, pred :=  predict(fit, dtest)]
    res[[length(res)+1]] <- dtest[, .(yearweek = yearweek_test[ss], symbol, 
                                      log_close_diffs = get(paste0("log_close_diffs", nstep_ahead[ss])), pred,
                                      rhs_formula = rhs_formula[ss])]
    
  }
  res <- rbindlist(res)
  if(nrow(res) == 0) {
    cat("WARNING:: formula ", as.character(form), "not working; skip\n")
    return(NULL)
  }
  
  
  res <- res[data.table(yearweek = yearweek_test, index = test_index), on = "yearweek"]    
  
  res
}


# =========================================
# RESAMPLING COVAR
# =========================================

robust_var <- function(data = dwide[,!"index"], N = 1e4, alpha = .01){
  
  library(Matrix)
  library(matrixcalc)
  
  RES <- array(data = NA, dim = c(ncol(data), ncol(data), N), dimnames = list(colnames(data), colnames(data)))
  for(n in 1:N){
    samp <- sample(1:nrow(data), nrow(data), replace = TRUE)
    RES[,,n] <- var(data[samp], use = "pairwise.complete.obs")
  }
  
  lower <- apply(RES, c(1,2), function(x) quantile(x, probs = alpha/2))
  upper <- apply(RES, c(1,2), function(x) quantile(x, probs = 1-alpha/2))
  insig <- lower < 0 & upper > 0
  
  resmean <- apply(RES, c(1,2), mean)
  resmean[insig] <- 0
  
  if(!is.positive.definite(resmean)){
    print("Matrix not PD --> nearPD()-Call")
    resmean <- as.matrix(nearPD(resmean, keepDiag = TRUE, ensureSymmetry = TRUE, maxit = 1e4)$mat)
  }
  
  list(C = resmean, insig = insig)
}


# =========================================
# simple beta calculation
# =========================================
get_beta <- function(x,y){
  if(var(x,na.rm = TRUE) == 0) return(0)
  x <- cbind(1, x)
  y <- y
  beta <- solve(crossprod(x,x)) %*% crossprod(x,y)
  beta <- list(alpha = beta[1,], beta = beta[2,])
  beta
}
