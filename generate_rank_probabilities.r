# ====================================================
# LIBS + SOURCE
# ====================================================

library(data.table)
library(matrixcalc)
library(splines)
library(mvtnorm)
library(ISOweek)
library(qs)
source("R/util.r")
  
# ====================================================
# INPUT
# ====================================================
    
# forecast horizon input
test_start <- as.Date("2023-01-09")
test_end <- test_start+3*7
test_index <- seq(test_start, test_end, by = 7) 
h <- c(5,4,5,5) # days per week within forecast horizon
nstep_ahead = 1:length(test_index) 
var_train_years = 3 # training history for covariance-matrix
N_RPS = 1e4  # N-trajectories for ranked probability score


# define lm-formulas for short time series  
rhs_formulas_short <- list(
  c("(bs(o_log_close_diffs_lag4, df = 5, degree = 1) + bs(o_lag2_m8, df = 6, degree = 2) + bs(lag2_sml_diff_12, df = 5, degree = 1)) * class", "local"),
  c("(o_log_close_diffs_lag3 + o_lag2_m24 + bs(lag2_beta_24, df = 4, degree = 1)) * class", "local"), 
  c("(bs(volume_ma5, df = 5, degree = 2) + bs(o_lag2_m8, df = 5, degree = 2)) * class", "global"),
  c("bs(m8_m12_diff, df = 5, degree = 1) + bs(o_log_close_diffs_lag4, df = 6, degree = 1)", "local")
) 

# define lm-formulas for short longer series
rhs_formulas_long <- list(
  c("(log_close_diffs_lag3 + o_lag2_s52 + lag2_alpha_12 + lag2_beta_12) * class", "global"),  
  c("bs(o_lag2_s52, df = 5, degree = 1) + bs(lag2_beta_24, df = 5, degree = 1)", "local"),  
  c("(bs(o_log_close_diffs_lag4, df = 5, degree = 1) + bs(o_lag2_m8, df = 6, degree = 2) + bs(lag2_sml_diff_12, df = 5, degree = 1)) * class", "local"),
  c("(o_log_close_diffs_lag3 + o_lag2_m24 + bs(lag2_beta_24, df = 4, degree = 1)) * class", "local"), 
  c("(bs(volume_ma5, df = 5, degree = 2) + bs(o_lag2_m8, df = 5, degree = 2)) * class", "global"),
  c("(bs(s52, df = 7, degree = 1) + bs(o_lag2_s52, df = 6, degree = 1)) * class", "local"), 
  c("(bs(m104, df = 5, degree = 3) + bs(s156, df = 5, degree = 1)) * class", "local"),
  c("(bs(lag2_s104, df = 5, degree = 1) + bs(o_lag2_s24_o_lag2_s104_diff, df = 7, degree = 1)) * class", "local")
) 


# ====================================================
# Datasources
# ====================================================

weekly_data_file <- "m6_weekly_data.qs"
weekly_data_files_extended <- "stocks_weekly_data.qs"
weekly_data_files_etf <- "etf_weekly_data.qs"
daily_data_file <- "m6_daily_data.qs"


# =======================================================
# Create point predictions based on weekly data
# =======================================================
dt <- get_and_clean_weekly_data(weekly_data_file = weekly_data_file, 
                                weekly_data_files_extended = weekly_data_files_extended,
                                weekly_data_files_etf = weekly_data_files_etf,
                                nstep_ahead = nstep_ahead, 
                                test_index = test_index, 
                                simulate = FALSE)

# ------------------------------------------------------
# Fit and predict with LMs given shorter training history
# ------------------------------------------------------
tmppred <- list()
for(rr in seq_along(rhs_formulas_short)){
  
  model_data <- copy(dt)
  if(rhs_formulas_short[[rr]][2] == "local") model_data <- model_data[m6univ == 1]
  tmppred[[rr]] <- fit_predict_nstep_ahead(data = copy(model_data), 
                                           nstep_ahead = nstep_ahead, 
                                           lhs_formula = "log_close_diffs", 
                                           rhs_formula = rep(rhs_formulas_short[[rr]][1], max(nstep_ahead)), 
                                           test_index = test_index, 
                                           verbose = FALSE,
                                           extrapolate = FALSE)
}

tmppred <- rbindlist(tmppred)
m6univ <- dt[m6univ == 1, .N, symbol][,symbol]

dpred <- tmppred[symbol %in% m6univ, .(pred = mean(pred)), 
                 by = .(yearweek, symbol, log_close_diffs, index)]

dres <- copy(dpred)
dres <- dres[!is.na(pred)]

# ------------------------------------------------------
# Fit and predict with LMs given longer training history
# ------------------------------------------------------

tmppred <- list()
for(rr in seq_along(rhs_formulas_long)){
  
  model_data <- copy(dt)
  if(rhs_formulas_long[[rr]][2] == "local") model_data <- model_data[m6univ == 1]
  tmppred[[rr]] <- fit_predict_nstep_ahead(data = copy(model_data), 
                                           nstep_ahead = nstep_ahead, 
                                           lhs_formula = "log_close_diffs", 
                                           rhs_formula = rep(rhs_formulas_long[[rr]][1], max(nstep_ahead)), 
                                           test_index = test_index, 
                                           verbose = FALSE,
                                           extrapolate = FALSE)
}
tmppred <- rbindlist(tmppred)
m6univ <- dt[m6univ == 1, .N, symbol][,symbol]

dpred <- tmppred[symbol %in% m6univ, .(pred = mean(pred)), 
                 by = .(yearweek, symbol, log_close_diffs, index)]

dres2 <- copy(dpred)
dres2 <- dres2[!is.na(pred)]

# ----------------------------------------
# combine dres and dres2
# ---------------------------------------

dres[dres2[, .(symbol, yearweek, pred_complex = pred)], on = .(symbol, yearweek),
      ':=' (pred = pred_complex)]


# ============================================================
# Esimate variance covariance based on daily data
# ============================================================

dt <- qs::qread(daily_data_file)

# clean index-column
dt[, index := gsub("X", "", index)]
dt[, index := gsub("[.]", "-", index)]
dt[, index := substr(index, 1, 10)]
dt[, index := as.Date(index)]

# trimmen allen NAs vor dem ersten preis
setorder(dt, symbol, index)
dt[, nacs := cumsum(!is.na(adjusted)), by = symbol]
dt <- dt[nacs > 0]

# volume für tage mit is.na(adjusted) auf 0 setzen
dt[is.na(adjusted), volume := 0]

# preise für jene tage naive imputieren, an denen keine einträge vorhanden sind
adjusted_nas <- dt[, any(is.na(adjusted))]
iter <- 0
while(adjusted_nas){
  iter <- iter +1
  dt[, adj_shift := shift(adjusted, n = 1L), by = symbol]
  dt[is.na(adjusted), adjusted := adj_shift]
  adjusted_nas <- dt[, any(is.na(adjusted))]
}

# drop irrelevant cols
dt[, c("open", "high", "low", "close", "nacs") := NULL]

# calculate log close diffs
dt[, lcd := c(NA, diff(log(adjusted))), by = symbol]
dt <- dt[!is.na(lcd)]

# setorder
tmp <- copy(dt)
setorder(tmp, symbol, index)
tmp <- na.omit(tmp)

# remove extreme outliers
tmp <- tmp[abs(lcd) < .4]

# train test split
train <- tmp[index > test_start - round(var_train_years*365) & index < test_start, ]

# dres auf train auf das selbe symbol-Set einschränken
predict_symbol <- intersect(dres$symbol, train[, .N, symbol]$symbol)
dres <- dres[symbol %in% predict_symbol]
train <- train[symbol %in% predict_symbol]
hdays <- train[index >= test_start & index <= test_end, unique(index)]

# If test_end after sysdate-14, then calculate h based on historical realization of trading days
if(test_end < Sys.Date()-14){
  tmp[, isoweek := ISOweek(index)]
  
  h_days <- tmp[index >= test_start & index <= test_end, .N, index][order(index), index]
  h_index <- data.table(isoweek = ISOweek(h_days), h_days)
  h_pred <- h_index[,.N, isoweek][order(isoweek), N]
  
  isoweek_days <- tmp[isoweek %in% ISOweek(test_index), .N, .(isoweek, index)][, .(h = .N), isoweek]
  isoweek_days <- isoweek_days[isoweek %in% unique(h_index$isoweek)]
  setorder(isoweek_days, isoweek)
  h_scale <- isoweek_days$h
  isoweek_days <- isoweek_days$isoweek
  
} else {
  h_pred <- h_scale <- h
  isoweek_days <- ISOweek(test_index)
}

# M aufsetzen und m skalieren
pred_yearweeks <- isoweek_days
M <- list()
setorder(dres, yearweek, symbol)
for(pp in seq_along(pred_yearweeks)){
  M[[pp]] <- dres[yearweek == pred_yearweeks[pp], .(symbol, m = pred/h_scale[pp])]
}


# ===============================
# RANKED PROBABILITY SCORE
# ===============================

# -------------------------------------------------
cat(format(Sys.time(), "%c"), "BOOTSTRAP COVARIANCE CALCULATION FOR RPS\n")
# -------------------------------------------------

dwide <- dcast(train, index ~ symbol, value.var = "lcd", fun.aggregate = mean)[,!"index"]
N_boot <- N_RPS
nrows <- nrow(dwide)
MAT <- array(data = NA, dim = c(sum(h_pred), ncol(dwide), N_boot), dimnames = list(NULL, M[[1]][, symbol] ,NULL))
set.seed(12345)
for(nn in 1:N_boot){ # 0000000000000000000 BOOTSTRAP LOOP START
  
  if(nn %% 100 == 0) cat(nn, "/", N_boot, "\n")
  boot <- sample(1:nrows, nrows, replace = TRUE)
  covmat <- var(dwide[boot, ], use = "pairwise.complete.obs")
  
  if(!is.positive.definite(covmat)){
    PD_covmat <- nearPD(covmat, keepDiag = TRUE, ensureSymmetry = TRUE, maxit = 200)
    if(!PD_covmat$converged){
      cat("nearPD-call not converged - skipping iteration", nn, "\n")
      next
    }
    covmat <- as.matrix(PD_covmat$mat)
  }
  
  
  # draw dample from multivariate distribution
  SAMP <- list()
  for(m in 1:length(M)){
    
    #test order of M and C
    if(all(M[[m]]$symbol == colnames(C))){
      SAMP[[m]] <- mvtnorm::rmvt(h_pred[m] , sigma = covmat, df = 4, delta = M[[m]]$m)
    } else {
      stop("ORDER OF C AND M ARE NOT IDENTICAL")
    }
  } 
  
  
  MAT[,,nn] <- do.call(rbind, SAMP)   
  
} # 0000000000000000000 BOOTSTRAP LOOP END

# drop NA-dimensions
drop_dims <- which(apply(MAT, 3, function(x) any(is.na(x))))
if(length(drop_dims)>0){
 MAT <- MAT[,,-drop_dims]
}
print(dim(MAT))

# ----------------------------------
# Calculate RPS
# ----------------------------------
M_sum <- apply(MAT, c(2,3), sum)
samp_rank <- apply(M_sum, 2, rank)
samp_rank[samp_rank %in% 1:20] <- 1
samp_rank[samp_rank %in% 21:40] <- 2
samp_rank[samp_rank %in% 41:60] <- 3
samp_rank[samp_rank %in% 61:80] <- 4
samp_rank[samp_rank %in% 81:100] <- 5
samp_rank <- as.data.table(t(samp_rank))
samp_rank <- melt(samp_rank, measure.vars = colnames(samp_rank), variable.name = "ID")
samp_rank <- samp_rank[, .(count = .N), .(ID, value)]
samp_rank[, prob := count/dim(MAT)[3]]
samp_rank[, rank := paste0("Rank", value)]
samp_rank <- dcast(samp_rank, ID ~ rank, value.var = "prob", fill = 0)


# add DRE
samp_rank <- rbind(samp_rank, 
              data.table(ID = "DRE", Rank1 = .2, Rank2 = .2, Rank3 = .2, Rank4 = .2, Rank5 = .2))

# save result
fwrite(samp_rank, file = "rank_probabilites.csv")
