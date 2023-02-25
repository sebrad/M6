# ====================================================
# LIBS
# ====================================================

library(data.table)
#library(MASS)
#library(matrixcalc)
library(splines)
#library(tsfeatures)
#library(Matrix)
library(ISOweek)
library(qs)

source("R/util.r")
  
# ====================================================
# CONFIG
# ====================================================

# forecast horizon input
start_prog <- as.Date("2023-01-09")
end_prog <- start_prog+3*7
test_index <- seq(start_prog, end_prog, by = 7) 
h <- c(5,4,5,5) # days per week within forecast horizon
nstep_ahead = 1:length(test_index) 

# result file
template_result_file = "~/MEGA/M6_data/M6/01_first_quarter/01_raw_data1/template_META.csv"


test_start = start_prog
test_end = end_prog
N_var = 1e4 #1e4
var_train_years = 3 # training history for covariance-matrix
N_RPS = 1e4 #1e4 # N-trajectories for ranked probability score
N_IR = 1e4 #1e4 # N-trajectories für Information rate


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
# Create point predictions based on  weekly data
# =======================================================
dt <- get_and_clean_weekly_data(weekly_data_file = weekly_data_file, 
                                weekly_data_files_extended = weekly_data_files_extended,
                                weekly_data_files_etf = weekly_data_files_etf,
                                nstep_ahead = nstep_ahead, 
                                test_index = test_index, 
                                simulate = FALSE)

# ------------------------------------------------------
# GA-Optimization based model for shorter time series
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
                                           verbose = TRUE,
                                           extrapolate = FALSE)
}

tmppred <- rbindlist(tmppred)
m6univ <- dt[m6univ == 1, .N, isin][,isin]

ggplot(tmppred[isin %in% m6univ], aes(pred, reorder(isin, pred), color = rhs_formula)) + geom_point() + facet_wrap(~index) +
  stat_summary(color = "red") +
  stat_summary(fun = "median", color = "blue")

dpred <- tmppred[isin %in% m6univ, .(pred = mean(pred)), 
                 by = .(yearweek, isin, log_close_diffs, index)]

dres <- copy(dpred)
dres <- dres[!is.na(pred)]

# ------------------------------------------------------
# GA-Optimization based model for longer time series
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
m6univ <- dt[m6univ == 1, .N, isin][,isin]

ggplot(tmppred[isin %in% m6univ], aes(pred, reorder(isin, pred))) + geom_point() + facet_wrap(~index) +
  stat_summary(color = "red") +
  stat_summary(fun = "median", color = "blue")



ggplot(tmppred[isin %in% c("SHY", "IEAA.L", "HIGH.L")], aes(pred, reorder(isin, pred))) + geom_point() + facet_wrap(~index) +
  stat_summary(color = "red") +
  stat_summary(fun = "median", color = "blue") +
  stat_summary(fun = function(x) mean(x, trim = .2), color = "green") 
  


dpred <- tmppred[isin %in% m6univ, .(pred = mean(pred)), 
                 by = .(yearweek, isin, log_close_diffs, index)]

dres2 <- copy(dpred)
dres2 <- dres2[!is.na(pred)]

# ----------------------------------------
# combine dtest_simple and dtest_v2
# ---------------------------------------

ggplot(dres[dres2[, .(isin, yearweek, pred_complex = pred)], on = .(isin, yearweek)], aes(pred, pred_complex)) + 
  geom_point() +
  geom_abline() + 
  geom_hline(yintercept = 0, col = "red") +
  geom_vline(xintercept = 0, col = "red") +
  facet_wrap(~yearweek)


dpred_test <- dres2[, .(isin, yearweek, pred_complex = pred)][dres, on = .(isin, yearweek)]
for(mm in 1:nrow(dpred_test)){
  isin_test <- dpred_test[mm, isin]
  dpred_test[mm, p_pred := dt[isin == isin_test, mean(log_close_diffs <= dpred_test[mm, pred],na.rm = TRUE)]  ]
  dpred_test[mm, p_pred_complex := dt[isin == isin_test, mean(log_close_diffs <= dpred_test[mm, pred_complex],na.rm = TRUE)]  ]
}
ggplot(dpred_test, aes(p_pred, p_pred_complex)) + geom_point() +
  geom_abline() +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))

# EDA
dpred_test[p_pred_complex > .55][order(p_pred_complex)]
dpred_test[p_pred_complex < .45][order(p_pred_complex)]
dpred_test[isin == "VXX"][order(p_pred_complex)]


dres[dres2[, .(isin, yearweek, pred_complex = pred)], on = .(isin, yearweek),
      ':=' (pred = pred_complex)]
# 
# # # manually set SHY, IEAA.L, VXX and HIGH.L for predictions greater than .55th quantile
# # # to 55th quantile
# # # an set SHY to 50th quantile
#  dres[isin == "SHY", pred := quantile(dt[isin == "SHY"]$log_close_diffs, probs = .50, na.rm = TRUE)]
#  dres[isin == "IEAA.L", pred := quantile(dt[isin == "IEAA.L"]$log_close_diffs, probs = .50, na.rm = TRUE)]
#  dres[isin == "HIGH.L", pred := quantile(dt[isin == "HIGH.L"]$log_close_diffs, probs = .55, na.rm = TRUE)]
#  dres[isin == "VXX", pred := quantile(dt[isin == "IEAA.L"]$log_close_diffs, probs = .55, na.rm = TRUE)]


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
setorder(dt, isin, index)
dt[, nacs := cumsum(!is.na(adjusted)), by = isin]
dt <- dt[nacs > 0]

# volume für tage mit is.na(adjusted) auf 0 setzen
dt[is.na(adjusted), volume := 0]

# preise für jene tage naive imputieren, an denen keine einträge vorhanden sind
adjusted_nas <- dt[, any(is.na(adjusted))]
iter <- 0
while(adjusted_nas){
  iter <- iter +1
  dt[, adj_shift := shift(adjusted, n = 1L), by = isin]
  dt[is.na(adjusted), adjusted := adj_shift]
  adjusted_nas <- dt[, any(is.na(adjusted))]
}

# drop irrelevant cols
dt[, c("open", "high", "low", "close", "nacs") := NULL]

# calculate log close diffs
dt[, lcd := c(NA, diff(log(adjusted))), by = isin]
dt <- dt[!is.na(lcd)]

# setorder
tmp <- copy(dt)
setorder(tmp, isin, index)
tmp <- na.omit(tmp)

# remove extreme outliers
cat("distribution of daily lcd before oulier removal \n")
summary(tmp$lcd)
quantile(tmp$lcd, probs = c(1e-6, 1e-5, 1e-4, 1e-3, 1-1e-3, 1-1e-4, 1-1e-5, 1-1e-6))
tmp <- tmp[abs(lcd) < .4]
cat("distribution of daily lcd after oulier removal \n")
summary(tmp$lcd)
quantile(tmp$lcd, probs = c(1e-6, 1e-5, 1e-4, 1e-3, 1-1e-3, 1-1e-4, 1-1e-5, 1-1e-6))

# train test split
train <- tmp[index > test_start - round(var_train_years*365) & index < test_start, ]

# dres auf train auf das selbe ISIN-Set einschränken
predict_isin <- intersect(dres$isin, train[, .N, isin]$isin)
dres <- dres[isin %in% predict_isin]
train <- train[isin %in% predict_isin]
hdays <- train[index >= test_start & index <= test_end, unique(index)]

# Falls test_end nach sysdate-14, dann berechne h basierend auf historie
if(test_end < Sys.Date()-14){
  tmp[, isoweek := ISOweek(index)]
  
  h_days <- tmp[index >= test_start & index <= test_end, .N, index][order(index), index]
  h_index <- data.table(isoweek = ISOweek(h_days), h_days)
  h_pred <- h_index[,.N, isoweek][order(isoweek), N]
  
  isoweek_days <- tmp[isoweek %in% ISOweek(test_index), .N, .(isoweek, index)][, .(h = .N), isoweek]
  isoweek_days <- isoweek_days[isoweek %in% unique(h_index$isoweek)]
  setorder(isoweek_days, isoweek)
  h_scale <- isoweek_days$h
  
} else {
  h_pred <- h_scale <- h
  isoweek_days <- ISOweek(test_index)
}

# M aufsetzen und m skalieren
pred_yearweeks <- isoweek_days
M <- list()
setorder(dres, yearweek, isin)
for(pp in seq_along(pred_yearweeks)){
  M[[pp]] <- dres[yearweek == pred_yearweeks[pp], .(isin, m = pred/h_scale[pp])]
}


# ===============================
# RANKED PROBABILITY SCORE
# ===============================

# ===============================
# RPS
# ===============================



# -------------------------------------------------
cat(format(Sys.time(), "%c"), "BOOTSTRAP SOFT COVARIANCE CALCULATION FOR RPS\n")
# -------------------------------------------------

dwide <- dcast(train, index ~ isin, value.var = "lcd", fun.aggregate = mean)[,!"index"]
N_boot <- N_RPS
nrows <- nrow(dwide)
MAT <- array(data = NA, dim = c(sum(h_pred), ncol(dwide), N_boot), dimnames = list(NULL, M[[1]][, isin] ,NULL))
set.seed(12345)
for(nn in 1:N_boot){ # 0000000000000000000 BOOTSTRAP LOOP START
  
  if(nn %% 100 == 0) cat(nn, "/", N_boot, "\n")
  boot <- sample(1:nrows, nrows, replace = TRUE)
  covmat <- var(dwide[boot, ], use = "pairwise.complete.obs")
  
  if(!is.positive.definite(covmat)){
    #print("Matrix not PD --> nearPD()-Call")
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
    if(all(M[[m]]$isin == colnames(C))){
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

# test
# all(c(all(samp[1:20,] == MAT[,,1]), all(samp[21:40,] == MAT[,,2])))

# ----------------------------------
# Ranked Probability score berechnen
# ----------------------------------
M_sum <- apply(MAT, c(2,3), sum)

# test
#all(apply(samp[1:20,1:97], 2, sum) == M_sum[1:97, 1])

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




# ===============================
# INFORMATION RATE
# ===============================


# -------------------------------------------------
cat(format(Sys.time(), "%c"), "BOOTSTRAP SOFT COVARIANCE CALCULATION FOR INFORMATION RATE\n")
# -------------------------------------------------

# ---------------------------------
# BLACKLIST too short time series for portfolio creation
# ---------------------------------  

ntrain <- train[,.N, by = isin]
whitelist <- ntrain[N > (var_train_years/3)*365, isin]
blacklist <- setdiff(ntrain$isin, whitelist)

M_white <- lapply(M, function(x) x[isin %in% whitelist])

short_only <- c("SHY")
short_only_bool <- M_white[[1]]$isin %in% short_only
long_only <- c("")
long_only_bool <- M_white[[1]]$isin %in% long_only

# ----------------------------------
# robuste
# Covarianzmatrix
# ----------------------------------

# covariance calculation
dwide <- dcast(train[isin %in% whitelist], index ~ isin, value.var = "lcd", fun.aggregate = mean)[,!"index"]
N_boot <- N_IR
nrows <- nrow(dwide)
MAT <- array(data = NA, dim = c(sum(h_pred), ncol(dwide), N_boot), dimnames = list(NULL, M_white[[1]][, isin] ,NULL))
C <- array(data = NA, dim = c(ncol(dwide), ncol(dwide), N_boot), dimnames = list(M_white[[1]][, isin], M_white[[1]][, isin] ,NULL))
set.seed(12345)
for(nn in 1:N_boot){ # 0000000000000000000 BOOTSTRAP LOOP START
  
  if(nn %% 100 == 0) cat(nn, "/", N_boot, "\n")
  boot <- sample(1:nrows, nrows, replace = TRUE)
  covmat <- var(dwide[boot, ], use = "pairwise.complete.obs")
  
  if(!is.positive.definite(covmat)){
    #print("Matrix not PD --> nearPD()-Call")
    PD_covmat <- nearPD(covmat, keepDiag = TRUE, ensureSymmetry = TRUE, maxit = 200)
    if(!PD_covmat$converged){
      cat("nearPD-call not converged - skipping iteration", nn, "\n")
      next
    }
    covmat <- as.matrix(PD_covmat$mat)
  }
  
  # draw dample from multivariate distribution
  SAMP <- list()
  for(m in 1:length(M_white)){
    
    #test order of M and C
    if(all(M_white[[m]]$isin == colnames(C))){
      SAMP[[m]] <- mvtnorm::rmvt(h_pred[m] , sigma = covmat, df = 4, delta = M_white[[m]]$m)
    } else {
      stop("ORDER OF C AND M_white ARE NOT IDENTICAL")
    }
  } 
  
  C[,,nn] <- covmat
  MAT[,,nn] <- do.call(rbind, SAMP)   
  
} # 0000000000000000000 BOOTSTRAP LOOP END

# drop NA-dimensions
drop_dims <- which(apply(MAT, 3, function(x) any(is.na(x))))
if(length(drop_dims)>0){
  C <- C[,,-drop_dims]
  MAT <- MAT[,,-drop_dims]
}

C <- apply(C, MARGIN = c(1,2), mean)



# -------------------------------------------------
# Find Minvar-Portfolio as warm-start input for GA
# -------------------------------------------------

if(ga_warm_start){
  nStocks <- ncol(C)
  
  # generate groups
  duniv <- fread("~/MEGA/M6_data/M6/01_first_quarter/01_raw_data1/M6_Universe_META.csv")
  duniv[, subtype := `GICS_industry/ETF_subtype`]
  duniv[, type := `GICS_sector/ETF_type`]
  duniv[class == "Stock", custom := `GICS_sector/ETF_type`]
  duniv[grep("Cap-North America", subtype), custom := "Cap_North_America"]
  duniv[grep("Cap-Europe", subtype), custom := "Cap_Europe"]
  duniv[grep("Cap-Asia", subtype), custom := "Cap_Asia"]
  duniv[type == "Equities" & is.na(custom), custom := subtype]
  
  # treat SPDR-Fund
  duniv[custom == "Sector-US"]
  duniv[custom == "Sector-US", custom := gsub(" Select Sector SPDR Fund", "", name)]
  
  duniv[custom == "Technology", custom := "Information Technology"]
  duniv[custom == "Financial", custom := "Financials"]
  duniv[custom == "Industrial", custom := "Industrials"]
  duniv[custom == "Communication", custom := "Communication Services"]
  
  # fixed income
  duniv[grep("t-Europe", subtype), custom := "Fixed_inc_europe"]
  duniv[grep("t-US", subtype), custom := "Fixed_inc_US"]
  
  # rest
  duniv[is.na(custom),custom := subtype]
  
  # finalize
  duniv[, sector := custom]
  
  # auf whitelist einchränken
  duniv <- duniv[symbol %in% whitelist]
  
  
  fitness_local <- function(w, C, M, group_info, disttype = "norm"){
    
    library(data.table)
    
    # not run
    # w <- c(runif(ncol(C), min = 1e-6, max = 1-1e-6), runif(1, -3.2, -1.8))
    # w <- ifelse(long_only_bool, max(w,0), w)
    # w <- ifelse(short_only_bool, min(w,0), w)
    
    m <- M$m
    if(any(M$isin != colnames(C))) stop("wrong order")
    len_w <- length(w)
    
    # wettanteil ---------------
    distr_sd <- 10^(w[length(w)])
    w <- w[-length(w)]
    
    if(disttype == "norm")
      w <- qnorm(w, sd = distr_sd)
    
    if(disttype == "t")
      w <- qt(w, df=10)*distr_sd
    
    # test if w is within range
    if(sum(abs(w)) < .25|sum(abs(w)) > .35) return(-sqrt(.Machine$double.xmax))
    
    # require positive return
    if(sum(w * (exp(m)-1))<=0) return(1e3*sum(w * m)^2)
    
    # calculate fitness
    #-drop(w %*% C %*% w)
    
    # calculate local fitness
    groups <- unique(group_info$sector)
    RES <- list()
    M_iter <- copy(M)
    M_iter[, i:=.I]
    for(gg in seq_along(groups)){
      
      group <- groups[gg]
      group_symb <- group_info[sector == group, symbol]
      
      ii <- M_iter[isin %in% group_symb, i]
      IR_approx <- log(sum(w[ii] * (exp(m[ii])-1))+1)/sqrt(drop(w[ii] %*% C[ii,ii] %*% w[ii]))
      
      RES[[gg]] <- data.table(group, IR_approx, N = length(group_symb))
      
    }
    
    RES <- rbindlist(RES)
    local_fittness <- RES[, sum(IR_approx*N)/sum(N)]      
    
    # calculate gloval fitness
    global_fittness <- log(sum(w * (exp(m)-1))+1)/sqrt(drop(w %*% C %*% w))      
    
    return(local_fittness + global_fittness)
  }
  
  
  lower_bound_isin <- rep(1e-6, length(M_white[[1]]$m))
  lower_bound_isin <- ifelse(long_only_bool, 0.5, lower_bound_isin)
  
  upper_bound_isin <- rep(1-1e-6, length(M_white[[1]]$m))
  upper_bound_isin <- ifelse(short_only_bool, 0.5, upper_bound_isin)
  
  cat(format(Sys.time(), "%c%"), "starting optimization over C and M\n")
  GA <- ga(type = "real-valued", 
           fitness = fitness_local,
           C = C,
           M = copy(M_white[[1]]),
           group_info = copy(duniv),
           disttype = "norm",
           lower = c(lower_bound_isin, -3.2), 
           upper = c(upper_bound_isin, -2.3), 
           popSize = 1e3,
           #numIslands = 6,
           pmutation = .2,
           #migrationInterval = 500,
           maxiter = 500, 
           optim = FALSE,
           seed = 12345)
  
  plot(GA)
  sgaC <- summary(GA)
  #rr <- rank(sgaC$fitnessValues) %in% 1:6
  warm_start <- sgaC$solution
  
}


# -------------------------------------------------
# Generate Trajectories 
# -------------------------------------------------

# N <- N_IR
# set.seed(12345)
# 
# SAMP <- list()
# for(m in 1:length(M_white)){
#   
#   #test order of M and C
#   if(all(M_white[[m]]$isin == colnames(C))){
#     SAMP[[m]] <- mvtnorm::rmvt(N*h_pred[m] , sigma = C, df = 4, delta = M_white[[m]]$m)
#   } else {
#     stop("ORDER OF C AND M ARE NOT IDENTICAL")
#   }
# }
# 
# tmp <- numeric(sum(sapply(lapply(SAMP, dim), prod)))
# for(ii in 1:N){
#   batch <- matrix(NA, ncol = nrow(M_white[[1]]), nrow = sum(h_pred))
#   for(m in 1:length(M_white)){
#     h_sub <- h_pred[m]
#     h_sub_bis <- cumsum(h_pred)[m]
#     h_sub_von <- h_sub_bis-h_sub+1
#     batch[h_sub_von:h_sub_bis,] <- SAMP[[m]][((ii-1)*h_sub+1):((ii)*h_sub),]
#   }
#   tmp[((ii-1)*sum(h_pred)*nrow(M_white[[1]])+1):((ii)*sum(h_pred)*nrow(M_white[[1]]))] <- batch
# }
# 
# MAT <- array(tmp, dim = c(sum(h_pred), nrow(M_white[[1]]), N), dimnames = list(NULL, M_white[[1]][, isin] ,NULL))

# test
# all(c(all(samp[1:20,] == MAT[,,1]), all(samp[21:40,] == MAT[,,2])))


# ------------------------------------------------------------
# INFORMATION RATE BERECHNEN gegeben der Monte-Carlos samples
# ------------------------------------------------------------

# generate groups
duniv <- fread("~/MEGA/M6_data/M6/01_first_quarter/01_raw_data1/M6_Universe_META.csv")
duniv[, subtype := `GICS_industry/ETF_subtype`]
duniv[, type := `GICS_sector/ETF_type`]
 duniv[class == "Stock", custom := `GICS_sector/ETF_type`]
duniv[grep("Cap-North America", subtype), custom := "Cap_North_America"]
duniv[grep("Cap-Europe", subtype), custom := "Cap_Europe"]
duniv[grep("Cap-Asia", subtype), custom := "Cap_Asia"]
duniv[type == "Equities" & is.na(custom), custom := subtype]

# treat SPDR-Fund
duniv[custom == "Sector-US"]
duniv[custom == "Sector-US", custom := gsub(" Select Sector SPDR Fund", "", name)]

duniv[custom == "Technology", custom := "Information Technology"]
duniv[custom == "Financial", custom := "Financials"]
duniv[custom == "Industrial", custom := "Industrials"]
duniv[custom == "Communication", custom := "Communication Services"]

# fixed income
duniv[grep("t-Europe", subtype), custom := "Fixed_inc_europe"]
duniv[grep("t-US", subtype), custom := "Fixed_inc_US"]

# rest
duniv[is.na(custom), custom := subtype]

# finalize
duniv[, sector := custom]

# auf whitelist einchränken
duniv <- duniv[symbol %in% whitelist]

get_IR <- function(w, MAT, group_info, disttype = "norm", filename = NULL){
  
  # not run
  # w <- c(runif(ncol(MAT), min = 1e-6, max = 1-1e-6), runif(1, -3.2, -1.8))
  
  len_w <- length(w)
  
  # wettanteil ---------------
  distr_sd <- 10^(w[length(w)])
  w <- w[-length(w)]
  
  if(disttype == "norm")
    w <- qnorm(w, sd = distr_sd)
  
  if(disttype == "t")
    w <- qt(w, df=10)*distr_sd
  
  # test ---------------
  if(sum(abs(w)) < .25|sum(abs(w)) > .35) return(-sqrt(.Machine$double.xmax))
  
  mdim <- dim(MAT)
  h <- mdim[1]
  k <- mdim[2]
  N <- mdim[3]
  
  WEIGHTS <- array(rep(rep(w, each = h), N), dim = c(h, k, N), dimnames = list(NULL, dimnames(MAT)[[2]] ,NULL))
  
  wMAT <- WEIGHTS*(exp(MAT)-1)
  
  # CALCULATE TOTAL IR and BETA
  RET_t <- sapply(1:dim(wMAT)[3], function(i) rowSums(wMAT[,,i]))
  ret_t <- log(RET_t+1)
  ret_t_IVV <- sapply(1:dim(MAT)[3], function(i) MAT[,"IVV",i])
  
  ret <- colSums(ret_t)
  sd_ret <- apply(ret_t, 2, sd)
  
  IR <- ret/sd_ret
  
  IR_mean <- mean(IR)
  IR_sd <- sd(IR)
  IR_min <- min(IR)
  IR_max <- max(IR)
  P_IR_pos <- mean(IR>0)
  
  
  # calculate beta and alpha
  alpha_beta <- sapply(1:ncol(ret_t), 
                       function(i) 
                         lm.fit(x = cbind(1, ret_t_IVV[,i]), 
                                y = ret_t[,i])$coefficients)
  
  alpha <- mean(alpha_beta[1,])
  beta <- mean(alpha_beta[2,])
  
  tres <- data.table(IR_mean, IR_sd, IR_min, IR_max, P_IR_pos, 
                     ret_mean = mean(ret), ret_sd = sd(ret), 
                     sd_ret_mean = mean(sd_ret), sd_ret_sd = sd(sd_ret),
                     alpha, beta)
  
  # CALCULATE PER GROUP IR and BETA
  groups <- group_info[, unique(sector)]
  RES <- list()
  for(gg in seq_along(groups)){
    
    group <- groups[gg]
    group_symb <- group_info[sector == group, symbol]
    
    if(length(group_symb) == 1){
      RET_t <- sapply(1:dim(wMAT)[3], function(i) wMAT[,group_symb,i])
    } else {
      RET_t <- sapply(1:dim(wMAT)[3], function(i) rowSums(wMAT[,group_symb,i, drop = FALSE]))
    }
    
    ret_t <- log(RET_t+1)
    
    ret <- colSums(ret_t)
    sd_ret <- apply(ret_t, 2, sd)
    
    IR <- ret/sd_ret
    
    IR_mean <- mean(IR)
    IR_sd <- sd(IR)
    IR_min <- min(IR)
    IR_max <- max(IR)
    P_IR_pos <- mean(IR>0)
    
    # Local beta
    alpha_beta <- sapply(1:ncol(ret_t), 
                         function(i) 
                           lm.fit(x = cbind(1, ret_t_IVV[,i]), 
                                  y = ret_t[,i])$coefficients)
    
    alpha <- mean(alpha_beta[1,])
    beta <- mean(alpha_beta[2,])
    
    
    RES[[gg]] <- data.table(group, IR_mean, IR_sd, IR_min, IR_max, P_IR_pos, 
                            alpha, beta, N = length(group_symb))
    
  }
  
  RES <- rbindlist(RES)
  RES_agg <- RES[, .(IR_mean_loc = sum(IR_mean*N)/sum(N), 
                     IR_mean_loc_neg = mean(IR_mean < 0), 
                     IR_sd_loc = sum(IR_sd*N)/sum(N),
                     P_IR_pos = sum(P_IR_pos*N)/sum(N), 
                     P_IR_pos_min = min(P_IR_pos), 
                     P_IR_pos_max = max(P_IR_pos), 
                     mean_alpha_loc = sum(alpha*N)/sum(N), 
                     max_abs_beta_loc = max(abs(beta)))]
  
  
  # CALCULATE FITNESS
  fitness_value <- tres$IR_mean + RES_agg$IR_mean_loc - pmax(abs(tres$beta)-.03, 0)*1000 - pmax(RES_agg$max_abs_beta_loc -.1, 0)*1000
  
  w <- matrix(w, nrow = 1)
  colnames(w) <- dimnames(MAT)[[2]]
  
  if(!is.null(filename)) {
    res <- data.table(timestamp = Sys.time(), tres, RES_agg,
                      fitness_value = fitness_value, invest_sum = sum(abs(w)), w)
    fwrite(res, filename, append = TRUE) 
  }
  
  return(fitness_value)
}

cat(format(Sys.time(), "%c%"), "starting optimization over MAT\n")
GA_res <- ga(type = "real-valued",
             fitness = get_IR,
             MAT = MAT,
             group_info = duniv,
             disttype = disttype,
             filename = filename_ga_details,
             lower = c(lower_bound_isin, -3.2), 
             upper = c(upper_bound_isin, -2.3), 
             popSize = ga_pop_size,
             pmutation = .3,
             #numIslands = 6,
             #migrationInterval = 10,
             maxiter = ga_max_iter, optim = ga_local_optim, monitor = TRUE, 
             optimArgs = list(method = "L-BFGS-B", 
                              poptim = 0.01,
                              pressel = 0.9,
                              control = list(fnscale = -1, maxit = 5)), 
             suggestions = if(ga_warm_start) warm_start else NULL,
             seed = 12345)

if(!is.null(filename_ga_object))
  qs::qsave(GA_res, filename_ga_object)

sGA <- summary(GA_res)
best <- which.max(sGA$fitness)
w <- sGA$solution[best,]
fittness <- sGA$fitnessValues[best]

# decode
len_w <- length(w)

# wettanteil ---------------
distr_sd <- 10^(w[length(w)])
w <- w[-length(w)]

if(disttype == "norm")
  w <- qnorm(w, sd = distr_sd)

if(disttype == "t")
  w <- qt(w, df=10)*distr_sd

# combine w1 and w2 ---------------
sol <- w
sol_whitelist <- data.table(ID = colnames(MAT), Decision = sol)
sol_blacklist <- data.table(ID = blacklist, Decision = 0)
sol <- rbind(sol_whitelist, sol_blacklist)

# ==============================
# create result given the template
# ===============================
dtempl <- fread(template_result_file)
dres <- copy(samp_rank)
dres[sol, on = "ID", ':=' (Decision = i.Decision)]
dres[, Decision := round(Decision, 5)]

# treat DRE
dres <- rbind(dres, 
              data.table(ID = "DRE", Rank1 = .2, Rank2 = .2, Rank3 = .2, Rank4 = .2, Rank5 = .2, Decision = 0))



  # save result
  fwrite(dres, file = abgabe_file)

  # --------------
  # clean env from big objects
  # --------------
  rm(MAT, MAT_RPS, GA_res)
  for(i in 1:10) gc()
  
  
  # --------------
  # print summary
  # --------------
  
  # load solution
  # not run
  # dres <- fread(paste0("Abgabe_20220625_20220716/","20220722_163759_", abgabe_bez, ".csv"))

  dd <- fread("~/MEGA/M6_data/M6/01_first_quarter/01_raw_data1/M6_Universe_META.csv")
  dd[,ID := symbol]
  dd[, sector := `GICS_sector/ETF_type`]
  dd[, industry := `GICS_industry/ETF_subtype`]
  
  dres[,ID := as.character(ID)]
  ddres <- dres[dd, on = "ID"]

  
  
  ddres[,.(abs_investment = sum(abs(Decision)))]
  ddres[,.(abs_investment = sum(abs(Decision))), by = class]
  ddres[,.(abs_investment = sum(abs(Decision))), by = sector]
  ddres[,.(abs_investment = sum(abs(Decision))), by = industry][order(abs_investment)]
  
  
  ddres[, .(sum_abs_Long = sum(abs(Decision))), by = sign(Decision)]
  ddres[, .(mean_abs_Long = mean(abs(Decision))), by = sign(Decision)]
  
  ddres[,.(mean_abs_investment = mean(abs(Decision)))]
  
  ddres[Decision == 0]
  
  ddres[, .(mean_Rank1 = mean(Rank1)), by = sign(Decision)]
  ddres[, .(mean_Rank2 = mean(Rank2)), by = sign(Decision)]
  ddres[, .(mean_Rank3 = mean(Rank3)), by = sign(Decision)]
  ddres[, .(mean_Rank4 = mean(Rank4)), by = sign(Decision)]
  ddres[, .(mean_Rank5 = mean(Rank5)), by = sign(Decision)]
  
  ddres[ID %in% c("SHY","IEAA.L", "HIGH.L" )]
  
  ddres[sector == 'Equities', .(sum_abs_Long = sum(abs(Decision))), by = sign(Decision)]
  ddres[sector == 'Equities'][order(Decision)]
  
  ddres[class == "ETF" & industry == "Sector-US"]
    
  