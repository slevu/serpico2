##---- calib_unit_test ----
#' Results from calibration study fro one serological test
calib_unit_test <- function(){
  setNames(c(309, # confirmed cases
             523, # pre-pandemic
             266, 
             523), 
           c("ncc", "npp", "TP", "TN"))
}

##---- simulate_mock_data ----
#' Simulate dataset
simulate_mock_data <- function(
  ## sample population
  vars = c("time", "sex", "age", "reg"),
  ntime = 3,
  nsex = 2,
  nage = 9,
  nreg = 17,
  prev = 0.08, ## true prevalence reference
  nsamplecell = 10,
  risk_time = c(0.1, 1, 1.2),
  risk_sex = c(1, 1.2),
  risk_age = c(0.5, rep(1, nage - 1)),
  risk_reg = c(rep(1, nreg -1), 2),
  calib = calib_unit_test()
  ){
  se <- calib["TP"] / calib["ncc"] # 0.86 # P(T+ | D+)
  sp <- calib["TN"] / calib["npp"] # 0.99 # P(T- | D-)
  
  ## nsamplecell individuals for each combination
  ##  x <- expand.grid(time = 1:ntime, sex = 1:nsex, age = 1:nage,
  ##  reg = 1:nreg, j = 1:nsamplecell)
  d0 <- data.frame(i = 1:(ntime * nsex * nage * nreg * nsamplecell),
                   time = rep(1:ntime, each = nsamplecell),
                   sex = rep(1:nsex, each = nsamplecell),
                   age = rep(1:nage, each = nsamplecell),
                   reg = rep(1:nreg, each = nsamplecell),
                   res = NA)
  
  set.seed(123)
  
  d0$res <- apply(d0, 1, function(r){
    ## disease status
    d <- rbinom(1,1, prev*
                  risk_time[r["time"]]*
                  risk_sex[r["sex"]]*
                  risk_age[r["age"]]*
                  risk_reg[r["reg"]]
    )
    ## test result
    r <- ifelse(d == 0, rbinom(1,1, (1 - sp)),
                rbinom(1,1, se))
  })
  
  ## obs prev
  if (FALSE){
    lapply(setNames(vars, vars), function(v){
      round( tapply(d0[,"res"], d0[,v], mean), 3)
    })
  }
  
  return(d0)
}
  
##---- set_stan ----
#' Set Stan inputs
set_stan <- function(
  df = d0,
  calib = calib_unit_test(),
  modelnb = 6,
  n_iter =  200, # 1500 #
  n_warmup = 150, # 250 #
  n_chains = 2, # 4 #
  n_core = 2 # 2 #
){
  require(rstan)
  ## name output
  iters <- (n_iter - n_warmup) * n_chains
  # dir.create("ouput/", showWarnings = FALSE)
  NMOUT <- paste0("output/", "fit", modelnb, "_", iters, ".rds")

  data_stan <- list(
    D = 2, # nb predictors : sex + intercept
    N = nrow(df),
    A = length(unique(df$age)),
    R = length(unique(df$reg)),
    T = length(unique(df$time)),
    y = as.integer(df$res),
    age = as.integer(df$age),
    reg = as.integer(df$reg),
    time = as.integer(df$time),
    x = data.frame(intercept = 1L,
                   sex = as.integer(df$sex)),
    nneg = calib["npp"],
    npos = calib["ncc"],
    TP = calib["TP"],
    TN = calib["TN"]
  )
parms_stan <- list(n_iter = n_iter, n_warmup = n_warmup,
                   n_chains = n_chains,
                   n_core = n_core)
return( list(nmout = NMOUT, data = data_stan, parms = parms_stan) )
}

##---- mci ----
#' Summarize iterations
#' Mean and CI
mci <- function(x, r = 2){
  round(
    c(m = mean(x, na.rm = TRUE),
      quantile(x, probs = c(.025, .975),
               na.rm = TRUE))
    , r)
}

##---- mci_var ----
#' Summarize iterations stratified by one categorical variable
#' For each period
mci_var <- function(v = "age", pop = pops, df = psw, time = "week"){
  .l <- lapply(unique(df[, time]), function(w){
    sapply(setNames(levels(df[, v]), levels(df[, v])), function(x) {
      ntot <- sum(df[df[, time] == w & df[, v] == x, "n"])
      # print(ntot)
      p <- apply(pop[df[, time] == w & df[, v] == x, ], 2, sum)
      mci( p / ntot * 100 )
    })
  } )
  t( do.call(rbind, .l) )
}

##---- mci_2var ----
#' Summarize iterations stratified by two categorical variables
#' For each period
mci_2var <- function(v1, v2, pop = pops, df = psw, time = "week"){
  l <- lapply(unique(df[, time]), function(w){
    # v1 = "sex"; v2 = "age"
    k <- df[, time] == w
    ntot <- tapply(df[k, "n"],
                   list(df[k, v1], df[k, v2]),
                   sum)
    ## p / ntot
    lp <- lapply(1:ncol(pop), function(j){
      tapply(pop[k, j], list(df[k, v1], df[k, v2]), sum) / ntot
    })
    
    a <- reshape2::melt( do.call(rbind, lp) )
    m <- do.call(data.frame, aggregate(a[,3], a[,1:2], function(x){
      round( c(mean(x, na.rm = TRUE),
               quantile(x, probs = c(.025,.975), na.rm = TRUE) ), 3)
    } ) )
    colnames(m) <- c(v1, v2, "m", "2.5%", "97.5%")
    cbind(w,m)
  })
  do.call(rbind, l)
}