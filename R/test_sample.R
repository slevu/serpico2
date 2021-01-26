if (FALSE){
  # change nsamplecell according to response rate varying by covariates
  
  ##---- simulate_mock_data ----
  #' Simulate dataset
  #' @param vars names of covariates
  #' @param ntime,nsex,nage,nreg number of covariates categories
  #' @param prev true prevalence in reference category
  #' @param resp varying response rate (logical)
  #' @param nsamplecell number of sample per poststratification * period cell
  #' @param risk_time,risk_sex,risk_age,risk_reg relative risks ({ntime,nsex,nage,nreg}_vectors)
  #' @param calib named vector of {"ncc", "npp", "TP", "TN"} counts
  #' @param obs.prev show observed prevalence by covariates
  #' @export
  simulate_mock_data2 <- function(
    ## sample population
    vars = c("time", "sex", "age", "reg"),
    ntime = 3,
    nsex = 2,
    nage = 9,
    nreg = 17,
    prev = 0.08, ## true prevalence reference
    resp = FALSE,
    nsamplecell = 10,
    N = 3000,
    risk_time = c(0.1, 1, 1.2),
    risk_sex = c(1, 1.2),
    risk_age = c(0.5, rep(1, nage - 1)),
    risk_reg = c(rep(1, nreg -1), 2),
    calib = calib_unit_test(),
    obs.prev = FALSE
  ){
    se <- calib["TP"] / calib["ncc"] # 0.86 # P(T+ | D+)
    sp <- calib["TN"] / calib["npp"] # 0.99 # P(T- | D-)
    
    ## nsamplecell individuals for each combination
    x <- expand.grid(sex = 1:nsex, age = 1:nage,
                     reg = 1:nreg) #, j = 1:nsamplecell) time = 1:ntime, 
    
    ## response rate
    # global
    pr_all <- N / sum(ps$n)
    # say women and older tend to respond more
    resp_sex <- c(1, 1.4) 
    pr_sex <- resp_sex/sum(resp_sex)
    resp_age <- c(.26, .34, .54, .64, .75, .95, 1.2, 1.3, 1) # Blanchoz 2018
    pr_age <- resp_age/sum(resp_age)
    resp_reg <- rep(1, nreg)
    pr_reg <- resp_reg/sum(resp_reg)
    resp_time <- rep(1, ntime)
    pr_time <- resp_time/sum(resp_time)
    
    x$pr_sex <- pr_sex[x$sex]
    x$pr_age <- pr_age[x$age]
    x$pr_reg <- pr_reg[x$reg]
    # x$pr_time <- pr_time[x$time]
    x$pr <- apply(x[,4:6], 1, prod)
    x$N <- -1
    for (i in 1:nrow(x)) x[i, "N"] <-  ps[which(ps$code == x[i, "reg"] &
                                                  ps$age == x[i, "age"] &
                                                  ps$sex == x[i, "sex"]), "n"]
    x$n <- sapply(x$N, function(x) rbinom(1, x, pr_all)) # sapply(1:nrow(x), rpois(n = x$pr * x$N * N/sum(ps$n)
    
    head(x)
    
    ## prob of sex among 10 samples
    sample(nsex, nsamplecell, replace = TRUE, prob = pr_sex)
    # barplot(resp_age)
    ps <- data.frame( lapply(poststrat, as.numeric) )
    
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
    if (obs.prev){
      print( lapply(setNames(vars, vars), function(v){
        round( tapply(d0[,"res"], d0[,v], mean), 3)
      }) )
    }
    
    return(d0)
  }
  
  
  ## introduce probability of "response" varying by age, sex, region
  ## voir Blanchoz 2018, fig 7
  ## 58 % des femmes ont réalisé une analyse contre 42 % des hommes
  ## c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  ## from webdigitizer
  if (FALSE){
    femme <- read.csv2("../serpico/data-raw/blanchoz_fig7_femmes.csv")
    homme <- read.csv2("../serpico/data-raw/blanchoz_fig7_hommes.csv")
    femme$cage <- cut(femme[,1], breaks = c( seq(0, 80, 10), 200 ), right = FALSE)
    f <- tapply(femme[,2], list(femme[,3]), mean)
    homme$cage <- cut(homme[,1], breaks = c( seq(0, 80, 10), 200 ), right = FALSE)
    h <- tapply(homme[,2], list(homme[,3]), mean)
    # recours <- data.frame(Homme = c(0.16, 0.18, 0.24, 0.30, 0.45, 0.60, 0.76, 0.85, 0.70),
    #                       Femme = c(0.15, 0.30, 0.54, 0.60, 0.62, 0.67, 0.72, 0.83, 0.65) )
    a <- data.frame(h, f)
    # a <- recours # / colSums(recours)
    poststrat <- readRDS("../serpico/data/poststrat.rds")
    b <- tapply(poststrat$n, list(poststrat$age, poststrat$sex), function(x) sum(x))# / sum(poststrat$n))
    colSums(a * b) / colSums(b)
    # h        f
    # 44.69958 57.72409
    
    poststrat2 <- data.frame( lapply(poststrat[poststrat$code != "6",], as.numeric) )
  }
  if (FALSE){
    with(a, h + f /2)
    # say women and older tend to responde more
    resp_sex <- c(1, 1.4) 
    resp_sex/sum(resp_sex)
    resp_age <- c(.26, .34, .54, .64, .75, .95, 1.2, 1.3, 1)
    resp_age/sum(resp_age)
    barplot(resp_age)
  }
}


