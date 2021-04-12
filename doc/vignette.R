## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE,
                       collapse = TRUE) #, comment = "#>")
knitr::opts_knit$set(root.dir = '../')
options(knitr.table.format = function() {
  if (knitr::is_latex_output()) 
    "latex" else "pandoc"
})
# options(scipen=999)

## ----pkg----------------------------------------------------------------------
library(serpico2)

## ----read, include = FALSE----------------------------------------------------
knitr::read_chunk("R/run_model.R")
run <- TRUE

## ----simsample----------------------------------------------------------------
  d0 <- simulate_mock_data()

## -----------------------------------------------------------------------------
rbind(head(d0), tail(d0))

## -----------------------------------------------------------------------------
vars = c("time", "sex", "age", "reg")
print( lapply(setNames(vars, vars), function(v){
      round( tapply(d0[,"res"], d0[,v], mean), 3)
    }) )

## ----extdata------------------------------------------------------------------
  poststrat <- droplevels( poststrat[poststrat$code != "6", ] )

## ----fit----------------------------------------------------------------------
  l_stan <- set_stan(df = d0,
                     modelnb = 1,
                     n_iter =  125, # increase this
                     n_warmup = 100, # and this
                     n_chains = 2, # and this
                     n_core = 2) # possibly this too
  NMOUT <- l_stan$nmout
  
  if ( !file.exists( NMOUT ) ){
    model <- rstan::stan_model("inst/stan/model.stan", auto_write = TRUE)
    system.time(
      stanfit <- rstan::sampling(model,
                                 data = l_stan$data,
                                 iter = l_stan$parms[["n_iter"]],
                                 warmup = l_stan$parms[["n_warmup"]],
                                 chains = l_stan$parms[["n_chains"]],
                                 cores = l_stan$parms[["n_core"]],
                                 control = list(adapt_delta = 0.99))
    )
    dir.create("output/", showWarnings = FALSE)
    saveRDS(stanfit, NMOUT)
  } else {
    stanfit <- readRDS(NMOUT)
  } # 

## ----poststratification-------------------------------------------------------
  {
    draws <- rstan::extract(stanfit, pars = c("beta", "alpha_a", "sigma_a",
                                              "alpha_r", "sigma_r",
                                              "alpha_t", "sigma_t"))
    
    ##- poststrat + week
    psw <- cbind(week = as.factor(rep(1:3, each = nrow(poststrat))),
                 poststrat,
                 row.names = NULL)
    psw2 <- data.frame(lapply(psw, as.numeric)) # matrix needed
    # 3*2*9*17 = 918
  }
  
  ##- fill matrices of predicted probability and
  ## expected population count positive
  FNRES0 <- paste0("output/", sub("fit", "raw_res", basename(l_stan$nmout) ) )
  if ( !file.exists(FNRES0) ){
    Rcpp::sourceCpp("src/regpred.cpp")
    probs <- loop_C(psw = as.matrix(psw2), parms = draws)
    ## pop counts
    pops <- psw$n * probs
    saveRDS(list(psw = psw, pops = pops), FNRES0)
  } else {
    .raw_res <- readRDS(FNRES0)
    psw <- .raw_res[[1]]
    pops <- .raw_res[[2]]
  }

## ----summarize----------------------------------------------------------------
  {
    ## Overall
    .l <- lapply(unique(psw$week), function(x){
      ntot <- sum(psw[psw$week == x, "n"])
      p <- apply(pops[psw$week == x,], 2, sum)
      mci( p / ntot * 100)
    } )
    .o <- do.call(c, .l)
    ## Strata
    .s <- mci_var(v = "sex",  pop = pops, df = psw, time = "week")
    .a <- mci_var(v = "age",  pop = pops, df = psw, time = "week")
    .r <- mci_var(v = "code",  pop = pops, df = psw, time = "week")
    rownames(.r) <- code_region$abb[match(rownames(.r), code_region$REGION)]
    res <- as.data.frame(rbind("Overall" = .o, .s, .a, .r))
    ## Combined strata
    rsa <- mci_2var("sex", "age", pop = pops, df = psw, time = "week")
    rac <- mci_2var("age", "code", pop = pops, df = psw, time = "week")
  }

## -----------------------------------------------------------------------------
knitr::kable(res)

