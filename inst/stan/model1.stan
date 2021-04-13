// Multilevel logistic Regression with fixed effects
// for one binary test

data {
  // int<lower=1> D; // nb fixed predictors (including intercept as 1s)
  int<lower=1> N; // nb indiv
  // int<lower=1> A; // nb age
  // int<lower=1> R; // nb reg
  int<lower=1> T; // nb time
  // test results
  int<lower = 0, upper = 1> y[N]; // LN|LS|PN positive
  // random effects
  // int<lower=1, upper=R> reg[N]; // region vector
  int<lower=1, upper=T> time[N]; // time vector
  vector[N] age; // age vector
  vector[N] sex; // sex vector
  // matrix[N, D] x; // fixed effects variables
  // calibration study
  int<lower=0> npos;
  int<lower=0> nneg;
  int<lower=0, upper=npos> TP;
  int<lower=0, upper=nneg> TN;
  real<lower = 0> scale_beta_prior;
  real<lower = 0> scale_sigma_prior;
  real<lower = 0> scale_inter_prior;
}

parameters {
  real alpha;
  real beta_s; // fixed coef for sex
  real beta_a; // fixed coef for age
  // real<lower=0> sigma_r; // variability of region // implicit prior N(0,Inf)
  real<lower=0> sigma_t; // variability of time
  // vector[A] alpha_a; // hierarchical varying intercept age
  // vector[R] alpha_r; // random effect reg
  vector[T] alpha_t; // random effect time
  real <lower=0,upper=1> se;
  real <lower=0,upper=1> sp;
}

model {

  vector [N] p = inv_logit( alpha +
                beta_s * sex +
                beta_a * age +
                // sigma_r * alpha_r[reg] +
                sigma_t * alpha_t[time]
                );

  vector [N] p_obs = se * p + (1 - sp) * (1 - p); // obs proportion test+

  y ~ bernoulli(p_obs);

  TP ~ binomial(npos, se);
  TN ~ binomial(nneg, sp);

  // priors
  alpha ~ normal(0, scale_inter_prior);
  beta_s ~  normal(0, scale_beta_prior); // student_t(1, 0, 2.5); //
  beta_a ~ normal(0, scale_beta_prior); 
  // alpha_r ~ normal(0, sigma_r); // hierarchical normal prior
  alpha_t ~ normal(0, sigma_t);

  // sigma_r ~ lognormal(0, scale_sigma_prior); // weakly informative hyper prior
  sigma_t ~ lognormal(0, scale_sigma_prior);
}

