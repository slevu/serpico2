// Multilevel logistic Regression with random intercept on age, region, week
// for one binary test

data {
  int<lower=1> D; // nb fixed predictors (including intercept as 1s)
  int<lower=1> N; // nb indiv
  int<lower=1> A; // nb age
  int<lower=1> R; // nb reg
  int<lower=1> T; // nb time
  // test results
  int<lower = 0, upper = 1> y[N]; // LN|LS|PN positive
  // random effects
  int<lower=1, upper=A> age[N]; // age vector
  int<lower=1, upper=R> reg[N]; // region vector
  int<lower=1, upper=T> time[N]; // time vector
  matrix[N, D] x; // fixed effects variables
  // calibration study
  int<lower=0> npos;
  int<lower=0> nneg;
  int<lower=0, upper=npos> TP;
  int<lower=0, upper=nneg> TN;
  real<lower = 0> scale_beta_prior;
  real<lower = 0> scale_sigma_prior;
}

parameters {
  vector[D] beta; // intercept and fixed coef for sex
  real<lower=0> sigma_a; // variability of age // implicit prior N(0,Inf)
  real<lower=0> sigma_r; // variability of region
  real<lower=0> sigma_t; // variability of time
  vector[A] alpha_a; // hierarchical varying intercept age
  vector[R] alpha_r; // random effect reg
  vector[T] alpha_t; // random effect time
  real <lower=0,upper=1> se;
  real <lower=0,upper=1> sp;
}

model {

  vector [N] p = inv_logit(x * beta +
                sigma_a * alpha_a[age] +
                sigma_r * alpha_r[reg] +
                sigma_t * alpha_t[time]);

  vector [N] p_obs = se * p + (1 - sp) * (1 - p); // obs proportion test+

  y ~ bernoulli(p_obs);

  TP ~ binomial(npos, se);
  TN ~ binomial(nneg, sp);

  // priors

  beta ~  normal(0, scale_beta_prior); // student_t(1, 0, 2.5); //
  alpha_a ~ normal(0, sigma_a); // hierarchical normal prior
  alpha_r ~ normal(0, sigma_r);
  alpha_t ~ normal(0, sigma_t);

  sigma_a ~ lognormal(0, scale_sigma_prior); // weakly informative hyper prior
  sigma_r ~ lognormal(0, scale_sigma_prior);
  sigma_t ~ lognormal(0, scale_sigma_prior);
}

