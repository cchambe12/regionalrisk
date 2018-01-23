// Gaussian Process Model for Regional Risk Analysis
// Started by Cat - 16 January 2018

data {
  int<lower=0> N;
  vector[N] fs;
  vector[N] sp;
  vector[N] mat;
  vector[N] site;
  vector[N] cc;
  
}

parameters {
  vector[N] a_sp;
  vector[N] b_mat;
  vector[N] b_site;
  vector[N] b_cc;
  
  real mu_a[N];
  real mu_b_mat;
  real mu_b_site;
  real mu_b_cc;
  
  real<lower=0> sigma_b_mat;
  real<lower=0> sigma_b_site;
  real<lower=0> sigma_b_cc;
  
  real<lower=0> sigma_a;
  
}

transformed parameters {
  
  matrix[N, N] cov =   cov_exp_quad(mu_a, mu_b_site, mu_b_mat)
                     + diag_matrix(rep_vector(1e-10, N));
  matrix[N, N] L_cov = cholesky_decompose(cov);
  
}

model {
  
  mu_b_mat ~ normal(0, 2);
  mu_b_site ~ normal(0, 2);
  mu_b_cc ~ normal(0, 2);
  
  sigma_b_mat ~ normal(0, 1);
  sigma_b_site ~ normal(0, 1);
  sigma_b_cc ~ normal(0, 1);
  
  a_sp ~ normal(mu_a, sigma_a);
  
  b_mat ~ normal(mu_b_mat, sigma_b_mat);
  b_site ~ normal(mu_b_site, sigma_b_site);
  b_cc ~ normal(mu_b_cc, sigma_b_cc);
  
  fs ~ multi_normal_cholesky(rep_vector(0, N), L_cov);
  
}


