// Gaussian Process Model for Regional Risk Analysis
// Started by Cat - 16 January 2018

data {
  int<lower=1> N;
  vector[N] fs;
  vector[N] mat;
  vector[N] sp;
  vector[N] site;
  vector[N] cc;
  
}

parameters{
  vector[N] a;
  vector[N] b_mat;
  vector[N] b_sp;
  vector[N] b_site;
  vector[N] b_cc;
  
  real mu_a[N];
  real mu_b_mat;
  real mu_b_sp;
  real mu_b_site;
  real mu_b_cc;
  
  real sigma_y;
  
}

transformed parameters {
  matrix[N, N] cov =   cov_exp_quad(mu_a, mu_b_site, sigma_y)
                     + diag_matrix(rep_vector(1e-10, N));
  matrix[N, N] L_cov = cholesky_decompose(cov);
}

model {
  
  fs ~ multi_normal_cholesky(rep_vector(0, N), L_cov);
  
}

