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
  
  real mu_a[N];
  real mu_b_mat;
  real mu_b_sp;
  real mu_b_cc;
  
}

transformed parameters {
  
  matrix[N, N] cov =   cov_exp_quad(mu_a, mu_b_mat, mu_b_sp)
                     + diag_matrix(rep_vector(1e-10, N));
  matrix[N, N] L_cov = cholesky_decompose(cov);
  
}

model {
  
  fs ~ multi_normal_cholesky(rep_vector(0, N), L_cov);
  
}


