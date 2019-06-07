// Gaussian Process Model for Regional Risk Analysis
// Started by Cat - 16 January 2018

data {
  int<lower=1> N;
  vector[N] fs;
  
  real site[N];
  real sp;
  real mat;
  real cc;
  
}

transformed data {
  matrix[N, N] cov =   cov_exp_quad(site, mat, sp)
                     + diag_matrix(rep_vector(1e-10, N));
  matrix[N, N] L_cov = cholesky_decompose(cov);
}

model {
  
  fs ~ multi_normal_cholesky(rep_vector(0, N), L_cov);
  
}

