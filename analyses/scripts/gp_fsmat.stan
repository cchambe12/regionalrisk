// Gaussian Process Model for Regional Risk Analysis
// Started by Cat - 16 January 2018

data {
  int<lower=1> N;
  real x[N];
  
  real<lower=0> mat;
  real<lower=0> sp;
  real<lower=0> site;
  real<lower=0> cc;
  
}

transformed data {
  matrix[N, N] cov =   cov_exp_quad(x, mat, site)
                     + diag_matrix(rep_vector(1e-10, N));
  matrix[N, N] L_cov = cholesky_decompose(cov);
}

model {
  
  mat ~ normal(0, 1);
  sp ~ normal(0, 2);
  site ~ normal(0, 1);
  cc ~ normal(0, 1);
  
}


generated quantities {
  vector[N] f = multi_normal_cholesky_rng(rep_vector(0, N), L_cov);
  vector[N] fs;
  for (i in 1:N)
    fs[i] = normal_rng(f[i], 0.1);
    
}
