// Gaussian Process Model for Regional Risk Analysis
// Started by Cat - 16 January 2018

data {
  int<lower=1> N;
  real mat[N];
  vector[N] fs;
}

parameters {
  real<lower=0> site;
  real<lower=0> cc;
  real<lower=0> sigma;
}

model {
  matrix[N, N] cov =   cov_exp_quad(mat, cc, site)
                     + diag_matrix(rep_vector(square(sigma), N));
  matrix[N, N] L_cov = cholesky_decompose(cov);

  fs ~ multi_normal_cholesky(rep_vector(0, N), L_cov);
}