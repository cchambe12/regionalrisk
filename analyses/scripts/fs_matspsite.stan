// Stan Model attempt for FS# ~ MAT + Site + Spp + CC 
// Started 19 January 2018 by Cat


data {
  int<lower=0> N;
  vector[N] fs;
  vector[N] mat;
  vector[N] sp;
  vector[N] site;
  vector[N] cc;
  
}

parameters {
  vector[N] b_sp;
  vector[N] b_mat;
  vector[N] b_site;
  vector[N] b_cc;
  
  real mu_b_sp;
  real mu_b_mat;
  real mu_b_site;
  real mu_b_cc;
  
  real<lower=0> sigma_b_mat;
  real<lower=0> sigma_b_site;
  real<lower=0> sigma_b_cc;
  
  real<lower=0> sigma_b_sp;
  
  real<lower=0> sigma_y;
  
}

transformed parameters { 
  vector[N] y_hat;
  
	for(i in 1:N)
		y_hat[i] = b_sp[i] * sp[i] +
		b_mat[i] * mat[i] + 
		b_site[i] * site[i] + 
		b_cc[i] * cc[i]
		;
	
}

model {
  mu_b_mat ~ normal(0, 2);
  mu_b_site ~ normal(0, 2);
  mu_b_cc ~ normal(0, 2);
  mu_b_sp ~ normal(0, 2);
  
  sigma_b_mat ~ normal(0, 1);
  sigma_b_site ~ normal(0, 1);
  sigma_b_cc ~ normal(0, 1);
  sigma_b_sp ~ normal(0, 1);
  
  b_sp ~ normal(mu_b_sp, sigma_b_sp);
  b_mat ~ normal(mu_b_mat, sigma_b_mat);
  b_site ~ normal(mu_b_site, sigma_b_site);
  b_cc ~ normal(mu_b_cc, sigma_b_cc);
  
	fs ~ normal(y_hat, sigma_y);

}

