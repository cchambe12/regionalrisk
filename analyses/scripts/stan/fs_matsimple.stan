// Stan Model attempt for FS# ~ MAT + Site + Spp + lon 
// Started 19 January 2018 by Cat


data {
  int<lower=0> N;
  vector[N] fs;
  vector[N] mat;
  vector[N] sp;
  vector[N] lat;
  vector[N] lon;
  
}

parameters {
  vector[N] a;
  vector[N] b_sp;
  vector[N] b_mat;
  vector[N] b_lat;
  vector[N] b_lon;
  
  real mu_a;
  real mu_b_sp;
  real mu_b_mat;
  real mu_b_lat;
  real mu_b_lon;
  
  real<lower=0> sigma_b_mat;
  real<lower=0> sigma_b_lat;
  real<lower=0> sigma_b_lon;
  real<lower=0> sigma_b_sp;
  
  real<lower=0> sigma_a;
  
  real<lower=0> sigma_y;
  
}

transformed parameters { 
  vector[N] y_hat;
  
	for(i in 1:N)
		y_hat[i] = a[i] +
		b_sp[i] * sp[i] +
		b_mat[i] * mat[i] + 
		b_lat[i] * lat[i] + 
		b_lon[i] * lon[i]
		;
	
}

model {
  mu_b_mat ~ normal(0, 2);
  mu_b_lat ~ normal(0, 2);
  mu_b_lon ~ normal(0, 2);
  mu_b_sp ~ normal(0, 2);
  
  sigma_b_mat ~ normal(0, 1);
  sigma_b_lat ~ normal(0, 1);
  sigma_b_lon ~ normal(0, 1);
  sigma_b_sp ~ normal(0, 1);
  
  a ~ normal(mu_a, sigma_a);
  
  b_sp ~ normal(mu_b_sp, sigma_b_sp);
  b_mat ~ normal(mu_b_mat, sigma_b_mat);
  b_lat ~ normal(mu_b_lat, sigma_b_lat);
  b_lon ~ normal(mu_b_lon, sigma_b_lon);
  
	fs ~ normal(y_hat, sigma_y);

}

