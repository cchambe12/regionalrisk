// Stan Model attempt for FS# ~ MAT + Site + Spp + CC 
// Started 19 January 2018 by Cat


data {
  int<lower=0> N;
  real fs[N];
  real<lower=0> sigma_y[N];
  
}

parameters {
  vector[N] mat;
  vector[N] sp;
  vector[N] site;
  vector[N] cc;
}


transformed parameters { 
  real y_hat[N];
  
	for(i in 1:N)
		y_hat[i] = mat[i] + sp[i] + site[i] + cc[i] * sigma_y[i];
	
}

model {
	fs ~ normal(y_hat, sigma_y);

}

