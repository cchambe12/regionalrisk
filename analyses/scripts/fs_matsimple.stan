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
  vector[5] beta;
   
  real<lower=0> sigma_y; 
}

model {
	fs ~ normal(beta[1] + beta[2] * mat + beta[3] * sp + beta[4] * site + beta[5] * cc, sigma_y);

}

