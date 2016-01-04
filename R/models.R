# DENSITY MODEL
run_density_model <- function(data, max_boundary_position = 221) {
  sim_boundary_position <- seq(0,max_boundary_position)
  n_sims <- length(sim_boundary_position)
  stan_data <- list(
    N = nrow(data),
    n_sites = length(unique(data$site)),
    site = data$site,
    n_transects = length(unique(data$transect_id)),
    transect = data$transect_id,
    n_points = length(unique(data$point_id)),
    point = data$point_id,
    boundary_position = data$position,
    n_obs = nrow(data[data$is_censored == 0,]),
    is_censored = data$is_censored,
    obs_index = which(data$is_censored == 0),
    n_censored = nrow(data[data$is_censored == 1,]),
    censored_index = which(data$is_censored == 1),
    y_obs = data$distance_m[data$is_censored == 0],
    sim_boundary_position = sim_boundary_position,
    n_sims = n_sims)
  
  model <-"
    data {
      # Required for estimating distances
      int<lower=1> N; 
      int<lower = 1> n_obs; 
      int<lower = 1> n_sites;
      int<lower = 1> n_transects;
      int<lower = 1> n_points;
      int<lower = 1> n_censored;
      int<lower = 1> site[N]; 
      int<lower = 1> transect[N];
      int<lower = 1> point[N];
      int<lower = 0> boundary_position[N];
      real<lower=0> y_obs[n_obs];
      # For merging back into one object
      int<lower=1, upper = N> censored_index[n_censored];
      int<lower=1, upper = N> obs_index[n_obs];
      int<lower=1> n_sims;
      int<lower=0> sim_boundary_position[n_sims];
    }
  
  parameters{
    #Estimating distances model
    real log_b_boundary_position;
    real raw_site[n_sites];
    real raw_transect[n_transects];
    real raw_point[n_points];
    real mu_log_site;
    real<lower=0> sigma_log_site;
    real<lower=0> sigma_log_transect;
    real<lower=0> sigma_log_point;
    real<lower=0> sigma_log_obs;
    real<lower=20> y_censored[n_censored];
  }
  
  transformed parameters{
    real log_alpha_site[n_sites];
    real log_transect_error[n_transects];
    real log_point_error[n_points];
    real log_y_obs_hat[n_obs];
    real log_y_censored_hat[n_censored];
    real full_data[N];
    
    # Random effect for sites
    for (s in 1:n_sites) {
      log_alpha_site[s] <- raw_site[s] * sigma_log_site + mu_log_site;
    }
    
    # Random effect for transects
    for (t in 1:n_transects) {
      log_transect_error[t] <- raw_transect[t] * sigma_log_transect;
    }
    
    # Random effect for points
    for (p in 1:n_points) {
      log_point_error[p] <- raw_point[p] * sigma_log_point;
    }
    
    # Likelihood for observed
    for (i in 1:n_obs){
      log_y_obs_hat[i] <- log_alpha_site[site[i]] + 
        log_b_boundary_position * boundary_position[i] + 
        log_transect_error[transect[i]] + 
        log_point_error[point[i]];
      
      # Adds observations to complete object
      full_data[obs_index[i]] <- y_obs[obs_index[i]];
    }
    # Estimates censored observations
    for (j in 1:n_censored){ 
      log_y_censored_hat[j] <- log_alpha_site[site[j]] + 
        log_b_boundary_position * boundary_position[j] +                                
        log_transect_error[transect[j]] + 
        log_point_error[point[j]];
      
      # Adds censored observations to complete object
      full_data[censored_index[j]] <- y_censored[j];
    }
  }
  model{
    # PRIORS
    log_b_boundary_position ~ normal(0,2.5);
    raw_site ~ normal(0,1);
    raw_transect ~ normal(0,1);
    raw_point ~ normal(0,1);
    mu_log_site ~ normal(0,2.5);
    sigma_log_site ~ cauchy(0, 25);
    sigma_log_transect ~ cauchy(0, 25);
    sigma_log_point ~ cauchy(0, 25);
    sigma_log_obs ~ cauchy(0, 25);
    
    y_obs ~ lognormal(log_y_obs_hat, sigma_log_obs);
    y_censored ~ lognormal(log_y_censored_hat, sigma_log_obs);
  }
  generated quantities {
    real mu_distances[n_sims];
    real seedling_density[n_sims];
    
    for(i in 1:n_sims) {
      mu_distances[i] <- exp(mu_log_site + log_b_boundary_position * sim_boundary_position[i]);
      seedling_density[i] <- 4*((4*1)-1)/(pi() * (4*pow(mu_distances[i],2))); # Multiplied by 4 to simulate 4 quarters.
    # see http://www.loujost.com/Statistics%20and%20Physics/PCQ/PCQJournalArticle.htm
    }
  }"
  
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  fit <-list(stan_data = stan_data, 
             fit = stan(model_code = model, 
                        data = stan_data,
                        pars = c("sigma_log_site","sigma_log_transect","sigma_log_point","mu_log_site",
                                 "sigma_log_obs","log_b_boundary_position","mu_distances","seedling_density"), 
                        chains = 3,
                        control=list(stepsize=0.01, max_treedepth =15, adapt_delta = 0.99),
                        iter = 2000))
  return(fit)
}


## GROWTH ANALYSIS
run_growth_model <- function(data, sapling = FALSE, sim_init = 1, max_sim_days = 130) {
  if(sapling == FALSE) {
    data <- subset(data, sapling ==0)
  } else {
    data <- subset(data, sapling == 1)
  }
  sim_days <- seq(0,max_sim_days)
  stan_data <- 
    list(
      n_obs = nrow(data),
      n_transects = length(unique(data$transect)),
      n_inds = length(unique(data$ind_id)),
      n_positions = length(unique(data$position_id)),
      transect = data$transect,
      ind = as.integer(as.factor(data$ind_id)),
      position = data$position_id,
      hmax = 5000,
      days = data$days,
      initial_height = data$initial_height,
      height = data$height_cm,
      sim_init = sim_init,
      n_sims = length(sim_days),
      sim_days = sim_days)
  
  model="
    data{
      int<lower=1> n_obs;
      int<lower=1> n_transects;
      int<lower=1> n_inds;
      int<lower=1> n_positions;
      int<lower=1> transect[n_obs];
      int<lower=1> ind[n_obs];
      int<lower=1> position[n_obs];
      real sim_init;
      real hmax;
      int<lower=0> days[n_obs];
      real<lower=0> initial_height[n_obs];
      real<lower=0> height[n_obs];
      int<lower=1> n_sims;
      int<lower=0> sim_days[n_sims];
    }
  parameters{ # Declare parameters the models must estimate
    real alpha[n_positions];
    real<lower=0> sigma_transect;
    real<lower=0> sigma_ind;
    real<lower=0> sigma_obs;
    real ranef_transect[n_transects];
    real ranef_ind[n_inds];
  }
  model { # Define priors and likelihood
    real R[n_obs];
    real height_hat[n_obs];
    
    #Plot random effects
    ranef_transect ~ normal(0, sigma_transect);
    
    #Estimate individual random effect
    ranef_ind ~ normal(0, sigma_ind);
    
    for (i in 1:n_obs) {
      # Estimate individual rate  
      R[i] <- 
        alpha[position[i]] +     
        ranef_transect[transect[i]] +
        ranef_ind[ind[i]];
      
      #Likelihood
      height_hat[i] <- hmax / (1 + (hmax/initial_height[i] - 1) * exp(-R[i] * (days[i]/365.25)));
    }
      height ~ normal(height_hat, sigma_obs);
    
    #Priors
    alpha ~ normal(0, 100);
    sigma_transect ~ cauchy(0,25);
    sigma_ind ~ cauchy(0,25);
    sigma_obs ~ cauchy(0,25);
  }
  generated quantities{
    real sdl_50below[n_sims];
    real sdl_timerline[n_sims];
    real sdl_100above[n_sims];
    real sapl_50below[n_sims];
    real sapl_timerline[n_sims];
    real sapl_100above[n_sims];
    
    for(i in 1:n_sims) {
      sdl_50below[i] <- hmax / (1 + (hmax/sim_init - 1) * exp(-alpha[1] * (sim_days[i]/365.25)));
      sdl_timerline[i] <- hmax / (1 + (hmax/sim_init - 1) * exp(-alpha[2] * (sim_days[i]/365.25)));
      sdl_100above[i] <- hmax / (1 + (hmax/sim_init - 1) * exp(-alpha[3] * (sim_days[i]/365.25)));
    }
  }"
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  fit <- list(stan_data = stan_data,
              fit = stan(model_code = model, 
                         data = stan_data,
                         pars = c("alpha",
                                  "sigma_transect","sigma_ind","sigma_obs",
                                  "sdl_50below","sdl_timerline","sdl_100above"), 
                         chains = 3,
                         control=list(stepsize=0.1, adapt_delta=0.99,max_treedepth =15),
                         iter = 4000))
  return(fit)
}