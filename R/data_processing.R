# Processes PCQ data for analysis
process_pcq_data <- function(csv_file) {
  data <- read.csv(csv_file)
  data %>%
    select(site = Site, transect = Transect, position = Boundary.Position, point = Point, 
           quarter = Quarter, species = Species, distance_cm = Distance_cm, dbh_cm = DBH) %>%
    mutate(
      distance_m = distance_cm/100,
      is_censored = ifelse(is.na(distance_m),1,0),
      transect_id = as.integer(as.factor(paste(site,transect, sep='_'))),
      point_id = as.integer(as.factor(paste(site,transect,point, sep='_')))) %>%
    select(site,transect,transect_id,position,point,point_id,quarter,species,distance_m,dbh_cm,is_censored) %>%
    arrange(is_censored,site,transect_id,position,quarter)
}

# Processes ht data for analysis
process_growth_data <- function(csv_file) {
  data <- read.csv(csv_file)
  data <- data %>%
    select(transect = Transect, plot_location = Plot.Location, individual = Individual,
           `0` = S1, `9` = S2,`36` = S3,`70` = S4,`100` = S5,`126` = S6, survived = Survival, sapling = Size) %>%
    filter(plot_location!='control') %>%
    gather(days, height_cm,-transect,-plot_location,-individual,-survived,-sapling) %>%
    mutate(days = as.integer(as.character(days)),
           ind_id = as.integer(as.factor(paste(transect,plot_location,individual,sapling)))) %>%
    group_by(ind_id) %>%
    mutate(initial_height = height_cm[days==0],
           position_id = as.integer(plot_location)) %>%
    filter(!is.na(height_cm))
}

# Summarises height observations
summarise_growth_obs <- function(data) {
  data %>%
    group_by(days, plot_location, sapling) %>%
    summarise(n = n(),
              ln_mn = mean(log(height_cm)),
              ln_sd = sd(log(height_cm))) %>%
    mutate(ln_ci = 1.96* (ln_sd/sqrt(n)),
           ln_lower_95_ci = ln_mn - ln_ci,
           ln_upper_95_ci = ln_mn + ln_ci) %>%
    ungroup %>%
    mutate(mean = exp(ln_mn),
           lower_95ci = exp(ln_lower_95_ci),
           upper_95ci = exp(ln_upper_95_ci)) %>%
    select(plot_location,sapling,days, n, mean, lower_95ci, upper_95ci)
}

# Summaries coefficients
summarise_coefficients <- function(model, params, quantiles = c(0.025,0.1,0.5,0.9, 0.975)) {
  samples <- rstan::extract(model$fit, pars=params)
  
  res <-lapply(samples, function(x) {
    if(is.matrix(x)) {
      df <- cbind.data.frame(
        mean = apply(x,2, mean),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, quantiles), c(2,1)))
      df$grp <- rownames(df)
      df
    }
    else {
      df <- cbind.data.frame(
        mean = mean(x),
        sd = sd(x),
        t(quantile(x, quantiles)))
      df
    }
  })
  res <-plyr::ldply(res, .id='parameter')
  if(nrow(res) > length(params)) {
    res$parameter <- factor(paste(res$parameter,res$grp, sep='_'), levels= rev(paste(res$parameter,res$grp, sep='_')))
    res <- dplyr::select(res,-grp)
  } else
    res$parameter <- factor(res$parameter, levels = rev(res$parameter))
  return(res)
}

#Combine growth analyses outputs
combine_outputs <- function(seedling_growth_model, sapling_growth_model, params) {
seedling <- summarise_coefficients(seedling_growth_model, params)
sapling <- summarise_coefficients(sapling_growth_model, params)
seedling$sapling <- factor('Seedlings')
sapling$sapling <- factor('Saplings')
res <- rbind(seedling, sapling)
return(res)
}

# Summaries predictions
summarise_predictions <- function(model, predictions, pred_var, quantiles = c(0.025, 0.975)) {
  pred_var <- data.frame(model$stan_data[pred_var])

  mu_samples <- rstan::extract(model$fit, predictions)
  
  mu_summary_df <-lapply(mu_samples, function(x) {
    df <- cbind.data.frame(
        mean = apply(x,2, mean),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, quantiles), c(2,1)))
    cbind(pred_var,df)
  })
    plyr::ldply(mu_summary_df, .id='prediction')
}
