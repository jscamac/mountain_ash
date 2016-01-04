# Theme for non-coefficient plots
partial_plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey') else element_blank()
  st <- if(strips==TRUE) element_text(color = "black", face = "bold", size = 10) else element_blank()
  theme_classic() + theme(axis.title = element_text(face = "bold", size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = st,
                          legend.title = element_text(size = 10),
                          legend.text = element_text(size = 10), 
                          plot.title = element_text(size = 18),
                          legend.title = element_blank(),
                          strip.background = sb,
                          legend.position = legend.position,
                          panel.margin = unit(4,"mm"))
}

# Theme for coefficient plots
coefficent_plot_theme <- function(strips = FALSE) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey') else element_blank()
  st <- if(strips==TRUE) element_text(color = "black", face = "bold", size = 10) else element_blank()
  theme_classic() + theme(axis.title.x = element_text(face = "bold", size = 12),
                          axis.title.y = element_blank(),
                          strip.text = st,
                          strip.background = sb,
                          axis.text.x = element_text(size = 10),
                          axis.text.y = element_text(size = 12),
                          plot.title = element_text(size = 18),
                          title = element_text(face = "bold"),
                          panel.margin = unit(4,"mm"))
}

# Plot Observations
plot_ht_obs <- function(data, ylab='Height (cm)',xlab = "Days") {
  obs <- summarise_growth_obs(data) %>%
    mutate(transplant = factor(sapling, labels=c('Seedlings','Saplings')))
  ggplot(obs, aes(x= days, y=mean, col = plot_location)) +
    geom_pointrange(aes(ymin= lower_95ci, ymax=upper_95ci), position = position_dodge(3), size=1) +
    scale_colour_manual('', values = c("-50" ="red","0" ="black", "100"="blue")) +
    geom_line(position = position_dodge(3), size=1) +
    xlim(-3,150) +
    ylim(0,60) +
    ylab(ylab) +
    xlab(xlab) +
    partial_plot_theme(strips = TRUE) +
    facet_wrap(~transplant, ncol=2,scale='free') 
}

# Plot coefficients
coefficient_plot <- function(model, params, y_axis_labels='',xlab ='Effect size', title=NULL) {
  summarised_coefficients <- summarise_coefficients(model,params = params)
  ggplot(summarised_coefficients, aes(x = mean,y = parameter)) + 
    geom_segment(aes(x=`2.5%`,y=parameter, xend=`97.5%`, yend=parameter), size=0.5)+
    geom_segment(aes(x=`10%`,y=parameter, xend=`90%`, yend=parameter), size=1)+
    geom_point(aes(x=mean, y=parameter), size =3) +
    geom_vline(aes(xintercept=0), linetype=2) +
    scale_y_discrete(labels =rev(y_axis_labels)) +
    xlab(xlab) +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    labs(title=title) +
    coefficent_plot_theme()
}

# Multipanel plot for growth analysis
multi_panel_growth_coefficients <- function(seedling_model,sapling_model, params ='alpha',y_axis_labels='',xlab ='Effect size') {
  summarised_coefficients <- combine_outputs(seedling_model, sapling_model, params)
  ggplot(summarised_coefficients, aes(x = mean,y = parameter)) + 
    geom_segment(aes(x=`2.5%`,y=parameter, xend=`97.5%`, yend=parameter), size=0.5)+
    geom_segment(aes(x=`10%`,y=parameter, xend=`90%`, yend=parameter), size=1)+
    geom_point(aes(x=mean, y=parameter), size =3) +
    geom_vline(aes(xintercept=0), linetype=2) +
    scale_y_discrete(labels =rev(y_axis_labels)) +
    xlab(xlab) +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    coefficent_plot_theme(strips = TRUE) +
    facet_wrap(~sapling, scales = 'free_y')
}

# Plot density model predictions
plot_density_predictions <- function(model, 
                                     predictions = 'seedling_density', 
                                     ylab = expression(bold('Seedling density'~(m^2))), 
                                     xlab ='Distance from timberline (m)') {
  preds <- summarise_predictions(model,predictions,pred_var='sim_boundary_position')
  
  ggplot(preds, aes(x = sim_boundary_position,y = mean), axis.line = element_line()) + 
    geom_line(size=1) +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour='grey60') +
    ylab(ylab) + 
    xlab(xlab) +
    scale_x_continuous(expand=c(0,0)) + 
    partial_plot_theme()
}

density_multipanel <- function(model) {
  p1 <- plot_density_predictions(model, predictions ='mu_distances',ylab ='Mean distance to seedling (m)', xlab='')
  p1 <- p1 + geom_text(x = 10, y = 32, label='a)', size = 6)
  p2 <- plot_density_predictions(model)
  p2 <- p2 + geom_text(x = 10,y = 0.4, label='b)', size = 6)
  grid.arrange(p1,p2, ncol=1)
}


coefficient_plot <- function(model, params, y_axis_labels='',xlab ='Effect size', title=NULL) {
  summarised_coefficients <- summarise_coefficients(model,params = params)
  ggplot(summarised_coefficients, aes(x = mean,y = parameter)) + 
    geom_segment(aes(x=`2.5%`,y=parameter, xend=`97.5%`, yend=parameter), size=0.5)+
    geom_segment(aes(x=`10%`,y=parameter, xend=`90%`, yend=parameter), size=1) +
    geom_point(aes(x=mean, y=parameter), size =3) +
    geom_vline(aes(xintercept=0), linetype=2) +
    scale_y_discrete(labels =rev(y_axis_labels)) +
    xlab(xlab) +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    labs(title=title) +
    coefficent_plot_theme()
}