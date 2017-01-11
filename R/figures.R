# Theme for partial plots
partial_plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey', size=0.7) else element_blank()
  st <- if(strips==TRUE) element_text() else element_blank()
  theme_classic(base_size = 8) + theme(strip.text = st,
                          legend.title = element_blank(),
                          strip.background = sb,
                          legend.position = legend.position,
                          plot.margin = unit(c(3,3,3,3), "mm"),
                          axis.text = element_text(colour="black"),
                          axis.line = element_blank(),
                          panel.border = element_rect(fill=NA, colour ="black"),
                          axis.ticks = element_line(size =0.3, colour="black"),
                          axis.ticks.length=unit(-0.7, "mm"),
                          axis.text.x = element_text(margin=unit(c(2,0,0,0), "mm")),
                          axis.text.y = element_text(margin=unit(c(0,2,0,0), "mm")))
}

# Coefficient plot theme
coefficent_plot_theme <- function() {
  theme_classic(base_size = 8) + theme(
                          axis.title.y = element_blank(),
                          plot.margin = unit(c(3,3,3,3), "mm"),
                          axis.line = element_blank(),
                          axis.title = element_text(margin=c(0,-0.5,0,0)),
                          axis.text = element_text(colour="black"),
                          panel.border = element_rect(fill=NA, colour ="black"),
                          axis.ticks = element_line(size =0.3, colour="black"),
                          axis.ticks.length=unit(-0.7, "mm"),
                          axis.text.x = element_text(margin=unit(c(2,0,0,0), "mm")),
                          axis.text.y = element_text(margin=unit(c(0,2,0,0), "mm")))
}

# Plot Observations
plot_ht_obs <- function(data, ylab='Height (cm)',xlab = "Days") {
  obs <- summarise_growth_obs(data) %>%
    mutate(transplant = factor(sapling, labels=c('Seedlings','Saplings')))
  ggplot(obs, aes(x= days, y=mean, group= plot_location)) +
    geom_path(aes(linetype= plot_location), position = position_dodge(7)) +
    scale_linetype_manual("Timberline position",values= c("-50" ="longdash","0" ="solid","100"="dotted")) +
    geom_linerange(aes(ymin= lower_95ci, ymax=upper_95ci), position = position_dodge(7), show.legend = FALSE) +
    geom_point(aes(shape=plot_location),position=position_dodge(7)) +
    scale_shape_manual("Timberline position", values = c(16, 15, 17)) +
    ylim(0,60) +
    ylab(ylab) +
    xlab(xlab) +
    partial_plot_theme(strips = TRUE) +
    facet_wrap(~transplant, ncol=2,scale='fixed') +
    theme(legend.position=c(0.87, .17), legend.title = element_text())
}

# Plot coefficients
coefficient_plot <- function(model, params, y_axis_labels='',xlab ='Effect size', title=NULL) {
  summarised_coefficients <- summarise_coefficients(model,params = params)
  ggplot(summarised_coefficients, aes(x = mean,y = parameter)) + 
    geom_segment(aes(x=`2.5%`,y=parameter, xend=`97.5%`, yend=parameter), size=0.25)+
    geom_segment(aes(x=`10%`,y=parameter, xend=`90%`, yend=parameter), size=0.5)+
    geom_point(aes(x=mean, y=parameter)) +
    geom_vline(aes(xintercept=0), linetype=2) +
    scale_y_discrete(labels =rev(y_axis_labels)) +
    xlab(xlab) +
    scale_x_continuous(breaks= scales::pretty_breaks(6), limits=c(-0.2,9)) +
    labs(title=title) +
    coefficent_plot_theme()
}

# Multipanel plot for growth analysis
multi_panel_growth_coefficients <- function(seedling_model,sapling_model, params ='alpha',y_axis_labels, xlab) {
  p1 <- coefficient_plot(seedling_model,params, y_axis_labels, xlab=xlab, title ="Seedlings")
  p2 <- coefficient_plot(sapling_model,params, y_axis_labels, xlab = xlab, title ="Saplings")
  plot_grid(p1,p2, labels=paste0(letters[1:2],')'), ncol=1, label_size = 8)
}

# Plot density model predictions
plot_density_predictions <- function(model, 
                                     predictions = 'seedling_density', 
                                     ylab = expression('Seedling density'~(m^{-2})), 
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
  p1 <- plot_density_predictions(model, predictions ='mu_distances',ylab ='Mean distance to seedling (m)')
  p2 <- plot_density_predictions(model)
  plot_grid(p1,p2, labels=paste0(letters[1:2],')'), ncol=1, label_size = 8)
}