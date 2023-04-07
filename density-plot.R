# load packages ----------------------------------------------------------------

library(tidyverse)
library(rlang)

# function: density-plot -------------------------------------------------------

density_plot <- function(data, x, color, ymax, xmin, xmax, alpha) {
  ggplot2::ggplot(data = data, aes({{x}}))+
    geom_density(color = "black", fill = color, linewidth = 0.9, bw = 0.3, alpha = alpha)+
    theme_classic()+
    scale_y_continuous(
      limits = c(0, ymax),
      expand = c(0, 0))+
    scale_x_continuous(
      limits = c(xmin, xmax),
      expand = c(0, 0.01))
}
