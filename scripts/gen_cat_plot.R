library(pacman)
p_load(tidyverse)

# source('scripts/fit_model.R')

generate_cat_plot <- function(model_ggs, target, title, limits = NULL,
                              color = TRUE, legend = FALSE,
                              levels = NULL) {
  model_ggs <- model_ggs %>% mutate(Parameter = fct_relabel(Parameter, function(x) str_replace_all(x, "aridity_[0-9]+", "aridity")))
  if (is.null(levels)) {
    param_levels = model_ggs$Parameter %>% levels()
  } else {
    param_levels = levels %>% str_replace_all("aridity_[0-9]+", "aridity")
  }

  p <- model_ggs %>%
    make_cat_plot(target = target, family_name = NULL, title = title,
                  thin_ci = thin_ci, thick_ci = thick_ci, param_levels = param_levels) +
    aes(shape = family, color = family)
  if (color) {
    p <- p + scale_color_manual(values = set_names(brewer.pal(3, 'Dark2')[2:3],
                                                   c('MSA-level', 'State-level')),
                                labels = c('MSA-level' = 'MSA', 'State-level' = 'State'),
                                name = 'Level')
  } else {
    p <- p + scale_color_manual(values = c('MSA-level' = "gray60", 'State-level' = 'black'),
                                labels = c('MSA-level' = 'MSA', 'State-level' = 'State'),
                                name = 'Level')
  }
  p <- p + scale_shape_manual(values = c('MSA-level' = 16, 'State-level' = 15),
                              labels = c('MSA-level' = 'MSA', 'State-level' = 'State'),
                              name = 'Level') +
    scale_x_continuous(breaks = seq(-1,1,0.1), limits=limits) +
    theme_get() +
    theme(panel.grid.minor.y = element_blank(),
          legend.box.margin = margin(t = 0.02, l = 0.1, b = 0.02, r = 0.1, unit = 'lines'),
          legend.background = element_rect(color = "gray50", fill = "white", size=0.25),
          legend.key = element_rect(color = NA),
          legend.position = ifelse(is.logical(legend), ifelse(legend, c(0.99, 0.01), "none"), legend),
          legend.justification = c(1,0),
          legend.key.height = unit(1, "lines"),
          legend.key.width = unit(2, 'lines')
    )
  p
}
