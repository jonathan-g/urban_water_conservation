# setwd("~/programming/r-projects/water_conservation/water_conserve/VWCI_May2016/PNAS_Paper")
source('scripts/load_data.R', chdir = T)
source('scripts/fit_model.R', chdir = T)
vwci <- load_data('data')
data_std <- standardize_data(vwci)
vwci_std <- data_std$vwci

sfit_2 <- fit_model_vary_slope_intercept(vwci_std)

g_2 <- make_ggs_var_slope_intercept(sfit_2, default_vars, family = '^(alpha|beta)[^_]')
g_2$state <- str_replace_all(g_2$Parameter, '.*\\[(.+\\.)?([A-Za-z]+)\\]', '\\2')
foo_2 <- data_frame(Parameter = levels(g_2$Parameter), stringsAsFactors = FALSE) %>%
  arrange(as.character(Parameter)) %>% mutate(value = row_number())
p2 <- ggs_caterpillar(g_2, X = foo_2, greek = T) + aes(color = state) +
  geom_vline(xintercept = 0) +
  scale_color_brewer(palette = 'Dark2', name = 'State') +
  scale_y_continuous(breaks = foo_2$value,
                     labels = unlist(lapply(foo_2$Parameter, function(x) parse(text = x)))) +
  xlim(-2,1.5) +
  labs(y = NULL, x = NULL, title = "No correlations") +
  theme_bw(base_size = 15)
print(p2)
pdf(file = 'p2.pdf')
print(p2)
dev.off()

sfit_corr <- fit_model_vary_slope_intercept_corr(vwci_std, accept = 0.99, max_treedepth = 15)

g_corr <- make_ggs_var_slope_intercept_corr(sfit_corr, default_vars, family = '^beta')
g_corr$state <- str_replace_all(g_corr$Parameter, '.*\\[(.+\\.)?([A-Za-z]+)\\]', '\\2')
foo_corr <- data_frame(Parameter = levels(g_corr$Parameter), stringsAsFactors = FALSE) %>%
  arrange(as.character(Parameter)) %>% mutate(value = row_number())
p_corr <- ggs_caterpillar(g_corr, X = foo_corr, greek = T) + aes(color = state) +
  geom_vline(xintercept = 0) +
  scale_color_brewer(palette = 'Dark2', name = 'State') +
  scale_y_continuous(breaks = foo_corr$value,
                     labels = unlist(lapply(foo_corr$Parameter, function(x) parse(text = x)))) +
  xlim(-2,1.5) +
  labs(y = NULL, x = NULL, title = "Correlations") +
  theme_bw(base_size = 15)
print(p_corr)
pdf(file = 'p_corr.pdf')
print(p_corr)
dev.off()
