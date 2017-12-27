library(pacman)
p_load(tidyverse)

f <- function(mf, model) {
  params = mf$ggs[[paste0('ggs_', model)]] %>% dplyr::select(Parameter, ParameterOriginal) %>%
    distinct() %>%
    mutate(Parameter = str_replace_all(Parameter, 'plain\\("|"\\)',""))
  sfit = mf$fits[[paste0("sfit_", model)]]

  pars = c("beta")
  if (any(str_detect(params, "gamma"))) pars = c(pars, "gamma")

  sfit %>% tidy(pars = pars, conf.int = TRUE) %>% mutate(term = map_chr(term, ~params$Parameter[.x == params$ParameterOriginal]))
}
