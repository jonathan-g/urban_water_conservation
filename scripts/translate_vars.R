#
# Translate model_numbers
#
library(tidyverse)
library(rstan)

data_dir = "data"

msa_vars_old_1 <- c('pvi', 'rpp', 'log.pop', 'pop.growth',
                'precip', 'temp' #, 'surface.water'
)

state_vars_old_1 <- c('state.pvi', 'state.rpp',
                  'state.precip', 'state.temp' #, 'state.surface.water'
)

captions_vars_old_1 <- list( year_fig = "1985-2014 with temp and precip",
                         vars_fig = "with temp and precip",
                         year_tbl = "1985--2014 with temp.\\ and precip.",
                         vars_tbl = "with temp.\\ and precip.")

vars_old_1 <- list(msa_vars = msa_vars_old_1, state_vars = state_vars_old_1,
               captions = captions_vars_old_1)

#
# Original analysis set.
#
msa_vars_old_2 <- c('pvi', 'rpi', 'log.pop', 'pop.growth',
                'aridity' #,'surface.water'
)

state_vars_old_2 <- c('state.pvi', 'state.rpi',
                  'state.aridity' #, 'state.surface.water'
)

captions_vars_old_2 <- list( year_fig = "1985-2014",
                         vars_fig = "baseline",
                         year_tbl = "1985--2014",
                         vars_tbl = "baseline")

vars_old_2 <- list(msa_vars = msa_vars_old_2, state_vars = state_vars_old_2,
               captions = captions_vars_old_2)


#
# Population density instead of population
#
msa_vars_old_3 <- c('pvi', 'rpi', 'log.pop.dens', 'pop.dens.growth',
                'aridity' #,'surface.water'
)

state_vars_old_3 <- c('state.pvi', 'state.rpi',
                  'state.aridity' #, 'state.surface.water'
)

captions_vars_old_3 <- list( year_fig = "1985-2014 with population density",
                         vars_fig = "with population density",
                         year_tbl = "1985--2014 with pop.\\ density",
                         vars_tbl = "with pop.\\ density")

vars_old_3 <- list(msa_vars = msa_vars_old_3, state_vars = state_vars_old_3,
               captions = captions_vars_old_3)


#
# Include MSA area as a predictor
#
msa_vars_old_4 <- c('pvi', 'rpi', 'log.pop', 'pop.growth', 'area',
                'aridity' #,'surface.water'
)

state_vars_old_4 <- c('state.pvi', 'state.rpi',
                  'state.aridity' #, 'state.surface.water'
)

captions_vars_old_4 <- list( year_fig = "1985-2014 with MSA area",
                         vars_fig = "with MSA area",
                         year_tbl = "1985--2014 with MSA area",
                         vars_tbl = "with MSA area")

vars_old_4 <- list(msa_vars = msa_vars_old_4, state_vars = state_vars_old_4,
               captions = captions_vars_old_4)

#
# Include Gini coefficient as a predictor
#
msa_vars_old_5 <- c('pvi', 'rpi', 'gini', 'log.pop', 'pop.growth',
                'aridity' #,'surface.water'
)

state_vars_old_5 <- c('state.pvi', 'state.rpi', 'state.gini',
                  'state.aridity' #, 'state.surface.water'
)

captions_vars_old_5 <- list( year_fig = "1985-2014 with MSA Gini",
                         vars_fig = "with MSA Gini",
                         year_tbl = "1985--2014 with MSA Gini coefficient",
                         vars_tbl = "with MSA Gini coefficient")

vars_old_5 <- list(msa_vars = msa_vars_old_5, state_vars = state_vars_old_5,
               captions = captions_vars_old_5)

#
# Consider aridity trend since 1985 (previous 30 years)
#
msa_vars_old_6 <- c('pvi', 'rpi', 'log.pop', 'pop.growth',
                'aridity_70' #,'surface.water'
)

state_vars_old_6 <- c('state.pvi', 'state.rpi',
                  'state.aridity_70' #, 'state.surface.water'
)

captions_vars_old_6 <- list( year_fig = "1970-2014",
                         vars_fig = "1970-2014",
                         year_tbl = "1970--2014",
                         vars_tbl = "1970--2014")

vars_old_6 <- list(msa_vars = msa_vars_old_6, state_vars = state_vars_old_6,
               captions = captions_vars_old_6)

#
# Consider aridity trend since 1995 (previous 20 years)
#
msa_vars_old_7 <- c('pvi', 'rpi', 'log.pop', 'pop.growth',
                'aridity_95' #,'surface.water'
)

state_vars_old_7 <- c('state.pvi', 'state.rpi',
                  'state.aridity_95' #, 'state.surface.water'
)

captions_vars_old_7 <- list( year_fig = "1995-2014",
                         vars_fig = "1995-2014",
                         year_tbl = "1995--2014",
                         vars_tbl = "1995--2014")

vars_old_7 <- list(msa_vars = msa_vars_old_7, state_vars = state_vars_old_7,
               captions = captions_vars_old_7)

#
# Consider aridity trend since 2005 (previous 10 years)
#
msa_vars_old_8 <- c('pvi', 'rpi', 'log.pop', 'pop.growth',
                'aridity_05' #,'surface.water'
)

state_vars_old_8 <- c('state.pvi', 'state.rpi',
                  'state.aridity_05' #, 'state.surface.water'
)

captions_vars_old_8 <- list( year_fig = "2005-2014",
                         vars_fig = "2005-2014",
                         year_tbl = "2005--2014",
                         vars_tbl = "2005--2014")

vars_old_8 <- list(msa_vars = msa_vars_old_8, state_vars = state_vars_old_8,
               captions = captions_vars_old_8)

#
# Omit PVI
#
msa_vars_old_9 <- c('rpi', 'log.pop', 'pop.growth',
                'aridity' #,'surface.water'
)

state_vars_old_9 <- c('state.rpi',
                  'state.aridity' #, 'state.surface.water'
)

captions_vars_old_9 <- list( year_fig = "1985-2014 without PVI",
                         vars_fig = "without PVI",
                         year_tbl = "1985--2014 without PVI",
                         vars_tbl = "without PVI")

vars_old_9 <- list(msa_vars = msa_vars_old_9, state_vars = state_vars_old_9,
               captions = captions_vars_old_9)

#
# Omit MSA-level PVI but include state-level
#
msa_vars_old_10 <- c('rpi', 'log.pop', 'pop.growth',
                 'aridity' #,'surface.water'
)

state_vars_old_10 <- c('state.pvi', 'state.rpi',
                   'state.aridity' #, 'state.surface.water'
)

captions_vars_old_10 <- list( year_fig = "1985-2014 without MSA PVI",
                          vars_fig = "without MSA-level PVI",
                          year_tbl = "1985--2014 without MSA PVI",
                          vars_tbl = "without MSA-level PVI")

vars_old_10 <- list(msa_vars = msa_vars_old_10, state_vars = state_vars_old_10,
                captions = captions_vars_old_10)

#
# Omit MSA-level PVI but include state-level and use population density at the MSA level
#
msa_vars_old_11 <- c('rpi', 'log.pop.dens', 'pop.dens.growth',
                 'aridity' #,'surface.water'
)

state_vars_old_11 <- c('state.pvi', 'state.rpi',
                   'state.aridity' #, 'state.surface.water'
)

captions_vars_old_11 <- list( year_fig = "1985-2014 with density, no MSA PVI",
                          vars_fig = "with density, no MSA PVI",
                          year_tbl = "1985--2014 with density, no MSA PVI",
                          vars_tbl = "with density, no MSA PVI")

vars_old_11 <- list(msa_vars = msa_vars_old_11, state_vars = state_vars_old_11,
                captions = captions_vars_old_11)

#
# Omit state-level PVI but include MSA-level
#
msa_vars_old_12 <- c('pvi', 'rpi', 'log.pop', 'pop.growth',
                 'aridity' #,'surface.water'
)

state_vars_old_12 <- c('state.rpi',
                   'state.aridity' #, 'state.surface.water'
)

captions_vars_old_12 <- list( year_fig = "1985-2014 without state PVI",
                          vars_fig = "without state-level PVI",
                          year_tbl = "1985--2014 without state PVI",
                          vars_tbl = "without state-level PVI")

vars_old_12 <- list(msa_vars = msa_vars_old_12, state_vars = state_vars_old_12,
                captions = captions_vars_old_12)

#
# Omit state PVI and use population density
#
msa_vars_old_13 <- c('pvi', 'rpi', 'log.pop.dens', 'pop.dens.growth',
                 'aridity' #, 'surface.water'
)

state_vars_old_13 <- c('state.rpi',
                   'state.aridity' #, 'state.surface.water'
)

captions_vars_old_13 <- list( year_fig = "1985-2014 with density, no state PVI",
                          vars_fig = "with pop.\\ density, no state PVI",
                          year_tbl = "1985--2014 with density, no state PVI",
                          vars_tbl = "with pop.\\ density, no state PVI")

vars_old_13 <- list(msa_vars = msa_vars_old_13, state_vars = state_vars_old_13,
                captions = captions_vars_old_13)

#
# Omit aridity and state PVI
#
msa_vars_old_14 <- c('pvi', 'rpi', 'log.pop', 'pop.growth' #, 'surface.water'
)

state_vars_old_14 <- c('state.rpi' #, 'state.surface.water'
)

captions_vars_old_14 <- list( year_fig = "1985-2014 without aridity or state PVI",
                          vars_fig = "without aridity or state PVI",
                          year_tbl = "1985--2014 without aridity or state PVI",
                          vars_tbl = "without aridity or state PVI")

vars_old_14 <- list(msa_vars = msa_vars_old_14, state_vars = state_vars_old_14,
                captions = captions_vars_old_14)

#
# Omit state aridity and state PVI
#
msa_vars_old_15 <- c('pvi', 'rpi', 'log.pop', 'pop.growth',
                 'aridity'#, 'surface.water'
)

state_vars_old_15 <- c('state.rpi' #, 'state.surface.water'
)

captions_vars_old_15 <- list( year_fig = "1985-2014 without state aridity or PVI",
                          vars_fig = "without state aridity or PVI",
                          year_tbl = "1985--2014 without state aridity or PVI",
                          vars_tbl = "without state aridity or PVI")

vars_old_15 <- list(msa_vars = msa_vars_old_15, state_vars = state_vars_old_15,
                captions = captions_vars_old_15)

#
# Omit MSA aridity and state PVI
#
msa_vars_old_16 <- c('pvi', 'rpi',
                 'log.pop.dens', 'pop.dens.growth' #, 'surface.water'
)

state_vars_old_16 <- c('state.rpi',
                   'state.aridity' #, 'state.surface.water'
)

captions_vars_old_16 <- list( year_fig = "1985-2014 without MSA aridity or state PVI",
                          vars_fig = "without MSA aridity or state PVI",
                          year_tbl = "1985--2014 without MSA aridity or state PVI",
                          vars_tbl = "without MSA aridity or state PVI")

vars_old_16 <- list(msa_vars = msa_vars_old_16, state_vars = state_vars_old_16,
                captions = captions_vars_old_16)

#
# Add Surface water, use density
#
msa_vars_old_17 <- c('pvi', 'rpi', 'log.pop.dens', 'pop.dens.growth',
                 'aridity', 'surface.water'
)

state_vars_old_17 <- c('state.pvi', 'state.rpi',
                   'state.aridity', 'state.surface.water'
)

captions_vars_old_17 <- list( year_fig = "1985-2014 with pop. density and surface water",
                          vars_fig = "with pop. density and surface water",
                          year_tbl = "1985--2014 with pop.\\ density and surface water",
                          vars_tbl = "with pop.\\ density and surface water")

vars_old_17 <- list(msa_vars = msa_vars_old_17, state_vars = state_vars_old_17,
                captions = captions_vars_old_17)


#
# Add Surface water
#
msa_vars_old_18 <- c('pvi', 'rpi', 'log.pop', 'pop.growth',
                 'aridity', 'surface.water'
)

state_vars_old_18 <- c('state.pvi', 'state.rpi',
                   'state.aridity', 'state.surface.water'
)

captions_vars_old_18 <- list( year_fig = "1985-2014 with surface water",
                          vars_fig = "with surface water",
                          year_tbl = "1985--2014 with surface water",
                          vars_tbl = "with surf water")

vars_old_18 <- list(msa_vars = msa_vars_old_18, state_vars = state_vars_old_18,
                captions = captions_vars_old_18)


#
# Omit Aridity, use density
#
msa_vars_old_19 <- c('pvi', 'rpi',
                 'log.pop.dens', 'pop.dens.growth'#, 'surface.water'
)

state_vars_old_19 <- c('state.pvi', 'state.rpi' #, 'state.surface.water'
)

captions_vars_old_19 <- list( year_fig = "1985-2014 with pop. density, no aridity",
                          vars_fig = "with pop. density, no aridity",
                          year_tbl = "1985--2014 with pop.\\ density, no aridity",
                          vars_tbl = "with pop.\\ density, no aridity")

vars_old_19 <- list(msa_vars = msa_vars_old_19, state_vars = state_vars_old_19,
                captions = captions_vars_old_19)

#
# Omit Aridity
#
msa_vars_old_20 <- c('pvi', 'rpi', 'log.pop', 'pop.growth' #,'surface.water'
)

state_vars_old_20 <- c('state.pvi', 'state.rpi' #, 'state.surface.water'
)

captions_vars_old_20 <- list( year_fig = "1985-2014 without aridity",
                          vars_fig = "without aridity",
                          year_tbl = "1985--2014 without aridity",
                          vars_tbl = "without aridity")

vars_old_20 <- list(msa_vars = msa_vars_old_20, state_vars = state_vars_old_20,
                captions = captions_vars_old_20)


msa_vars_old_21 <- c('aridity', 'pvi', 'rpi', 'log.pop', 'pop.growth',
                 'pvi.aridity', 'pvi.state.aridity' #,'surface.water'
)

state_vars_old_21 <- c('state.aridity', 'state.pvi', 'state.rpi',
                   'state.pvi.aridity' #, 'state.surface.water'
)

captions_vars_old_21 <- list( year_fig = "1985-2014 with PVI/aridity interactions",
                          vars_fig = "with PVI/aridity interactions",
                          year_tbl = "1985--2014  with PVI/aridity interactions",
                          vars_tbl = "with PVI/aridity interactions")

vars_old_21 <- list(msa_vars = msa_vars_old_21, state_vars = state_vars_old_21,
                captions = captions_vars_old_21)


model_vars_old <- list(vars_1 = vars_old_1, vars_2 = vars_old_2, vars_3 = vars_old_3,
                   vars_4 = vars_old_4, vars_5 = vars_old_5,
                   vars_6 = vars_old_6, vars_7 = vars_old_7, vars_8 = vars_old_8,
                   vars_9 = vars_old_9, vars_10 = vars_old_10,
                   vars_11 = vars_old_11, vars_12 = vars_old_12, vars_13 = vars_old_13,
                   vars_14 = vars_old_14, vars_15 = vars_old_15,
                   vars_16 = vars_old_16, vars_17 = vars_old_17, vars_18 = vars_old_18,
                   vars_19 = vars_old_19, vars_20 = vars_old_20, vars_21 = vars_old_21)


vars_xlate <- c(vars_1 = "vars_2", vars_2 = "vars_3", vars_3 = "vars_4", vars_4 = "vars_5", vars_5 = "vars_6",
                   vars_6 = "vars_7", vars_7 = "vars_8", vars_8 = "vars_9", vars_9 = "vars_18", vars_10 = "vars_20",
                   vars_11 = "vars_21") %>% set_names(names(.), .)

sfit_xlate <- vars_xlate %>% map_chr(~str_replace(.x, "vars_([0-9]+)$", "sfit_\\1_")) %>%
  set_names(., str_replace(names(.), "vars_([0-9]+)$", "sfit_\\1_"))

ggs_xlate <- vars_xlate %>% map_chr(~str_replace(.x, "vars_([0-9]+)$", "ggs_\\1_")) %>%
  set_names(., str_replace(names(.), "vars_([0-9]+)$", "ggs_\\1_"))

res_xlate <- vars_xlate %>% map_chr(~str_replace(.x, "vars_([0-9]+)$", "res_\\1_")) %>%
  set_names(., str_replace(names(.), "vars_([0-9]+)$", "res_\\1_"))


translate_set <- function(x) {
  x$fits <- x$fits %>% set_names(str_replace_all(names(.), sfit_xlate))
  x$ggs <- x$ggs %>% set_names(str_replace_all(names(.), ggs_xlate))
  x$res <- x$res %>% set_names(str_replace_all(names(.), res_xlate))
  invisible(x)
}

model_fits <- read_rds(file.path(data_dir, "model_fits.Rds"))
test_model_fits <- read_rds(file.path(data_dir, "test_model_fits.Rds"))
pooled_model_fits <- read_rds(file.path(data_dir, "test_model_fits_pooled.Rds"))

new_model_fits <- translate_set(model_fits)
new_test_model_fits <- translate_set(test_model_fits)
new_pooled_model_fits <- translate_set(pooled_model_fits)

write_rds(new_model_fits, file.path(data_dir, "model_fits.Rds"))
write_rds(new_test_model_fits, file.path(data_dir, "test_model_fits.Rds"))
write_rds(new_pooled_model_fits, file.path(data_dir, "test_model_fits_pooled.Rds"))
