library(tidyverse)

script_dir = "scripts"
data_dir = "data"

source(file.path(script_dir, 'load_data.R'))
pooled <- process_data(pooled = TRUE)

std_data <- pooled$standardized_data
fdata <- pooled$filtered_data
msa_data <- fdata$msa_data
state_data <- fdata$state_data

std_msa_data <- std_data$msa_data
std_state_data <- std_data$state_data

msa_means <- std_data$means
msa_sds <- std_data$sds

state_means <- std_data$state_means
state_sds <- std_data$state_sds

head(msa_data$precip)
head(std_msa_data$precip * 2.0 * msa_sds$precip + msa_means$precip)

head(msa_data$pop)
head(std_msa_data$pop * 2.0 * msa_sds$pop + msa_means$pop)

head(state_data$precip)
head(std_state_data$state.precip * 2.0 * state_sds$precip + state_means$precip)

head(state_data$pdsi)
head(std_state_data$state.pdsi * 2.0 * state_sds$pdsi + state_means$pdsi)

bar <- msa_data %>% select(fips = msa.fips, one_of(names(msa_means))) %>% gather(-fips, key = var, value = value)
foo <- std_msa_data %>% select(fips = msa.fips, one_of(names(msa_means))) %>% gather(-fips, key = var, value = std)
foo_means <- msa_means %>% gather(key = var, value = mean)
foo_sds <- msa_sds %>% gather(key = var, value = sd)
foo <- foo %>% left_join(foo_means, by = "var") %>% left_join(foo_sds, by = "var") %>%
  mutate(reconst = (std * 2.0 * sd) + mean)
foobar <- foo %>% left_join(bar, by = c("fips", "var")) %>% mutate(delta = value - reconst)

foobar %>% group_by(var) %>% summarize(mean = mean(delta), variance = var(delta)) %>% print(n = 50)


bar <- state_data %>% select(fips = state.fips, one_of(names(state_means))) %>% gather(-fips, key = var, value = value)
foo <- std_state_data %>% set_names(names(.) %>% str_replace_all("^state\\.", "")) %>%
  select(fips, one_of(names(state_means))) %>% gather(-fips, key = var, value = std)
foo_means <- state_means %>% gather(key = var, value = mean)
foo_sds <- state_sds %>% gather(key = var, value = sd)
foo <- foo %>% left_join(foo_means, by = "var") %>% left_join(foo_sds, by = "var") %>%
  mutate(reconst = (std * 2.0 * sd) + mean)
foobar <- foo %>% left_join(bar, by = c("fips", "var")) %>% mutate(delta = value - reconst)

foobar %>% group_by(var) %>% summarize(mean = mean(delta), variance = var(delta)) %>% print(n = 50)


unpooled <- process_data(pooled = FALSE)

std_data <- unpooled$standardized_data
fdata <- unpooled$filtered_data
msa_data <- fdata$msa_data
state_data <- fdata$state_data

std_msa_data <- std_data$msa_data
std_state_data <- std_data$state_data

msa_means <- std_data$means
msa_sds <- std_data$sds

state_means <- std_data$state_means
state_sds <- std_data$state_sds

bar <- msa_data %>% select(fips = msa.fips, one_of(names(msa_means))) %>% gather(-fips, key = var, value = value)
foo <- std_msa_data %>% select(fips = msa.fips, state, one_of(names(msa_means))) %>%
  gather(-fips, -state, key = var, value = std)
foo_state <- std_state_data %>% set_names(names(.) %>% str_replace("^state\\.", "")) %>%
  select(state, one_of(names(state_means))) %>% gather(-state, key = var, value = st_val)
foo <- foo %>% left_join(foo_state, by = c('state', 'var')) %>%
  mutate(std = ifelse(is.na(st_val), std, std + st_val)) %>% select(-state, -st_val)

foo_means <- msa_means %>% gather(key = var, value = mean)
foo_sds <- msa_sds %>% gather(key = var, value = sd)

foo <- foo %>% left_join(foo_means, by = "var") %>% left_join(foo_sds, by = "var") %>%
  mutate(reconst = (std * 2.0 * sd) + mean)
foobar <- foo %>% left_join(bar, by = c("fips", "var")) %>% mutate(delta = value - reconst)

foobar %>% group_by(var) %>% summarize(mean = mean(delta), variance = var(delta)) %>% print(n = 50)

