# ch. 4.1 : in which we calculate TFP-LP


options(scipen = 999)

library(tidyverse)
library(prodest)


# value-added
sum(is.na(manuf_panel$raw_mat_expdt))  # 347861 nulls
sum(is.na(manuf_panel$raw_mat_consumed))  # 443626 nulls

  # cost of raw material CONSUMED is the actual variable for VA; we use cost when not available
manuf_panel <- manuf_panel %>% 
  mutate(
    rm_source_dummy = case_when(
      !is.na(raw_mat_consumed) ~ 1,  # using raw_mat_consumed
      is.na(raw_mat_consumed) & !is.na(raw_mat_expdt) ~ 0,  # fallback to raw_mat_expdt
      TRUE ~ NA_real_  # neither available
    ),
    raw_mat_used = coalesce(raw_mat_consumed, raw_mat_expdt),
    VA = income_nonfin - raw_mat_used - finished_good_expdt - power_fuel_water
  )


# TFP
tfp_data <- manuf_panel %>%
  select(prowess_code, year,
         VA, salary_wages, nfa, 
         raw_mat_used,  # intermediate input
         oecd_code) %>%
  filter(VA > 0,
         salary_wages > 0,
         nfa > 0,
         raw_mat_used > 0) %>%
  mutate(
    ly = log(VA),
    ll = log(salary_wages),
    lk = log(nfa),
    lm = log(raw_mat_used)
  )

tfp_lp <- prodestLP(
  Y = tfp_data$ly,
  fX = tfp_data$ll,
  sX = tfp_data$lk,
  pX = tfp_data$lm,
  idvar = tfp_data$prowess_code,
  timevar = tfp_data$year
)

summary(tfp_lp)

# extract TFP
tfp_data$tfp_lp <- as.numeric(tfp_lp@Data$FSresiduals)

manuf_panel <- manuf_panel %>%
  left_join(
    tfp_data %>% select(prowess_code, year, tfp_lp),
    by = c("prowess_code", "year")
  )

manuf_panel <- manuf_panel %>% 
  mutate(logtfp = log(tfp_lp))

manuf_panel <- manuf_panel %>% 
  group_by(prowess_code) %>% 
  arrange(year) %>% 
  mutate(lag_tfp = lag(logtfp)) %>% 
  ungroup()
