# ch. 6.2 : in which we model GVC position w.r.t. credit


options(scipen = 999)

library(tidyverse)
library(fixest)
library(car)


# multicollinearity
vif(lm(gvc_position ~ lag_liq_w + lag_tfp + lag_tfp:lag_liq_w + 
         age + size + business_group, data = manuf_panel))  


# regression models : liquidity

  # industry FE
gvcpos_ind_liq <- feols(
  gvc_position ~ lag_liq_w + lag_tfp + lag_tfp:lag_liq_w + age + size + business_group
  | isic_code + year,
  data = manuf_panel,
  cluster = ~prowess_code
)
summary(gvcpos_ind_liq)

  # industry-year FE
gvcpos_indyear_liq <- feols(
  gvc_position ~ lag_liq_w + lag_tfp + lag_tfp:lag_liq_w + age + size + business_group
  | isic_code^year,
  data = manuf_panel,
  cluster = ~prowess_code 
)
summary(gvcpos_indyear_liq)

  # firm FE
gvcpos_firm_liq <- feols(
  gvc_position ~ lag_liq_w + lag_tfp + lag_tfp:lag_liq_w + age + size + business_group
  | prowess_code + year,
  data = manuf_panel,
  cluster = ~prowess_code + year
)
summary(gvcpos_firm_liq)


# exporting summary stats
sample_ind <- manuf_panel[gvcpos_ind_liq$obs_selection$obs, ] %>% 
  select(key_vars)
sample_ind_stats <- datasummary(
  gvc_position + lag_tfp + lag_liq_w + lag_lev_w + lag_adv + lag_innov + lag_ict + age + size ~
    Mean + SD + Min + Max + N,
  data = sample_ind,
  fmt = 2,
  title = 'Summary Statistics for Industry & Year FE',
  output = 'data.frame'
)
saveRDS(sample_ind_stats, 'output/stats_ind_liq.rds')

sample_indyear <- manuf_panel[gvcpos_indyear_liq$obs_selection$obs, ] %>% 
  select(key_vars)
sample_indyear_stats <- datasummary(
  gvc_position + lag_tfp + lag_liq_w + lag_lev_w + lag_adv + lag_innov + lag_ict + age + size ~
    Mean + SD + Min + Max + N,
  data = sample_indyear,
  fmt = 2,
  title = 'Summary Statistics for Industry-Year FE',
  output = 'data.frame'
)
saveRDS(sample_indyear_stats, 'output/stats_indyear_liq.rds')

sample_firm <- manuf_panel[gvcpos_firm_liq$obs_selection$obs, ] %>% 
  select(key_vars)
sample_firm_stats <- datasummary(
  gvc_position + lag_tfp + lag_liq_w + lag_lev_w + lag_adv + lag_innov + lag_ict + age + size ~
    Mean + SD + Min + Max + N,
  data = sample_firm,
  fmt = 2,
  title = 'Summary Statistics for Firm FE',
  output = 'data.frame'
)
saveRDS(sample_firm_stats, 'output/stats_firm_liq.rds')


# export
saveRDS(gvcpos_ind_liq, 'output/gvcpos_ind_liq.rds')
saveRDS(gvcpos_indyear_liq, 'output/gvcpos_indyear_liq.rds')
saveRDS(gvcpos_firm_liq, 'output/gvcpos_firm_liq.rds')
