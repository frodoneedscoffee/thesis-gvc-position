# ch. 6.3.2 : in which we incorporate ICT capabilities


options(scipen = 999)

library(tidyverse)
library(fixest)
library(car)


# multicollinearity
vif(lm(gvc_position ~ lag_adv + lag_ict + lag_adv:lag_ict + lag_tfp + 
         age + size + business_group, data = manuf_panel)) 

vif(lm(gvc_position ~ lag_innov + lag_ict + lag_innov:lag_ict + lag_tfp + 
         age + size + business_group, data = manuf_panel)) 


# regression models : advertising

  # industry FE
gvcpos_ind_advict <- feols(
  gvc_position ~ lag_adv + lag_ict + lag_adv:lag_ict + lag_tfp + age + size + business_group
  | isic_code + year,
  data = manuf_panel,
  cluster = ~prowess_code + year
)
summary(gvcpos_ind_advict)

  # industry-year FE
gvcpos_indyear_advict <- feols(
  gvc_position ~ lag_adv + lag_ict + lag_adv:lag_ict + lag_tfp + age + size + business_group
  | isic_code^year,
  data = manuf_panel,
  panel.id = ~prowess_code + year,
  cluster = ~prowess_code 
)
summary(gvcpos_indyear_advict)

  # firm FE
gvcpos_firm_advict <- feols(
  gvc_position ~ lag_adv + lag_ict + lag_adv:lag_ict + lag_tfp + age + size + business_group
  | prowess_code + year,
  data = manuf_panel,
  cluster = ~prowess_code + year
)
summary(gvcpos_firm_advict)


# regression models : innovation

  # industry FE
gvcpos_ind_innovict <- feols(
  gvc_position ~ lag_innov + lag_ict + lag_innov:lag_ict + lag_tfp + age + size + business_group
  | isic_code + year,
  data = manuf_panel,
  cluster = ~prowess_code + year
)
summary(gvcpos_ind_innovict)

  # industry-year FE
gvcpos_indyear_innovict <- feols(
  gvc_position ~ lag_innov + lag_ict + lag_innov:lag_ict + lag_tfp + age + size + business_group
  | isic_code^year,
  data = manuf_panel,
  panel.id = ~prowess_code + year,
  cluster = ~prowess_code 
)
summary(gvcpos_indyear_innovict)

  # firm FE
gvcpos_firm_innovict <- feols(
  gvc_position ~ lag_innov + lag_ict + lag_innov:lag_ict + lag_tfp + age + size + business_group
  | prowess_code + year,
  data = manuf_panel,
  cluster = ~prowess_code + year
)
summary(gvcpos_firm_innovict)


# exporting summary stats
sample_firm <- manuf_panel[gvcpos_firm_advict$obs_selection$obs, ] %>% 
  select(key_vars)
sample_firm_stats <- datasummary(
  gvc_position + lag_tfp + lag_liq_w + lag_lev_w + lag_adv + lag_innov + lag_ict + age + size ~
    Mean + SD + Min + Max + N,
  data = sample_firm,
  fmt = 2,
  title = 'Summary Statistics for Firm FE',
  output = 'data.frame'
)
saveRDS(sample_firm_stats, 'output/stats_firm_advict.rds')

sample_firm <- manuf_panel[gvcpos_firm_innovict$obs_selection$obs, ] %>% 
  select(key_vars)
sample_firm_stats <- datasummary(
  gvc_position + lag_tfp + lag_liq_w + lag_lev_w + lag_adv + lag_innov + lag_ict + age + size ~
    Mean + SD + Min + Max + N,
  data = sample_firm,
  fmt = 2,
  title = 'Summary Statistics for Firm FE',
  output = 'data.frame'
)
saveRDS(sample_firm_stats, 'output/stats_firm_innovict.rds')


# export
saveRDS(gvcpos_ind_advict, 'output/gvcpos_ind_advict.rds')
saveRDS(gvcpos_indyear_advict, 'output/gvcpos_indyear_advict.rds')
saveRDS(gvcpos_firm_advict, 'output/gvcpos_firm_advict.rds')

# export
saveRDS(gvcpos_ind_innovict, 'output/gvcpos_ind_innovict.rds')
saveRDS(gvcpos_indyear_innovict, 'output/gvcpos_indyear_innovict.rds')
saveRDS(gvcpos_firm_innovict, 'output/gvcpos_firm_innovict.rds')
