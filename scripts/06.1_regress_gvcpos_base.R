# ch. 6.1 : in which we model GVC position (baseline)


options(scipen = 999)

library(tidyverse)
library(fixest)
library(corrplot)
library(car)


# multicollinearity

  # correlation plots
vars <- manuf_panel %>%
  select(gvc_position,
         lag_liq_w, lag_lev_w,
         lag_adv, lag_innov, lag_ict,
         lag_tfp, age, size)
cor_matrix <- cor(vars, use = 'pairwise.complete.obs')
cor_matrix
corrplot(cor_matrix, method = 'color', type = 'upper',
         order = 'hclust', addCoef.col = 'black',
         tl.col = 'black', tl.srt = 45)

  # vif
vif(lm(gvc_position ~ lag_tfp + age + size + business_group, data = manuf_panel))  


# regression models

  # industry FE
gvcpos_ind_base <- feols(
  gvc_position ~ lag_tfp + age + size + business_group
  | isic_code + year,
  data = manuf_panel,
  cluster = ~prowess_code + year
)
summary(gvcpos_ind_base)


  # industry-year FE
gvcpos_indyear_base <- feols(
  gvc_position ~ lag_tfp + age + size + business_group
  | isic_code^year,
  data = manuf_panel,
  cluster = ~prowess_code 
)
summary(gvcpos_indyear_base)

  # firm FE
gvcpos_firm_base <- feols(
  gvc_position ~ lag_tfp + age + size + business_group
  | prowess_code + year,
  data = manuf_panel,
  cluster = ~prowess_code + year
)
summary(gvcpos_firm_base)


# exporting summary stats
sample_ind <- manuf_panel[gvcpos_ind_base$obs_selection$obs, ] %>% 
  select(key_vars)
sample_ind_stats <- datasummary(
  gvc_position + lag_tfp + lag_liq_w + lag_lev_w + lag_adv + lag_innov + lag_ict + age + size ~
    Mean + SD + Min + Max + N,
  data = sample_ind,
  fmt = 2,
  title = 'Summary Statistics for Industry & Year FE',
  output = 'data.frame'
)
saveRDS(sample_ind_stats, 'output/stats_ind_base.rds')

sample_indyear <- manuf_panel[gvcpos_indyear_base$obs_selection$obs, ] %>% 
  select(key_vars)
sample_indyear_stats <- datasummary(
  gvc_position + lag_tfp + lag_liq_w + lag_lev_w + lag_adv + lag_innov + lag_ict + age + size ~
    Mean + SD + Min + Max + N,
  data = sample_indyear,
  fmt = 2,
  title = 'Summary Statistics for Industry-Year FE',
  output = 'data.frame'
)
saveRDS(sample_indyear_stats, 'output/stats_indyear_base.rds')

sample_firm <- manuf_panel[gvcpos_firm_base$obs_selection$obs, ] %>% 
  select(key_vars)
sample_firm_stats <- datasummary(
  gvc_position + lag_tfp + lag_liq_w + lag_lev_w + lag_adv + lag_innov + lag_ict + age + size ~
    Mean + SD + Min + Max + N,
  data = sample_firm,
  fmt = 2,
  title = 'Summary Statistics for Firm FE',
  output = 'data.frame'
)
saveRDS(sample_firm_stats, 'output/stats_firm_base.rds')


# export
saveRDS(gvcpos_ind_base, 'output/gvcpos_ind_base.rds')
saveRDS(gvcpos_indyear_base, 'output/gvcpos_indyear_base.rds')
saveRDS(gvcpos_firm_base, 'output/gvcpos_firm_base.rds')
