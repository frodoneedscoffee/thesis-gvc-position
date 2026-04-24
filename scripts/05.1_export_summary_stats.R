# ch. 5.1 : in which we do a sanity check and export summary stats


options(scipen = 999)


library(tidyverse)
library(modelsummary)


colMeans(is.na(manuf_panel))


# summary stats (check)
datasummary_skim(manuf_panel[, c('gvc_position', 'lag_tfp',
                                 'lag_liq', 'lag_lev', 
                                 'lag_adv', 'lag_innov', 'lag_ict', 
                                 'age', 'size')])

# extreme values in lag_liq and lag_lev => winsorize at 1st and 99th percentiles
manuf_panel <- manuf_panel %>%  
  mutate(
    
    lag_liq_w = pmin(
      pmax(lag_liq, quantile(lag_liq, 0.01, na.rm = TRUE)),
      quantile(lag_liq, 0.99, na.rm = TRUE)
    ),
    
    lag_lev_w = pmin(
      pmax(lag_lev, quantile(lag_lev, 0.01, na.rm = TRUE)),
      quantile(lag_lev, 0.99, na.rm = TRUE)
    )
  )

datasummary_skim(manuf_panel[, c('gvc_position', 'lag_tfp',
                                 'lag_liq_w', 'lag_lev_w', 
                                 'lag_adv', 'lag_innov', 'lag_ict',
                                 'age', 'size')])


# exporting summary stats
summ_stats <- datasummary(
  gvc_position + lag_tfp + lag_liq + lag_lev + lag_adv + lag_innov + lag_ict + age + size ~
    Mean + SD + Min + Max + N,
  data = manuf_panel,
  fmt = 2,
  title = 'Summary Statistics',
  output = 'data.frame'
)
saveRDS(summ_stats, 'output/summary-stats.RDS')

summ_stats_w <- datasummary(
  gvc_position + lag_tfp + lag_liq_w + lag_lev_w + lag_adv + lag_innov + lag_ict + age + size ~
    Mean + SD + Min + Max + N,
  data = manuf_panel,
  fmt = 2,
  title = 'Summary Statistics (Winsorized)',
  output = 'data.frame'
)
saveRDS(summ_stats_w, 'output/summary-stats-winsorized.RDS')


# variable description table
var_definitions <- data.frame(
  Variable = c('GVC Position', 'Backward Participation', 'Forward Participation', 
               'Liquidity', 'Leverage', 'Advertising', 'Innovation', 'ICT Usage', 
               'Productivity', 'Firm Size', 'Firm Age'),
  Formula = c(
    '$\\ln(1+FP) - \\ln(1+BP)$',
    '$\\frac{\\text{Imported Raw Material \\& Consumer Goods}}{\\text{Total Sales}}$',
    '$\\frac{\\text{Intermediate Exports}}{\\text{Total Exports}} \\cdot \\frac{\\text{Value Added}}{\\text{Total Sales}}$',
    '$\\frac{\\text{Current Assets}-\\text{Current Liabilities}}{\\text{Total Assets}}$',
    '$\\frac{\\text{Total Debt}}{\\text{Total Assets}}$',
    '$\\frac{\\text{Advertising \\& Marketing Expdt.}}{\\text{Total Sales}}$',
    '$\\frac{\\text{R\\&D Expdt. + CWIP \\& Intangibles}}{\\text{Total Sales}}$',
    '$\\frac{\\text{IT \\& Communication Expdt.}}{\\text{Total Sales}}$',
    '$\\text{Following Levinsohn \\& Petrin (2003)}$',
    '$\\ln(\\text{Total Assets})$',
    '$\\text{From year of incorporation}$'
  ),
  Description = c(
    'Position in Global Value Chain',
    'Backward Participation',
    'Forward Participation',
    'Working Capital over Assets',
    'Debt to Asset Ratio',
    'Intensity of Marketing Spend',
    'Intensity of R\\&D Spend',
    'Intensity of ICT Use',
    'Firm Productivity',
    'Log of Total Assets',
    'Age of the firm in 2026'
  )
)

saveRDS(var_definitions, 'output/var-definitions.RDS')
