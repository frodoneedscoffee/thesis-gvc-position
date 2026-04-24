# ch. 5.2 : in which we export industry summary stats


options(scipen = 999)


library(tidyverse)
library(ggplot2)


# industry summary statistics
key_vars <- c('gvc_position', 'lag_liq_w', 'lag_lev_w', 'lag_adv',
              'lag_innov', 'lag_ict', 'lag_tfp', 'age', 'size')

industry_summary <- manuf_panel %>%
  group_by(isic_code) %>%
  summarise(
    across(all_of(key_vars),
           list(mean = ~mean(.x, na.rm = TRUE),
                sd   = ~sd(.x, na.rm = TRUE)),
           .names = '{.col}_{.fn}'),
    n_firms = n_distinct(prowess_code),  # distinct firms
    n_obs = n(),                         # firm-year observations
    .groups = 'drop'
  )

industry_summary_long <- manuf_panel %>%
  group_by(isic_code) %>%
  summarise(
    across(all_of(key_vars),
           list(mean = ~mean(.x, na.rm = TRUE),
                sd   = ~sd(.x, na.rm = TRUE))),
    .groups = 'drop'
  ) %>%
  pivot_longer(-isic_code,
               names_to = c('variable', '.value'),
               names_pattern = '(.*)_(mean|sd)')


# number of firms per industry (bar chart)
ggplot(industry_summary, aes(x = reorder(isic_code, n_firms), y = n_firms)) +
  geom_col() +
  geom_text(aes(label = n_firms), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    x = 'Industry (ISIC Code)',
    y = 'Number of Firms'
  ) +
  theme_minimal()
