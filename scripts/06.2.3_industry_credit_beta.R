# ch. 6.2.3 : in which we find the industry-specific betas for liquidity and leverage


options(scipen = 999)


library(tidyverse)
library(fixest)
library(broom)
library(ggplot2)
library(ggrepel)


# liquidity
liq_pos_indspecific <- feols(
  gvc_position ~ lag_tfp + age + size + business_group + lag_liq:factor(isic_code)
  | year,
  data = manuf_panel,
  cluster = ~prowess_code + year
)
summary(liq_pos_indspecific)

write.csv(etable(liq_pos_indspecific), 'output/manuf-liq-indspecific.csv')

  # plot liquidity coefficients
liq_coefs <- tidy(liq_pos_indspecific, conf.int = TRUE) %>%
  filter(grepl('lag_liq:factor\\(isic_code\\)', term)) %>%
  mutate(
    isic_code = gsub('lag_liq:factor\\(isic_code\\)', '', term)
  )

liq_coefs <- liq_coefs %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

ggplot(liq_coefs, aes(x = reorder(isic_code, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(
    x = 'Industry (ISIC code)',
    y = 'Liquidity Coefficient'
  ) +
  theme_minimal()

  # histogram of industry betas (kernel density)
ggplot(liq_coefs, aes(x = estimate)) +
  geom_density(fill = 'steelblue', alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(
    x = 'Liquidity coefficient',
    y = 'Density',
    title = 'Distribution of liquidity effects across industries'
  ) +
  theme_minimal()


# leverage 
lev_pos_indspecific <- feols(
  gvc_position ~ lag_tfp + age + size + business_group + lag_lev:factor(isic_code)
  | year,
  data = manuf_panel,
  cluster = ~prowess_code + year
)
summary(lev_pos_indspecific)

write.csv(etable(lev_pos_indspecific), 'output/manuf-lev-indspecific.csv')

  # plot leverage coefficients
lev_coefs <- tidy(lev_pos_indspecific, conf.int = TRUE) %>%
  filter(grepl('lag_lev:factor\\(isic_code\\)', term)) %>%
  mutate(
    isic_code = gsub('lag_lev:factor\\(isic_code\\)', '', term)
  )
lev_coefs <- lev_coefs %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

ggplot(lev_coefs, aes(x = reorder(isic_code, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(
    x = 'Industry (ISIC code)',
    y = 'Leverage Coefficient'
  ) +
  theme_minimal()

  # histogram of industry betas (kernel density)
ggplot(lev_coefs, aes(x = estimate)) +
  geom_density(fill = 'steelblue', alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(
    x = 'Leverage coefficient',
    y = 'Density',
    title = 'Distribution of leverage effects across industries'
  ) +
  theme_minimal()


# liquidity vs leverage : industry sensitivty to type of financing

liq_plot <- liq_coefs %>%
  select(isic_code, liq_beta = estimate)

lev_plot <- lev_coefs %>%
  select(isic_code, lev_beta = estimate)

credit_plot <- merge(liq_plot, lev_plot, by = 'isic_code')

ggplot(credit_plot, aes(x = liq_beta, y = lev_beta, label = isic_code)) +
  geom_point(size = 3) +
  
  geom_text_repel(
    size = 3,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = Inf
  ) +
  
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(
    x = 'Liquidity coefficient',
    y = 'Leverage coefficient',
    title = 'Industry sensitivity to liquidity vs leverage'
  ) +
  theme_minimal()

