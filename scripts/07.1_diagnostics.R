# ch. 9.1 : in which we run diagnostic tests


options(scipen = 999)


library(strucchange)
library(broom)
library(lmtest)
library(plm)


model_specs <- c('Productivity Model', 'Liquidity Model', 'Leverage Model',
                 'Capabilities Model', 'Advertising × ICT', 'Innovation × ICT')


# Hausman test : RE is consistent + BP test : no heteroscedasticity
fe_mod <- plm(gvc_position ~ lag_tfp + age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'within')
bptest(fe_mod)
re_mod <- plm(gvc_position ~ lag_tfp + age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'random')
phtest(fe_mod, re_mod) 

fe_mod <- plm(gvc_position ~ lag_liq + lag_tfp + lag_liq:lag_tfp +
                age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'within')
bptest(fe_mod)
re_mod <- plm(gvc_position ~ lag_liq + lag_tfp + lag_liq:lag_tfp +
                age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'random')
phtest(fe_mod, re_mod) 

fe_mod <- plm(gvc_position ~ lag_lev + lag_tfp + lag_lev:lag_tfp +
                age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'within')
bptest(fe_mod)
re_mod <- plm(gvc_position ~ lag_lev + lag_tfp + lag_lev:lag_tfp +
                age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'random')
phtest(fe_mod, re_mod)

fe_mod <- plm(gvc_position ~ lag_adv + lag_innov + lag_tfp +
                age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'within')
bptest(fe_mod)
re_mod <- plm(gvc_position ~ lag_adv + lag_innov + lag_tfp +
                age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'random')
phtest(fe_mod, re_mod) 

fe_mod <- plm(gvc_position ~ lag_adv + lag_ict + lag_adv:lag_ict + lag_tfp +
                age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'within')
bptest(fe_mod)
re_mod <- plm(gvc_position ~ lag_adv + lag_ict + lag_adv:lag_ict + lag_tfp +
                age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'random')
phtest(fe_mod, re_mod) 

fe_mod <- plm(gvc_position ~ lag_innov + lag_ict + lag_innov:lag_ict + lag_tfp +
                age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'within')
bptest(fe_mod)
re_mod <- plm(gvc_position ~ lag_innov + lag_ict + lag_innov:lag_ict + lag_tfp +
                age + size + business_group,
              data = manuf_panel,
              index = c('prowess_code', 'year'),
              model = 'random')
phtest(fe_mod, re_mod)


model_re <- plm(
  gvc_position ~ lag_tfp + age + size + business_group,
  data = manuf_panel,
  index = c('prowess_code', 'year'),
  model = 'random'
)
summary(model_re)
coeftest(model_re, vcov = vcovHC(model_re, type = 'HC1', cluster = 'group'))


# Wooldridge test : no serial correlation
pwartest(gvc_position ~ lag_tfp + age + size + business_group, 
         data = manuf_panel, 
         index = c('prowess_code', 'year'))
pwartest(gvc_position ~ lag_liq + lag_tfp + lag_liq:lag_tfp + age + size + business_group,
         data = manuf_panel, 
         index = c('prowess_code', 'year'))
pwartest(gvc_position ~ lag_lev + lag_tfp + lag_lev:lag_tfp + age + size + business_group,
         data = manuf_panel, 
         index = c('prowess_code', 'year'),)
pwartest(gvc_position ~ lag_adv + lag_innov + lag_tfp + age + size + business_group,
         data = manuf_panel,
         index = c('prowess_code', 'year'))
pwartest(gvc_position ~ lag_adv + lag_ict + lag_adv:lag_ict + lag_tfp + age + size + business_group,
         data = manuf_panel,
         index = c('prowess_code', 'year'))
pwartest(gvc_position ~ lag_innov + lag_ict + lag_innov:lag_ict + lag_tfp + age + size + business_group,
         data = manuf_panel,
         index = c('prowess_code', 'year'))


# export test results
test_hausman <- data.frame(
  'Model' = model_specs,
  'Chi sq.' = c(2.5544, 29.505, 13.73, 37.909, 27.814, 9.7452),
  'DF' = c(2, 4, 4, 4, 5, 5),
  'p-value' = c(0.2788, 0.000006172, 0.008208, 0.000000117, 0.00003957, 0.08278),
  check.names = FALSE  # prevents the spaces from turning into dots
)
saveRDS(test_hausman, 'output/test-hausman.rds')

test_bp <- data.frame(
  'Model' = model_specs,
  'BP stat' = c(253.49, 98.48, 69.49, 233.32, 172.23, 71.364),
  'DF' = c(7, 9, 9, 9, 10, 10),
  'p-value' = c('$<$ 2.2e-16', '$<$ 2.2e-16', '1.917e-11', '$<$ 2.2e-16', '$<$ 2.2e-16', '2.416e-11'),
  check.names = FALSE
)
saveRDS(test_bp, 'output/test-bp.rds')

test_serialcorr <- data.frame(
  'Model' = model_specs,
  'F-stat' = c(10.26, 0.85506, 2.1861, 36.408, 0.32732, 3.5462),
  'DF1' = c(1, 1, 1, 1, 1, 1),
  'DF2' = c(10835, 4742, 3084, 3923, 2450, 967),
  'p-value' = c(0.001363, 0.3552, 0.1394, 1.749e-9, 0.5673, 0.05998),
  check.names = FALSE
)
saveRDS(test_serialcorr, 'output/test-serialcorr.rds')

  # VIF results from regression scripts
test_vif <- data.frame(
  'Model' = model_specs,
  'Max Adjusted VIF' = c(1.10, 1.85, 2.35, 1.10, 1.13, 1.93),
  check.names = FALSE
)
saveRDS(test_vif, 'output/test-vif.rds')

