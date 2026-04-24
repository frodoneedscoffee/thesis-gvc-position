# ch. 4.2 : in which we create the financial variables 


options(scipen = 999)


library(tidyverse)


# business group
manuf_panel <- manuf_panel %>% mutate(business_group = as.factor(business_group))


# financial constraints
manuf_panel <- manuf_panel %>% 
  
  # liquidity - scaled working capital
  mutate(liquidity = (curr_assets - curr_liab)/total_assets) %>% 
  
  # leverage - total debt scaled by assets
  mutate(
    leverage = (sr_borr + lr_borr)/total_assets,  # Reddy & Sasidharan (2021)
    leverage2 = curr_liab/curr_assets             # Manova & Yu (2016)
    )

  # lagged financial variables (endogeneity)
manuf_panel <- manuf_panel %>% 
  group_by(prowess_code) %>%
  arrange(year) %>%
  mutate(
    lag_liq  = lag(liquidity),
    lag_lev  = lag(leverage),
    lag_lev2 = lag(leverage2),
    lag_adv  = lag(advert),
    lag_innov  = lag(innovation)
  ) %>%
  ungroup()

  # leave-one-out means
manuf_panel <- manuf_panel %>%
  group_by(isic_code, year) %>%
  mutate(
    
    # liquidity
    n_liq = sum(!is.na(lag_liq)),  # number of firms in industry-year
    sum_liq = sum(lag_liq, na.rm = TRUE),  # industry totals
    ind_liq = ifelse(n_liq > 1, (sum_liq - lag_liq)/(n_liq - 1), NA),  # leave-one-out means
    
    # leverage
    n_lev = sum(!is.na(lag_lev)),
    sum_lev = sum(lag_lev, na.rm = TRUE),
    ind_lev = ifelse(n_lev > 1, (sum_lev - lag_lev)/(n_lev - 1), NA)
    
  ) %>%
  ungroup() %>% 
  select(-c(n_liq, sum_liq, n_lev, sum_lev))


# age & size of the firm
manuf_panel <- manuf_panel %>% 
  mutate(age = 2026 - inc_year,
         size = case_when(total_assets == 0 ~ log(1 + total_assets),  # prevent Inf
                          TRUE ~ log(total_assets))
         )

# labour productivity & capital intensity of firm

 # mean industry wage (to account for missing employee data)
manuf_panel <- manuf_panel %>%
  group_by(isic_code, year) %>%
  mutate(ind_wage = mean(salary_wages, na.rm = TRUE),
         n_emp = case_when(salary_wages > 0 ~ ind_wage/salary_wages,  # proxy for no. of employees
                           TRUE ~ NA)
         ) %>%
  ungroup()

manuf_panel <- manuf_panel %>% 
  mutate(
    
    # labour productivity
    lproductivity = case_when(!is.na(emp) & emp > 0 ~ VA/emp,
                              TRUE ~ VA/n_emp),
    log_lprod = case_when(lproductivity > 0 ~ log(lproductivity),  # prevent -Inf
                          TRUE ~ NA),
    lag_lprod = lag(log_lprod),
    
    # capital intensity
    kintensity = case_when(!is.na(emp) & emp > 0 ~ nfa/emp,
                           is.na(emp) ~ nfa/n_emp,
                           TRUE ~ NA),
    log_kintensity = case_when(kintensity > 0 ~ log(kintensity),  # prevent -Inf
                               TRUE ~ NA),
    lag_kintensity = lag(log_kintensity)
  )


# advertising and innovation
manuf_panel <- manuf_panel %>% 
  mutate(
    advert = case_when(
      !is.na(advertising_expdt) ~ (marketing_expdt + advertising_expdt)/income_nonfin,
      TRUE ~ marketing_expdt/income_nonfin  # when advertising expdt. not given
    ),
    
    innovation = case_when(
      !is.na(rnd_expdt) ~ (cwip_intangible_under_dev + rnd_expdt)/income_nonfin,
      TRUE ~ rnd_expdt/income_nonfin  # when CWIP/intangibles not given
    )
  )


# ICT-usage
manuf_panel <- manuf_panel %>% 
  mutate(ict = it_comm_expdt/income_nonfin,
         lag_ict = lag(ict))
