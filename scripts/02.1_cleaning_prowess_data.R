# ch. 2.1 : in which we clean the CMIE Prowess data


options(scipen = 999)

library(tidyverse)


years_cmie <- 1994:2022 


# id dataframe
  
  # import csv file
id_df <- read.csv('raw_data/all-firms-cmie-prowess.csv')
  
  # rename columns
colnames(id_df) <- c('company_name', 'state', 'prowess_code', 'inc_year',
                     'ind_group', 'ind_group_code', 'nic_name', 'nic_code',
                     'own_group_code', 'own_group')

  # create business group affiliation dummy
id_df <- id_df %>%
  mutate(
    business_group = case_when(
      own_group == 'Private (Indian)' ~ 0,  # standalone private Indian firm
      own_group == 'Private (Foreign)' ~ 1,  # standalone private foreign firm
      own_group %in% c('Central Govt. - Commercial Enterprises',
                       'State Govt. - Commercial Enterprises',
                       'State Government') ~ 3,  # pure government-owned enterprises
      own_group == "State and Private sector" ~ 4,  # joint state–private ownership
      TRUE ~ 2   # all remaining are business group affiliated (indian or foreign parent group)
    )
  )

  # export
write.csv(id_df, 'output/all-firms-id.csv')


# main financial variables dataframe

  # import csv files
financials_df_1 <- read_csv("raw_data/all-firms-standardisedfinancials1-1994to2022.csv")
financials_df_2 <- read_csv("raw_data/all-firms-standardisedfinancials2-1994to2022.csv")

  # drop year column
financials_df_1 <- financials_df_1 %>% 
  select(-matches('^year'))
financials_df_2 <- financials_df_2 %>% 
  select(-matches('^year'))

  # create column names + column-year vector
finvar_1 <- c('total_inc', 'income_nonfin', 'nfa', 'curr_assets', 
              'curr_liab', 'sr_borr', 'lr_borr', 'total_assets')
finvar_2 <- c('raw_mat_expdt', 'power_fuel_water', 'power_fuel',
              'salary_wages', 'it_comm_expdt', 'emp', 'rnd_expdt')

finvar_1_years <- as.vector(sapply(years_cmie, function(y) paste0(finvar_1, "_", y)))
finvar_2_years <- as.vector(sapply(years_cmie, function(y) paste0(finvar_2, "_", y)))

  # rename columns
colnames(financials_df_1) <- c('company_name', finvar_1_years)
colnames(financials_df_2) <- c('company_name', finvar_2_years)

  # pivot dataframes
financials_df_1 <- financials_df_1 %>%
  pivot_longer(
    cols = -c(company_name),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d{4})"
  )

financials_df_2 <- financials_df_2 %>%
  pivot_longer(
    cols = -c(company_name),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d{4})"
  )

  # multiply monetary variables by 1 crore
financials_df_1 <- financials_df_1 %>%
  mutate(across(.cols = -c(company_name, year), .fns  = ~ . * 1e7))
financials_df_2 <- financials_df_2 %>%
  mutate(across(.cols = -c(company_name, year), .fns  = ~ . * 1e7))

  # export
write.csv(financials_df_1, 'output/all-firms-financials1-1994to2022.csv')
write.csv(financials_df_2, 'output/all-firms-financials2-1994to2022.csv')


# exim variables dataframe

  # import csv file
exim_df <- read.csv('raw_data/all-firms-eximdata-1994to2022.csv')

  # drop year column
exim_df <- exim_df %>% 
  select(-matches('^year'))

  # create column names + column-year vector
eximvar <- c('export_fob', 'import_raw_mat', 'import_stores_spares', 
             'import_finished_goods', 'import_capital_goods', 
             'raw_mat_consumed', 'indig_raw_mat_consumed', 'import_raw_mat_consumed')

eximvar_years <- as.vector(sapply(years_cmie, function(y) paste0(eximvar, "_", y)))

  # rename columns
colnames(exim_df) <- c('company_name', eximvar_years)

  # pivot dataframe
exim_df <- exim_df %>%
  pivot_longer(
    cols = -c(company_name),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d{4})"
  )

  # multiply monetary variables by 1 crore
exim_df <- exim_df %>%
  mutate(across(.cols = -c(company_name, year), .fns  = ~ . * 1e7))

  # export
write.csv(exim_df, 'output/all-firms-exim-1994to2022.csv')


# other variables dataframe

  # import csv file
othervar_df <- read.csv('raw_data/all-firms-otherinterestingvar-1994to2022.csv')

  # drop year column
othervar_df <- othervar_df %>% 
  select(-matches('^year'))

  # create column names + column-year vector
othervar <- c('outsourced_manuf', 'outsourced_services', 'selling_dist_expdt',
             'advertising_expdt', 'marketing_expdt', 'dist_expdt',
             'net_intangible_assets', 'cwip_intangible_under_dev',
             'cwip_under_dev', 'intangible_under_dev')

othervar_years <- as.vector(sapply(years_cmie, function(y) paste0(othervar, "_", y)))

  # rename columns
colnames(othervar_df) <- c('company_name', othervar_years)

  # pivot dataframe
othervar_df <- othervar_df %>%
  pivot_longer(
    cols = -c(company_name),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d{4})"
  )

  # multiply monetary variables by 1 crore
othervar_df <- othervar_df %>%
  mutate(across(.cols = -c(company_name, year), .fns  = ~ . * 1e7))

  # export
write.csv(othervar_df, 'output/all-firms-othervar-1994to2022.csv')


# finished goods dataframe

# import csv file
finishedgood_df <- read.csv('raw_data/all-firms-finishedgoodexpdt-1994to2022.csv')

# drop year column
finishedgood_df <- finishedgood_df %>% 
  select(-matches('^year'))

# create column names + column-year vector
finishedgoodvar <- 'finished_good_expdt'

finishedgoodvar_years <- as.vector(sapply(years_cmie, function(y) paste0(finishedgoodvar, "_", y)))

# rename columns
colnames(finishedgood_df) <- c('company_name', finishedgoodvar_years)

# pivot dataframe
finishedgood_df <- finishedgood_df %>%
  pivot_longer(
    cols = -c(company_name),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d{4})"
  )

# multiply monetary variables by 1 crore
finishedgood_df <- finishedgood_df %>%
  mutate(across(.cols = -c(company_name, year), .fns  = ~ . * 1e7))

# export
write.csv(finishedgood_df, 'output/all-firms-finishedgoodexpdt-1994to2022.csv')


# check NAs
id_na <- colMeans(is.na(id_df))
fin1_na <- colMeans(is.na(financials_df_1))
fin2_na <- colMeans(is.na(financials_df_2))
exim_na <- colMeans(is.na(exim_df))
other_na <- colMeans(is.na(othervar_df))
finished_na <- colMeans(is.na(finishedgood_df))

null_values <- data.frame(
  dataframe = c(rep("id_df", length(id_na)),
                rep("financials_df_1", length(fin1_na)),
                rep("financials_df_2", length(fin2_na)),
                rep("exim_df", length(exim_na)),
                rep("othervar_df", length(other_na)),
                rep("finishedgood_df", length(finished_na))),
  variable = c(names(id_na), names(fin1_na), names(fin2_na), 
               names(exim_na), names(other_na), names(finished_na)),
  null_proportion = c(id_na, fin1_na, fin2_na, exim_na, other_na, finished_na)
)

null_values <- null_values %>% 
  mutate(null_proportion = 100*null_proportion) %>% 
  arrange(desc(null_proportion))

