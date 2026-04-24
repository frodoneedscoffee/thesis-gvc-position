# ch. 4.3 : in which we create GVC variables


options(scipen = 999)

library(tidyverse)


# backward GVC 
sum(is.na(manuf_panel$import_raw_mat)) # 448371
sum(is.na(manuf_panel$import_raw_mat_consumed)) # 467878 -> more nulls; same as above

manuf_panel <- manuf_panel %>% 
  mutate(
    
    imp_rm_source_dummy = case_when(
      !is.na(import_raw_mat_consumed) ~ 1,  # using consumed
      is.na(import_raw_mat_consumed) & !is.na(import_raw_mat) ~ 0,  # fallback to import_raw_mat
      TRUE ~ NA_real_  # neither available
    ),
    
    import_raw_mat_used = coalesce(import_raw_mat_consumed, import_raw_mat),
    
    back_GVC = case_when(
      !is.na(import_finished_goods) ~ (import_raw_mat_used + import_finished_goods)/income_nonfin,
      TRUE ~ import_raw_mat_used/income_nonfin
    ),
    
    back_GVC_source_dummy = case_when(
      !is.na(import_finished_goods) ~ 1,  # using both raw materials & finished goods
      is.na(import_finished_goods) ~ 0,  # using only raw material
      TRUE ~ NA_real_  # neither available
    )
  )

sum(is.na(manuf_panel$back_GVC))  # 512724 nulls


# forward GVC 

  # assigning industry intermediate export share to firms
industry_intermediate_share <- industry_intermediate_share %>% 
  rename(oecd_code = industry,
         ind_interm_share = share_intermediate,
         ind_interm_exp = intermediate_exports,
         ind_exp = total_exports)

manuf_panel <- manuf_panel %>% 
  left_join(industry_intermediate_share, by = c('year', 'oecd_code'))


    # any industries that have been systematically dropped?
missing_share <- manuf_panel %>% filter(is.na(ind_interm_share))
missing_share %>% group_by(isic_code, oecd_code) %>% 
  summarise(no_of_firms = length(isic_code)) %>% 
  print(n = Inf)  # 11, 309, 32 among others : 147,000 obs.

missing_share_ind <- manuf_panel %>%
  filter(is.na(ind_interm_share)) %>%
  distinct(isic_code)

missing_share_diagnosis <- manuf_panel %>%
  filter(isic_code %in% missing_share_ind$isic_code) %>%
  group_by(isic_code) %>%
  summarise(
    any_non_na = any(!is.na(ind_interm_share)),
    n_non_na = sum(!is.na(ind_interm_share)),
    n_total = n(),
    .groups = 'drop'
  )  # systematically dropped (FALSE values) -> 11, 12, 14, 15, 18, 24 (expected), 303, 309, 32

missing_share_diagnosis <- missing_share_diagnosis %>% 
  left_join(manuf_map, by = 'isic_code') %>% 
  select(-c(nic_code))

write.csv(missing_share_diagnosis, 'output/candidate-industries-for-recoding.csv')

  # since OECD assumes similar production technology within these,
  # we recode these industries:
  # C10T12 → 10, 11, 12
  # C13T15 → 13, 14, 15
  # C17_18 → 17, 18
  # C302T309 → 302, 303, 309
  # C31T33 → 31, 32

manuf_panel <- manuf_panel %>%
  mutate(
    oecd_code_new = case_when(
      isic_code %in% c(11, 12) ~ 'C10T12',
      isic_code %in% c(14, 15) ~ 'C13T15',
      isic_code == 18 ~ 'C17_18',
      isic_code %in% c(303, 309) ~ 'C302T309',
      isic_code == 32 ~ 'C31T33',
      TRUE ~ oecd_code
    ),
    recoded_ind = if_else(oecd_code_new != oecd_code, 1, 0)
  )

    # assign industry intermediate shares according to recoded codes
manuf_panel <- manuf_panel %>% 
  select(-ind_interm_exp, -ind_exp, -ind_interm_share)
manuf_panel <- manuf_panel %>% 
  left_join(
    industry_intermediate_share,
    by = c('year', 'oecd_code_new' = 'oecd_code')
  )

    # check
missing_share_post <- manuf_panel %>% filter(is.na(ind_interm_share))
missing_share_post %>% group_by(isic_code, oecd_code_new) %>% 
  summarise(no_of_firms = length(isic_code)) %>% 
  print(n = Inf)  # 27,000 obs.

  # add recoding column to (separate) MAPPING TABLE
manuf_map_recoded <- manuf_map %>%
  mutate(
    oecd_recode = case_when(
      isic_code %in% c(11, 12) ~ 'C10T12',
      isic_code %in% c(14, 15) ~ 'C13T15',
      isic_code == 18 ~ 'C17_18',
      isic_code %in% c(303, 309) ~ 'C302T309',
      isic_code == 32 ~ 'C31T33',
      TRUE ~ '-'
    ),
    recoded_ind = if_else(oecd_recode != oecd_code, 1, 0)
  )
saveRDS(manuf_map_recoded, 'output/nic-oecd-final-concordance.rds')

  # creating FP measure
manuf_panel <- manuf_panel %>% 
  mutate(
    forw_GVC = case_when(
      export_fob == 0 ~ 0,
      TRUE ~ (VA/income_nonfin) * (ind_interm_share)
      )
    )


# GVC position
manuf_panel <- manuf_panel %>% 
  mutate(
    gvc_position = log(1 + forw_GVC) - log(1 + back_GVC),
    gvc_position_m = gvc_position - mean(gvc_position, na.rm = TRUE)
)

sum(is.na(manuf_panel$forw_GVC))  # 459282 wow
sum(is.na(manuf_panel$back_GVC))  # 443671 fucking WOW
sum(is.na(manuf_panel$gvc_position))  # 487795 am i a joke to you 


  # can we use in intermediate export SHARE of industry to proxy for firm intermediate EXPORT?
manuf_panel <- manuf_panel %>% 
  mutate(
    forw_GVC2 = case_when(
      export_fob == 0 ~ 0,
      TRUE ~ (VA/income_nonfin)*(ind_interm_share/export_fob)
      )
    )
summary(manuf_panel$forw_GVC2)  # all summary stats are zero; so no
