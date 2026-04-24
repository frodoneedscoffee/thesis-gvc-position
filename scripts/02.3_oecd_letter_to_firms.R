# ch. 2.3 : in which we extract the ISIC codes for each firm for OECD concordance


options(scipen = 999)

library(tidyverse)


# assign ISIC codes to each firm
firm_panel <- firm_panel %>% 
  mutate(
    isic_code =  case_when(
      # split ISIC 24 based on NIC codes
      str_starts(nic_code, "^241") ~ "241",  # NIC 2410-2419 → 241
      str_starts(nic_code, "^242") ~ "242",  # NIC 2420-2429 → 242
      str_starts(nic_code, "^2431") ~ "2431",
      str_starts(nic_code, "^2432") ~ "2432",
      
      # split ISIC 30 based on NIC codes
      str_starts(nic_code, "^301") ~ "301",
      str_starts(nic_code, "^302") ~ "302",
      str_starts(nic_code, "^303") ~ "303",
      str_starts(nic_code, "^304") ~ "304",
      str_starts(nic_code, "^309") ~ "309",
      
      TRUE ~ substr(nic_code, 1, 2)
    )
  )


# filter out manufacturing firms
manuf_panel <- firm_panel %>% 
  filter(isic_code %in% c('10':'33') |
           isic_code %in% c('241', '242', '2431', '2432', '301':'304', '309'))

  # all companies in the set
company_list <- manuf_panel %>%
  distinct(company_name)
write.csv(company_list, "output/manufacturing-firms-in-panel.csv", row.names = FALSE)

  # sanity checks
sum(is.na(manuf_panel$isic_code)) # no NAs
ind2431 <- manuf_panel %>% filter(isic_code == '2431') # industry exists

# join OECD letter identifiers according to concordance table
manuf_panel <- manuf_panel %>% 
  left_join(manuf_map %>% 
              select(-c(nic_code)),
            by = c("isic_code" = "isic_code"))


# checking whether certain industries exist with correct concordance
manuf24 <- manuf_panel %>% filter(isic_code == '241' | isic_code == '242')
manuf243 <- manuf_panel %>% filter(isic_code == '2431' | isic_code == '2432')
manuf30 <- manuf_panel %>% filter(isic_code == '301' | isic_code == '302' 
                                  | isic_code == '303' | isic_code == '304' 
                                  | isic_code == '309')


missing_oecd <- manuf_panel[is.na(manuf_panel$oecd_code), ] 
missing_oecd %>% filter(isic_code != 24)

# PROBLEM: sector 24 has no corresponding OECD code
# OECD industry identifiers for this sector use 3-4 digit ISIC codes for the mapping
# that is, the OECD intermediate output data is available at a higher granularity


