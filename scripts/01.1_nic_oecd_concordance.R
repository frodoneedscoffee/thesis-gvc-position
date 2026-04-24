# ch. 1.1 : in which we concord NIC to the OECD alphanumeric soup


options(scipen = 999)

library(tidyverse)


nic2008key <- read.csv('raw_data/NIC 2008.csv') # NIC 2008 codes
oecdisic4key <- read.csv('raw_data/OECD IO ISIC 4 Concordance.csv') # OECD-ISIC4 concordance 


colnames(oecdisic4key) <- c('s_no', 'oecd_code', 'desc_oecd', 'isic_code') # rename columns
oecdisic4key <- oecdisic4key %>% 
  select('oecd_code', 'desc_oecd', 'isic_code') %>% 
  mutate(isic_code = as.character(isic_code))

nic2008key <- nic2008key %>% 
  rename(desc_nic = description,
         nic_code = division,
         group_nic = group)


# NIC maps one-to-one to ISIC at the 2-digit level
nic2008key <- nic2008key %>% 
  mutate(nic_code = as.character(nic_code), # convert number to text for string operation
         isic_code = substr(nic_code, 1, 2)) # extract the first 2 digits 

nic2008key <- nic2008key %>%
  mutate(
    isic_code = case_when(
      # split ISIC 24 based on NIC codes
      str_starts(group_nic, "^241") ~ "241",  # NIC 2410-2419 → 241
      str_starts(group_nic, "^242") ~ "242",  # NIC 2420-2429 → 242
      str_starts(group_nic, "^243") ~ "243",  # NIC 2430-2439 → 243
      
      # split ISIC 30 based on NIC codes
      str_starts(group_nic, "^301") ~ "301",
      str_starts(group_nic, "^302") ~ "302",
      str_starts(group_nic, "^303") ~ "303",
      str_starts(group_nic, "^304") ~ "304",
      str_starts(group_nic, "^309") ~ "309",
      
      TRUE ~ isic_code
    )
  )


# concord the two by common ISIC code
nic_oecd_concordance <- nic2008key %>% 
  left_join(oecdisic4key, by = c("isic_code" = "isic_code"))

nic_oecd_concordance <- nic_oecd_concordance %>% 
  select(nic_code, group_nic, isic_code, oecd_code, desc_nic, desc_oecd)


# 2431 and 2432 are manually added
nic_oecd_concordance <- nic_oecd_concordance %>% 
  add_row(nic_code = '24', group_nic = 243, isic_code = '2431', oecd_code = 'C24A', 
          desc_nic = 'Casting of metals', 
          desc_oecd = 'Manufacture of basic iron and steel') %>% 
  add_row(nic_code = '24',  group_nic = 243, isic_code = '2432', oecd_code = 'C24B', 
          desc_nic = 'Casting of metals', 
          desc_oecd = 'Manufacture of basic precious and other non-ferrous metals ')

nic_oecd_concordance <- nic_oecd_concordance %>% 
  mutate(nic_code = as.integer(nic_code),
         isic_code = as.character(isic_code)) %>% 
  arrange(nic_code, group_nic, oecd_code)


# check for missing concordances
concord_missing <- nic_oecd_concordance[is.na(nic_oecd_concordance$oecd_code), ] # only 243 is missing
nic_oecd_concordance <- nic_oecd_concordance %>% drop_na() # drop 243 as it's covered by 2431 & 2432


# export concordance file
write.csv(nic_oecd_concordance, 'output/nic-oecd-concordance.csv')
