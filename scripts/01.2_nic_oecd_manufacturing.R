# ch. 1.2 : in which we create a one-to-one mapping for manufacturing


options(scipen = 999)

library(tidyverse)


# creating a one-to-one key to identify OECD industry
mapping <- nic_oecd_concordance %>% 
  select(nic_code, isic_code, oecd_code, desc_nic, desc_oecd)
mapping <- mapping[!duplicated(mapping[c('isic_code')]), ] # FINAL MAP

write.csv(mapping, 'output/nic-oecd-mapping.csv')


# filter out manufacturing: 10 to 33
manuf_map <- mapping %>% 
  filter(nic_code %in% c('10':'33'))

write.csv(manuf_map, 'output/nic-oecd-mapping-manufacturing.csv')
