# ch. 3.1 : in which we tidy up the I/O data into a clean table of intermediate and total outputs


options(scipen = 999)

library(tidyverse)


# importing I/O data (2022)
io_2022 <- read.csv('raw_data/IND2022ttl.csv')

# summing over row to find intermediate output of each industry
interm_2022 <- io_2022 %>% 
  rename(oecd_ind = X) %>% # select 'X' as oecd_ind later
  mutate(interm_output = rowSums(io_2022[2:50], na.rm = TRUE)) %>% # sum rows (col 2 to 50)
  select(oecd_ind, interm_output)

interm_2022 <- interm_2022[-c(51:59),] # dropping the non-industry rows at the end

interm_2022 <- interm_2022 %>%
  mutate(oecd_ind = str_remove(oecd_ind, "^TTL_")) # removes TTL_ at the start of identifier

# extracting total output per industry from output row, transposing and adding to interm_1995
row_data <- t(as.matrix(io_2022[59, , drop = FALSE])) # select OUTPUT row (59) + transpose
outputs_2022 <- data.frame(
  oecd_ind = colnames(io_2022), 
  output = as.numeric(row_data))

interm_2022 <- interm_2022 %>% 
  left_join(outputs_2022, by = c('oecd_ind' = 'oecd_ind'))



# we do this for all years from 1995 to 2021 by looping above (source: semiconductor entity)
years_oecd <- 1995:2021 

all_io <- list()  # empty list to store yearly data

for (y in years_oecd) { # the almighty loop
  
  # 1. read file
  file_name <- paste0("raw_data/IND", y, "ttl.csv")
  io_data <- read.csv(file_name)
  
  # 2. intermediate output (row sums of industry columns)
  interm <- io_data %>%
    rename(oecd_ind = X) %>%
    mutate(interm_output = rowSums(across(2:50), na.rm = TRUE)) %>%
    select(oecd_ind, interm_output)
  
  # drop non-industry rows (last rows)
  interm <- interm %>%
    filter(!str_detect(oecd_ind, "^TOTAL|^VA|^OUT"))
  
  # clean industry names
  interm <- interm %>%
    mutate(oecd_ind = str_remove(oecd_ind, "^TTL_"))
  
  # 3. extract OUTPUT row dynamically (instead of hardcoding row 59)
  output_row <- io_data %>%
    filter(str_detect(X, "OUTPUT|OUT")) %>%
    select(-X)
  
  outputs <- data.frame(oecd_ind = colnames(output_row),
                        output   = as.numeric(output_row[1, ]))
  
  # 4. merge
  final_year <- interm %>%
    left_join(outputs, by = "oecd_ind") %>%
    mutate(year = y)
  
  # 5. store
  all_io[[as.character(y)]] <- final_year
}


# combine all years
io_panel <- bind_rows(all_io)
io_panel <- bind_rows(io_panel, interm_2022) # binding separately as 2022 was done prior


# export
write.csv(io_panel, 'output/oecd-io-1995to2022.csv')
