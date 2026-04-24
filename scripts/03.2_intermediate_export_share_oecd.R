# ch. 3.2 : in which we calculate industry intermediate export share using OECD ICIO tables


options(scipen = 999)

library(tidyverse)


# years
years_oecd <- 1995:2022

# empty list to store yearly results
results_list <- list()

for (y in years_oecd) {
  
  # file name
  file_name <- paste0('raw_data/ICIO/', y, '_SML.csv')
  
  # read
  df <- read.csv(file_name, check.names = FALSE)
  
  # clean
  rownames(df) <- df$V1
  df$V1 <- NULL
  
  # keep only IND rows
  ind_rows <- grep('^IND_', rownames(df))
  ind_df <- df[ind_rows, ]
  
  # remove IND columns
  ind_cols <- grep('^IND_', colnames(ind_df))
  foreign_df <- ind_df[, -ind_cols]
  
  # remove OUT column
  foreign_df <- foreign_df[, !colnames(foreign_df) %in% 'OUT']
  
  # identify final demand columns
  fd_cols <- grep('_HFCE$|_NPISH$|_GGFC$|_GFCF$|_INVNT$|_DPABR$', 
                  colnames(foreign_df))
  
  # intermediate columns = everything else
  int_cols <- setdiff(seq_along(colnames(foreign_df)), fd_cols)
  
  # compute exports
  intermediate_exports <- rowSums(foreign_df[, int_cols])
  total_exports <- rowSums(foreign_df)
  share_intermediate <- intermediate_exports / total_exports
  
  # industry code
  industry_id <- sub('^IND_', '', rownames(foreign_df))
  
  # create dataframe
  result_year <- data.frame(
    year = y,
    industry = industry_id,
    intermediate_exports = intermediate_exports,
    total_exports = total_exports,
    share_intermediate = share_intermediate
  )
  
  # store
  results_list[[as.character(y)]] <- result_year
}

# bind all years
industry_intermediate_share <- do.call(rbind, results_list)

# remove rownames
rownames(industry_intermediate_share) <- NULL


# delete objects not required anymore
rm(fd_cols, file_name, ind_cols, ind_rows, industry_id, int_cols, 
   intermediate_exports, share_intermediate, total_exports, y, ind_df, 
   df, foreign_df, result_year, results_list)


# save intermediate share data

write.csv(industry_intermediate_share, file = 'output/industry-intermediate-shares-1995to2022.csv')
