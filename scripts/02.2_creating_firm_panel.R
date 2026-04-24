# ch. 2.2 : in which we create the final panel


options(scipen = 999)

library(tidyverse)


# there are more companies in id_df than in the others
# we create a new id_df : id_df_new for the common companies
all_companies <- unique(c(financials_df_1$company_name,
                          financials_df_2$company_name,
                          exim_df$company_name, 
                          othervar_df$company_name))

id_df_new <- data.frame(company_name = all_companies)
id_df_new <- id_df_new %>% left_join(id_df, by = 'company_name')  

sum(is.na(id_df_new$prowess_code))  # 5 NAs


# create final dataframe using id_df_new
firm_panel <- id_df_new %>% left_join(financials_df_1, by = 'company_name')

firm_panel <- firm_panel %>% left_join(financials_df_2, by = c('company_name', 'year'))
firm_panel <- firm_panel %>% left_join(exim_df, by = c('company_name', 'year'))
firm_panel <- firm_panel %>% left_join(othervar_df, by = c('company_name', 'year'))
firm_panel <- firm_panel %>% left_join(finishedgood_df, by = c('company_name', 'year'))

firm_panel <- firm_panel %>% mutate(year = as.numeric(year))
firm_panel <- firm_panel %>% mutate(inc_year = as.numeric(inc_year))

sum(is.na(firm_panel$prowess_code))  # 145 NAs


# export
write.csv(firm_panel, 'output/all-firm-panel-1994to2022.csv')


# check nulls
null_values <- colMeans(is.na(firm_panel))
null_in_panel <- data.frame(variable = names(null_values),
                             null_proportion = null_values)

null_in_panel <- null_in_panel %>% arrange(desc(null_proportion))
rownames(null_in_panel) <- NULL

subset(null_in_panel, null_proportion > 0.5)

       