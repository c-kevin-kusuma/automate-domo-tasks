# ---------------------------------------------------------------------------------------
# Description
# This R Script is to perform these tasks:
# Delete users' memberships from certain groups
# Add users to certain groups
# ---------------------------------------------------------------------------------------
library(tidyverse)
library(pRoDomo)
amlrs_domo <- Domo(client_id='e4f0cc6d-3d15-4e13-a270-1bb0a5973e40',
                   secret='68971f2478e0f2e1772917351f10d144e55d24db4ff36fbe366c956cc8e5cdf4')
# ---------------------------------------------------------------------------------------

# Automated List
automated_list <- 
  amlrs_domo$ds_query('648b8b0b-f1a1-4311-bd00-c1dab51b1a88', 
                'select `Group ID`, `DOMO employeeId`*1 `DOMO employeeId` from table')

group_list <- automated_list %>% 
  group_by(`Group ID`) %>% 
  summarise(n = n()) %>% 
  rowid_to_column() %>% 
  select(-n) 

group_user <- automated_list %>% select(`Group ID`, `DOMO employeeId`) %>% 
  left_join(group_list)

# ---------------------------------------------------------------------------------------

# Update the Groups and Users
for (i in 1:nrow(group_list)) {
  group_id <- group_list[i, ]$`Group ID`
  
  # Current Users
  current_list <- tibble(`DOMO employeeId` = amlrs_domo$groups_list_users(group_id = group_id))
  correct_list <- group_user %>% filter(`Group ID` == group_id) %>% select(`DOMO employeeId`)
  
  # Delete Users
  delete_list <- current_list %>% anti_join(correct_list)
  if(nrow(delete_list) > 0) {amlrs_domo$groups_remove_users(group_id = group_id, users = delete_list$`DOMO employeeId`)}
  
  # Add Users
  add_list <- correct_list %>% anti_join(current_list)
  if(nrow(add_list) > 0) {amlrs_domo$groups_add_users(group_id = group_id, users = add_list$`DOMO employeeId`)}
}

# ---------------------------------------------------------------------------------------

# Record Log
amlrs_domo$ds_update("a90f97f0-b0db-40a3-8909-95fac3f44e37", tibble(Operation = 'Group Management', Time = Sys.time(), Status = 'Successful'))
