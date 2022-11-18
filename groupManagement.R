# ---------------------------------------------------------------------------------------
# Description
# This R Script is to perform these tasks:
# Delete users' memberships from certain groups
# Add users to certain groups
# ---------------------------------------------------------------------------------------
library(tidyverse)
library(rdomo)
domo <- Domo(client_id='client_id', secret='secret')
# ---------------------------------------------------------------------------------------

# Automated List
automated_list <- 
  domo$ds_query('datasetID', 
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
  current_list <- tibble(`DOMO employeeId` = domo$groups_list_users(group_id = group_id))
  correct_list <- group_user %>% filter(`Group ID` == group_id) %>% select(`DOMO employeeId`)
  
  # Delete Users
  delete_list <- current_list %>% anti_join(correct_list)
  if(nrow(delete_list) > 0) {domo$groups_remove_users(group_id = group_id, users = delete_list$`DOMO employeeId`)}
  
  # Add Users
  add_list <- correct_list %>% anti_join(current_list)
  if(nrow(add_list) > 0) {domo$groups_add_users(group_id = group_id, users = add_list$`DOMO employeeId`)}
}

# ---------------------------------------------------------------------------------------

# Record Log
domo$ds_update("datasetID", tibble(Operation = 'Group Management', Time = Sys.time(), Status = 'Successful'))
