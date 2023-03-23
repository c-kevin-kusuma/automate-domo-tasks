# ---------------------------------------------------------------------------------------
# Description
# This R Script is to perform these tasks:
  # Add users from PDP list that do not have DOMO accounts
  # Add necessary info to the new users
  # Add the new users to a certain group
# ---------------------------------------------------------------------------------------
library(tidyverse)
library(rdomo)
library(rlist)
amlrs_domo <- Domo(client_id='e4f0cc6d-3d15-4e13-a270-1bb0a5973e40', secret='68971f2478e0f2e1772917351f10d144e55d24db4ff36fbe366c956cc8e5cdf4')
# ---------------------------------------------------------------------------------------

# Data
userList <- amlrs_domo$users_list()
currentEmail <- userList %>% select(email) %>% unique() %>% na.omit()
currentEmployeeId <- userList %>% select(employeeId) %>% unique() %>% na.omit()
newList <- amlrs_domo$ds_query('dbf8ca2c-ca69-46d3-a699-78632203628a', "select name, email, title, employeeId, role, department, location, group_id, `Invitation Email (1/0)` from table where `Update Status` = 'Ready'")

if(nrow(newList) > 0) {newUserList <- anti_join(newList, currentEmail)} else {newUserList <- tibble()} #Remove Existing Users by email
if(nrow(newUserList) > 0) {newUserList <- anti_join(newUserList, currentEmployeeId)} else {newUserList <- tibble()} #Remove Existing Users by employeeId


for (n in 1:nrow(newUserList)) {
  if(nrow(newUserList) == 0) {break}
  else {
    # Adding a new worker
    invite <- ifelse(newUserList[n, ]$`Invitation Email (1/0)` == 1, TRUE, FALSE)
    x <- amlrs_domo$users_add(x_name = newUserList[n, ]$name, x_email = newUserList[n, ]$email, x_role = newUserList[n, ]$role, x_sendInvite = invite)
    y <- list.stack(list(x))$id %>% unique()
    
    # Update user info
    amlrs_domo$users_update(user_id = y, user_def = tibble(email = newUserList[n, ]$email, employeeId = newUserList[n, ]$employeeId, title = newUserList[n, ]$title))
    
    # Update user group membership
    amlrs_domo$groups_add_users(group_id = newUserList[n, ]$group_id, users = y)} }

# ---------------------------------------------------------------------------------------

# Record Log
amlrs_domo$ds_update("a90f97f0-b0db-40a3-8909-95fac3f44e37", tibble(Operation = 'New User', Time = Sys.time(), Status = 'Successful'))
