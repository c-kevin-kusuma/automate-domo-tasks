# ---------------------------------------------------------------------------------------
# Description
# This R Script is to perform these tasks:
# Delete terminated users based on NetSuite data
# Update User Info such as name, email, title, department, and location with Paycom data
# ---------------------------------------------------------------------------------------
library(tidyverse)
library(rdomo)
domo <- Domo(client_id='client_id', secret='secret')
# ---------------------------------------------------------------------------------------

# Data
user_list <- domo$users_list()
allEmployee <- domo$ds_query('datasetID', "select `NS.Employee Name` name, `Email` email, `Position` title, `Employee Paycom ID` employeeId, `Employee Department` department, `Employee Country` location, `Employee Status` from table where `Email` is not null and `Employee Paycom ID` is not null and `Has DOMO Account` = 'Yes'")

# ---------------------------------------------------------------------------------------

# Terminated Employees
term_list <- allEmployee %>% filter(`Employee Status` == 'Terminated') %>% select(email)
update_term_list <- inner_join(term_list, user_list) 
for (i in 1:nrow(update_term_list)) {if(nrow(update_term_list) == 0){break} else{domo$users_delete(user_id = update_term_list$id[i])} }

# ---------------------------------------------------------------------------------------

# Update employeeId Based on Email
currentEmployeeId <- domo$users_list() %>% select(id, email, employeeId) %>% mutate(employeeId = replace_na(employeeId, '')) %>% rename(currentEmployeeId = employeeId)
idComb <- select(allEmployee, c(email, employeeId)) %>% inner_join(currentEmployeeId) %>% filter(replace_na(employeeId != currentEmployeeId))

for (i in 1:nrow(idComb)) {if(nrow(idComb) == 0){break} else{domo$users_update(user_id = idComb[i, ]$id, user_def = tibble(email = idComb[i, ]$email, employeeId = idComb[i, ]$employeeId))} }

# ---------------------------------------------------------------------------------------

# Updated User Lists
user_list <- domo$users_list()

# Role and Paycom ID
domo <- user_list %>% select(id, employeeId, email, name, title, department, location) %>% filter(is.na(employeeId) == F) %>% rename(currentEmail = email, currentName = name, currentTitle = title, currentDepartment = department, currentLocation = location)
comb <- inner_join(allEmployee, domo) 
comb[is.na(comb)] <- ''

update_user_list <- comb %>% 
  mutate(aa = paste(name, email, title, department, location), bb = paste(currentName, currentEmail, currentTitle, currentDepartment, currentLocation)) %>% 
  mutate(status = ifelse(aa == bb, 1, 0)) %>% 
  filter(status == 0) %>% 
  select(id, employeeId, name, email, title, department, location)

update_user_list[is.na(update_user_list)] <- ''

for (i in 1:nrow(update_user_list)) {if(nrow(update_user_list) == 0){break} else{domo$users_update(user_id = update_user_list[i, ]$id, user_def = update_user_list[i,-1])} }

# ---------------------------------------------------------------------------------------

# Record Log
domo$ds_update("datasetID", tibble(Operation = 'User Info', Time = Sys.time(), Status = 'Successful'))
