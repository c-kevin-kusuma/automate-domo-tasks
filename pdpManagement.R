# ---------------------------------------------------------------------------------------
# Description
# This R Script is to perform these tasks:
  # Delete PDP policies from certain datasets
  # Add policies to certain datasets
  # Update policies on certain datasets
# ---------------------------------------------------------------------------------------
library(tidyverse)
library(rdomo)
library(data.table)
library(rlist)
amlrs_domo <- Domo(client_id='e4f0cc6d-3d15-4e13-a270-1bb0a5973e40', secret='68971f2478e0f2e1772917351f10d144e55d24db4ff36fbe366c956cc8e5cdf4')
# ---------------------------------------------------------------------------------------

# Functions
`%!in%` <- Negate(`%in%`)
`%!like%` <- Negate(`%like%`)
extractPdp <- function(x) {
  if(length(x)==0) {break}
  for (i in 1:length(x)) {
    if(length(x[[i]]$users) == 0){users <- tibble(users = '')} else{users <- tibble(users = x[[i]]$users) %>% mutate(users = as.character(users)) %>% arrange(users) %>% group_by() %>% summarise(users = paste(users, collapse = '|'))} # Extract Users
    if(length(x[[i]]$filters) == 0){filters <- tibble(column = '', values = '')} else{filters <- x[[i]]$filters %>% list.stack() %>% select(column, values) %>% mutate(values = as.character(values)) %>% arrange(values) %>% group_by(column) %>% summarise(values = paste(values, collapse = '|'))}
    x[[i]] <- tibble(`Policy ID` = x[[i]]$id, `Policy Name` = x[[i]]$name, `Policy Column` = filters$column, `User ID` = users$users, `Policy Value` = filters$values)}
  
  y <- bind_rows(x)
}
reorderPdp <- function(x) {
  z <- list()
  for (i in 1:nrow(x)) {
    if(nrow(x) == 0){break} else{
      di = x$`Dataset ID`[i]
      pm = x$`Policy Name`[i]
      pc = x$`Policy Column`[i]
      
      `User ID` <- tibble(`User ID` = strsplit(x$`User ID`[i], '|', fixed = TRUE) %>% unlist() %>% as.character())
      `User ID` <- `User ID` %>% arrange(`User ID`) %>% group_by() %>% summarise(`User ID` = paste(`User ID`, collapse = '|'))
      
      `Policy Value` <- tibble(`Policy Value` = strsplit(x$`Policy Value`[i], '|', fixed = TRUE) %>% unlist() %>% as.character())
      `Policy Value` <- `Policy Value` %>% arrange(`Policy Value`) %>% group_by() %>% summarise(`Policy Value` = paste(`Policy Value`, collapse = '|'))
      
      z[[i]] <- tibble(`Dataset ID` = di, `Policy Name` = pm, `Policy Column` = pc, `User ID` = `User ID`$`User ID`, `Policy Value` = `Policy Value`$`Policy Value`)
    }
  }
  z <- bind_rows(z)
}
createPdpList <- function(x){
    if('Policy ID' %in% colnames(x)){id <- as.integer(x$`Policy ID`)} else{id <- NULL}
    filters <- list()
    users <- list()
    longFilters <- x$`Policy Value` %>% strsplit('|', fixed = TRUE) %>% unlist()
    longUsers <- x$`User ID` %>% strsplit('|', fixed = TRUE) %>% unlist()
    for (i in 1:length(longFilters)) {filters[[i]] <- list(column = x$`Policy Column`, values = list(longFilters[i]), operator = 'EQUALS', not = FALSE) } # Create Filters
    for (i in 1:length(longUsers)) {users[[i]] <- as.integer(longUsers) } # Create Users
    pdpList <- list(id = id, type = 'user', name = x$`Policy Name`, filters = filters, users = users, virtualUsers = list(), groups = list())
    if('Policy ID' %!in% colnames(x)){pdpList$id <- NULL}
    return(pdpList)
}

# ---------------------------------------------------------------------------------------

# Data
pdpData <- amlrs_domo$ds_query('e1872c03-b644-487d-83ce-88f04273be8d', "select `Dataset ID`, `Policy Name`, `Policy Column`, `User ID`, `Policy Value` from table")
pdpDs <- pdpData %>% select(`Dataset ID`, `Policy Column`) %>% unique()

# ---------------------------------------------------------------------------------------

# Update Loop
for (a in 1:nrow(pdpDs)) {
  if(nrow(pdpDs) == 0){break} else{
    dsID <- pdpDs$`Dataset ID`[a]
    polColumn <- pdpDs$`Policy Column`[a]
    
    # Current PDP List
    curPolicy <- amlrs_domo$pdp_list(ds = dsID)
    curPolicy <- extractPdp(curPolicy) %>% filter(`Policy Name` %!like% 'AA - Restricted' & `Policy Name` != 'All Rows')
    
    # IF NO current policies found on the dataset
    if(nrow(curPolicy) == 0){
      # ADD POLICIES
      emptData <- pdpData %>% filter(`Dataset ID` == dsID)
      for (i in 1:nrow(emptData)) {if(nrow(emptData) == 0) {break} else{amlrs_domo$pdp_create(ds = dsID, policy_def = createPdpList(emptData[i,]))} }
    } 
    else{
      # Correct PDP List
      corPolicy <- pdpData %>% filter(`Dataset ID` == dsID)
      corPolicy <- corPolicy %>% reorderPdp()
      
      # Action List
      addList <- anti_join(corPolicy, curPolicy, by = c('Policy Name'='Policy Name'))
      delList <- anti_join(curPolicy, corPolicy, by = c('Policy Name'='Policy Name'))
      updList <- anti_join(curPolicy, corPolicy) %>% filter(`Policy ID` %!in% delList$`Policy ID`) %>% select(`Policy ID`, `Policy Name`)
      updList <- left_join(updList, corPolicy)
      
      # DELETE POLICIES
      for (i in 1:nrow(delList)) { if(nrow(delList) == 0) {break} else{amlrs_domo$pdp_delete(ds = dsID, policy = delList$`Policy ID`[i])} }
      
      # ADD POLICIES
      for (i in 1:nrow(addList)) { if(nrow(addList) == 0) {break} else{amlrs_domo$pdp_create(ds = dsID, policy_def = createPdpList(addList[i,]))} }
      
      # UPDATE POLICIES
      for (i in 1:nrow(updList)) { if(nrow(updList) == 0) {break} else{amlrs_domo$pdp_update(ds = dsID, policy = updList$`Policy ID`[i], policy_def = createPdpList(updList[i,]))} }
      }
    }
}

# ---------------------------------------------------------------------------------------

# Record Log
amlrs_domo$ds_update("a90f97f0-b0db-40a3-8909-95fac3f44e37", tibble(Operation = 'PDP Management', Time = Sys.time(), Status = 'Successful'))
