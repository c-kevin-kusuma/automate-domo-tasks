# Libraries
library(tidyverse)
library(rdomo)
library(httr)
library(rlist)
library(lubridate)

# Credentials
amlrs_domo <- Domo(client_id='e4f0cc6d-3d15-4e13-a270-1bb0a5973e40', secret='68971f2478e0f2e1772917351f10d144e55d24db4ff36fbe366c956cc8e5cdf4')
user <- 'ISU_Netsuite_RaaS_Reports'
password <- '7m*DcCrwP5WD*2'
mainURL <- 'https://services1.myworkday.com/ccx/service/customreport2/amlrightsource/'

# RaaS Table
time_off <- httr::GET(url = paste0(mainURL, 'ISU_Netsuite_RaaS_Reports/CRI_NetSuite_Time_Off_Entry_Details_RaaS?format=json'), authenticate(user = user, password = password), query = list(Date_and_Time_Approved='2021-06-30T09:10:14')) %>% content()
time_off <- time_off$Report_Entry

# Loop
uploadReady <- list()
for (i in 1:length(time_off)) {
  x <- time_off[[i]]
  mainBody <- x %>% list() %>% list.stack() %>% select(-Time_Off_Details_group) %>% unique()
  timeBody <- x$Time_Off_Details_group %>% list.stack()
  combined <- merge(mainBody, timeBody)
  uploadReady[[i]] <- combined
}
uploadReady <- bind_rows(uploadReady)

uploadReady_cleaned <- uploadReady %>% mutate(
  units = as.double(units),
  hours = ifelse(unitOfTime == 'Days', units * 8, as.double(units)))

amlrs_domo$ds_update("44ad5749-3518-495c-bd55-56041e5b5557", uploadReady_cleaned)

# Record Log
amlrs_domo$ds_update("a90f97f0-b0db-40a3-8909-95fac3f44e37", tibble(Operation = 'Workday PTO', Time = Sys.time(), Status = 'Successful'))

