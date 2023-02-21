# get data and dates for ergon, rt and mcc
pacman::p_load("tidyverse", "googlesheets4", "googledrive")

# MCC - see MCC.2 "DATA PREP_2021plus2019 # The CPIs are different.  Also, 2019 does not measure positivity and satisfaction
  
  
### ERGON  # don't need to run the code below again

ergon_sept2019 <- readRDS("~/Dropbox/Clients/Frontiers research/Ergon_2019.rds") %>%
  mutate(Year = replace_na("2019")) %>%
  add_column(month = "September")# see (DCI/)Clients/Ergon/data prep Ergon 2019.R
ergon_oct2022 <- readRDS("~/Dropbox/Clients/Frontiers research/ergon_oct2022.rds") %>% rename(LOCATION = "REGION") %>%
  add_column(month = "September") # reran the data prep script carefully and added df_change to the set
 colnames(ergon_sept2019)[1:68] <- colnames(ergon_oct2022)[1:68] # there were some difference in company vs organization in the item language
ergon <- full_join(ergon_sept2019, ergon_oct2022) %>% add_column(company = "Ergon")
saveRDS(ergon, "~/Dropbox/Clients/Frontiers research/ergon.rds")

######

MCC <- readRDS("~/Dropbox/Clients/Frontiers research/MCC.rds") %>% filter(!is.na(Change))
ergon <- readRDS("~/Dropbox/Clients/Frontiers research/ergon.rds")

df_change_erg <- ergon %>% 
  select(ResponseId, Change, nps, company, Year, month, TENURE, GENERATION, ROLE, LOCATION, COMPANY)
df_change_mcc <- MCC %>% 
  select(ResponseId, Change, nps, company, Year, month, TENURE, GENERATION, ROLE, LOCATION, GENDER)

df_change <- df_change_erg %>% 
  full_join(., df_change_mcc) %>% 
  select(ResponseId, nps:GENDER, Change) %>% 
  filter(!is.na(Change) & Change != "N/A" & Change != "N/a" & Change != "n/a")

write_csv(df_change, "~/Dropbox/Clients/Frontiers research/df_change_frontiers.csv")

drive_trash("df_change_labelling")
gs4_create(name = "df_change_labelling", sheets = list(
  df = df_change %>% select(ResponseId, company, Change) %>% arrange(ResponseId)))#, anovas = anovas_sig_no_nests_basic, nps_regression = model))
drive_mv("df_change_labelling", path = paste0("~/DCI/Frontiers pre-post research/data/"))   
