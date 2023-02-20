##### ERGON qualitative analyses

df_labelled_gsheet <- "https://docs.google.com/spreadsheets/d/1vQgXI6KzRiaT4nHrOZlh8Fw27hDqidvl2bw2fq6P7Ag/edit#gid=1415113395"
df_change_gsheet <- "https://docs.google.com/spreadsheets/d/138WUGnw9qT8ebrjkymcoKbwYQJFBr5xJgvuOC7jEg2I/edit#gid=280769640"
df_change <- read_sheet(df_change_gsheet)


df_labelled_read <- read_sheet(df_labelled_gsheet, sheet = "Izzy") %>% 
  rename(org_valence = "org valence (1-5) 5 = worst", amount_change = "degree(amount) of change (1-5) 5 = most") %>% 
  filter(!is.na(org_valence))

df_labelled <- df_labelled_read %>% 
  inner_join(., df_change) %>% # df_change is the other dataset in gdrive called
  unite("tags", tag1:tag16, sep = ",", na.rm = TRUE) %>% 
  mutate(valence_amount_sum = org_valence + amount_change) %>% 
  select(ResponseId, company, Year, org_valence, amount_change, valence_amount_sum, tags, TENURE:COMPANY)

measures <- c("org_valence", "amount_change", "valence_amount_sum")
measures_loop <- list()

for(m in 1:length(measures)){
  measures_loop[[m]] <- tidy(t.test(get(measures[[m]]) ~ Year, data = df_labelled)) %>% 
  select(p.value) %>% 
  add_column(measure = measures[[m]]) %>% 
  add_column(mean2019 = mean(df_labelled %>% filter(Year == "2019") %>% pull(get(measures[[m]]))),
             mean2022 = mean(df_labelled %>% filter(Year == "2022") %>% pull(get(measures[[m]])))) %>% 
  add_column(`22minus19-meanDiff` = mean(df_labelled %>% filter(Year == "2022") %>% pull(get(measures[[m]])))- 
               mean(df_labelled %>% filter(Year == "2019") %>% pull(get(measures[[m]]))),.before=1) %>% 
    add_column(n19n22 = str_c(df_labelled %>% group_by(Year) %>% count() %>% arrange(Year) %>% pull(n), collapse = ", ")) %>% 
  mutate(meaning = case_when(
    sign(`22minus19-meanDiff`) == -1 & p.value <= .05 ~ "2019 shows sig higher negativity",
    sign(`22minus19-meanDiff`) == -1 & p.value <= .1 ~ "2019 shows marginally higher negativity",
    sign(`22minus19-meanDiff`) == -1 & p.value <= .2 ~ "2019 is trending toward higher negativity",
    sign(`22minus19-meanDiff`) == -1 & p.value > .2 ~ "2019 is higher but not noteworthy yet",
    sign(`22minus19-meanDiff`) == 1 & p.value <= .05 ~ "2022 shows sig higher negativity",
    sign(`22minus19-meanDiff`) == 1 & p.value <= .1 ~ "2022 shows marginally higher negativity",
    sign(`22minus19-meanDiff`) == 1 & p.value <= .2 ~ "2022 is trending toward higher negativity",
    sign(`22minus19-meanDiff`) == 1 & p.value > .2 ~ "2022 is higher in negativity but not noteworthy yet"))
}
    
t_tests <- bind_rows(measures_loop) %>% select(measure, p.value, meaning, everything())