
df_tag_groups <- 
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