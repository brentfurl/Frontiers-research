#  read in data

df_labelled_gsheet <- "https://docs.google.com/spreadsheets/d/1vQgXI6KzRiaT4nHrOZlh8Fw27hDqidvl2bw2fq6P7Ag/edit#gid=1415113395"
df_change_gsheet <- "https://docs.google.com/spreadsheets/d/138WUGnw9qT8ebrjkymcoKbwYQJFBr5xJgvuOC7jEg2I/edit#gid=280769640"
tag_groups_url <- "https://docs.google.com/spreadsheets/d/1LO2OJqLkSz2cnqMsPmDGf_-GUABBXzDuA8qtWS9Fr4k/edit#gid=0"

tag_groups_raw <- read_sheet(tag_groups_url)
df_change <- read_sheet(df_change_gsheet) %>% mutate(nps = unlist(nps))
df_labelled_izzy <- read_sheet(df_labelled_gsheet, sheet = "Izzy") 
df_labelled_brent <- read_sheet(df_labelled_gsheet, sheet = "Brent") 
df_labelled_final <- read_sheet(df_labelled_gsheet, sheet = "Final") 


################################################################

tag_groups <- tag_groups_raw %>% 
  pivot_longer(everything(), names_to = "grouping", values_to = "tag")


df_labelled_tot <- full_join(df_labelled_izzy %>% 
                                      unite("tags", tag1:tag10, sep = ", ", na.rm = TRUE) %>% 
                                      select(ResponseId, company, Change, tags) %>% 
                                      unnest_tokens(tag, tags, token = 'regex', pattern=",") %>% 
                                      mutate(tag = str_trim(tag)), 
                             df_labelled_brent %>% 
                                      unite("tags", tag1:tag10, sep = ", ", na.rm = TRUE) %>% 
                                      select(ResponseId, company, Change, tags) %>% 
                                      unnest_tokens(tag, tags, token = 'regex', pattern=",") %>% 
                                      mutate(tag = str_trim(tag))) 

df_cats <- df_labelled_tot %>% 
  left_join(tag_groups, by = "tag") %>% 
  mutate(grouping = replace_na(grouping, "")) %>% 
  group_by(ResponseId) %>% 
  summarise(groupings = paste(unique(grouping)[unique(grouping) != ""], collapse=","), tags = paste(unique(tag), collapse=",")) %>% 
  right_join(., df_change, by = "ResponseId") %>% 
  filter(company == "Ergon") %>% 
  mutate(ROLE = factor(ROLE)) %>% 
  mutate(ROLE = fct_collapse(ROLE,
                             Technical = c("Technical", "Management", "Research + Technology Development", "Health, Safety + Environment"))) %>% 
  mutate(roleOpsVsAll = fct_collapse(ROLE,
                                     nonOperations = c("Technical", "Admin + Corporate Support", "Finance + Accounting", "Information Technology", "Sales + Marketing"))) %>% 
  filter(!is.na(tags)) 

# df_cats %>%  filter(Year == "2022") %>%  group_by(ROLE) %>% count()



