###### FUNCTION CHI SQUARE
chi <- function(tag_groupings_chi = NULL, tags_chi = NULL, groupingsAndOr = "|", tagsAndOr = "|") {
  
  if(!is.null(tag_groupings_chi) & is.null(tags_chi)){
    df <- df_cats %>% 
      mutate(tag_test = grepl(paste(tag_groupings_chi, collapse = groupingsAndOr), groupings,ignore.case = TRUE))
  } else if (is.null(tag_groupings_chi) & !is.null(tags_chi)){
    df <- df_cats %>% 
      mutate(tag_test = grepl(paste(tags_chi, collapse = tagsAndOr), tags,ignore.case = TRUE)) 
  } else if (!is.null(tag_groupings_chi) & !is.null(tags_chi)){
    df <- df_cats %>% 
      mutate(tag_test = grepl(paste(tag_groupings_chi, collapse = groupingsAndOr), groupings,ignore.case = TRUE) & 
               grepl(paste(tags_chi, collapse = tagsAndOr), tags,ignore.case = TRUE)) 
  }
  
  df <- df %>%  
    filter(company == "Ergon") %>% 
    filter(!is.na(tags)) %>% 
    select(ResponseId, groupings, tag_test, everything())
  
  test <- tidy(chisq.test(df$Year, df$tag_test))
  
  freqs <- df %>% group_by(Year, tag_test) %>% count() %>% 
    ungroup() %>% 
    pivot_wider(names_from = Year, values_from = n) %>% 
    pivot_wider(names_from = tag_test, values_from = c(`2019`, `2022`)) %>% 
    mutate(`2019_ratio` = `2019_TRUE`/(`2019_FALSE` + `2019_TRUE`), `2022_ratio` = `2022_TRUE`/(`2022_FALSE` + `2022_TRUE`)) %>% 
    add_column(p = test$p.value) %>% 
    add_column(groupings = str_c(tag_groupings_chi, collapse = ", "), AndOrGroupings = groupingsAndOr, tags = str_c(tags_chi, collapse = ", "),AndOrTags = tagsAndOr,  .before=1) %>% 
    select(groupings, AndOrGroupings, tags, AndOrTags, p, `2019_ratio`, `2022_ratio`, `2019_TRUE`, `2022_TRUE`, `2019_FALSE`, `2022_FALSE`)
  return(freqs)
}


# loop through length of uniqueGroupings on the following function
uniqueGroupings <- unique(df_cats %>% filter(company == "Ergon") %>% unnest_tokens(allGroupings, groupings, token = 'regex', pattern=",") %>% pull(allGroupings)) # this get all unique groupings in df
groupings_chi_loop <- list()
for(g in 1:length(uniqueGroupings)) {
  groupings_chi_loop[[g]] <- chi(tag_groupings_chi = uniqueGroupings[[g]])
}
uniqueGroupingsChi <- bind_rows(groupings_chi_loop)

# loop through length of uniqueTags on the following function
uniqueTags <- unique(df_cats %>% filter(company == "Ergon") %>% unnest_tokens(allTags, tags, token = 'regex', pattern=",") %>% pull(allTags)) # this get all unique tags in df
tags_chi_loop <- list()
for(t in 1:length(uniqueTags)) {
  tags_chi_loop[[t]] <- chi(tags_chi = uniqueTags[[t]])
}
uniqueTagsChi <- bind_rows(tags_chi_loop) %>% arrange(p)


groupingsAndTags <- bind_rows(uniqueGroupingsChi, uniqueTagsChi) %>% arrange(p)

drive_trash("chi-square-tags")
gs4_create(name = "chi-square-tags", sheets = list(
  chiSquare = groupingsAndTags))#, anovas = anovas_sig_no_nests_basic, nps_regression = model))
drive_mv("chi-square-tags", path = paste0("~/DCI/Frontiers pre-post research/analyses/"))   

