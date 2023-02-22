####
library(MASS)
# df_cats...for now get from "qualitative tag grouping.R", I'll save it later when tagging is complete

#loglin <- function(tag_groupings_chi = NULL, tags_chi = NULL, groupingsAndOr = "|", tagsAndOr = "|") {
  
tag_groupings_chi = "talent"
tags_chi = NULL 
groupingsAndOr = "|"
tagsAndOr = "|"



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
    dplyr::select(ResponseId, groupings, tag_test, everything()) %>% 
    filter(ROLE != "NA" & ROLE != "Board of Directors") %>% 
    droplevels 
  
table <- xtabs(~tag_test + Year + roleOpsVsAll, data=df)
  #UCBAdmissions
# https://www.youtube.com/watch?v=6xB2bA-8ymo # walk through a log-linear analysis
# figure out what you might want to see for each analysis...

# saturated <- loglm(~ tag_test*roleOpsVsAll*Year, data = table)
# summary(saturated)
# threeWay <- update(saturated, .~. -tag_test:roleOpsVsAll:Year)
# summary(threeWay)
# 
# anova(saturated, threeWay)

  loglm(~ tag_test + roleOpsVsAll + Year, data = table)
  
  plot(table, shade=TRUE) 
  
#   return(x)
# }