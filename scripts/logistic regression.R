library(MASS)
library(sjPlot)
library(emmeans)
library(effects)

# tag_groupings_logit = NULL
# tags_logit = "development" 
# groupingsAndOr = "|"
# tagsAndOr = "|"

logistic <- function(tag_groupings_logit = NULL, tags_logit = NULL, groupingsAndOr = "|", tagsAndOr = "|") {

if(!is.null(tag_groupings_logit) & is.null(tags_logit)){
  df <- df_cats %>% 
    mutate(tag_test = grepl(paste(tag_groupings_logit, collapse = groupingsAndOr), groupings,ignore.case = TRUE))
} else if (is.null(tag_groupings_logit) & !is.null(tags_logit)){
  df <- df_cats %>% 
    mutate(tag_test = grepl(paste(tags_logit, collapse = tagsAndOr), tags,ignore.case = TRUE)) 
} else if (!is.null(tag_groupings_logit) & !is.null(tags_logit)){
  df <- df_cats %>% 
    mutate(tag_test = grepl(paste(tag_groupings_logit, collapse = groupingsAndOr), groupings,ignore.case = TRUE) & 
             grepl(paste(tags_logit, collapse = tagsAndOr), tags,ignore.case = TRUE)) 
}

df <- df %>%  
  filter(company == "Ergon") %>% 
  filter(!is.na(tags)) %>% 
  dplyr::select(ResponseId, groupings, tag_test, everything()) %>% 
  filter(ROLE != "NA" & ROLE != "Board of Directors") %>% 
  droplevels 

test <- tidy(glm(tag_test ~ Year + roleOpsVsAll + Year*roleOpsVsAll, data = df, family = binomial))

main <- glm(tag_test ~ Year + roleOpsVsAll, data = df, family = binomial)
interaction <- glm(tag_test ~ Year + roleOpsVsAll + Year*roleOpsVsAll, data = df, family = binomial)
anv <- anova(main, interaction, test = "Chisq")

mains <- tidy(main) %>% slice(2:3) %>%  mutate(p.value = round(p.value,3)) %>%  dplyr::select(p.value) %>% pull(p.value)  
interaction_p <- round(anv$`Pr(>Chi)`[[2]],3)

ps <- tibble(main_Year = mains[[1]], main_Role = mains[[2]], interaction = interaction_p) %>% 
  add_column(groupings = str_c(tag_groupings_logit, collapse = ", "), AndOrGroupings = groupingsAndOr, tags = str_c(tags_logit, collapse = ", "),AndOrTags = tagsAndOr,  .before=1) 


freqs <- df %>% group_by(Year, roleOpsVsAll) %>% count(tag_test) %>% 
  pivot_wider(names_from = Year, values_from = n) %>% 
  replace_na(list(`2019` = 0, `2022` = 0)) %>% 
  pivot_wider(names_from = roleOpsVsAll, values_from =c(`2019`, `2022`)) %>% # c(`2019_FALSE`, `2019_TRUE`, `2022_FALSE`, `2022_TRUE`)) %>% 
  pivot_wider(names_from = tag_test, values_from = c(`2019_nonOperations`, `2019_Operations`, `2022_nonOperations`, `2022_Operations`))
  
tot <- bind_cols(ps, freqs)

# if (ps$interaction < .1){
#  plot <- emmip(interaction, Year ~ roleOpsVsAll, CIs=TRUE, plotit=T)+theme_bw()
# }
return(tot)

}

uniqueGroupings <- unique(df_cats %>% filter(company == "Ergon") %>% unnest_tokens(allGroupings, groupings, token = 'regex', pattern=",") %>% pull(allGroupings)) # this get all unique groupings in df
groupings_logit_loop <- list()
for(g in 1:length(uniqueGroupings)) {
  groupings_logit_loop[[g]] <- logistic(tag_groupings_logit = uniqueGroupings[[g]])
}
uniqueGroupingsLogit <- bind_rows(groupings_logit_loop) %>% arrange(interaction)
uniqueGroupingsLogitSig <- uniqueGroupingsLogit %>% filter(interaction < .1)

# loop through length of uniqueTags on the following function
uniqueTags <- unique(df_cats %>% filter(company == "Ergon") %>% unnest_tokens(allTags, tags, token = 'regex', pattern=",") %>% pull(allTags)) # this get all unique tags in df
tags_logit_loop <- list()
for(t in 1:length(uniqueTags)) {
  tags_logit_loop[[t]] <- logistic(tags_logit = uniqueTags[[t]])
}
uniqueTagsLogit <- bind_rows(tags_logit_loop) %>% arrange(interaction)
uniqueTagsLogitSig <- uniqueTagsLogit %>% filter(interaction < .1)



# COMBINE TAG GROUPINGS AND TAG CHI SQUARES INTO ONE DF AND CREATE GSHEET

groupingsAndTags <- bind_rows(uniqueGroupingsLogit, uniqueTagsLogit) 
drive_trash("logistic-tags")
gs4_create(name = "logistic-tags", sheets = list(
  logit = groupingsAndTags))#, anovas = anovas_sig_no_nests_basic, nps_regression = model))
drive_mv("logistic-tags", path = paste0("~/DCI/Frontiers pre-post research/analyses/"))   



# pretty_table <- tab_model(logitInt, p.style = "numeric_stars", transform=NULL) # https://yury-zablotski.netlify.app/post/multiple-logistic-regression-with-interactions/
# effects <- allEffects(logitInt)
# emmip(logitInt, Year ~ roleOpsVsAll, CIs=TRUE, plotit=T)+theme_bw()



# also might want to make/save plot if sig or < .1