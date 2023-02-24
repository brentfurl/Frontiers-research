####
library(MASS)
library(sjPlot)
library(emmeans)
library(effects)
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

loglm(~ tag_test + roleOpsVsAll + Year + tag_test:roleOpsVsAll + tag_test:Year + roleOpsVsAll:Year, data = table, fit = TRUE)
loglm(~ tag_test + Year, data = table, fit = TRUE)
  
  mosaicplot(model$fit, shade=TRUE) 
  
  #1
  satmodel <-  loglm(~ tag_test*roleOpsVsAll*Year, data = table)  
  #2
  noThreeWay <- loglm(~ tag_test + roleOpsVsAll + Year + tag_test:roleOpsVsAll + tag_test:Year + roleOpsVsAll:Year, data = table, fit = TRUE)# update(satmodel, .~., -tag_test:roleOpsVsAll:Year) this wasn't subtracting the 3way for some reason
  summary(noThreeWay)
  #3
  anova(satmodel, noThreeWay)
  #4
  tagtestRole <- update(noThreeWay, .~. -tag_test:roleOpsVsAll)
  tagtestYear <- update(noThreeWay, .~. -tag_test:Year)
  yearRole <- update(noThreeWay, .~. -Year:roleOpsVsAll)
  
  anova(threeWay, tagtestRole)
  anova(threeWay, tagtestYear)
  anova(threeWay, yearRole)
  
#   return(x)
  logit <- glm(tag_test ~ Year + roleOpsVsAll, data = df, family = binomial(link = 'logit'))
  logitInt <- glm(tag_test ~ Year + roleOpsVsAll + Year*roleOpsVsAll, data = df, family = binomial)
 pretty_table <- tab_model(logitInt, p.style = "numeric_stars", transform=NULL) 
 effects <- allEffects(logitInt)
  
  summary(logit)
  summary(logitInt)
  anova(logit, test = "Chisq")
  anova(logitInt, test = "Chisq")
  exp(cbind(OR=coef(logit), confint(logit)))
# }
