#/* Run this first piece of code only if you want to create a PDF report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::pdf_document(),
                  output_dir = "output",
                  knit_root_dir = getwd()) #*/
#' ---
#' author: ""
#' classoption: "landscape"
#' ---
#+ results="asis"

## MCI EMO TABLES SCRIPT ##

# Creates a table for the output of our four linear mixed-effects models. The upper half of the table includes ANOVA-
# style type III tests (F-tests), the bottom half contains planned follow-up contrasts. For the F-tests, F-values,
# degrees of freedom, and p-values are printed, whereas for the contrasts, regression estimates, 95% confidence
# intervals, and p-values are printed.

## PREPARATION ## ------------------------------------------------------------------------------------------------------

# Load packages
library(Rmisc)        # Version 1.5
library(tidyverse)    # Version 1.3.0
library(magrittr)     # Version 1.5
library(officer)      # Version 0.3.14
library(flextable)    # Version 0.5.11
library(huxtable)     # version 5.0.0

## TABLE 2: MEAN RATINGS ## --------------------------------------------------------------------------------------------

# Load single-trial data
a1 <- readRDS("EEG/export/a1.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 %<>% filter(!error) %>% na.omit()

# Adjust range of response scales
a1$Valence <- a1$ValenzResp + 3
a1$Arousal <- a1$ArousalResp + 3

# Compute mean ratings
tab2 <- summarySEwithin(a1, measurevar = "Valence", withinvars = "context") %>%
  select(m_val = Valence, sd_val = sd) %>%
  bind_cols(summarySEwithin(a1, measurevar = "Arousal", withinvars = "context") %>% 
              select(m_aro = Arousal, sd_aro = sd)) %>%
  set_rownames(c("Neutral", "Negative")) %>%
  huxtable(add_rownames = "", add_colnames = FALSE) %>%
  add_rows(c("Context", "M", "SD", "M", "SD"), after = 0) %>%
  add_rows(c("", "Valence Rating", "", "Arousal Rating", ""), after = 0)

# Output as markdown for GitHub
suppressWarnings(print_md(tab2, max_width = Inf))

# Save as Word document
tab2 %>% quick_docx(file = "EEG/tables/table_2.docx", open = FALSE)

## TABLE 3: LMMS FOR N400 ## -------------------------------------------------------------------------------------------

# Load output of linear mixed-effects models
load("EEG/export/stats.RData")

# Extract a table for the F tests for each model (columns: F value (df), p-value)
anovas_tab3 <- map(tests[c("N400_VERB", "N400_PICT")], function(x){
  coefs <- data.frame(paste0(format(round(x$`F value`, 2), trim = TRUE, nsmall = 2),
                             "<br/> (", x$NumDF, ", ", format(round(x$DenDF, 1), trim = TRUE, nsmall = 1), ")"),
                      format(round(x$`Pr(>F)`, 3), nsmall = 3),
                      fix.empty.names = FALSE)
  coefs[,2] <- substr(coefs[,2], 1, 5)
  coefs[coefs[,2] == "0.000", 2] <- "< .001"
  return(coefs)
})

# Bind all the F-tests to one data frame
anovas_tab3 <- do.call(cbind, anovas_tab3)
anovas_tab3 <- rbind(c("**_F_** (**_df_**)", "**_p_**"), anovas_tab3)

# Extract a table for the planned contrasts for each model (columns: estimate [CI], p-value)
conts_tab3 <- map(means_nested[c("N400_VERB", "N400_PICT")], function(x){
  x <- as.data.frame(x)
  coefs <- data.frame(paste0(format(round(x$estimate, 2), trim = TRUE, nsmall = 2),
                             "<br/> [", format(round(x$lower.CL, 2), trim = TRUE, nsmall = 2), ", ",
                             format(round(x$upper.CL, 2), trim = TRUE, nsmall = 2), "]"),
                      format(round(x$p.value, 3), nsmall = 3),
                      fix.empty.names = FALSE)
  coefs[,2] <- substr(coefs[,2], 1, 5)
  coefs[coefs[,2] == "0.000", 2] <- "< .001"
  return(coefs)
})

# Bind all the planned contrasts to one data frame
conts_tab3 <- do.call(cbind, conts_tab3)
conts_tab3 <- rbind(c("**Est. [95% CI]**", "**_p_**"), conts_tab3)

# Bind both data frames (F-tests and contrats) below one another
tab3 <- rbind(anovas_tab3, conts_tab3)

# Add model names (dependent variables) as the first row
tab3 <- rbind(c("Verb-Related N400", "", "Picture-Related N400", ""), tab3)

# Add a stub column
tab3 <- cbind(c("", "**Fixed effects**", "Semantics", "Context", "Semantics × context",
                "**Planned contrasts**", "Vio. - int.<br/> (neutral context)", "MCI - int.<br/> (neutral context)",
                "Vio. - int.<br/> (negative context)", "MCI - int.<br/> (negative context)"), tab3)

# Remove old column names
names(tab3) <- NULL

# Create a huxtable and output as markdown
huxt_tab3 <- huxtable(tab3, add_colnames = FALSE)
print_md(huxt_tab3, max_width = Inf)

# Export as a word file (after some re-formatting)
tab3_word <- data.frame(map(tab3, function(x){gsub("<br/> ", "\n", x)}))
tab3_word <- data.frame(map(tab3_word, function(x){gsub("\\*|\\_", "", x)}))
huxt_tab3_word <- huxtable(tab3_word, add_colnames = FALSE)
quick_docx(huxt_tab3_word, file = "EEG/tables/table_3.docx", open = FALSE)

## TABLE A1: LMM FOR P600 ## -------------------------------------------------------------------------------------------

# Extract a table for the F tests for the model (columns: F value (df), p-value)
anovas_tabA1 <- map(tests["P600_VERB"], function(x){
  coefs <- data.frame(paste0(format(round(x$`F value`, 2), trim = TRUE, nsmall = 2),
                             "<br/> (", x$NumDF, ", ", format(round(x$DenDF, 1), trim = TRUE, nsmall = 1), ")"),
                      format(round(x$`Pr(>F)`, 3), nsmall = 3),
                      fix.empty.names = FALSE)
  coefs[,2] <- substr(coefs[,2], 1, 5)
  coefs[coefs[,2] == "0.000", 2] <- "< .001"
  return(coefs)
})

# Bind all the F-tests to one data frame
anovas_tabA1 <- do.call(cbind, anovas_tabA1)
anovas_tabA1 <- rbind(c("**_F_** (**_df_**)", "**_p_**"), anovas_tabA1)

# Extract a table for the planned contrasts for each model (columns: estimate [CI], p-value)
conts_tabA1 <- map(means_nested["P600_VERB"], function(x){
  x <- as.data.frame(x)
  coefs <- data.frame(paste0(format(round(x$estimate, 2), trim = TRUE, nsmall = 2),
                             "<br/> [", format(round(x$lower.CL, 2), trim = TRUE, nsmall = 2), ", ",
                             format(round(x$upper.CL, 2), trim = TRUE, nsmall = 2), "]"),
                      format(round(x$p.value, 3), nsmall = 3),
                      fix.empty.names = FALSE)
  coefs[,2] <- substr(coefs[,2], 1, 5)
  coefs[coefs[,2] == "0.000", 2] <- "< .001"
  return(coefs)
})

# Bind all the planned contrasts to one data frame
conts_tabA1 <- do.call(cbind, conts_tabA1)
conts_tabA1 <- rbind(c("**Est. [95% CI]**", "**_p_**"), conts_tabA1)

# Bind both data frames (F-tests and contrats) below one another
tabA1 <- rbind(anovas_tabA1, conts_tabA1)

# Add model names (dependent variables) as the first row
tabA1 <- rbind(c("Verb-Related P600", ""), tabA1)

# Add a stub column
tabA1 <- cbind(c("", "**Fixed effects**", "Semantics", "Context", "Semantics × context",
               "**Planned contrasts**", "Vio. - int.<br/> (neutral context)", "MCI - int.<br/> (neutral context)",
               "Vio. - int.<br/> (negative context)", "MCI - int.<br/> (negative context)"), tabA1)

# Remove old column names
names(tabA1) <- NULL

# Create a huxtable and output as markdown
huxt_tabA1 <- huxtable(tabA1, add_colnames = FALSE)
print_md(huxt_tabA1, max_width = Inf)

# Export as a word file (after some re-formatting)
tabA1_word <- data.frame(map(tabA1, function(x){gsub("<br/> ", "\n", x)}))
tabA1_word <- data.frame(map(tabA1_word, function(x){gsub("\\*|\\_", "", x)}))
huxt_tabA1_word <- huxtable(tabA1_word, add_colnames = FALSE)
quick_docx(huxt_tabA1_word, file = "EEG/tables/table_A1.docx", open = FALSE)

#+

# ## ADDITIONAL TABLE INTERACTION EFFECTS ## ---------------------------------------------------------------------------
# 
# # Checking the MCI-intuitive x context and SEV-intuitive x context interactions separetely (verb)
# summary(models$N400_VERB)$coefficients %>%
#   set_colnames(c("Est.", "SE", "df", "t", "p")) %>%
#   set_rownames(c("(Intercept)", "Semantics: Vio. - int.", "Semantics: MCI - int", "Context",
#                  "(Vio. - int.) × context", "(MCI - int.) × context")) %>%
#   huxtable(add_rownames = "Verb-related N400") %>% add_colnames() %>%
#   set_number_format(value = "%3.3f") %>%
#   quick_docx(file = "EEG/tables/table_ias_verb.docx", open = FALSE)
# 
# # Checking the MCI-intuitive x context and SEV-intuitive x context interactions separetely (verb)
# summary(models$N400_PICT)$coefficients %>%
#   set_colnames(c("Est.", "SE", "df", "t", "p")) %>%
#   set_rownames(c("(Intercept)", "Semantics: Vio. - int.", "Semantics: MCI - int", "Context",
#                  "(Vio. - int.) × context", "(MCI - int.) × context")) %>%
#   huxtable(add_rownames = "Picture-related N400") %>% add_colnames() %>%
#   set_number_format(value = "%3.3f") %>%
#   quick_docx(file = "EEG/tables/table_ias_pict.docx", open = FALSE)

# System specs and package versions
sessionInfo()

