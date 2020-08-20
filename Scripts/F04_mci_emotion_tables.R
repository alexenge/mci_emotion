#/* Run this first piece of code only if you want to create a markdown report for GitHub
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd()) #*/
#+ results="asis"

### MCI EMO TABLES SCRIPT ###

# Creates a table for the output of our four linear mixed-effects models. The upper half of
# the table includes ANOVA-style type III tests (F-tests), the bottom half contains planned
# follow-up contrasts. For the F-tests, F-values, degrees of freedom, and p-values are
# printed, whereas for the contrasts, regression estimates, 95% confidence intervals, and
# p-values are printed.

## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(huxtable)     # version 5.0.0

# Load output from mixed models
load("EEG/export/stats.RData")

# Extract a table for the F tests for each model (columns: F value (df), p-value)
anovas <- lapply(tests, function(x){
  coefs <- data.frame(paste0(format(round(x$`F value`, 2), trim = TRUE, nsmall = 2),
                             "<br/>(", x$NumDF, ", ", format(round(x$DenDF, 1), trim = TRUE, nsmall = 1), ")"),
                      format(round(x$`Pr(>F)`, 3), nsmall = 3),
                      fix.empty.names = FALSE)
  coefs[,2] <- substr(coefs[,2], 1, 5)
  coefs[coefs[,2] == "0.000", 2] <- "< .001"
  return(coefs)})

# Bind all the F-tests to one data frame
anovas <- do.call(cbind, anovas)
anovas <- rbind(c("**_F_** (**_df_**)", "**_p_**"), anovas)

# Extract a table for the planned contrasts for each model (columns: estimate [CI], p-value)
conts <- lapply(means.nested, function(x){
  x <- as.data.frame(x)
  coefs <- data.frame(paste0(format(round(x$estimate, 2), trim = TRUE, nsmall = 2),
                             "<br/>[", format(round(x$lower.CL, 2), trim = TRUE, nsmall = 2), ", ",
                             format(round(x$upper.CL, 2), trim = TRUE, nsmall = 2), "]"),
                      format(round(x$p.value, 3), nsmall = 3),
                      fix.empty.names = FALSE)
  coefs[,2] <- substr(coefs[,2], 1, 5)
  coefs[coefs[,2] == "0.000", 2] <- "< .001"
  return(coefs)})

# Bind all the planned contrasts to one data frame
conts <- do.call(cbind, conts)
conts <- rbind(c("**Est. [95% CI]**", "**_p_**"), conts)

## CREATE A SINGLE TABLE ## -----------------------------------------------------------------------

# Bind both data frames (F-tests and contrats) below one another
tab <- rbind(anovas, conts)

# Add model names (dependent variables) as the first row
tab <- rbind(c("Valence Rating", "", "Arousal Rating", "", "Verb-Related N400", "", "Picture-Related N400", ""), tab)

# Add a stub column
tab <- cbind(c("", "**Model output**", "Semantics", "Context", "Semantics × context",
               "**Planned contrasts**", "Vio. - int.<br/>(neutral)", "MCI - int.<br/>(neutral)",
               "Vio. - int.<br/>(negative)", "MCI - int.<br/>(negative)"), tab)

# Remove old column names
names(tab) <- NULL

# Create a huxtable and output as markdown
huxt <- huxtable(tab, add_colnames = FALSE)
print_md(huxt, max_width = Inf)

# Export as a word file (after some re-formatting)
tab_word <- data.frame(lapply(tab, function(x){gsub("<br/>", "\n", x)}))
tab_word <- data.frame(lapply(tab_word, function(x){gsub("\\*|\\_", "", x)}))
huxt_word <- huxtable(tab_word, add_colnames = FALSE)
quick_docx(huxt_word, file = "EEG/tables/lmm_table.docx", open = FALSE)

## MEAN RATINGS ## --------------------------------------------------------------------------------

# Load some new packages
library(Rmisc)
library(tidyverse)
library(magrittr)

# Read data
a1 <- readRDS("EEG/export/a1.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 <- na.omit(a1[!a1$error,])

# Adjust range of response scalse
a1$Valence <- a1$ValenzResp + 3
a1$Arousal <- a1$ArousalResp + 3

# Compute mean ratings
summarySEwithin(a1, measurevar = "Valence", withinvars = "context") %>%
  select(m_val = Valence, sd_val = sd) %>%
  bind_cols(summarySEwithin(a1, measurevar = "Arousal", withinvars = "context") %>% 
              select(m_aro = Arousal, sd_aro = sd)) %>%
  set_rownames(c("Neutral", "Negative")) %>%
  huxtable(add_rownames = "", add_colnames = FALSE) %>%
  add_rows(c("Context emotionality", "M", "SD", "M", "SD"), after = 0) %>%
  add_rows(c("", "Valence Rating", "", "Arousal Rating", ""), after = 0) %>%
  quick_docx(file = "EEG/tables/ratings_table.docx", open = FALSE)

## INTERACTION EFFECTS ## -------------------------------------------------------------------------

# Checking the MCI-intuitive x context and SEV-intuitive x context interactions separetely (verb)
summary(models$N400.VERB)$coefficients %>%
  set_colnames(c("Est.", "SE", "df", "t", "p")) %>%
  set_rownames(c("(Intercept)", "Semantics: Vio. - int.", "Semantics: MCI - int", "Context",
                 "(Vio. - int.) × context", "(MCI - int.) × context")) %>%
  huxtable(add_rownames = "Verb-related N400") %>% add_colnames() %>%
  set_number_format(value = "%3.3f") %>%
  quick_docx(file = "EEG/tables/ias_table_verb.docx", open = FALSE)

# Checking the MCI-intuitive x context and SEV-intuitive x context interactions separetely (verb)
summary(models$N400.PICT)$coefficients %>%
  set_colnames(c("Est.", "SE", "df", "t", "p")) %>%
  set_rownames(c("(Intercept)", "Semantics: Vio. - int.", "Semantics: MCI - int", "Context",
                 "(Vio. - int.) × context", "(MCI - int.) × context")) %>%
  huxtable(add_rownames = "Picture-related N400") %>% add_colnames() %>%
  set_number_format(value = "%3.3f") %>%
  quick_docx(file = "EEG/tables/ias_table_pict.docx", open = FALSE)

#+

# Full system specs and package versions
sessionInfo()

