#/* Run this first piece of code only if you want to create a PDF report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::pdf_document(),
                  output_dir = "output",
                  knit_root_dir = getwd()) #*/
#' ---
#' author: ""
#' classoption: "landscape"
#' ---

## MCI EMO PRE-RATINGS SCRIPT ##

# Pre-ratings of cloze probability, plausibility, metaphoricity, and imageability of the context stories were conducted 
# on five-point rating scales. Script computes analyses of variances testing for potential differences in these ratings
# between semantic conditions. Additionally, pairwise t-tests test differences between each pair of semantic conditions
# (violation - intuitive, MCI - intuitive, MCI - violation). The Bonferroni-Holm-correction was applied to control for
# multiple comparisons.

## SETUP ## ------------------------------------------------------------------------------------------------------------

# Load packages
library(tidyverse)    # Version 1.3.0
library(magrittr)     # Version 1.5
library(emmeans)      # Version 1.4.8
library(huxtable)     # Version 5.0.0

# Load pre-rating data from SPSS file
pilot <- haven::read_sav("FB/gesamt_2.sav")

# Trim whitespace
pilot %<>% mutate(KonzeptNr = KonzeptNr %>% trimws() %>% as.numeric(),
                  VerbBedingung = VerbBedingung %>% trimws())

# Rename conditions
pilot %<>% mutate(semantics = factor(VerbBedingung, levels = c("neutral", "sem", "mci"),
                                     labels = c("int", "vio", "mci")))

# Summarize by participants
avgs <- pilot %>%
  group_by(VP, semantics) %>%
  summarise(clozeprob = mean(Frage1),
            plausibility = mean(Frage2), 
            metaphoricity = mean(Frage3),
            imageability = mean(Frage4)) %>%
  mutate(VP = factor(VP))

## ANOVAs ## -----------------------------------------------------------------------------------------------------------

# Semantics is a within subjects factor; data are fully balanced

# Cloze probability
summary(anova_cloze <- aov(clozeprob ~ semantics + Error(VP/semantics), data = avgs))
mean(anova_cloze$`VP:semantics`$residuals^2) # Mean squared error of the effect

# Plausibility
summary(anova_plausibility <- aov(plausibility ~ semantics + Error(VP/semantics), data = avgs))
mean(anova_plausibility$`VP:semantics`$residuals^2) # Mean squared error of the effect

# Imageability
summary(anova_imageability <- aov(imageability ~ semantics + Error(VP/semantics), data = avgs))
mean(anova_imageability$`VP:semantics`$residuals^2) # Mean squared error of the effect

# Metaphoricity
summary(anova_metaphoricity <- aov(metaphoricity ~ semantics + Error(VP/semantics), data = avgs))
mean(anova_metaphoricity$`VP:semantics`$residuals^2) # Mean squared error of the effect

## PAIRWISE TESTS ## ---------------------------------------------------------------------------------------------------

# Cloze probability
(pairwise_clozeprob <- anova_cloze %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))

# Plausibility
(pairwise_plausibility <- anova_plausibility %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))

# Imageability
(pairwise_imageability <- anova_imageability %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))

# Metaphoricity
(pairwise_metaphoricity <- anova_metaphoricity %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))

## TABLES ## ----------------------------------------------------------------------------------------------------------

# Create a table summarizing the rating data in the paper
map2(
  c("Frage1", "Frage2", "Frage3", "Frage4"),
  c("Cloze probability", "Plausibility", "Metaphoricity", "Imageability"),
  function(old_name, new_name){
    pilot %>%
      # Give proper names to our semantic conditions
      mutate(semantics = fct_recode(semantics, Intuitive = "int", Violation = "vio", MCI = "mci")) %>%
      # Summarize the data (see http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/)
      Rmisc::summarySEwithin(measurevar = old_name, withinvars = "semantics") %>%
      rename(m = all_of(old_name)) %>%
      # Re-format the confidence interval
      mutate(lower = m - ci, upper = m + ci) %>%
      select(semantics, m, sd, lower, upper) %>%
      mutate(across(-semantics, .fns = ~format(round(.x, 2), nsmall = 2, trim = TRUE))) %>%
      mutate(ci = paste0("[", lower, ", ", upper, "]"), .keep = "unused") %>%
      # Re-shape so that semantic conditions are columns and different stats are rows
      pivot_longer(-semantics, names_to = " ") %>%
      spread(semantics, value) %>%
      # Give proper names to the different stats and order them
      mutate(` ` = factor(` `, levels = c("m", "sd", "ci"), labels = c("M", "SD", "95% CI"))) %>%
      arrange(` `) %>%
      # Add a header row for the name of the dependent (rating) variable
      bind_rows(c(` ` = new_name, Intuitive = "", Violation = "", MCI = ""), .)
  }) %>%
  # Bind all ratings together
  bind_rows() %>%
  # Convert to huxtable and style
  huxtable() %>%
  set_bold(row = c(1, seq(2, 17, by = 4)), value = TRUE) %>%
  set_italic(row = c(seq(3, 17, by = 4), seq(4, 17, by = 4)), col = 1, value = TRUE) %T>%
  # Export to a Word file
  quick_docx(file = "EEG/tables/table_pilot.docx")

# System specs and package versions
sessionInfo()

