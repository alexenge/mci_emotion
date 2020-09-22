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
library(emmeans)      # version 1.4.8

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

# System specs and package versions
sessionInfo()

