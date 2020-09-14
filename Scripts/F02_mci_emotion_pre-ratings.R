#/* Run this first piece of code only if you want to create a markdown report for GitHub
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd()) #*/

### MCI EMO PRE-RATINGS OF MATERIAL ###

# Pre-ratings of cloze probability, plausibility, metaphoricity, and imageability of the context 
# stories were conducted on five-point rating scales. Script computes analyses of variances testing
# for potential differences in these ratings between semantic conditions. Additionally, pairwise 
# t-tests test differences between each pair of semantic conditions (violation - intuitive, 
# MCI - intuitive, MCI - violation). The Bonferroni-Holm-correction was applied to control for multiple
# comparisons.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(dplyr)

# Load pre-rating data from SPSS file
pilot <- haven::read_sav("FB/gesamt_2.sav")

# Trim whitespace
pilot$KonzeptNr <- as.numeric(trimws(pilot$KonzeptNr))
pilot$VerbBedingung <- trimws(pilot$VerbBedingung)

# Rename conditions
pilot <- pilot %>% mutate(semantics = factor(VerbBedingung, levels = c("neutral", "sem", "mci"), labels = c("int", "vio", "mci")))

## Create a summary file ## ---------------------------------------------------------------------------------------
means.summary <- pilot %>% group_by(VP, semantics) %>%
  summarise(clozeprob = mean(Frage1), plausibility = mean(Frage2), 
            metaphoricity = mean(Frage3), imageability = mean(Frage4)) %>%
  mutate(VP = as.factor(VP))

## ANOVAs AND PAIRWISE TESTS ## -----------------------------------------------------------------------------------
# semantics is a within subjects factor; data are fully balances
# cloze probability
summary(anova.cloze <- aov(clozeprob ~ semantics + Error(VP/semantics), data = means.summary))
# mean squared error of the effect
mean(anova.cloze$`VP:semantics`$residuals^2)
# # this is identical to
# (anova.cloze <- ez::ezANOVA(data = means.summary, dv = clozeprob, wid = VP, 
#              within = .(semantics), detailed = TRUE, return_aov = TRUE, type = 1))

# plausibility
summary(anova.plausibility <- aov(plausibility ~ semantics + Error(VP/semantics), data = means.summary))
# mean squared error of the effect
mean(anova.plausibility$`VP:semantics`$residuals^2)

# imageability
summary(anova.imageability <- aov(imageability ~ semantics + Error(VP/semantics), data = means.summary))
mean(anova.imageability$`VP:semantics`$residuals^2)

# metaphoricity
summary(anova.metaphoricity <- aov(metaphoricity ~ semantics + Error(VP/semantics), data = means.summary))
mean(anova.metaphoricity$`VP:semantics`$residuals^2)

## PAIRWISE TESTS ## -------------------------------------------------------------------------------------------------
# cloze probability
(pairwise.clozeprob <- anova.cloze %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))

# plausibility
(pairwise.plausibility <- anova.plausibility %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))

# imageability
(pairwise.imageability <- anova.imageability %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))

# metaphoricity
(pairwise.metaphoricity <- anova.metaphoricity %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))




# Full system specs and package versions
sessionInfo()