#/* Run this first piece of code only if you want to create a PDF report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::pdf_document(),
                  output_dir = "output",
                  knit_root_dir = getwd()) #*/
#' ---
#' author: ""
#' classoption: "landscape"
#' ---

## MCI EMO MIXED MODELS SCRIPT ##

# Computes linear mixed-effects regression models with simple contrast coding for the fixed effects of semantics and
# emotional context. Thus, in each model, the estimate of the intercept is the grand mean, while the estimates of the
# slopes contrast "treatment" levels to their respective reference levels (semantics: violation - intuitive, mci -
# intuitive; emotional context (negative - neutral). The maximal random effects structure is used with all by-
# participant and by-item random slopes and random intercepts. Correlations between random effects are removed if the
# model fails two converge with two different numerical optimizers. Planned follow-up contrasts are computed for the
# main effects and the effects of semantics separately within each type of emotional context.

## SETUP ## ------------------------------------------------------------------------------------------------------------

# Load packages
library(MASS)         # version 7.3-51.6
library(lme4)         # version 1.1-23
library(lmerTest)     # version 3.1-2
library(afex)         # version 0.27-2
library(emmeans)      # version 1.4.8
library(tidyverse)    # Version 1.3.0
library(magrittr)     # Version 1.5

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 %<>% filter(!error) %>% na.omit()

# Define simple contrast coding for context emotionality (negative - neutral)
#     H0(Intercept): (mu1+mu2)/2 = 0 <-> mu1+mu2 = 0
#     H0(Slope): -mu1 + mu2 = 0
#     with mu1 = mean of the neutral contexts and mu2 = mean of the neg contexts
t(contrasts.context <- t(cbind(c("neu" = -1, "neg" = 1))))
contrasts(a1$context) <- ginv(contrasts.context)

# Define simple contrast coding for semantics (violation - intuitive, mci - intuitive)
#     H0(Intercept): (mu1+mu2+mu3)/3 = 0 <-> mu1+mu2+mu3 = 0
#     H0(Slope1): -1*mu1 +1*mu2 + 0*mu3 = 0
#     H0(Slope2): -1*mu1 +0*mu2 + 1*mu3 = 0
#     with mu1 = mean of intuitive concepts, mu2 = mean of violations, mu3 = mean of MCIs
t(contrasts.semantics <- t(cbind(c("int" = -1, "vio" = 1, "mci" = 0),
                                 c("int" = -1, "vio" = 0, "mci" = 1))))
contrasts(a1$semantics) <- ginv(contrasts.semantics)

## LINEAR MIXED-EFFECTS MODELS ## --------------------------------------------------------------------------------------

# LMM for valence ratings (converged on first attempt)
mod_valence <- lmer(ValenzResp ~ context + (context|participant) + (context|item),
                    data = a1, control = lmerControl(calc.derivs = FALSE))

# LMM for arousal ratings (converged on first attempt)
mod_aroursal <- lmer(ArousalResp ~ context + (context|participant) + (context|item),
                     data = a1, control = lmerControl(calc.derivs = FALSE))

# LMM for verb-related N400 (converged after changing the optimizer and removing correlations between REs)
mod_N400_verb <- lmer_alt(N400_verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                          data = a1, control = lmerControl(calc.derivs = FALSE,
                                                           optimizer = "bobyqa",
                                                           optCtrl = list(maxfun = 2e5)))

# LMM for picture-related N400 (converged after changing the optimizer)
mod_N400_pict <- lmer(N400_pict ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                      data = a1, control = lmerControl(calc.derivs = FALSE,
                                                       optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 2e5)))

# LMM for verb-related P600 (converged after changing the optimizer and removing correlations between REs)
mod_P600_verb <- lmer_alt(P600_verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                          data = a1, control = lmerControl(calc.derivs = FALSE,
                                                           optimizer = "bobyqa",
                                                           optCtrl = list(maxfun = 2e5)))

# LMM for picture-related N400 in a narrower time window (converged after changing the optimizer)
mod_N400_pict_posthoc_narrow250_350 <- lmer(N400_pict_posthoc_narrow250_350 ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                                            data = a1, control = lmerControl(calc.derivs = FALSE,
                                                                             optimizer = "bobyqa",
                                                                             optCtrl = list(maxfun = 2e5)))

# Create a list of all models
models <- list("VALENCE" = mod_valence, "AROUSAL" = mod_aroursal, "N400_VERB" = mod_N400_verb, "N400_PICT" = mod_N400_pict,
               "P600_VERB" = mod_P600_verb, "N400_PICT_POSTHOC_NARROW250_350" = mod_N400_pict_posthoc_narrow250_350)

# F-tests (type III tests)
(tests <- map(models, anova))

## PLANNED FOLLOW-UP CONTRASTS ## --------------------------------------------------------------------------------------

# Allow emmeans to use Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# We want to test most effects for the *ERP* models only, so let's create a seperate list
models_erp <- models[c("N400_VERB", "N400_PICT", "P600_VERB", "N400_PICT_POSTHOC_NARROW250_350")]

# Follow-up contrasts for the main effect of semantics
(means_semantics <- map(models_erp, function(x){
  emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts
}))

# Follow-up contrasts for the main effect of context
(means_context <- map(models, function(x){
  emmeans(x, trt.vs.ctrl ~ context, infer = TRUE, adjust = "bonferroni")$contrasts
}))

# Follow-up contrasts for semantics within each contexts
(means_nested <- map(models_erp, function(x){
  emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts
}))

# Follow-up contrasts for contexts within each semantic condition
(means_nested_rev <- map(models_erp, function(x){
  emmeans(x, trt.vs.ctrl ~ context|semantics, infer = TRUE, adjust = "bonferroni")$contrasts
}))

# Backup results
save(models, tests, means_semantics, means_context, means_nested, means_nested_rev, file = "EEG/export/stats.RData")

# System specs and package versions
sessionInfo()

