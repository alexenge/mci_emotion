#/* Run this first piece of code only if you want to create a markdown report for GitHub
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd()) #*/

### MCI EMO MIXED MODELS SCRIPT - APPENDIX (EFFECT OF LAG1 SEMANTICS) ###

# The expected effect of semantic violations on N400 amplitudes was not found in the main analyses.
# Additional "sequence analyses" were conducted in order to test whether the type of semantic
# manipulation in the previous trial affected present N400 amplitudes. 
# Script computes linear mixed-effects regression models with simple contrast coding for the fixed 
# effects of semantics, lag1Semantics, and emotional context. Thus, in each model, the estimate 
# of the intercept is the grand mean, while the estimates of the slopes contrast "treatment" levels 
# to their respective reference levels (semantics & lag1Semantics: violation - intuitive, mci - 
# intuitive; emotional context: negative - neutral). The maximal random effects structure is used 
# with all by-participant and by-item random slopes and random intercepts. Correlations between random 
# effects are removed if the model fails two converge with two different numerical optimizers. Planned 
# follow-up contrasts are computed for the main effects and the effects of semantics separately within
# each type of lag1 semantics and emotional context.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(MASS)         # version 7.3-51.6
library(lme4)         # version 1.1-23
library(lmerTest)     # version 3.1-2
library(afex)         # version 0.27-2
library(emmeans)      # version 1.4.8

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 <- na.omit(a1[!a1$error,])

# Define simple contrast coding for context emotionality (negative - neutral)
    # HO(Intercept): (mu1+mu2)/2 = 0 <-> mu1+mu2 = 0
    # H0(Slope): -mu1 + mu2 = 0
    # with mu1 = mean of the neutral contexts and mu2 = mean of the neg contexts
t(contrasts.context <- t(cbind(c("neu" = -1, "neg" = 1))))
contrasts(a1$context) <- ginv(contrasts.context)

# Define simple contrast coding for semantics and lag1Semantics
# (violation - intuitive, mci - intuitive)
    # H0(Intercept): (mu1+mu2+mu3)/3 = 0 <-> mu1+mu2+mu3 = 0
    # H0(Slope1): -1*mu1 +1*mu2 + 0*mu3 = 0
    # H0(Slope2): -1*mu1 +0*mu2 + 1*mu3 = 0
    # with mu1 = mean of intuitive concepts, mu2 = mean of violations, mu3 = mean of MCIs
t(contrasts.semantics <- t(cbind(c("int" = -1, "vio" = 1, "mci" = 0),
                                 c("int" = -1, "vio" = 0, "mci" = 1))))
contrasts(a1$semantics) <- ginv(contrasts.semantics)
contrasts(a1$lag1Semantics) <- ginv(contrasts.semantics)

## LINEAR MIXED-EFFECTS MODELS ## -----------------------------------------------------------------

# LMM for verb-related N400: Controlling for sequence of trials (lag)
    # same random structure as for mod.N400.verb (main analyses) intended
    # however, model converged only after bobyqa optimizer was used and correlation parameters were excluded
mod.N400.verb.lag <- lmer_alt(N400.verb ~ lag1Semantics*semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # to compare the two models, the N400.verb random structure needs to be identical
mod.N400.verb.comp <- lmer_alt(N400.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                              data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # model comparison
anova(mod.N400.verb.comp, mod.N400.verb.lag)

# LMM for picture-related N400: Controlling for sequence of trials
    # same random structure as for mod.N400.pict (main analyses) intended
    # however, model converged only after exclusion of correlation parameters
mod.N400.pict.lag <- lmer_alt(N400.pict ~ lag1Semantics*semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # to compare the two models, N400.pict random structure needs to be identical
mod.N400.pict.comp <- lmer_alt(N400.pict ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# model comparison
anova(mod.N400.pict.comp, mod.N400.pict.lag)

# Create a list of models
models <- list("N400.VERB.LAG" = mod.N400.verb.lag, "N400.PICT.LAG" = mod.N400.pict.lag)

# F-tests (type III tests)
(tests <- lapply(models, anova))


## PLANNED FOLLOW-UP CONTRASTS ## -----------------------------------------------------------------

# Allow emmeans to compute Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics <- lapply(models,function(x){
    emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for the main effect of context
(means.context <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ context, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for the main effect of lag
(means.lag <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ lag1Semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for semantics within each contexts
(means.nested.context <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for semantics within each lag1 semantic condition
(means.nested.lag <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|lag1Semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for semantics within each lag1Semantics and each context condition
(means.nested <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|lag1Semantics*context, infer = TRUE, adjust = "bonferroni")$contrasts}))


# Backup results
save(models, tests, means.semantics, means.context, means.lag, 
     means.nested.context, means.nested.lag,means.nested, file = "EEG/export/stats_appendixLag.RData")

# Full system specs and package versions
sessionInfo()

