#/* Run this first piece of code only if you want to create a markdown report for GitHub
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd()) #*/

### MCI EMO MIXED MODELS SCRIPT ###

# Computes linear mixed-effects regression models with simple contrast coding for the fixed effects
# of semantics and emotional context. Thus, in each model, the estimate of the intercept is the
# grand mean, while the estimates of the slopes contrast "treatment" levels to their respective
# reference levels (semantics: violation - intuitive, mci - intuitive; emotional context (negative 
# - neutral). The maximal random effects structure is used with all by-participant and by-item random 
# slopes and random intercepts. Correlations between random effects are removed if the model fails two 
# converge with two different numerical optimizers. Planned follow-up contrasts are computed for the
# main effects and the effects of semantics separately within each type of emotional context.

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

# Define simple contrast coding for semantics (violation - intuitive, mci - intuitive)
    # H0(Intercept): (mu1+mu2+mu3)/3 = 0 <-> mu1+mu2+mu3 = 0
    # H0(Slope1): -1*mu1 +1*mu2 + 0*mu3 = 0
    # H0(Slope2): -1*mu1 +0*mu2 + 1*mu3 = 0
    # with mu1 = mean of intuitive concepts, mu2 = mean of violations, mu3 = mean of MCIs
t(contrasts.semantics <- t(cbind(c("int" = -1, "vio" = 1, "mci" = 0),
                                 c("int" = -1, "vio" = 0, "mci" = 1))))
contrasts(a1$semantics) <- ginv(contrasts.semantics)

## LINEAR MIXED-EFFECTS MODELS ## -----------------------------------------------------------------

# LMM for valence ratings (converged on first attempt)
mod.valence <- lmer(ValenzResp ~ context + (context|participant) + (context|item),
                    data = a1, control = lmerControl(calc.derivs = FALSE))

# LMM for arousal ratings (converged after changing the optimizer + removing correlations between REs)
mod.aroursal <- lmer(ArousalResp ~ context + (context||participant) + (context||item),
                         data = a1, control = lmerControl(calc.derivs = FALSE))

# LMM for verb-related N400 (converged on first attempt)
mod.N400.verb <- lmer_alt(N400.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for picture-related N400 (converged after changing the optimizer)
mod.N400.pict <- lmer(N400.pict ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Create a list of models
models <- list("VALENCE" = mod.valence, "AROUSAL" = mod.aroursal, "N400.VERB" = mod.N400.verb, "N400.PICT" = mod.N400.pict)

# F-tests (type III tests)
(tests <- lapply(models, anova))

## PLANNED FOLLOW-UP CONTRASTS ## -----------------------------------------------------------------

# Allow emmeans to compute Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics <- lapply(models[3:4],function(x){
    emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for the main effect of context
(means.context <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ context, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for semantics within each contexts
(means.nested <- lapply(models[3:4], function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Backup results
save(models, tests, means.semantics, means.context, means.nested, file = "EEG/export/stats_new ratings.RData")

# Full system specs and package versions
sessionInfo()

