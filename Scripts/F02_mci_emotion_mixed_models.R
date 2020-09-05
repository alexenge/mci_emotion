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

## LINEAR MIXED-EFFECTS MODELS ## -----------------------------------------------------------------

# LMM for valence ratings (converged on first attempt)
mod.valence <- lmer(ValenzResp ~ context + (context|participant) + (context|item),
                    data = a1, control = lmerControl(calc.derivs = FALSE))

# LMM for arousal ratings (converged after changing the optimizer + removing correlations between REs)
mod.aroursal <- lmer(ArousalResp ~ context + (context|participant) + (context|item),
                     data = a1, control = lmerControl(calc.derivs = FALSE))

# LMM for verb-related N400 (converged on first attempt)
mod.N400.verb <- lmer_alt(N400.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                          data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for picture-related N400 (converged after changing the optimizer)
mod.N400.pict <- lmer(N400.pict ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related P600 (short time window, old ROI) (converged after changing the optimizer + removing correlations between REs)
mod.P600_1.verb <- lmer_alt(P600_1.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                            data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related P600 (long time window, old ROI) (converged after changing the optimizer + removing correlations between REs)
mod.P600_2.verb <- lmer_alt(P600_2.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                            data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related P600 (short time window, new ROI) (converged after changing the optimizer + removing correlations between REs)
mod.P600_3.verb <- lmer_alt(P600_3.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                            data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related P600 (long time window, new ROI) (converged after changing the optimizer + removing correlations between REs)
mod.P600_4.verb <- lmer_alt(P600_4.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                            data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Create a list of models
models <- list("VALENCE" = mod.valence, "AROUSAL" = mod.aroursal, "N400.VERB" = mod.N400.verb, "N400.PICT" = mod.N400.pict,
               "P600_1" = mod.P600_1.verb, "P600_2" = mod.P600_2.verb, "P600_3" = mod.P600_3.verb, "P600_4" = mod.P600_4.verb)

# F-tests (type III tests)
(tests <- lapply(models, anova))

## PLANNED FOLLOW-UP CONTRASTS ## -----------------------------------------------------------------

# Allow emmeans to use Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics <- lapply(models[-1:-2],function(x){
    emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for the main effect of context
(means.context <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ context, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for semantics within each contexts
(means.nested <- lapply(models[-1:-2], function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Backup results
save(models, tests, means.semantics, means.context, means.nested, file = "EEG/export/stats.RData")

## ADDITIONAL ANALYSES INCLUDING COVARIATES ## ----------------------------------------------------

# Load packages
library(dplyr)

# Read pilot rating data from SPSS file
pilot <- haven::read_sav("FB/gesamt_2.sav")

# Trim whitespace
pilot$KonzeptNr <- as.numeric(trimws(pilot$KonzeptNr))

# Check if pilot and experimental data are matching
all.equal(sort(unique(pilot$KonzeptNr)), sort(unique(a1$KonzeptNr)))
all.equal(sort(unique(pilot$VerbNr)), sort(unique(a1$VerbNr)))

# Average pilot data across participants
covariates <- pilot %>% group_by(KonzeptNr, VerbNr) %>% summarize(clozeprob = mean(Frage1),
                                                                  plausibility = mean(Frage2),
                                                                  metaphoricity = mean(Frage3),
                                                                  imageability = mean(Frage4))

# Bind covariates to the experimental data
a1 <- left_join(a1, covariates, by = c("KonzeptNr", "VerbNr"))

# # Center covariates for LMMs (shouldn't be necessary if we don't compute interactions)
# a1 <- a1 %>% mutate(across(.cols = c(clozeprob, plausibility, metaphoricity, imageability), .fns = function(x){x - 2.5}))
# a1 <- a1 %>% mutate(across(.cols = c(clozeprob, plausibility, metaphoricity, imageability), .fns = scale, scale = FALSE))

# LMM including all covariates as fixed effects
mod.N400.verb.covs <- lmer_alt(N400.verb ~ (clozeprob + plausibility + metaphoricity + imageability) + semantics*context
                               + (semantics*context||participant) + (semantics*context||item),
                               data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Model output
summary(mod.N400.verb.covs) # MCI - correct remains significant, no other effects
anova(mod.N400.verb, mod.N400.verb.covs) # Takes a while because models need to be refitted with ML
emmeans(mod.N400.verb.covs, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts

# # And another LMM including interactions with each covariate
# mod.N400.verb.covs.ia <- lmer_alt(N400.verb ~ (clozeprob + plausibility + metaphoricity + imageability) * semantics*context
#                                   + (semantics*context||participant) + (semantics*context||item),
#                                   data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# 
# # Model summary
# summary(mod.N400.verb.covs.ia)
# anova(mod.N400.verb, mod.N400.verb.covs.ia)

# Full system specs and package versions
sessionInfo()

