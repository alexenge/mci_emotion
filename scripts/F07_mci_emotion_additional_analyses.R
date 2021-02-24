#/* Run this first piece of code only if you want to create a PDF report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::pdf_document(),
                  output_dir = "output",
                  knit_root_dir = getwd()) #*/
#' ---
#' author: ""
#' classoption: "landscape"
#' ---

## MCI EMO - ADDITIONAL ANALYSES ##

## 1) EFFECT OF LAG1 SEMANTICS
## 2) MODELS WITH COVARIATES METAPHORICITY, PLAUSIBILITY, IMAGEABILIY, CLOZE PROBABILITY
## 3) P-hacking the verb-related N400 (ANOVA instead of LMM, Half I vs. Half II)
## 4) P600 figure

# THESE ANALYSES WERE EXPLORATORY ANALYSES NOT REPORTED IN THE MANUSCRIPT

# -------------------------------------------------------------------------------------------------
## 1) EFFECT OF LAG1 SEMANTICS ## -----------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

# The expected effect of semantic violations on N400 amplitudes was not found in the main analyses. Additional "sequence
# analyses" were conducted in order to test whether the type of semantic manipulation in the previous trial affected
# present N400 amplitudes. Script computes linear mixed-effects regression models with simple contrast coding for the
# fixed effects of semantics, lag1Semantics, and emotional context. Thus, in each model, the estimate of the intercept
# is the grand mean, while the estimates of the slopes contrast "treatment" levels to their respective reference levels
# (semantics & lag1Semantics: violation - intuitive, mci - intuitive; emotional context: negative - neutral). The
# maximal random effects structure is used with all by-participant and by-item random slopes and random intercepts.
# Correlations between random effects are removed if the model fails two converge with two different numerical
# optimizers. Planned follow-up contrasts are computed for the main effects and the effects of semantics separately
# within each type of lag1 semantics and emotional context.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(MASS)         # version 7.3-51.6
library(lme4)         # version 1.1-23
library(lmerTest)     # version 3.1-2
library(afex)         # version 0.27-2
library(emmeans)      # version 1.4.8

# Load preprocessed data
a1 <- readRDS("EEG/export/a1_RDS")

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
    # same random structure as for mod_N400_verb (main analyses) intended
    # however, model converged only after bobyqa optimizer was used and correlation parameters were excluded
mod_N400_verb_lag <- lmer_alt(N400_verb ~ lag1Semantics*semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # to compare the two models, the N400_verb random structure needs to be identical
mod_N400_verb_comp <- lmer_alt(N400_verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                              data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # model comparison
anova(mod_N400_verb_comp, mod_N400_verb_lag)

# LMM for picture-related N400: Controlling for sequence of trials
    # same random structure as for mod_N400_pict (main analyses) intended
    # however, model converged only after exclusion of correlation parameters
mod_N400_pict_lag <- lmer_alt(N400_pict ~ lag1Semantics*semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # to compare the two models, N400_pict random structure needs to be identical
mod_N400_pict_comp <- lmer_alt(N400_pict ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# model comparison
anova(mod_N400_pict_comp, mod_N400_pict_lag)

# Create a list of models
models <- list("N400_verb_LAG" = mod_N400_verb_lag, "N400_pict_LAG" = mod_N400_pict_lag)

# F-tests (type III tests)
(tests <- lapply(models, anova))

## PLANNED FOLLOW-UP CONTRASTS ## -----------------------------------------------------------------

# Allow emmeans to compute Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means_semantics <- lapply(models,function(x){
    emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for the main effect of context
(means_context <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ context, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for the main effect of lag
(means_lag <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ lag1Semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for semantics within each contexts
(means_nested.context <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for semantics within each lag1 semantic condition
(means_nested.lag <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|lag1Semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Follow-up contrasts for semantics within each lag1Semantics and each context condition
(means_nested <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|lag1Semantics*context, infer = TRUE, adjust = "bonferroni")$contrasts}))

# Backup results
save(models, tests, means_semantics, means_context, means_lag, 
     means_nested.context, means_nested.lag,means_nested, file = "EEG/export/stats_appendixLag.RData")

## PLOTTING ## ------------------------------------------------------------------------------------

# Creates a bar plot

## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(tidyverse)    # Version 1.3.0
library(magrittr)     # Version 1.5
library(eeguana)      # Version 0.1.4.9000
library(cowplot)      # Version 1.0.0

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 %<>% filter(!error) %>% na.omit()

# Define ggplot theme
styling <- theme(panel.grid = element_blank(),
                 panel.border = element_rect(colour = "black", size = 1),
                 legend.position = "right",
                 axis.ticks = element_line(colour = "black"),
                 axis.title = element_text(color = "black", family = "Helvetica", size = 10),
                 axis.text = element_text(color = "black", family = "Helvetica", size = 10),
                 legend.title = element_text(color = "black", family = "Helvetica", size = 10, face = "bold"),
                 legend.text = element_text(color = "black", family = "Helvetica", size = 10),
                 strip.background = element_blank(),
                 strip.text = element_text(color = "black", family = "Helvetica", size = 10))

# Rename some factor levels
a1 %<>% mutate(semantics = factor(semantics, levels = c("int", "vio", "mci"),
                                  labels = c("Intuitive", "Violation", "MCI")),
               lag1Semantics = factor(lag1Semantics, levels = c("int", "vio", "mci"),
                                  labels = c("Intuitive", "Violation", "MCI")),
               context = factor(context, levels = c("neu", "neg"),
                                labels = c("Neutral context", "Negative context")))

# Define color scheme for conditions
colors_conditions <- viridisLite::plasma(3, end = 0.9, direction = -1)[c(1, 2, 3)] %>%
    set_names(c("Intuitive", "Violation", "MCI"))

## BAR PLOTS ## -----------------------------------------------------------------------------------

# Convert dependent variables to long format
a1_long <- a1 %>% gather(key = "dv", value = "value", N400_verb, N400_pict, factor_key = TRUE)

# Compute summary statistics (means and confidence intervals) for verb-related and picture-related N400
summs <- map(c("N400_verb", "N400_pict"), function(dv){
    summ <- Rmisc::summarySEwithin(a1, measurevar = dv, withinvars = c("lag1Semantics", "semantics", "context"),
                                   idvar = "participant", na.rm = TRUE)
    colnames(summ)[colnames(summ) == dv] <- "value"
    summ$dv <- dv
    return(summ)
}) %>% set_names(c("N400_verb", "N400_pict"))

# Bar plots for verb-related and picture-related N400
bars <- map(c("N400_verb", "N400_pict"), function(what){
    if (what == "N400_verb") {
        # Brackets and stars for statistical significance
        bracket <- data.frame(context = as.factor("Neutral context"),
                              lag1Semantics = rep(as.factor(c("Intuitive", "Violation", "MCI")), each = 3),
                              ymin = c(0.2, 0.5, 0.2, rep(NA, times = 6)),
                              ymax = c(0.5, 0.5, 0.5, rep(NA, times = 6)),
                              xmin = c(0.7, 0.7, 1.3, rep(NA, times = 6)),
                              xmax = c(0.7, 1.3, 1.3, rep(NA, times = 6)),
                              star = rep(c("**", NA, NA), each = 3),
                              ystar = 0.45)
        # Actual plotting
        ggplot(summs[names(summs) == what][[1]], aes(x = context, y = value, fill = semantics)) +
            geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
            geom_errorbar(aes(ymin = value - ci, ymax = value + ci), position = position_dodge(width = 0.9), width = 0.5) +
            geom_segment(data = bracket, aes(x = xmin, y = ymin, xend = xmax, yend = ymax), inherit.aes = FALSE) +
            geom_label(data = bracket, aes(x = context, y = ystar, label = star), inherit.aes = FALSE, size = 6, label.size = 0) +
            scale_fill_manual(values = colors_conditions) +
            labs(fill = "Semantics") +
            coord_cartesian(ylim = c(-4, 1)) +
            scale_x_discrete(labels = c("Neutral\ncontext", "Negative\ncontext")) +
            scale_y_continuous(name = "ROI amplitude (µV)", breaks = seq(-4, 1, 1)) +
            geom_hline(yintercept = 0) +
            theme_bw() + styling + theme(axis.title.x = element_blank(), legend.position = "none") +
            facet_grid(.~lag1Semantics)
    } else {
        # Brackets and stars for statistical significance
        bracket <- data.frame(context = as.factor("Neutral context"),
                              lag1Semantics = rep(as.factor(c("Intuitive", "Violation", "MCI")), each = 3),
                              ymin = c(rep(NA, times = 3), rep(c(0.2, 0.5, 0.2), times = 2)),
                              ymax = c(rep(NA, times = 3), rep(c(0.5, 0.5, 0.5), times = 2)),
                              xmin = c(rep(NA, times = 3), rep(c(0.7, 0.7, 1.3), times = 2)),
                              xmax = c(rep(NA, times = 3), rep(c(0.7, 1.3, 1.3), times = 2)),
                              star = rep(c(NA, "*", "*"), each = 3),
                              ystar = 0.45)
        # Actual plotting
        ggplot(summs[names(summs) == what][[1]], aes(x = context, y = value, fill = semantics)) +
            geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
            geom_errorbar(aes(ymin = value - ci, ymax = value + ci), position = position_dodge(width = 0.9), width = 0.5) +
            geom_segment(data = bracket, aes(x = xmin, y = ymin, xend = xmax, yend = ymax), inherit.aes = FALSE) +
            geom_label(data = bracket, aes(x = context, y = ystar, label = star), inherit.aes = FALSE, size = 6, label.size = 0) +
            scale_fill_manual(values = colors_conditions) +
            labs(fill = "Semantics") +
            coord_cartesian(ylim = c(-4, 1)) +
            scale_x_discrete(labels = c("Neutral\ncontext", "Negative\ncontext")) +
            scale_y_continuous(name = "ROI amplitude (µV)", breaks = seq(-4, 1, 1)) +
            geom_hline(yintercept = 0) +
            theme_bw() + styling + theme(axis.title.x = element_blank(), legend.position = "none") +
            facet_grid(.~lag1Semantics)
    }
}) %>% set_names(c("N400_verb", "N400_pict"))

## EXAMPLE TRIAL ## -------------------------------------------------------------------------------

# Example for one sentence with verbs in three conditions
stim <- ggplot() + theme_void() + theme(plot.background = element_rect(fill = "white", color = "white")) +
    coord_cartesian(xlim = c(0, 1.2), ylim = c(0, 1)) + 
    geom_text(aes(x = 0.5, y = 0.5, label = '"The old barren birch tree'), size = 4.939, family = "Helvetica", hjust = 1) +
    geom_segment(aes(x = 0.51, xend = 0.54, y = 0.5, yend = 0.8)) +
    geom_segment(aes(x = 0.51, xend = 0.54, y = 0.5, yend = 0.5)) +
    geom_segment(aes(x = 0.51, xend = 0.54, y = 0.5, yend = 0.2)) +
    geom_text(aes(x = 0.55, y = 0.8, label = "creaks"), size = 4.939, family = "Helvetica", fontface = "bold", color = colors_conditions[1], hjust = 0) +
    geom_text(aes(x = 0.55, y = 0.5, label = "talks"), size = 4.939, family = "Helvetica", fontface = "bold", color = colors_conditions[2], hjust = 0) +
    geom_text(aes(x = 0.55, y = 0.2, label = "blossoms"), size = 4.939, family = "Helvetica", fontface = "bold", color = colors_conditions[3], hjust = 0) +
    geom_text(aes(x = 0.675, y = 0.8, label = 'in the wind"'), size = 4.939, family = "Helvetica", hjust = 0) +
    geom_text(aes(x = 0.643, y = 0.5, label = 'to the girl"'), size = 4.939, family = "Helvetica", hjust = 0) +
    geom_text(aes(x = 0.730, y = 0.2, label = 'above the girl"'), size = 4.939, family = "Helvetica", hjust = 0) +
    draw_plot(get_legend(bars$N400_verb + theme(legend.position = "right", legend.title = element_blank())), x = 0.65, y = 0.5, vjust = 0.48)

# Figure 1: Verb-Related N400 Effects
plot_grid(stim, bars$N400_verb,
          nrow = 2, rel_heights = c(0.2, 1), labels = c("A", "B", NULL), label_fontfamily = "Helvetica") %>%
    ggsave(filename = "EEG/figures/lag_N400_verb.pdf", width = 18, height = 22, units = "cm")

# Figure 1: Picture-Related N400 Effects
plot_grid(stim, bars$N400_pict,
          nrow = 2, rel_heights = c(0.2, 1), labels = c("A", "B", NULL), label_fontfamily = "Helvetica") %>%
    ggsave(filename = "EEG/figures/lag_N400_pict.pdf", width = 18, height = 22, units = "cm")

## TABLES -----------------------------------------------------------------------------------------
    
    # Script creates a table for the output of our two linear mixed-effects models. The upper half of
    # the table includes ANOVA-style type III tests (F-tests), the bottom half contains planned
    # follow-up contrasts. For the F-tests, F-values, degrees of freedom, and p-values are
    # printed, whereas for the contrasts, regression estimates, 95% confidence intervals, and
    # p-values are printed.
    
    ## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(huxtable)     # version 5.0.0

# Load output from mixed models
load("EEG/export/stats_appendixLag.RData")

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
conts <- lapply(means_nested, function(x){
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
tab <- rbind(c("Verb-Related N400", "", "Picture-Related N400", ""), tab)

# Add a stub column
tab <- cbind(c("", "**Model output**", "Lag1Semantics", "Semantics", "Context", "Lag1Semantics x semantics", 
               "Lag1Semantics x context", "Semantics × context", "lag1Semantics x semantics x context",
               "**Planned contrasts**", "Vio. - int.<br/>(lag1Int, neutral)", "MCI - int.<br/>(lag1Int, neutral)",
               "Vio. - int.<br/>(lag1Vio, neutral)", "MCI - int.<br/>(lag1Vio, neutral)",
               "Vio. - int.<br/>(lag1MCI, neutral)", "MCI - int.<br/>(lag1MCI, neutral)",
               "Vio. - int.<br/>(lag1Int, negative)", "MCI - int.<br/>(lag1Int, negative)",
               "Vio. - int.<br/>(lag1Vio, negative)", "MCI - int.<br/>(lag1Vio, negative)",
               "Vio. - int.<br/>(lag1MCI, negative)", "MCI - int.<br/>(lag1MCI, negative)"), tab)

# Remove old column names
names(tab) <- NULL

# Create a huxtable and output as markdown
huxt <- huxtable(tab, add_colnames = FALSE)
print_md(huxt, max_width = Inf)

# Export as a word file (after some re-formatting)
tab_word <- data.frame(lapply(tab, function(x){gsub("<br/>", "\n", x)}))
tab_word <- data.frame(lapply(tab_word, function(x){gsub("\\*|\\_", "", x)}))
huxt_word <- huxtable(tab_word, add_colnames = FALSE)
quick_docx(huxt_word, file = "EEG/tables/table_lag.docx", open = FALSE)


# -------------------------------------------------------------------------------------------------
## 2) MODELS WITH COVARIATES METAPHORICITY, PLAUSIBILITY, IMAGEABILIY, CLOZE PROBABILITY ## -------
# -------------------------------------------------------------------------------------------------

# Pre-ratings revealed differences between semantic material regarding metaphoricity, cloze
# probability, imageability, and plausibility. Here, additional linear mixed model analyses
# are conducted, testing whether these covariates affect N400 amplitudes directly and/or
# in interaction with semantic conditions and context emotionality.  
# Script computes linear mixed-effects regression models with simple contrast coding for the fixed 
# effects of semantics and emotional context, and with centered continuous predictor variables
# cloze probability/imageability/plausibility/metaphoricity. Thus, in each model, the estimate 
# of the intercept is the grand mean, while the estimates of the slopes contrast "treatment" levels 
# to their respective reference levels (semantics & lag1Semantics: violation - intuitive, mci - 
# intuitive; emotional context: negative - neutral). The random effects structure of the original 
# models of the main analyses is used. Models are compared using Chi^2 Likelihood-Ratio-tests.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(dplyr)

# Read pilot rating data from SPSS file
pilot <- haven::read_sav("FB/gesamt_2.sav")

# Trim whitespace
pilot$KonzeptNr <- as.numeric(trimws(pilot$KonzeptNr))
pilot$VerbBedingung <- trimws(pilot$VerbBedingung)

# Check if pilot and experimental data are matching
all.equal(sort(unique(pilot$KonzeptNr)), sort(unique(a1$KonzeptNr)))
all.equal(sort(unique(pilot$VerbNr)), sort(unique(a1$VerbNr)))

# Rename conditions
pilot <- pilot %>% mutate(semantics = factor(VerbBedingung, levels = c("neutral", "sem", "mci"), labels = c("int", "vio", "mci")))

# Average pilot data across participants
covariates <- pilot %>% group_by(KonzeptNr, VerbNr, semantics) %>% summarize(clozeprob = mean(Frage1),
                                                                             plausibility = mean(Frage2),
                                                                             metaphoricity = mean(Frage3),
                                                                             imageability = mean(Frage4))
# Bind covariates to the experimental data
a1 <- left_join(a1, covariates, by = c("KonzeptNr", "VerbNr"))

# Center covariates for LMMs (shouldn't be necessary if we don't compute interactions)
a1 <- a1 %>% mutate(across(.cols = c(clozeprob, plausibility, metaphoricity, imageability), .fns = scale, scale = FALSE))

## LINEAR MIXED-EFFECTS MODELS ## -----------------------------------------------------------------

# LMM including all covariates as fixed effects
mod_N400_verb_covs <- lmer_alt(N400_verb ~ (clozeprob + plausibility + metaphoricity + imageability) + semantics*context
                               + (semantics*context||participant) + (semantics*context||item), data = a1,
                               control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Model output
anova(mod_N400_verb_covs) # Semantics remains significant, no other effects
anova(mod_N400_verb, mod_N400_verb_covs) # Takes a while because models need to be refitted with ML; p = 0.765
emmeans(mod_N400_verb_covs, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts
# MCI - intuitive remains significant (still ~ -0.5 Microvolts, p = 0.004)

# rePCA(mod_N400_verb_covs)
# mod_N400_verb_covs.alt <- lmer_alt(N400_verb ~ (clozeprob + plausibility + metaphoricity + imageability) + semantics*context
#                                + (context||participant) + (semantics*context||item),
#                                data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# # refit mod_N400_verb_alt with the same random structure so model comparison is possible
# mod_N400_verb_alt <- lmer_alt(N400_verb ~ semantics*context
#                                    + (context||participant) + (semantics*context||item),
#                                    data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#
# # Model output
# anova(mod_N400_verb_covs.alt) # MCI - correct remains significant, no other effects
# anova(mod_N400_verb_alt, mod_N400_verb_covs.alt) # Takes a while because models need to be refitted with ML - n.s.
# emmeans(mod_N400_verb_covs.alt, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts
#
# # And another LMM including interactions with each covariate
# mod_N400_verb_covs.ia <- lmer_alt(N400_verb ~ (clozeprob + plausibility + metaphoricity + imageability) * semantics*context
#                                   + (semantics*context||participant) + (semantics*context||item),
#                                   data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# 
# # Model summary
# summary(mod_N400_verb_covs.ia)
# anova(mod_N400_verb, mod_N400_verb_covs.ia) # n.s.
#
# # Set up a model with covariates only
# mod_N400_verb_covs.only <- lmer_alt(N400_verb ~ (clozeprob + plausibility + metaphoricity + imageability)
#                                + (semantics*context||participant) + (semantics*context||item),
#                                data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# summary(mod_N400_verb_covs.only)

# -------------------------------------------------------------------------------------------------
## 3) P-HACKING THE VERB-RELATED N400 EFFECT ## ---------------------------------------------------
# -------------------------------------------------------------------------------------------------

## SETUP AS BEFORE ## -----------------------------------------------------------------------------

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
t(contrasts.context <- t(cbind(c("neu" = -1, "neg" = 1))))
contrasts(a1$context) <- ginv(contrasts.context)

# Define simple contrast coding for semantics (violation - intuitive, mci - intuitive)
t(contrasts.semantics <- t(cbind(c("int" = -1, "vio" = 1, "mci" = 0),
                                 c("int" = -1, "vio" = 0, "mci" = 1))))
contrasts(a1$semantics) <- ginv(contrasts.semantics)

## RM-ANOVA INSTEAD OF LMM ## ---------------------------------------------------------------------

# Fit a repeated-measures ANOVA instead of a LMM
mod_N400_aov <- aov_ez(
    id = "participant",
    dv = "N400_verb",
    within = c("semantics", "context"),
    fun_aggregate = mean,
    data = a1
)

# Show ANOVA summary
summary(mod_N400_aov)

# Follow-up contrasts for semantics within each contexts
emmeans(
    mod_N400_aov, 
    trt.vs.ctrl ~ semantics|context,
    infer = TRUE,
    adjust = "bonferroni"
)$contrasts %>% 
    as.data.frame()

## FATIGUE ## -------------------------------------------------------------------------------------

# Create a new variable for the half of the experiment (I vs. II, split-half)
a1 %<>%
    group_by(participant) %>%
    mutate(
        trial = row_number(),
        half = ifelse(trial < max(trial) / 2, "I", "II") %>% factor()
    )

# Set new contrasts
t(contrasts.half <- t(cbind(c("I" = -1, "II" = 1))))
contrasts(a1$half) <- ginv(contrasts.half)

# Fit LMM with an additional fixed effect
mod_N400_half <- lmer_alt(N400_verb ~ semantics*context*half
                          + (semantics*context||participant) 
                          + (semantics*context||item),
                          data = a1, control = lmerControl(calc.derivs = FALSE,
                                                           optimizer = "bobyqa",
                                                           optCtrl = list(maxfun = 2e5)))

# F-tests
anova(mod_N400_half)

# Follow-up contrasts for semantics within each contexts
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)
emmeans(
    mod_N400_half, 
    trt.vs.ctrl ~ semantics|context|half,
    infer = TRUE,
    adjust = "bonferroni"
)$contrasts %>%
    as.data.frame()

# -------------------------------------------------------------------------------------------------
## 4) P600 FIGURE ## ------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(tidyverse)   # Version 1.3.0
library(magrittr)    # Version 1.5
library(eeguana)     # Version 0.1.4.9000
library(cowplot)     # Version 1.0.0

# Load preprocessed data
a1 <- readRDS("EEG/export/a1_RDS")
avgs.verb <- readRDS("EEG/export/avgs_verb_RDS")
avgs.pict <- readRDS("EEG/export/avgs_pict_RDS")

# Combine verb-related and picture-related potentials
avgs.verb <- avgs.verb %>% mutate(type = "Verb-related")
avgs.pict <- avgs.pict %>% mutate(type = "Picture-related")
avgs <- suppressWarnings(bind(avgs.verb, avgs.pict))
avgs$.segments$type <- factor(avgs$.segments$type, levels = c("Verb-related", "Picture-related"))

# Remove trials with errors or invalid RTs/ERPs
a1 <- na.omit(a1[!a1$error,])

# Define ggplot theme
styling <- theme(panel.grid = element_blank(),
                 panel.border = element_rect(colour = "black", size = 1),
                 legend.position = "right",
                 axis.ticks = element_line(colour = "black"),
                 axis.title = element_text(color = "black", family = "Helvetica", size = 10),
                 axis.text = element_text(color = "black", family = "Helvetica", size = 10),
                 legend.title = element_text(color = "black", family = "Helvetica", size = 10, face = "bold"),
                 legend.text = element_text(color = "black", family = "Helvetica", size = 10),
                 strip.background = element_blank(),
                 strip.text = element_text(color = "black", family = "Helvetica", size = 10))

# Rename some factor levels
a1$semantics <- factor(a1$semantics, levels = c("int", "vio", "mci"), labels = c("Intuitive", "Violation", "MCI"))
a1$context <- factor(a1$context, levels = c("neu", "neg"), labels = c("Neutral context", "Negative context"))
avgs$.segments$semantics <- factor(avgs$.segments$semantics, levels = c("int", "vio", "mci"), labels = c("Intuitive", "Violation", "MCI"))
avgs$.segments$context <- factor(avgs$.segments$context, levels = c("neu", "neg"), labels = c("Neutral context", "Negative context"))

# # Define color scheme (brewer)
# colors.conditions <- RColorBrewer::brewer.pal(3, name = "Set1")[c(3, 1, 2)]
# colors.highlight <- "#ffff33"
# colors.topo <- "RdBu"
# scale.topo <- scale_fill_distiller(palette = "RdBu", guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 1), breaks = c(-0.7, 0, 0.7))

# Define color scheme for conditions (plasma)
colors.conditions <- viridisLite::plasma(3, end = 0.9, direction = -1)[c(1, 2, 3)]
colors.highlight <- viridisLite::plasma(1, direction = -1)
colors.topo <- "plasma"
scale.topo <- scale_fill_viridis_c(option = "plasma", guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 1), breaks = c(-0.7, 0, 0.7))

# Assign names to colors
names(colors.conditions) <- c("Intuitive", "Violation", "MCI")



## ONE ADDITIONAL FIGURE FOR THE APPENDIX ## ------------------------------------------------------

# # Define color scales
# colors.topo <- "RdBu"
# scale.topo <- scale_fill_distiller(palette = "RdBu", breaks = c(-0.7, 0, 0.7),
#                                    guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 0.5, title.vjust = 1))
# colors.topo <- "plasma"
# scales.topo <- scale_fill_viridis_c(option = "plasma", breaks = c(-0.7, 0, 0.7),
#                                     guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 0.5, title.vjust = 1))

# Update some parameters for the color bar
scale.topo$guide$label.hjust <- 0.5
scale.topo$guide$title.vjust <- 1

# Create a color bar for the topographies
colbar <- get_legend(ggplot(data.frame(x = c(0, 0), y = c(0, 0), fill = c(-0.7, 0.7)), aes(x = x, y = y, fill = fill)) + geom_raster() +
                         scale.topo + labs(fill = "Ampl.\n(µV)") +
                         theme(legend.direction = "horizontal",
                               legend.key.width = unit(0.3, "cm"),
                               legend.title = element_text(family = "Helvetica", size = 10, color = "black"),
                               legend.text = element_text(family = "Helvetica", size = 10, color = "black"),
                               legend.title.align = 0.5))

# Create scalp topographies for P600 (new ROI)
topos.P600 <- map(c(0.5, 0.6, 0.7, 0.8), function(tmin){
    tmax <- tmin + 0.1
    # Compute differences between conditions in time window at all electrodes
    avgs %>% filter(type == "Verb-related", between(as_time(.sample), !!tmin, !!tmax)) %>%
        group_by(semantics, context) %>% summarise_at(channel_names(.), mean) %>%
        signal_tbl() %>% select(Fp1:A1) %>% t() %>% as.data.frame() %>%
        set_colnames(c("int.neu", "int.neg", "vio.neu", "vio.neg", "mci.neu", "mci.neg")) %>%
        transmute(diff.vio.neu = vio.neu - int.neu,
                  diff.vio.neg = vio.neg - int.neg,
                  diff.mci.neu = mci.neu - int.neu,
                  diff.mci.neg = mci.neg - int.neg) %>%
        # Create four topoplots for the current time window
        map(function(amplitudes){
            dat <- data.frame(amplitude = amplitudes, electrode = avgs %>% select(Fp1:A1) %>% channel_names())
            p <- eegUtils::topoplot(data = dat,
                                    limits = c(-0.7, 0.7),
                                    r = 0.9,
                                    palette = colors.topo,
                                    interp_limit = "skirt", 
                                    contour = FALSE,
                                    highlights = c("C1", "C2", "CZ", "FC1", "FC2", "FZ"),
                                    scaling = 0.4)
            p$layers[[6]]$aes_params$size <- 0.1
            p$layers[[7]]$aes_params$colour <- "black"
            p + theme(legend.position = "none")}) %>%
        # Add another plot denoting the current time window in ms
        prepend(list(ggplot() + theme_void() +
                         annotate("text", x = 0, y = 0, label = paste0(tmin*1000, "-", tmax*1000, " ms"), size = 3.528, family = "Helvetica"))) %>%
        # Combine the five plots below one another
        plot_grid(plotlist = ., nrow = 5, rel_heights = c(1, 3, 3, 3, 3))}) %>%
    prepend(list(plot_grid(NULL,
                           ggplot() + theme_void() +
                               annotate("text", x = 0, y = 0, label = paste0("Violation - intuitive\n(neutral context)"), size = 3.528, family = "Helvetica"),
                           ggplot() + theme_void() +
                               annotate("text", x = 0, y = 0, label = paste0("Violation - intuitive\n(negative context)"), size = 3.528, family = "Helvetica"),
                           ggplot() + theme_void() +
                               annotate("text", x = 0, y = 0, label = paste0("MCI - intuitive\n(neutral context)"), size = 3.528, family = "Helvetica"),
                           ggplot() + theme_void() +
                               annotate("text", x = 0, y = 0, label = paste0("MCI - intuitive\n(negative context)"), size = 3.528, family = "Helvetica"),
                           nrow = 5, rel_heights = c(1, 3, 3, 3, 3)))) %>%
    # Combine all time windows next to each other
    plot_grid(plotlist = ., nrow = 1) +
    # Add colorbar
    draw_plot(colbar, x = -0.403, y = 0.42)

# Save plot
ggsave(topos.P600, filename = "EEG/figures/P600_appendix.pdf", width = 18, height = 15, units = "cm")

# # Creates a bar plot, an ERP waveform, and scalp topographies for the verb-related P600 effect 
# # different semantic conditions (intuitive, violation, MCI) within each type of emotional 
# # context (neutral, negative).
# 
# ## PREPARATION ## ---------------------------------------------------------------------------------
# 
# # Load packages
# library(tidyverse) # Version 1.3
# library(magrittr)  # Version 1.5
# library(eeguana)   # Version 0.1.4.9000
# library(cowplot)   # Version 1.0.0
# 
# # Load preprocessed data
# a1 <- readRDS("EEG/export/a1_appendix.RDS")
# avgs <- readRDS("EEG/export/avgs_verb_appendix.RDS")
# 
# # Remove trials with errors or invalid RTs/ERPs
# a1 <- na.omit(a1[!a1$error,])
# 
# # Define ggplot theme
# styling <- theme(panel.grid = element_blank(),
#                  panel.border = element_rect(colour = "black", size = 1),
#                  legend.position = "right",
#                  axis.ticks = element_line(colour = "black"),
#                  axis.title = element_text(color = "black", family = "Helvetica", size = 10),
#                  axis.text = element_text(color = "black", family = "Helvetica", size = 10),
#                  legend.title = element_text(color = "black", family = "Helvetica", size = 10, face = "bold"),
#                  legend.text = element_text(color = "black", family = "Helvetica", size = 10),
#                  strip.background = element_blank(),
#                  strip.text = element_text(color = "black", family = "Helvetica", size = 10))
# 
# # Rename some factor levels
# a1$semantics <- factor(a1$semantics, levels = c("int", "vio", "mci"), labels = c("Intuitive", "Violation", "MCI"))
# a1$context <- factor(a1$context, levels = c("neu", "neg"), labels = c("Neutral context", "Negative context"))
# avgs$.segments$semantics <- factor(avgs$.segments$semantics, levels = c("int", "vio", "mci"), labels = c("Intuitive", "Violation", "MCI"))
# avgs$.segments$context <- factor(avgs$.segments$context, levels = c("neu", "neg"), labels = c("Neutral context", "Negative context"))
# 
# # Define colours for conditions
# colors_conditions <- RColorBrewer::brewer.pal(3, name = "Set1")[c(3, 1, 2)]
# names(colors_conditions) <- c("Intuitive", "Violation", "MCI")
# 
# ## TOPOGRAPHIES ONLY ## ---------------------------------------------------------------------------
# 
# # Define color scales
# colors.topo <- "RdBu"
# scales.topo <- scale_fill_distiller(palette = "RdBu", breaks = c(-0.7, 0, 0.7),
#                                     guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 0.5, title.vjust = 1))
# # colors.topo <- "plasma"
# # scales.topo <- scale_fill_viridis_c(option = "plasma", breaks = c(-0.7, 0, 0.7),
# #                                     guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 0.5, title.vjust = 1))
# 
# # Create a color bar for the topographies
# colbar <- get_legend(ggplot(data.frame(x = c(0, 0), y = c(0, 0), fill = c(-0.7, 0.7)), aes(x = x, y = y, fill = fill)) + geom_raster() +
#                          scales.topo +
#                          labs(fill = "Ampl.\n(µV)") +
#                          theme(legend.direction = "horizontal",
#                                legend.key.width = unit(0.3, "cm"),
#                                legend.title = element_text(family = "Helvetica", size = 10, color = "black"),
#                                legend.text = element_text(family = "Helvetica", size = 10, color = "black"),
#                                legend.title.align = 0.5))
# 
# # Create scalp topographies for P600 (new ROI)
# map(c(0.5, 0.6, 0.7, 0.8), function(tmin){
#     tmax <- tmin + 0.1
#     # Compute differences between conditions in time window at all electrodes
#     avgs %>% filter(between(as_time(.sample), !!tmin, !!tmax)) %>%
#         group_by(semantics, context) %>% summarise_at(channel_names(.), mean) %>%
#         signal_tbl() %>% select(Fp1:A1) %>% t() %>% as.data.frame() %>%
#         set_colnames(c("int.neu", "int.neg", "vio.neu", "vio.neg", "mci.neu", "mci.neg")) %>%
#         transmute(diff.vio.neu = vio.neu - int.neu,
#                   diff.vio.neg = vio.neg - int.neg,
#                   diff.mci.neu = mci.neu - int.neu,
#                   diff.mci.neg = mci.neg - int.neg) %>%
#         # Create four topoplots for the current time window
#         map(function(amplitudes){
#             dat <- data.frame(amplitude = amplitudes, electrode = avgs %>% select(Fp1:A1) %>% channel_names())
#             p <- eegUtils::topoplot(data = dat,
#                                     limits = c(-0.7, 0.7),
#                                     r = 0.9,
#                                     palette = colors.topo,
#                                     interp_limit = "skirt", 
#                                     contour = FALSE,
#                                     highlights = c("C1", "C2", "CZ", "FC1", "FC2", "FZ"),
#                                     scaling = 0.4)
#             p$layers[[6]]$aes_params$size <- 0.1
#             p$layers[[7]]$aes_params$colour <- "black"
#             p + theme(legend.position = "none")}) %>%
#         # Add another plot denoting the current time window in ms
#         prepend(list(ggplot() + theme_void() +
#                          annotate("text", x = 0, y = 0, label = paste0(tmin*1000, "-", tmax*1000, " ms"), size = 3.528, family = "Helvetica"))) %>%
#         # Combine the five plots below one another
#         plot_grid(plotlist = ., nrow = 5, rel_heights = c(1, 3, 3, 3, 3))}) %>%
#     prepend(list(plot_grid(NULL,
#                            ggplot() + theme_void() +
#                                annotate("text", x = 0, y = 0, label = paste0("Violation - intuitive\n(neutral context)"), size = 3.528, family = "Helvetica"),
#                            ggplot() + theme_void() +
#                                annotate("text", x = 0, y = 0, label = paste0("Violation - intuitive\n(negative context)"), size = 3.528, family = "Helvetica"),
#                            ggplot() + theme_void() +
#                                annotate("text", x = 0, y = 0, label = paste0("MCI - intuitive\n(neutral context)"), size = 3.528, family = "Helvetica"),
#                            ggplot() + theme_void() +
#                                annotate("text", x = 0, y = 0, label = paste0("MCI - intuitive\n(negative context)"), size = 3.528, family = "Helvetica"),
#                            nrow = 5, rel_heights = c(1, 3, 3, 3, 3)))) %>%
#     # Combine all time windows next to each other
#     plot_grid(plotlist = ., nrow = 1) +
#     # Add colorbar
#     draw_plot(colbar, x = -0.403, y = 0.42) +
#     # Save plot
#     ggsave(filename = "EEG/figures/P600_appendix.pdf", width = 18, height = 15, units = "cm")

# ## BAR PLOTS ## -----------------------------------------------------------------------------------
# 
# # Convert dependent variables to long format
# a1_long <- a1 %>% gather(key = "dv", value = "value", P600_1.verb, P600_2.verb, P600_3.verb, P600_4.verb, factor_key = TRUE)
# 
# # Compute summary statistics (means and confidence intervals) for verb-related and picture-related N400
# summs <- sapply(c("P600_1.verb", "P600_2.verb", "P600_3.verb", "P600_4.verb"), function(dv){
#   summ <- Rmisc::summarySEwithin(a1, measurevar = dv, withinvars = c("semantics", "context"), idvar = "participant", na.rm = TRUE)
#   colnames(summ)[colnames(summ) == dv] <- "value"
#   summ$dv <- dv
#   return(summ)}, simplify = FALSE)
# 
# # Bar plots for verb-related P600 (old ROI)
# bars_old <- sapply(c("P600_1.verb", "P600_2.verb"), function(what){
#   # Actual plotting
#   ggplot(summs[names(summs) == what][[1]], aes(x = context, y = value, fill = semantics)) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#     geom_errorbar(aes(ymin = value - ci, ymax = value + ci), position = position_dodge(width = 0.9), width = 0.5) +
#     scale_fill_manual(values = colors_conditions) +
#     labs(fill = "Semantics") +
#     coord_cartesian(ylim = c(-2, 3)) +
#     scale_x_discrete(labels = c("Neutral\ncontext", "Negative\ncontext")) +
#     scale_y_continuous(name = "ROI amplitude (µV)", breaks = seq(-2, 2, 2)) +
#     geom_hline(yintercept = 0) +
#     theme_bw() + styling + theme(axis.title.x = element_blank(), legend.position = "none")
# }, simplify = FALSE)
# 
# # Bar plots for verb-related P600 (new ROI)
# bars_new <- sapply(c("P600_3.verb", "P600_4.verb"), function(what){
#   # Brackets and stars for statistical significance
#   bracket <- data.frame(context = as.factor("Negative context"),
#                         ymin = c(-0.4, -0.6, -0.2),
#                         ymax = c(-0.6, -0.6, -0.6),
#                         xmin = c(1.7, 1.7, 2.0),
#                         xmax = c(1.7, 2.0, 2.0))
#   # Actual plotting
#   ggplot(summs[names(summs) == what][[1]], aes(x = context, y = value, fill = semantics)) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#     geom_errorbar(aes(ymin = value - ci, ymax = value + ci), position = position_dodge(width = 0.9), width = 0.5) +
#     geom_segment(data = bracket, aes(x = xmin, y = ymin, xend = xmax, yend = ymax), inherit.aes = FALSE) +
#     geom_label(aes(x = 1.85, y = -0.67, label = "*"), inherit.aes = FALSE, size = 6, label.size = 0) +
#     scale_fill_manual(values = colors_conditions) +
#     labs(fill = "Semantics") +
#     coord_cartesian(ylim = c(-2, 3)) +
#     scale_x_discrete(labels = c("Neutral\ncontext", "Negative\ncontext")) +
#     scale_y_continuous(name = "ROI amplitude (µV)", breaks = seq(-2, 3, 1)) +
#     geom_hline(yintercept = 0) +
#     theme_bw() + styling + theme(axis.title.x = element_blank(), legend.position = "none")
# }, simplify = FALSE)
# 
# ## WAVEFORMS ## -----------------------------------------------------------------------------------
# 
# # ERP waveforms for P600 (old ROI)
# waves_old <- sapply(c("Verb-related (500-700 ms)", "Verb-related (500-900 ms)"), function(what){
#   tmin <- 0.500
#   tmax <- ifelse(what == "Verb-related (500-700 ms)", 0.700, 0.900)
#   x_lim <- ifelse(what == "Verb-related (500-700 ms)", 0.8, 1.0)
#   x_break <- ifelse(what == "Verb-related (500-700 ms)", 0.7, 0.9)
#   x_label <- ifelse(what == "Verb-related (500-700 ms)", 700, 900)
#   # Actual plotting
#   avgs %>%
#     select(ROI) %>%
#     ggplot(aes(x = .time, y = .value, color = semantics)) +
#     geom_rect(aes(xmin = tmin, xmax = tmax, ymin = -Inf, ymax = Inf), fill = "gray90", inherit.aes = FALSE) +
#     geom_hline(yintercept = 0, linetype = "dotted") +
#     geom_vline(xintercept = 0, linetype = "dotted") +
#     stat_summary(fun = "mean", geom = "line") +
#     scale_color_manual(values = colors_conditions) +
#     coord_cartesian(xlim = c(-0.2, x_lim), ylim = c(-4.5, 4.5), expand = FALSE) +
#     scale_x_continuous(breaks = seq(-0.1, x_break, 0.2), labels = seq(-100, x_label, 200)) +
#     scale_y_continuous(breaks = seq(-4, 4, 2)) +
#     xlab("Time (ms)") + ylab("ROI amplitude (µV)") +
#     labs(color = NULL) +
#     theme_bw() + styling + theme(legend.position = "none") +
#     facet_grid(.~context)
# }, simplify = FALSE)
# 
# # ERP waveforms for P600 (new ROI)
# waves_new <- sapply(c("Verb-related (500-700 ms)", "Verb-related (500-900 ms)"), function(what){
#   tmin <- 0.500
#   tmax <- ifelse(what == "Verb-related (500-700 ms)", 0.700, 0.900)
#   x_lim <- ifelse(what == "Verb-related (500-700 ms)", 0.8, 1.0)
#   x_break <- ifelse(what == "Verb-related (500-700 ms)", 0.7, 0.9)
#   x_label <- ifelse(what == "Verb-related (500-700 ms)", 700, 900)
#   # Significant area to highlight (MCI - intuitive in the neutral context)
#   highlight <- avgs %>%
#     select(ROI2) %>% filter(between(as_time(.sample), !!tmin, !!tmax)) %>% 
#     group_by(semantics, context, .sample) %>% summarise_at(channel_names(.), mean, na.rm = TRUE)
#   highlight <- data.frame(seq(tmin, tmax, 0.002),
#                           highlight %>% filter(semantics == "Violation", context == "Negative context") %>% signal_tbl %>% select(ROI2),
#                           highlight %>% filter(semantics == "Intuitive", context == "Negative context") %>% signal_tbl %>% select(ROI2))
#   names(highlight) <- c(".time", "vio", "int")
#   highlight$context <- as.factor("Negative context")
#   # Stars for significance levels
#   star <- data.frame(context = as.factor("Negative context"), stars = "*")
#   # Actual plotting
#   avgs %>%
#     select(ROI2) %>%
#     ggplot(aes(x = .time, y = .value, color = semantics)) +
#     geom_rect(aes(xmin = tmin, xmax = tmax, ymin = -Inf, ymax = Inf), fill = "gray90", inherit.aes = FALSE) +
#     geom_ribbon(data = highlight, aes(x = .time, ymin = vio, ymax = int), fill = "#ffff33", inherit.aes = FALSE) +
#     geom_text(data = star, aes(x = tmin+(tmax-tmin)/2, y = 3.5, label = stars), inherit.aes = FALSE, size = 6) +
#     geom_hline(yintercept = 0, linetype = "dotted") +
#     geom_vline(xintercept = 0, linetype = "dotted") +
#     stat_summary(fun = "mean", geom = "line") +
#     scale_color_manual(values = colors_conditions) +
#     coord_cartesian(xlim = c(-0.2, x_lim), ylim = c(-4.5, 4.5), expand = FALSE) +
#     scale_x_continuous(breaks = seq(-0.1, x_break, 0.2), labels = seq(-100, x_label, 200)) +
#     scale_y_continuous(breaks = seq(-4, 4, 2)) +
#     xlab("Time (ms)") + ylab("ROI amplitude (µV)") +
#     labs(color = NULL) +
#     theme_bw() + styling + theme(legend.position = "none") +
#     facet_grid(.~context)
# }, simplify = FALSE)
# 
# ## TOPOGRAPHIES ## --------------------------------------------------------------------------------
# 
# # Create scalp topographies for P600 (old ROI)
# topos_old <- sapply(c("Verb-related (500-700 ms)", "Verb-related (500-900 ms)"), function(what){
#   if(what == "Verb-related (500-700 ms)"){
#     tmp <- avgs %>% filter(between(as_time(.sample), 0.500, 0.700))
#   } else{
#     tmp <- avgs %>% filter(between(as_time(.sample), 0.500, 0.900))}
#   tmp <- tmp %>%
#     group_by(semantics, context) %>% summarise_at(channel_names(.), mean) %>%
#     signal_tbl() %>% select(Fp1:A1) %>% t() %>% as.data.frame()
#   names(tmp) <- c("int.neu", "int.neg", "vio.neu", "vio.neg", "mci.neu", "mci.neg")
#   tmp <- data.frame("diff.vio.neu" = tmp$vio.neu - tmp$int.neu,
#                     "diff.vio.neg" = tmp$vio.neg - tmp$int.neg,
#                     "diff.mci.neu" = tmp$mci.neu - tmp$int.neu,
#                     "diff.mci.neg" = tmp$mci.neg - tmp$int.neg,
#                     "electrode" = rownames(tmp))
#   topos <- lapply(1:4, function(x){
#     p <- eegUtils::topoplot(data = tmp, quantity = colnames(tmp)[x], limits = c(-0.7, 0.7), r = 0.9, palette = "RdBu", interp_limit = "skirt", 
#                             contour = FALSE, highlights = c("C1", "C2", "CZ", "CP1", "CP2", "CPZ"), scaling = 0.5)
#     p$layers[[6]]$aes_params$size <- 0.1
#     p$layers[[7]]$aes_params$colour <- "black"
#     p <- p + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, family = "Helvetica"))})
# }, simplify = FALSE)
# 
# # Create scalp topographies for P600 (new ROI)
# topos_new <- sapply(c("Verb-related (500-700 ms)", "Verb-related (500-900 ms)"), function(what){
#   if(what == "Verb-related (500-700 ms)"){
#     tmp <- avgs %>% filter(between(as_time(.sample), 0.500, 0.700))
#   } else{
#     tmp <- avgs %>% filter(between(as_time(.sample), 0.500, 0.900))}
#   tmp <- tmp %>%
#     group_by(semantics, context) %>% summarise_at(channel_names(.), mean) %>%
#     signal_tbl() %>% select(Fp1:A1) %>% t() %>% as.data.frame()
#   names(tmp) <- c("int.neu", "int.neg", "vio.neu", "vio.neg", "mci.neu", "mci.neg")
#   tmp <- data.frame("diff.vio.neu" = tmp$vio.neu - tmp$int.neu,
#                     "diff.vio.neg" = tmp$vio.neg - tmp$int.neg,
#                     "diff.mci.neu" = tmp$mci.neu - tmp$int.neu,
#                     "diff.mci.neg" = tmp$mci.neg - tmp$int.neg,
#                     "electrode" = rownames(tmp))
#   topos <- lapply(1:4, function(x){
#     p <- eegUtils::topoplot(data = tmp, quantity = colnames(tmp)[x], limits = c(-0.7, 0.7), r = 0.9, palette = "RdBu", interp_limit = "skirt", 
#                             contour = FALSE, highlights = c("C1", "C2", "CZ", "FC1", "FC2", "FZ"), scaling = 0.5)
#     p$layers[[6]]$aes_params$size <- 0.1
#     p$layers[[7]]$aes_params$colour <- "black"
#     p <- p + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, family = "Helvetica"))})
# }, simplify = FALSE)
# 
# # Create a colorbar
# simdat1 <- data.frame(a = 1:10, b = 1:10, c = seq(-0.7, 0.7, length.out = 10))
# colbar <- get_legend(ggplot(simdat1, aes(x = a, y = b, fill = c)) + geom_raster() + geom_line() +
#                        scale_fill_distiller(palette = "RdBu", guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 1), breaks = c(-0.7, 0, 0.7)) +
#                        labs(fill = "Ampl.\n(µV)") +
#                        theme(legend.position = "right",
#                              legend.background = element_blank(),
#                              legend.key.height = unit(0.3, "cm"),
#                              legend.title = element_text(family = "Helvetica", size = 10, color = "black"),
#                              legend.text = element_text(family = "Helvetica", size = 10, color = "black"),
#                              legend.title.align = 0.5))
# 
# # Create one plot combining all four topographies (time window 500-700 ms, old ROI)
# simdat2 <- data.frame("semantics" = factor(c("Violation - Intuitive", "MCI - Intuitive")),
#                       "context" = factor(c("Neutral\ncontext", "Negative\ncontext"), levels = c("Neutral\ncontext", "Negative\ncontext")))
# topos.P600_1 <- ggplot(simdat2, aes(x = context, y = semantics)) +
#   geom_point() +
#   draw_plot(topos_old$`Verb-related (500-700 ms)`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-700 ms)`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-700 ms)`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-700 ms)`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
#   annotate("text", x = 1.5, y = 0.53, label = "500-700 ms", size = 3.528, family = "Helvetica") + 
#   draw_plot(colbar, x = 0.95, y = 1) +
#   styling +
#   theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
#         panel.background = element_rect(fill = "white"),
#         axis.title = element_blank(),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# # Create one plot combining all four topographies (time window 500-900 ms, old ROI)
# topos.P600_2 <- ggplot(simdat2, aes(x = context, y = semantics)) +
#   geom_point() +
#   draw_plot(topos_old$`Verb-related (500-900 ms)`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-900 ms)`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-900 ms)`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-900 ms)`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
#   annotate("text", x = 1.5, y = 0.53, label = "500-900 ms", size = 3.528, family = "Helvetica") +
#   draw_plot(colbar, x = 0.95, y = 1) +
#   styling +
#   theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
#         panel.background = element_rect(fill = "white"),
#         axis.title = element_blank(),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# # Create one plot combining all four topographies (time window 500-700 ms, new ROI)
# topos.P600_3 <- ggplot(simdat2, aes(x = context, y = semantics)) +
#   geom_point() +
#   draw_plot(topos_new$`Verb-related (500-700 ms)`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-700 ms)`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-700 ms)`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-700 ms)`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
#   annotate("text", x = 1.5, y = 0.53, label = "500-700 ms", size = 3.528, family = "Helvetica") + 
#   draw_plot(colbar, x = 0.95, y = 1) +
#   styling +
#   theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
#         panel.background = element_rect(fill = "white"),
#         axis.title = element_blank(),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# # Create one plot combining all four topographies (time window 500-900 ms, new ROI)
# topos.P600_4 <- ggplot(simdat2, aes(x = context, y = semantics)) +
#   geom_point() +
#   draw_plot(topos_new$`Verb-related (500-900 ms)`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-900 ms)`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-900 ms)`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-900 ms)`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
#   annotate("text", x = 1.5, y = 0.53, label = "500-900 ms", size = 3.528, family = "Helvetica") +
#   draw_plot(colbar, x = 0.95, y = 1) +
#   styling +
#   theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
#         panel.background = element_rect(fill = "white"),
#         axis.title = element_blank(),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# ## PUBLICATION-READY FIGURES ## -------------------------------------------------------------------
# 
# # Figure 1: Verb-related potentials (ROI 500-700 ms, old ROI)
# plot_grid(plot_grid(waves_old$`Verb-related (500-700 ms)`) +
#             draw_plot(get_legend(bars_old$P600_1.verb +
#                                    theme(legend.position = "right", legend.title = element_blank(), legend.background = element_blank())),
#                       x = 0.41, y = -0.22),
#           plot_grid(bars_old$P600_1.verb, topos.P600_1, nrow = 1, rel_widths = c(0.6, 1), labels = c("B", "C"),
#                     label_fontfamily = "Helvetica", label_y = 1.03),
#           nrow = 2, rel_heights = c(0.8, 1), labels = c("A", NULL), label_fontfamily = "Helvetica") %>%
#   ggsave(filename = "EEG/figures/appendix_P600_500-700ms_old_ROI.pdf", width = 18, height = 19.8, units = "cm")
# 
# # Figure 1: Verb-related potentials (ROI 500-900 ms, old ROI)
# plot_grid(plot_grid(waves_old$`Verb-related (500-900 ms)`) +
#             draw_plot(get_legend(bars_old$P600_2.verb +
#                                    theme(legend.position = "right", legend.title = element_blank(), legend.background = element_blank())),
#                       x = 0.41, y = -0.22),
#           plot_grid(bars_old$P600_2.verb, topos.P600_2, nrow = 1, rel_widths = c(0.6, 1), labels = c("B", "C"),
#                     label_fontfamily = "Helvetica", label_y = 1.03),
#           nrow = 2, rel_heights = c(0.8, 1), labels = c("A", NULL), label_fontfamily = "Helvetica") %>%
#   ggsave(filename = "EEG/figures/appendix_P600_500-900ms_old_ROI.pdf", width = 18, height = 19.8, units = "cm")
# 
# # Figure 1: Verb-related potentials (ROI 500-700 ms, new ROI)
# plot_grid(plot_grid(waves_new$`Verb-related (500-700 ms)`) +
#             draw_plot(get_legend(bars_new$P600_3.verb +
#                                    theme(legend.position = "right", legend.title = element_blank(), legend.background = element_blank())),
#                       x = 0.41, y = -0.22),
#           plot_grid(bars_new$P600_3.verb, topos.P600_3, nrow = 1, rel_widths = c(0.6, 1), labels = c("B", "C"),
#                     label_fontfamily = "Helvetica", label_y = 1.03),
#           nrow = 2, rel_heights = c(0.8, 1), labels = c("A", NULL), label_fontfamily = "Helvetica") %>%
#   ggsave(filename = "EEG/figures/appendix_P600_500-700ms_new_ROI.pdf", width = 18, height = 19.8, units = "cm")
# 
# # Figure 1: Verb-related potentials (ROI 500-900 ms, new ROI)
# plot_grid(plot_grid(waves_new$`Verb-related (500-900 ms)`) +
#             draw_plot(get_legend(bars_new$P600_4.verb +
#                                    theme(legend.position = "right", legend.title = element_blank(), legend.background = element_blank())),
#                       x = 0.41, y = -0.22),
#           plot_grid(bars_new$P600_4.verb, topos.P600_4, nrow = 1, rel_widths = c(0.6, 1), labels = c("B", "C"),
#                     label_fontfamily = "Helvetica", label_y = 1.03),
#           nrow = 2, rel_heights = c(0.8, 1), labels = c("A", NULL), label_fontfamily = "Helvetica") %>%
#   ggsave(filename = "EEG/figures/appendix_P600_500-900ms_new_ROI.pdf", width = 18, height = 19.8, units = "cm")
# 

# System specs and package versions
sessionInfo()

