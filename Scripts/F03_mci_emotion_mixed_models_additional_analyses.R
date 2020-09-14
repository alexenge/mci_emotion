#/* Run this first piece of code only if you want to create a markdown report for GitHub
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd()) #*/

### MCI EMO - ADDITIONAL ANALYSES ###
### 1) EFFECT OF LAG1 SEMANTICS
### 2) MODELS WITH COVARIATES METAPHORICITY, PLAUSIBILITY, IMAGEABILIY, CLOZE PROBABILITY

# THESE ANALYSES WERE EXPLORATORY ANALYSES NOT REPORTED IN THE MANUSCRIPT

# -------------------------------------------------------------------------------------------------
## 1) EFFECT OF LAG1 SEMANTICS ## -----------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

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


## PLOTTING ## ------------------------------------------------------------------------------------

# Creates a bar plot

## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(dplyr)   # Version 1.0.0
library(tidyr)   # Version 1.1.0
library(eeguana) # Version 0.1.4.9000
library(ggplot2) # Version 3.3.2
library(cowplot) # Version 1.0.0

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")
avgs.verb <- readRDS("EEG/export/avgs_verb.RDS")
avgs.pict <- readRDS("EEG/export/avgs_pict.RDS")

# Combine verb-related and picture-related potentials
avgs.verb <- avgs.verb %>% mutate(type = "Verb-related")
avgs.pict <- avgs.pict %>% mutate(type = "Picture-related")
avgs <- bind(avgs.verb, avgs.pict)
avgs$.segments$type <- factor(avgs$.segments$type, levels = c("Verb-related", "Picture-related"))

# Remove trials with errors or invalid RTs/ERPs
a1 <- na.omit(a1[!a1$error,])

# Define ggplot theme
styling <-   theme(panel.grid = element_blank(),
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
a1$lag1Semantics <- factor(a1$lag1Semantics, levels = c("int", "vio", "mci"), labels = c("Intuitive", "Violation", "MCI"))
a1$context <- factor(a1$context, levels = c("neu", "neg"), labels = c("Neutral context", "Negative context"))
avgs$.segments$semantics <- factor(avgs$.segments$semantics, levels = c("int", "vio", "mci"), labels = c("Intuitive", "Violation", "MCI"))
avgs$.segments$context <- factor(avgs$.segments$context, levels = c("neu", "neg"), labels = c("Neutral context", "Negative context"))

# Define colours for conditions
condition.colors <- RColorBrewer::brewer.pal(3, name = "Set1")[c(3, 1, 2)]
names(condition.colors) <- c("Intuitive", "Violation", "MCI")

## BAR PLOTS ## -----------------------------------------------------------------------------------

# Convert dependent variables to long format
a1.long <- a1 %>% gather(key = "dv", value = "value", N400.verb, N400.pict, factor_key = TRUE)

# Compute summary statistics (means and confidence intervals) for verb-related and picture-related N400
summs <- sapply(c("N400.verb", "N400.pict"), function(dv){
    summ <- Rmisc::summarySEwithin(a1, measurevar = dv, withinvars = c("lag1Semantics", "semantics", "context"), idvar = "participant", na.rm = TRUE)
    colnames(summ)[colnames(summ) == dv] <- "value"
    summ$dv <- dv
    return(summ)}, simplify = FALSE)

# Bar plots for verb-related and picture-related N400
bars <- sapply(c("N400.verb", "N400.pict"), function(what){
    if (what == "N400.verb") {
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
            scale_fill_manual(values = condition.colors) +
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
            scale_fill_manual(values = condition.colors) +
            labs(fill = "Semantics") +
            coord_cartesian(ylim = c(-4, 1)) +
            scale_x_discrete(labels = c("Neutral\ncontext", "Negative\ncontext")) +
            scale_y_continuous(name = "ROI amplitude (µV)", breaks = seq(-4, 1, 1)) +
            geom_hline(yintercept = 0) +
            theme_bw() + styling + theme(axis.title.x = element_blank(), legend.position = "none") +
            facet_grid(.~lag1Semantics)
    }}, simplify = FALSE)

## EXAMPLE TRIAL ## -------------------------------------------------------------------------------

# Example for one sentence with verbs in three conditions
stim <- ggplot() + theme_void() + theme(plot.background = element_rect(fill = "white", color = "white")) +
    coord_cartesian(xlim = c(0, 1.2), ylim = c(0, 1)) + 
    geom_text(aes(x = 0.5, y = 0.5, label = '"The old barren birch tree'), size = 4.939, family = "Helvetica", hjust = 1) +
    geom_segment(aes(x = 0.51, xend = 0.54, y = 0.5, yend = 0.8)) +
    geom_segment(aes(x = 0.51, xend = 0.54, y = 0.5, yend = 0.5)) +
    geom_segment(aes(x = 0.51, xend = 0.54, y = 0.5, yend = 0.2)) +
    geom_text(aes(x = 0.55, y = 0.8, label = "creaks"), size = 4.939, family = "Helvetica", fontface = "bold", color = condition.colors[1], hjust = 0) +
    geom_text(aes(x = 0.55, y = 0.5, label = "talks"), size = 4.939, family = "Helvetica", fontface = "bold", color = condition.colors[2], hjust = 0) +
    geom_text(aes(x = 0.55, y = 0.2, label = "blossoms"), size = 4.939, family = "Helvetica", fontface = "bold", color = condition.colors[3], hjust = 0) +
    geom_text(aes(x = 0.675, y = 0.8, label = 'in the wind"'), size = 4.939, family = "Helvetica", hjust = 0) +
    geom_text(aes(x = 0.643, y = 0.5, label = 'to the girl"'), size = 4.939, family = "Helvetica", hjust = 0) +
    geom_text(aes(x = 0.730, y = 0.2, label = 'above the girl"'), size = 4.939, family = "Helvetica", hjust = 0) +
    draw_plot(get_legend(bars$N400.verb + theme(legend.position = "right", legend.title = element_blank())), x = 0.65, y = 0.5, vjust = 0.48)

# Figure 1: Verb-Related N400 Effects
plot_grid(stim, bars$N400.verb,
          nrow = 2, rel_heights = c(0.2, 1), labels = c("A", "B", NULL), label_fontfamily = "Helvetica") %>%
    ggsave(filename = "EEG/figures/N400_verb_lag.pdf", width = 18, height = 22, units = "cm")

# Figure 1: Picture-Related N400 Effects
plot_grid(stim, bars$N400.pict,
          nrow = 2, rel_heights = c(0.2, 1), labels = c("A", "B", NULL), label_fontfamily = "Helvetica") %>%
    ggsave(filename = "EEG/figures/N400_pict_lag.pdf", width = 18, height = 22, units = "cm")

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
quick_docx(huxt_word, file = "EEG/tables/lmm_table_appendixLag.docx", open = FALSE)


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
mod.N400.verb.covs <- lmer_alt(N400.verb ~ (clozeprob + plausibility + metaphoricity + imageability) + semantics*context
                               + (semantics*context||participant) + (semantics*context||item), data = a1,
                               control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Model output
anova(mod.N400.verb.covs) # Semantics remains significant, no other effects
anova(mod.N400.verb, mod.N400.verb.covs) # Takes a while because models need to be refitted with ML; p = 0.765
emmeans(mod.N400.verb.covs, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts
# MCI - intuitive remains significant (still ~ -0.5 Microvolts, p = 0.004)

# rePCA(mod.N400.verb.covs)
# mod.N400.verb.covs.alt <- lmer_alt(N400.verb ~ (clozeprob + plausibility + metaphoricity + imageability) + semantics*context
#                                + (context||participant) + (semantics*context||item),
#                                data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# # refit mod.N400.verb.alt with the same random structure so model comparison is possible
# mod.N400.verb.alt <- lmer_alt(N400.verb ~ semantics*context
#                                    + (context||participant) + (semantics*context||item),
#                                    data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#
# # Model output
# anova(mod.N400.verb.covs.alt) # MCI - correct remains significant, no other effects
# anova(mod.N400.verb.alt, mod.N400.verb.covs.alt) # Takes a while because models need to be refitted with ML - n.s.
# emmeans(mod.N400.verb.covs.alt, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts
#
# # And another LMM including interactions with each covariate
# mod.N400.verb.covs.ia <- lmer_alt(N400.verb ~ (clozeprob + plausibility + metaphoricity + imageability) * semantics*context
#                                   + (semantics*context||participant) + (semantics*context||item),
#                                   data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# 
# # Model summary
# summary(mod.N400.verb.covs.ia)
# anova(mod.N400.verb, mod.N400.verb.covs.ia) # n.s.
#
# # Set up a model with covariates only
# mod.N400.verb.covs.only <- lmer_alt(N400.verb ~ (clozeprob + plausibility + metaphoricity + imageability)
#                                + (semantics*context||participant) + (semantics*context||item),
#                                data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# summary(mod.N400.verb.covs.only)



# Full system specs and package versions
sessionInfo()

