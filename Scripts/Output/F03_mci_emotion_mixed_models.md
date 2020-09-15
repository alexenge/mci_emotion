F03\_mci\_emotion\_mixed\_models.R
================
kirstenstark
2020-09-15

``` r
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
```

    ##     [,1]
    ## neu   -1
    ## neg    1

``` r
contrasts(a1$context) <- ginv(contrasts.context)

# Define simple contrast coding for semantics (violation - intuitive, mci - intuitive)
#     H0(Intercept): (mu1+mu2+mu3)/3 = 0 <-> mu1+mu2+mu3 = 0
#     H0(Slope1): -1*mu1 +1*mu2 + 0*mu3 = 0
#     H0(Slope2): -1*mu1 +0*mu2 + 1*mu3 = 0
#     with mu1 = mean of intuitive concepts, mu2 = mean of violations, mu3 = mean of MCIs
t(contrasts.semantics <- t(cbind(c("int" = -1, "vio" = 1, "mci" = 0),
                                 c("int" = -1, "vio" = 0, "mci" = 1))))
```

    ##     [,1] [,2]
    ## int   -1   -1
    ## vio    1    0
    ## mci    0    1

``` r
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


# Create a list of models
models <- list("VALENCE" = mod.valence, "AROUSAL" = mod.aroursal, "N400.VERB" = mod.N400.verb, "N400.PICT" = mod.N400.pict)

# F-tests (type III tests)
(tests <- lapply(models, anova))
```

    ## $VALENCE
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##         Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## context 87.284  87.284     1 37.783  164.14 2.523e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $AROUSAL
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##         Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## context 51.538  51.538     1 37.677  83.292 4.402e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $N400.VERB
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                    Sum Sq Mean Sq NumDF   DenDF F value    Pr(>F)    
    ## semantics         290.562 145.281     2 101.879  8.2637 0.0004722 ***
    ## context             0.355   0.355     1  24.294  0.0202 0.8881097    
    ## semantics:context  42.207  21.104     2  71.656  1.2004 0.3070540    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $N400.PICT
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                    Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
    ## semantics          23.533  11.766     2 37.017  0.7277 0.48982  
    ## context             0.085   0.085     1 44.147  0.0053 0.94243  
    ## semantics:context 125.895  62.947     2 52.128  3.8929 0.02656 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## PLANNED FOLLOW-UP CONTRASTS ## -----------------------------------------------------------------

# Allow emmeans to use Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics <- lapply(models[-1:-2],function(x){
    emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $N400.VERB
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0288 0.118 90.5   -0.299    0.241 -0.243  1.0000 
    ##  mci - int  -0.3847 0.102 92.4   -0.617   -0.152 -3.771  0.0006 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.PICT
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.073 0.129 35.5   -0.228    0.374  0.567  1.0000 
    ##  mci - int   -0.121 0.138 37.7   -0.444    0.202 -0.872  0.7774 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Follow-up contrasts for the main effect of context
(means.context <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $VALENCE
    ##  contrast  estimate   SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu    -1.41 0.11 37.8    -1.63    -1.19 -12.812 <.0001 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $AROUSAL
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu     1.04 0.114 37.7    0.809     1.27 9.126   <.0001 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $N400.VERB
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0135 0.0946 24.3   -0.182    0.209 0.142   0.8881 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $N400.PICT
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu -0.00706 0.0972 44.1   -0.203    0.189 -0.073  0.9424 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95

``` r
# Follow-up contrasts for semantics within each contexts
(means.nested <- lapply(models[-1:-2], function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## $N400.VERB
    ## context = neu:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   -0.174 0.163 182   -0.543    0.196 -1.062  0.5791 
    ##  mci - int   -0.531 0.151 117   -0.874   -0.188 -3.517  0.0012 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.116 0.163 182   -0.253    0.485  0.711  0.9560 
    ##  mci - int   -0.238 0.151 117   -0.581    0.104 -1.579  0.2341 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.PICT
    ## context = neu:
    ##  contrast  estimate    SE   df lower.CL  upper.CL t.ratio p.value
    ##  vio - int  -0.0363 0.163 34.4   -0.419  0.346616 -0.222  1.0000 
    ##  mci - int  -0.4060 0.175 47.1   -0.811 -0.000838 -2.320  0.0494 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE   df lower.CL  upper.CL t.ratio p.value
    ##  vio - int   0.1823 0.178 43.2   -0.230  0.594864  1.026  0.6209 
    ##  mci - int   0.1645 0.170 35.8   -0.233  0.561694  0.969  0.6778 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Backup results
save(models, tests, means.semantics, means.context, means.nested, file = "EEG/export/stats.RData")


### MCI EMO MIXED MODELS SCRIPT  - APPENDIX (P600) ### --------------------------------------------

## SETUP ## ---------------------------------------------------------------------------------------

# Load preprocessed data
a1_app <- readRDS("EEG/export/a1_appendix.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1_app <- na.omit(a1_app[!a1_app$error,])

# Define simple contrast coding for context emotionality (negative - neutral)
# HO(Intercept): (mu1+mu2)/2 = 0 <-> mu1+mu2 = 0
# H0(Slope): -mu1 + mu2 = 0
# with mu1 = mean of the neutral contexts and mu2 = mean of the neg contexts
t(contrasts.context <- t(cbind(c("neu" = -1, "neg" = 1))))
```

    ##     [,1]
    ## neu   -1
    ## neg    1

``` r
contrasts(a1_app$context) <- ginv(contrasts.context)

# Define simple contrast coding for semantics (violation - intuitive, mci - intuitive)
# H0(Intercept): (mu1+mu2+mu3)/3 = 0 <-> mu1+mu2+mu3 = 0
# H0(Slope1): -1*mu1 +1*mu2 + 0*mu3 = 0
# H0(Slope2): -1*mu1 +0*mu2 + 1*mu3 = 0
# with mu1 = mean of intuitive concepts, mu2 = mean of violations, mu3 = mean of MCIs
t(contrasts.semantics <- t(cbind(c("int" = -1, "vio" = 1, "mci" = 0),
                                 c("int" = -1, "vio" = 0, "mci" = 1))))
```

    ##     [,1] [,2]
    ## int   -1   -1
    ## vio    1    0
    ## mci    0    1

``` r
contrasts(a1_app$semantics) <- ginv(contrasts.semantics)

## LINEAR MIXED-EFFECTS MODELS ## -----------------------------------------------------------------

# LMM for verb-related P600 - 500-900 ms, same ROI as for the N400 analyses
mod.P600.verb <- lmer_alt(P600_2.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                            data = a1_app, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Create a list of models
models_app <- list( "P600.VERB" = mod.P600.verb)

# F-tests (type III tests)
(tests_app <- lapply(models_app, anova))
```

    ## $P600.VERB
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF   DenDF F value Pr(>F)
    ## semantics         44.183 22.0916     2 103.585  1.1044 0.3353
    ## context            0.758  0.7577     1  29.625  0.0379 0.8470
    ## semantics:context 28.736 14.3682     2  62.211  0.7183 0.4916

``` r
## PLANNED FOLLOW-UP CONTRASTS ## -----------------------------------------------------------------

# Allow emmeans to compute Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics_app <- lapply(models_app,function(x){
  emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $P600.VERB
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.1087 0.109 86.1   -0.139    0.356  1.001  0.6391 
    ##  mci - int  -0.0762 0.130 83.8   -0.373    0.221 -0.586  1.0000 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Follow-up contrasts for the main effect of context
(means.context_app <- lapply(models_app, function(x){
  emmeans(x, trt.vs.ctrl ~ context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $P600.VERB
    ##  contrast  estimate   SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0234 0.12 29.6   -0.222    0.269 0.195   0.8470 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95

``` r
# Follow-up contrasts for semantics within each contexts
(means.nested_app <- lapply(models_app, function(x){
  emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## $P600.VERB
    ## context = neu:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0184 0.154 187   -0.367    0.330 -0.119  1.0000 
    ##  mci - int  -0.1114 0.170 136   -0.497    0.274 -0.654  1.0000 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.2359 0.154 186   -0.112    0.584  1.533  0.2542 
    ##  mci - int  -0.0411 0.170 134   -0.426    0.344 -0.242  1.0000 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Backup results
save(models_app, tests, means.semantics_app, means.context_app, means.nested_app, file = "EEG/export/stats_appendix.RData")




# Full system specs and package versions
sessionInfo()
```

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.6
    ## 
    ## Matrix products: default
    ## BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] de_DE.UTF-8/de_DE.UTF-8/de_DE.UTF-8/C/de_DE.UTF-8/de_DE.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices datasets  utils     methods   base     
    ## 
    ## other attached packages:
    ## [1] emmeans_1.4.8  afex_0.27-2    lmerTest_3.1-2 lme4_1.1-23    Matrix_1.2-18 
    ## [6] MASS_7.3-51.6 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] statmod_1.4.34      xfun_0.16           tidyselect_1.1.0    purrr_0.3.4        
    ##  [5] reshape2_1.4.4      splines_4.0.2       haven_2.3.1         lattice_0.20-41    
    ##  [9] carData_3.0-4       colorspace_1.4-1    vctrs_0.3.2         generics_0.0.2     
    ## [13] htmltools_0.5.0     yaml_2.2.1          rlang_0.4.7         pillar_1.4.6       
    ## [17] nloptr_1.2.2.2      foreign_0.8-80      glue_1.4.1          readxl_1.3.1       
    ## [21] lifecycle_0.2.0     plyr_1.8.6          stringr_1.4.0       munsell_0.5.0      
    ## [25] gtable_0.3.0        cellranger_1.1.0    zip_2.1.1           mvtnorm_1.1-1      
    ## [29] evaluate_0.14       knitr_1.29          rio_0.5.16          forcats_0.5.0      
    ## [33] curl_4.3            parallel_4.0.2      highr_0.8           Rcpp_1.0.5         
    ## [37] xtable_1.8-4        renv_0.12.0         scales_1.1.1        abind_1.4-5        
    ## [41] digest_0.6.25       ggplot2_3.3.2       hms_0.5.3           stringi_1.4.6      
    ## [45] openxlsx_4.1.5      dplyr_1.0.0         numDeriv_2016.8-1.1 grid_4.0.2         
    ## [49] tools_4.0.2         magrittr_1.5        tibble_3.0.3        crayon_1.3.4       
    ## [53] car_3.0-8           pkgconfig_2.0.3     ellipsis_0.3.1      data.table_1.13.0  
    ## [57] estimability_1.3    rmarkdown_2.3       minqa_1.2.4         rstudioapi_0.11    
    ## [61] R6_2.4.1            boot_1.3-25         nlme_3.1-148        compiler_4.0.2
