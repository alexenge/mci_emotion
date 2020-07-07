F02\_mci\_emotion\_mixed\_models.R
================
alexander
2020-07-07

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
    # HO(Intercept): (mu1+mu2)/2 = 0 <-> mu1+mu2 = 0
    # H0(Slope): -mu1 + mu2 = 0
    # with mu1 = mean of the neutral contexts and mu2 = mean of the neg contexts
t(contrasts.context <- t(cbind(c("neu" = -1, "neg" = 1))))
```

    ##     [,1]
    ## neu   -1
    ## neg    1

``` r
contrasts(a1$context) <- ginv(contrasts.context)

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
contrasts(a1$semantics) <- ginv(contrasts.semantics)

## LINEAR MIXED-EFFECTS MODELS ## -----------------------------------------------------------------

# LMM for valence ratings (converged on first attempt)
mod.valence <- lmer(ValenzResp ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                 data = a1, control = lmerControl(calc.derivs = FALSE))
```

    ## Warning: Model failed to converge with 1 negative eigenvalue: -1.5e+00

``` r
# LMM for arousal ratings (converged after changing the optimizer + removing correlations between REs)
mod.aroursal <- lmer_alt(ArousalResp ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                     data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related N400 (converged on first attempt)
mod.N400.verb <- lmer(N400.verb ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                 data = a1, control = lmerControl(calc.derivs = FALSE))

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
    ##                   Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
    ## semantics          0.007   0.004     2 60.038   0.0092    0.9909    
    ## context           64.836  64.836     1 37.920 164.1355 2.376e-15 ***
    ## semantics:context  0.012   0.006     2 60.466   0.0157    0.9844    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $AROUSAL
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## semantics          0.131   0.066     2 65.904  0.1209    0.8863    
    ## context           45.860  45.860     1 37.925 84.5358 3.405e-11 ***
    ## semantics:context  0.054   0.027     2 87.852  0.0495    0.9517    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $N400.VERB
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                    Sum Sq Mean Sq NumDF  DenDF F value   Pr(>F)   
    ## semantics         247.963 123.981     2 63.114  7.0832 0.001678 **
    ## context             0.317   0.317     1 34.979  0.0181 0.893730   
    ## semantics:context  43.707  21.853     2 69.610  1.2485 0.293276   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $N400.PICT
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                    Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
    ## semantics          23.533  11.766     2 37.017  0.7277 0.48981  
    ## context             0.085   0.085     1 44.147  0.0053 0.94243  
    ## semantics:context 125.895  62.948     2 52.128  3.8929 0.02656 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## PLANNED FOLLOW-UP CONTRASTS ## -----------------------------------------------------------------

# Allow emmeans to compute Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics <- lapply(models,function(x){
    emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $VALENCE
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  0.00939 0.0693 60.2   -0.150    0.169 0.135   1.0000 
    ##  mci - int  0.00477 0.0676 60.0   -0.151    0.160 0.071   1.0000 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $AROUSAL
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  0.02545 0.0518 60.5  -0.0935    0.144 0.492   1.0000 
    ##  mci - int  0.00209 0.0449 60.1  -0.1012    0.105 0.046   1.0000 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.VERB
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0288 0.131 61.2   -0.330    0.272 -0.220  1.0000 
    ##  mci - int  -0.3830 0.114 66.0   -0.644   -0.123 -3.373  0.0025 
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
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $VALENCE
    ##  contrast  estimate   SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu    -1.41 0.11 37.9    -1.63    -1.19 -12.812 <.0001 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $AROUSAL
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu     1.04 0.113 37.9    0.813     1.27 9.194   <.0001 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $N400.VERB
    ##  contrast  estimate    SE df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0139 0.103 35   -0.195    0.223 0.135   0.8937 
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
(means.nested <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## $VALENCE
    ## context = neu:
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int 0.014482 0.0960 60.3   -0.206    0.235 0.151   1.0000 
    ##  mci - int 0.000146 0.0919 60.3   -0.211    0.211 0.002   1.0000 
    ## 
    ## context = neg:
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int 0.004289 0.0729 59.9   -0.163    0.172 0.059   1.0000 
    ##  mci - int 0.009390 0.0754 60.1   -0.164    0.183 0.125   1.0000 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $AROUSAL
    ## context = neu:
    ##  contrast  estimate     SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.0221 0.0658 115   -0.127    0.172  0.336  1.0000 
    ##  mci - int   0.0148 0.0623 121   -0.127    0.156  0.238  1.0000 
    ## 
    ## context = neg:
    ##  contrast  estimate     SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.0288 0.0658 115   -0.121    0.178  0.437  1.0000 
    ##  mci - int  -0.0107 0.0623 121   -0.152    0.131 -0.171  1.0000 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.VERB
    ## context = neu:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   -0.170 0.193 63.3   -0.613    0.273 -0.882  0.7623 
    ##  mci - int   -0.531 0.156 97.9   -0.887   -0.175 -3.395  0.0020 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.113 0.157 69.3   -0.248    0.473  0.715  0.9541 
    ##  mci - int   -0.235 0.160 70.4   -0.601    0.130 -1.474  0.2898 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.PICT
    ## context = neu:
    ##  contrast  estimate    SE   df lower.CL  upper.CL t.ratio p.value
    ##  vio - int  -0.0363 0.163 34.4   -0.419  0.346617 -0.222  1.0000 
    ##  mci - int  -0.4060 0.175 47.1   -0.811 -0.000838 -2.320  0.0494 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE   df lower.CL  upper.CL t.ratio p.value
    ##  vio - int   0.1823 0.178 43.2   -0.230  0.594863  1.026  0.6209 
    ##  mci - int   0.1645 0.170 35.8   -0.233  0.561694  0.969  0.6778 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Backup results
save(models, tests, means.semantics, means.context, means.nested, file = "EEG/export/stats.RData")

# Full system specs and package versions
sessionInfo()
```

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.5
    ## 
    ## Matrix products: default
    ## BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] emmeans_1.4.8  afex_0.27-2    lmerTest_3.1-2 lme4_1.1-23    Matrix_1.2-18  MASS_7.3-51.6 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-148        matrixStats_0.56.0  RColorBrewer_1.1-2  httr_1.4.1          numDeriv_2016.8-1.1 tools_4.0.2         R6_2.4.1           
    ##  [8] lazyeval_0.2.2      mgcv_1.8-31         colorspace_1.4-1    tidyselect_1.1.0    curl_4.3            compiler_4.0.2      eegUtils_0.5.0.9000
    ## [15] plotly_4.9.2.1      scales_1.1.1        mvtnorm_1.1-1       stringr_1.4.0       digest_0.6.25       foreign_0.8-80      minqa_1.2.4        
    ## [22] rmarkdown_2.3       R.utils_2.9.2       rio_0.5.16          ini_0.3.1           pkgconfig_2.0.3     htmltools_0.5.0     highr_0.8          
    ## [29] fastmap_1.0.1       htmlwidgets_1.5.1   Rmisc_1.5           rlang_0.4.6         readxl_1.3.1        rstudioapi_0.11     shiny_1.5.0        
    ## [36] generics_0.0.2      jsonlite_1.7.0      dplyr_1.0.0         zip_2.0.4           car_3.0-8           R.oo_1.23.0         magrittr_1.5       
    ## [43] R.matlab_3.6.2      Rcpp_1.0.5          munsell_0.5.0       abind_1.4-5         lifecycle_0.2.0     R.methodsS3_1.8.0   stringi_1.4.6      
    ## [50] yaml_2.2.1          carData_3.0-4       plyr_1.8.6          grid_4.0.2          parallel_4.0.2      listenv_0.8.0       promises_1.1.1     
    ## [57] forcats_0.5.0       crayon_1.3.4        miniUI_0.1.1.1      lattice_0.20-41     haven_2.3.1         splines_4.0.2       hms_0.5.3          
    ## [64] knitr_1.29          pillar_1.4.4        boot_1.3-25         estimability_1.3    future.apply_1.6.0  reshape2_1.4.4      codetools_0.2-16   
    ## [71] glue_1.4.1          evaluate_0.14       data.table_1.12.8   vctrs_0.3.1         nloptr_1.2.2.2      httpuv_1.5.4        cellranger_1.1.0   
    ## [78] gtable_0.3.0        purrr_0.3.4         tidyr_1.1.0         future_1.17.0       ggplot2_3.3.2       xfun_0.15           openxlsx_4.1.5     
    ## [85] mime_0.9            xtable_1.8-4        pracma_2.2.9        coda_0.19-3         later_1.1.0.1       viridisLite_0.3.0   signal_0.7-6       
    ## [92] tibble_3.0.1        tinytex_0.24        globals_0.12.5      statmod_1.4.34      ellipsis_0.3.1
