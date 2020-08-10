F02\_mci\_emotion\_mixed\_models\_appendix.R
================
kirstenstark
2020-08-10

``` r
### MCI EMO MIXED MODELS SCRIPT  - APPENDIX (P600) ###

# Computes a linear mixed-effects regression model predicting verb-related P600 amplitudes with 
# simple contrast coding for the fixed effects of semantics and emotional context. Thus in the
# model, the estimate of the intercept is the grand mean, while the estimates of the slopes 
# contrast "treatment" levels to their respective reference levels (semantics: violation - intuitive, 
# mci - intuitive; emotional context (negative - neutral). The maximal random effects structure is 
# used with all by-participant and by-item random slopes and random intercepts. Correlations between 
# random effects are removed if the model fails two converge with two different numerical optimizers. 
# Follow-up contrasts are computed for the main effects and the effects of semantics separately 
# within each type of emotional context.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(MASS)         # version 7.3-51.6
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(lme4)         # version 1.1-23
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

``` r
library(lmerTest)     # version 3.1-2
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(afex)         # version 0.27-2
```

    ## ************
    ## Welcome to afex. For support visit: http://afex.singmann.science/

    ## - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
    ## - Methods for calculating p-values with mixed(): 'KR', 'S', 'LRT', and 'PB'
    ## - 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests
    ## - NEWS: library('emmeans') now needs to be called explicitly!
    ## - Get and set global package options with: afex_options()
    ## - Set orthogonal sum-to-zero contrasts globally: set_sum_contrasts()
    ## - For example analyses see: browseVignettes("afex")
    ## ************

    ## 
    ## Attaching package: 'afex'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

``` r
library(emmeans)      # version 1.4.8

# Load preprocessed data
a1 <- readRDS("EEG/export/a1_appendix.RDS")

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

# LMM for verb-related P600 - time window as in the old manuscript
mod.P600_1.verb <- lmer_alt(P600_1.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# (converged after changing the optimizer + removing correlations between REs)

# LMM for verb-related P600 - time window as in the old manuscript
mod.P600_2.verb <- lmer_alt(P600_2.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                        data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Create a list of models
models <- list("P600.VERB - 1" = mod.P600_1.verb, "P600.VERB - 2" = mod.P600_2.verb)

# F-tests (type III tests)
(tests <- lapply(models, anova))
```

    ## $`P600.VERB - 1`
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF   DenDF F value Pr(>F)
    ## semantics         96.609  48.305     2 112.691  2.2723 0.1078
    ## context            3.108   3.108     1  26.036  0.1462 0.7053
    ## semantics:context 73.140  36.570     2  65.481  1.7203 0.1870
    ## 
    ## $`P600.VERB - 2`
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
(means.semantics <- lapply(models,function(x){
    emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $`P600.VERB - 1`
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.0695 0.119 81.6   -0.203   0.3416  0.583  1.0000 
    ##  mci - int  -0.2188 0.132 80.3   -0.521   0.0834 -1.654  0.2041 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $`P600.VERB - 2`
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
(means.context <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $`P600.VERB - 1`
    ##  contrast  estimate    SE df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0467 0.122 26   -0.204    0.298 0.382   0.7053 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $`P600.VERB - 2`
    ##  contrast  estimate   SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0234 0.12 29.6   -0.222    0.269 0.195   0.8470 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95

``` r
# Follow-up contrasts for semantics within each contexts
(means.nested <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## $`P600.VERB - 1`
    ## context = neu:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   -0.146 0.169 176  -0.5280    0.236 -0.864  0.7778 
    ##  mci - int   -0.267 0.175 131  -0.6632    0.129 -1.527  0.2583 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.285 0.169 176  -0.0966    0.667  1.688  0.1862 
    ##  mci - int   -0.171 0.174 129  -0.5661    0.225 -0.978  0.6593 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $`P600.VERB - 2`
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
save(models, tests, means.semantics, means.context, means.nested, file = "EEG/export/stats_appendix.RData")

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
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] emmeans_1.4.8      afex_0.27-2        lmerTest_3.1-2    
    ##  [4] lme4_1.1-23        Matrix_1.2-18      MASS_7.3-51.6     
    ##  [7] cowplot_1.0.0      ggplot2_3.3.2      eeguana_0.1.4.9000
    ## [10] tidyr_1.1.0        dplyr_1.0.0        Rmisc_1.5         
    ## [13] plyr_1.8.6         lattice_0.20-41    huxtable_5.0.0    
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] minqa_1.2.4          colorspace_1.4-1     ellipsis_0.3.1      
    ##   [4] rio_0.5.16           estimability_1.3     flextable_0.5.10    
    ##   [7] base64enc_0.1-3      rstudioapi_0.11      farver_2.0.3        
    ##  [10] listenv_0.8.0        R.matlab_3.6.2       fansi_0.4.1         
    ##  [13] mvtnorm_1.1-1        xml2_1.3.2           codetools_0.2-16    
    ##  [16] splines_4.0.2        R.methodsS3_1.8.0    knitr_1.29          
    ##  [19] eegUtils_0.5.0       jsonlite_1.7.0       nloptr_1.2.2.2      
    ##  [22] packrat_0.5.0        R.oo_1.23.0          shinydashboard_0.7.1
    ##  [25] shiny_1.5.0          compiler_4.0.2       httr_1.4.1          
    ##  [28] assertthat_0.2.1     fastmap_1.0.1        lazyeval_0.2.2      
    ##  [31] cli_2.0.2            later_1.1.0.1        htmltools_0.5.0     
    ##  [34] tools_4.0.2          coda_0.19-3          gtable_0.3.0        
    ##  [37] glue_1.4.1           reshape2_1.4.4       Rcpp_1.0.5          
    ##  [40] carData_3.0-4        cellranger_1.1.0     vctrs_0.3.2         
    ##  [43] nlme_3.1-148         xfun_0.15            stringr_1.4.0       
    ##  [46] globals_0.12.5       openxlsx_4.1.5       mime_0.9            
    ##  [49] miniUI_0.1.1.1       lifecycle_0.2.0      edfReader_1.2.1     
    ##  [52] statmod_1.4.34       future_1.18.0        scales_1.1.1        
    ##  [55] hms_0.5.3            promises_1.1.1       parallel_4.0.2      
    ##  [58] RColorBrewer_1.1-2   yaml_2.2.1           curl_4.3            
    ##  [61] gridExtra_2.3        gdtools_0.2.2        stringi_1.4.6       
    ##  [64] highr_0.8            boot_1.3-25          zip_2.0.4           
    ##  [67] commonmark_1.7       systemfonts_0.2.3    rlang_0.4.7         
    ##  [70] pkgconfig_2.0.3      matrixStats_0.56.0   pracma_2.2.9        
    ##  [73] evaluate_0.14        purrr_0.3.4          labeling_0.3        
    ##  [76] htmlwidgets_1.5.1    tidyselect_1.1.0     magrittr_1.5        
    ##  [79] R6_2.4.1             generics_0.0.2       ini_0.3.1           
    ##  [82] withr_2.2.0          pillar_1.4.6         haven_2.3.1         
    ##  [85] foreign_0.8-80       mgcv_1.8-31          abind_1.4-5         
    ##  [88] tibble_3.0.3         future.apply_1.6.0   crayon_1.3.4        
    ##  [91] car_3.0-8            utf8_1.1.4           uuid_0.1-4          
    ##  [94] plotly_4.9.2.1       rmarkdown_2.3        officer_0.3.12      
    ##  [97] viridis_0.5.1        grid_4.0.2           readxl_1.3.1        
    ## [100] data.table_1.12.8    forcats_0.5.0        digest_0.6.25       
    ## [103] xtable_1.8-4         httpuv_1.5.4         numDeriv_2016.8-1.1 
    ## [106] R.utils_2.9.2        signal_0.7-6         munsell_0.5.0       
    ## [109] viridisLite_0.3.0
