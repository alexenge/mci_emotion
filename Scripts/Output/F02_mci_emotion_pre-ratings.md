F02\_mci\_emotion\_pre-ratings.R
================
kirstenstark
2020-09-15

``` r
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
```

    ## `summarise()` regrouping output by 'VP' (override with `.groups` argument)

``` r
## ANOVAs AND PAIRWISE TESTS ## -----------------------------------------------------------------------------------
# semantics is a within subjects factor; data are fully balances
# cloze probability
summary(anova.cloze <- aov(clozeprob ~ semantics + Error(VP/semantics), data = means.summary))
```

    ## 
    ## Error: VP
    ##           Df Sum Sq Mean Sq F value Pr(>F)
    ## Residuals 19  7.689  0.4047               
    ## 
    ## Error: VP:semantics
    ##           Df Sum Sq Mean Sq F value   Pr(>F)    
    ## semantics  2  16.74   8.369   17.66 3.78e-06 ***
    ## Residuals 38  18.01   0.474                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# mean squared error of the effect
mean(anova.cloze$`VP:semantics`$residuals^2)
```

    ## [1] 0.450286

``` r
# # this is identical to
# (anova.cloze <- ez::ezANOVA(data = means.summary, dv = clozeprob, wid = VP, 
#              within = .(semantics), detailed = TRUE, return_aov = TRUE, type = 1))

# plausibility
summary(anova.plausibility <- aov(plausibility ~ semantics + Error(VP/semantics), data = means.summary))
```

    ## 
    ## Error: VP
    ##           Df Sum Sq Mean Sq F value Pr(>F)
    ## Residuals 19  5.338   0.281               
    ## 
    ## Error: VP:semantics
    ##           Df Sum Sq Mean Sq F value   Pr(>F)    
    ## semantics  2  13.55   6.777   10.74 0.000201 ***
    ## Residuals 38  23.98   0.631                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# mean squared error of the effect
mean(anova.plausibility$`VP:semantics`$residuals^2)
```

    ## [1] 0.5994464

``` r
# imageability
summary(anova.imageability <- aov(imageability ~ semantics + Error(VP/semantics), data = means.summary))
```

    ## 
    ## Error: VP
    ##           Df Sum Sq Mean Sq F value Pr(>F)
    ## Residuals 19  6.643  0.3497               
    ## 
    ## Error: VP:semantics
    ##           Df Sum Sq Mean Sq F value   Pr(>F)    
    ## semantics  2  12.43   6.215   14.61 1.96e-05 ***
    ## Residuals 38  16.16   0.425                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mean(anova.imageability$`VP:semantics`$residuals^2)
```

    ## [1] 0.4041053

``` r
# metaphoricity
summary(anova.metaphoricity <- aov(metaphoricity ~ semantics + Error(VP/semantics), data = means.summary))
```

    ## 
    ## Error: VP
    ##           Df Sum Sq Mean Sq F value Pr(>F)
    ## Residuals 19  62.24   3.276               
    ## 
    ## Error: VP:semantics
    ##           Df Sum Sq Mean Sq F value   Pr(>F)    
    ## semantics  2  5.142  2.5712   8.988 0.000636 ***
    ## Residuals 38 10.870  0.2861                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mean(anova.metaphoricity$`VP:semantics`$residuals^2)
```

    ## [1] 0.2717558

``` r
## PAIRWISE TESTS ## -------------------------------------------------------------------------------------------------
# cloze probability
(pairwise.clozeprob <- anova.cloze %>% emmeans::emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))
```

    ## Note: re-fitting model with sum-to-zero contrasts

    ## $emmeans
    ##  semantics emmean   SE   df lower.CL upper.CL
    ##  int         3.47 0.15 56.7     3.10     3.84
    ##  vio         2.65 0.15 56.7     2.28     3.02
    ##  mci         2.19 0.15 56.7     1.82     2.56
    ## 
    ## Warning: EMMs are biased unless design is perfectly balanced 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## 
    ## $contrasts
    ##  contrast  estimate    SE df t.ratio p.value
    ##  int - vio    0.814 0.218 38 3.739   0.0012 
    ##  int - mci    1.278 0.218 38 5.870   <.0001 
    ##  vio - mci    0.464 0.218 38 2.131   0.0396 
    ## 
    ## P value adjustment: holm method for 3 tests

``` r
# plausibility
(pairwise.plausibility <- anova.plausibility %>% emmeans::emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))
```

    ## Note: re-fitting model with sum-to-zero contrasts

    ## $emmeans
    ##  semantics emmean   SE   df lower.CL upper.CL
    ##  int         2.84 0.16 51.7     2.44     3.24
    ##  vio         2.13 0.16 51.7     1.73     2.53
    ##  mci         1.69 0.16 51.7     1.29     2.08
    ## 
    ## Warning: EMMs are biased unless design is perfectly balanced 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## 
    ## $contrasts
    ##  contrast  estimate    SE df t.ratio p.value
    ##  int - vio    0.710 0.251 38 2.826   0.0150 
    ##  int - mci    1.154 0.251 38 4.594   0.0001 
    ##  vio - mci    0.444 0.251 38 1.769   0.0850 
    ## 
    ## P value adjustment: holm method for 3 tests

``` r
# imageability
(pairwise.imageability <- anova.imageability %>% emmeans::emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))
```

    ## Note: re-fitting model with sum-to-zero contrasts

    ## $emmeans
    ##  semantics emmean    SE   df lower.CL upper.CL
    ##  int         3.65 0.141 56.5     3.30     4.00
    ##  vio         2.99 0.141 56.5     2.64     3.34
    ##  mci         2.54 0.141 56.5     2.19     2.89
    ## 
    ## Warning: EMMs are biased unless design is perfectly balanced 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## 
    ## $contrasts
    ##  contrast  estimate    SE df t.ratio p.value
    ##  int - vio    0.660 0.206 38 3.199   0.0056 
    ##  int - mci    1.108 0.206 38 5.373   <.0001 
    ##  vio - mci    0.448 0.206 38 2.174   0.0360 
    ## 
    ## P value adjustment: holm method for 3 tests

``` r
# metaphoricity
(pairwise.metaphoricity <- anova.metaphoricity %>% emmeans::emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))
```

    ## Note: re-fitting model with sum-to-zero contrasts

    ## $emmeans
    ##  semantics emmean    SE   df lower.CL upper.CL
    ##  int         2.30 0.253 25.8     1.65     2.95
    ##  vio         2.49 0.253 25.8     1.84     3.13
    ##  mci         2.99 0.253 25.8     2.34     3.64
    ## 
    ## Warning: EMMs are biased unless design is perfectly balanced 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## 
    ## $contrasts
    ##  contrast  estimate    SE df t.ratio p.value
    ##  int - vio   -0.189 0.169 38 -1.115  0.2720 
    ##  int - mci   -0.693 0.169 38 -4.100  0.0006 
    ##  vio - mci   -0.505 0.169 38 -2.985  0.0099 
    ## 
    ## P value adjustment: holm method for 3 tests

``` r
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
    ## [1] dplyr_1.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] nlme_3.1-148        matrixStats_0.56.0  RColorBrewer_1.1-2  httr_1.4.2         
    ##   [5] numDeriv_2016.8-1.1 tools_4.0.2         R6_2.4.1            afex_0.27-2        
    ##   [9] lazyeval_0.2.2      mgcv_1.8-31         colorspace_1.4-1    tidyselect_1.1.0   
    ##  [13] emmeans_1.4.8       curl_4.3            compiler_4.0.2      cli_2.0.2          
    ##  [17] eegUtils_0.5.0.9000 plotly_4.9.2.1      scales_1.1.1        mvtnorm_1.1-1      
    ##  [21] readr_1.3.1         stringr_1.4.0       digest_0.6.25       foreign_0.8-80     
    ##  [25] minqa_1.2.4         rmarkdown_2.3       R.utils_2.9.2       rio_0.5.16         
    ##  [29] ini_0.3.1           pkgconfig_2.0.3     htmltools_0.5.0     lme4_1.1-23        
    ##  [33] highr_0.8           fastmap_1.0.1       htmlwidgets_1.5.1   Rmisc_1.5          
    ##  [37] rlang_0.4.7         readxl_1.3.1        rstudioapi_0.11     shiny_1.5.0        
    ##  [41] generics_0.0.2      jsonlite_1.7.0      zip_2.1.1           car_3.0-8          
    ##  [45] R.oo_1.23.0         magrittr_1.5        R.matlab_3.6.2      Matrix_1.2-18      
    ##  [49] fansi_0.4.1         Rcpp_1.0.5          munsell_0.5.0       abind_1.4-5        
    ##  [53] lifecycle_0.2.0     R.methodsS3_1.8.0   yaml_2.2.1          stringi_1.4.6      
    ##  [57] carData_3.0-4       MASS_7.3-51.6       plyr_1.8.6          grid_4.0.2         
    ##  [61] parallel_4.0.2      listenv_0.8.0       promises_1.1.1      forcats_0.5.0      
    ##  [65] crayon_1.3.4        miniUI_0.1.1.1      lattice_0.20-41     haven_2.3.1        
    ##  [69] splines_4.0.2       hms_0.5.3           knitr_1.29          pillar_1.4.6       
    ##  [73] boot_1.3-25         estimability_1.3    future.apply_1.6.0  reshape2_1.4.4     
    ##  [77] codetools_0.2-16    glue_1.4.1          evaluate_0.14       data.table_1.13.0  
    ##  [81] renv_0.12.0         vctrs_0.3.2         nloptr_1.2.2.2      httpuv_1.5.4       
    ##  [85] cellranger_1.1.0    gtable_0.3.0        purrr_0.3.4         tidyr_1.1.0        
    ##  [89] assertthat_0.2.1    future_1.18.0       ggplot2_3.3.2       xfun_0.16          
    ##  [93] openxlsx_4.1.5      mime_0.9            xtable_1.8-4        pracma_2.2.9       
    ##  [97] later_1.1.0.1       viridisLite_0.3.0   signal_0.7-6        tibble_3.0.3       
    ## [101] lmerTest_3.1-2      globals_0.12.5      statmod_1.4.34      ellipsis_0.3.1
