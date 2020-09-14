F02\_mci\_emotion\_pre-ratings.R
================
kirstenstark
2020-09-14

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
(pairwise.clozeprob <- anova.cloze %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))
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
(pairwise.plausibility <- anova.plausibility %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))
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
(pairwise.imageability <- anova.imageability %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))
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
(pairwise.metaphoricity <- anova.metaphoricity %>% emmeans(specs = pairwise ~ semantics) %>% summary(adjust = "holm"))
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
    ## [1] dplyr_1.0.0    emmeans_1.4.8  afex_0.27-2    lmerTest_3.1-2 lme4_1.1-23   
    ## [6] Matrix_1.2-18  MASS_7.3-51.6 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.5          mvtnorm_1.1-1       lattice_0.20-41     assertthat_0.2.1   
    ##  [5] digest_0.6.25       R6_2.4.1            cellranger_1.1.0    plyr_1.8.6         
    ##  [9] evaluate_0.14       ggplot2_3.3.2       highr_0.8           pillar_1.4.6       
    ## [13] rlang_0.4.7         Rmisc_1.5           curl_4.3            readxl_1.3.1       
    ## [17] rstudioapi_0.11     minqa_1.2.4         data.table_1.13.0   car_3.0-8          
    ## [21] nloptr_1.2.2.2      rmarkdown_2.3       splines_4.0.2       statmod_1.4.34     
    ## [25] readr_1.3.1         stringr_1.4.0       foreign_0.8-80      munsell_0.5.0      
    ## [29] compiler_4.0.2      numDeriv_2016.8-1.1 xfun_0.16           pkgconfig_2.0.3    
    ## [33] htmltools_0.5.0     tidyselect_1.1.0    tibble_3.0.3        rio_0.5.16         
    ## [37] fansi_0.4.1         crayon_1.3.4        grid_4.0.2          nlme_3.1-148       
    ## [41] xtable_1.8-4        gtable_0.3.0        lifecycle_0.2.0     magrittr_1.5       
    ## [45] scales_1.1.1        zip_2.0.4           cli_2.0.2           estimability_1.3   
    ## [49] stringi_1.4.6       carData_3.0-4       renv_0.12.0         reshape2_1.4.4     
    ## [53] ellipsis_0.3.1      generics_0.0.2      vctrs_0.3.2         boot_1.3-25        
    ## [57] openxlsx_4.1.5      RColorBrewer_1.1-2  tools_4.0.2         forcats_0.5.0      
    ## [61] glue_1.4.1          purrr_0.3.4         hms_0.5.3           abind_1.4-5        
    ## [65] parallel_4.0.2      yaml_2.2.1          colorspace_1.4-1    knitr_1.29         
    ## [69] haven_2.3.1