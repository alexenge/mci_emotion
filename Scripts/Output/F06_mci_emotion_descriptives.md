F06\_mci\_emotion\_descriptives.R
================
kirstenstark
2020-09-15

``` r
### MCI EMO DESCRIPTIVES SCRIPT ###

# Gives descriptive information about the number of (a) rejected ERP epochs and (b) errors or
# unrealistically short reaction times per participant. For the latter, an ANOVA also checks whether
# the number of errors differs between experimental conditions.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(Rmisc)     # version 1.5

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Add a column for artifact-rejected ERPs
a1$rejected <- is.na(a1$N400.verb) | is.na(a1$N400.pict)

# Check number of rejected ERPs participant
mean(table(a1$rejected, a1$participant)[2,]) # Mean
```

    ## [1] 3.6

``` r
mean(table(a1$rejected, a1$participant)[2,])/366 # Percent
```

    ## [1] 0.009836066

``` r
median(table(a1$rejected, a1$participant)[2,]) # Median
```

    ## [1] 1

``` r
min(table(a1$rejected, a1$participant)[2,]) # Min
```

    ## [1] 0

``` r
max(table(a1$rejected, a1$participant)[2,]) # Max
```

    ## [1] 27

``` r
# Remove rejected ERPs from the data
a1 <- a1[!a1$rejected,]

# Check number of errors per participant
mean(table(a1$error, a1$participant)[2,]) # Mean
```

    ## [1] 23.86667

``` r
mean(table(a1$error, a1$participant)[2,])/366 # Percent
```

    ## [1] 0.06520947

``` r
median(table(a1$error, a1$participant)[2,]) # Median
```

    ## [1] 20

``` r
min(table(a1$error, a1$participant)[2,]) # Min
```

    ## [1] 5

``` r
max(table(a1$error, a1$participant)[2,]) # Max
```

    ## [1] 69

``` r
# Check if error tates differ between conditions
accuracy <- summarySEwithin(a1, measurevar = "error", withinvars = c("semantics", "context"),
                            betweenvars = "participant", idvar = "participant", na.rm = TRUE)
summary(aov(error ~ semantics*context + Error(participant/(semantics*context)), data = accuracy))
```

    ## 
    ## Error: participant
    ##           Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Residuals 29 3.986e-06 1.374e-07               
    ## 
    ## Error: participant:semantics
    ##           Df  Sum Sq   Mean Sq F value Pr(>F)
    ## semantics  2 0.00107 0.0005335   0.551   0.58
    ## Residuals 58 0.05620 0.0009689               
    ## 
    ## Error: participant:context
    ##           Df  Sum Sq   Mean Sq F value Pr(>F)
    ## context    1 0.00090 0.0009002   0.848  0.365
    ## Residuals 29 0.03077 0.0010610               
    ## 
    ## Error: participant:semantics:context
    ##                   Df  Sum Sq   Mean Sq F value Pr(>F)
    ## semantics:context  2 0.00005 0.0000264   0.039  0.961
    ## Residuals         58 0.03876 0.0006683

``` r
### MCI EMO DESCRIPTIVES APPENDIX (P600) ### ------------------------------------------------------

# Load preprocessed data
a1_app <- readRDS("EEG/export/a1_appendix.RDS")

# Add a column for artifact-rejected ERPs
a1_app$rejected <- is.na(a1_app$P600_2.verb)

# Check number of rejected ERPs participant
mean(table(a1_app$rejected, a1_app$participant)[2,]) # Mean
```

    ## [1] 0.4666667

``` r
mean(table(a1_app$rejected, a1_app$participant)[2,])/366 # Percent
```

    ## [1] 0.001275046

``` r
median(table(a1_app$rejected, a1_app$participant)[2,]) # Median
```

    ## [1] 0

``` r
min(table(a1_app$rejected, a1_app$participant)[2,]) # Min
```

    ## [1] 0

``` r
max(table(a1_app$rejected, a1_app$participant)[2,]) # Max
```

    ## [1] 3

``` r
# Remove rejected ERPs from the data
a1_app <- a1_app[!a1_app$rejected,]

# Check number of errors per participant
mean(table(a1_app$error, a1_app$participant)[2,]) # Mean
```

    ## [1] 23.9

``` r
mean(table(a1_app$error, a1_app$participant)[2,])/366 # Percent
```

    ## [1] 0.06530055

``` r
median(table(a1_app$error, a1_app$participant)[2,]) # Median
```

    ## [1] 20

``` r
min(table(a1_app$error, a1_app$participant)[2,]) # Min
```

    ## [1] 5

``` r
max(table(a1_app$error, a1_app$participant)[2,]) # Max
```

    ## [1] 69

``` r
# Check if error tates differ between conditions
accuracy_app <- summarySEwithin(a1_app, measurevar = "error", withinvars = c("semantics", "context"),
                            betweenvars = "participant", idvar = "participant", na.rm = TRUE)
summary(aov(error ~ semantics*context + Error(participant/(semantics*context)), data = accuracy_app))
```

    ## 
    ## Error: participant
    ##           Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Residuals 29 2.424e-06 8.359e-08               
    ## 
    ## Error: participant:semantics
    ##           Df  Sum Sq   Mean Sq F value Pr(>F)
    ## semantics  2 0.00087 0.0004349   0.456  0.636
    ## Residuals 58 0.05537 0.0009546               
    ## 
    ## Error: participant:context
    ##           Df   Sum Sq   Mean Sq F value Pr(>F)
    ## context    1 0.000794 0.0007938   0.755  0.392
    ## Residuals 29 0.030503 0.0010518               
    ## 
    ## Error: participant:semantics:context
    ##                   Df  Sum Sq   Mean Sq F value Pr(>F)
    ## semantics:context  2 0.00004 0.0000208   0.031  0.969
    ## Residuals         58 0.03861 0.0006657

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
    ## [1] Rmisc_1.5       plyr_1.8.6      lattice_0.20-41
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-148        matrixStats_0.56.0  httr_1.4.2          numDeriv_2016.8-1.1
    ##  [5] tools_4.0.2         R6_2.4.1            afex_0.27-2         lazyeval_0.2.2     
    ##  [9] mgcv_1.8-31         colorspace_1.4-1    tidyselect_1.1.0    emmeans_1.4.8      
    ## [13] curl_4.3            compiler_4.0.2      eegUtils_0.5.0.9000 plotly_4.9.2.1     
    ## [17] scales_1.1.1        mvtnorm_1.1-1       stringr_1.4.0       digest_0.6.25      
    ## [21] foreign_0.8-80      minqa_1.2.4         rmarkdown_2.3       R.utils_2.9.2      
    ## [25] rio_0.5.16          ini_0.3.1           pkgconfig_2.0.3     htmltools_0.5.0    
    ## [29] lme4_1.1-23         fastmap_1.0.1       highr_0.8           htmlwidgets_1.5.1  
    ## [33] rlang_0.4.7         readxl_1.3.1        rstudioapi_0.11     shiny_1.5.0        
    ## [37] generics_0.0.2      jsonlite_1.7.0      dplyr_1.0.0         zip_2.1.1          
    ## [41] car_3.0-8           R.oo_1.23.0         magrittr_1.5        R.matlab_3.6.2     
    ## [45] Matrix_1.2-18       Rcpp_1.0.5          munsell_0.5.0       abind_1.4-5        
    ## [49] lifecycle_0.2.0     R.methodsS3_1.8.0   stringi_1.4.6       yaml_2.2.1         
    ## [53] carData_3.0-4       MASS_7.3-51.6       grid_4.0.2          parallel_4.0.2     
    ## [57] listenv_0.8.0       promises_1.1.1      forcats_0.5.0       crayon_1.3.4       
    ## [61] miniUI_0.1.1.1      haven_2.3.1         splines_4.0.2       hms_0.5.3          
    ## [65] knitr_1.29          pillar_1.4.6        boot_1.3-25         estimability_1.3   
    ## [69] future.apply_1.6.0  reshape2_1.4.4      codetools_0.2-16    glue_1.4.1         
    ## [73] evaluate_0.14       data.table_1.13.0   renv_0.12.0         vctrs_0.3.2        
    ## [77] nloptr_1.2.2.2      httpuv_1.5.4        cellranger_1.1.0    gtable_0.3.0       
    ## [81] purrr_0.3.4         tidyr_1.1.0         future_1.18.0       ggplot2_3.3.2      
    ## [85] xfun_0.16           openxlsx_4.1.5      mime_0.9            xtable_1.8-4       
    ## [89] pracma_2.2.9        later_1.1.0.1       viridisLite_0.3.0   signal_0.7-6       
    ## [93] tibble_3.0.3        lmerTest_3.1-2      globals_0.12.5      statmod_1.4.34     
    ## [97] ellipsis_0.3.1
