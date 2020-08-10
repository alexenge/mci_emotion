F05\_mci\_emotion\_descriptives\_appendix.R
================
kirstenstark
2020-08-10

``` r
### MCI EMO DESCRIPTIVES SCRIPT - APPENDIX (P600) ###

# Gives descriptive information about the number of (a) rejected ERP epochs and (b) errors or
# unrealistically short reaction times per participant. For the latter, an ANOVA also checks whether
# the number of errors differs between experimental conditions.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(Rmisc)     # version 1.5

# Load preprocessed data
a1 <- readRDS("EEG/export/a1_appendix.RDS")

# Add a column for artifact-rejected ERPs
a1$rejected <- is.na(a1$P600_1.verb) | is.na(a1$P600_2.verb)

# Check number of rejected ERPs participant
mean(table(a1$rejected, a1$participant)[2,]) # Mean
```

    ## [1] 0.4666667

``` r
mean(table(a1$rejected, a1$participant)[2,])/366 # Percent
```

    ## [1] 0.001275046

``` r
median(table(a1$rejected, a1$participant)[2,]) # Median
```

    ## [1] 0

``` r
min(table(a1$rejected, a1$participant)[2,]) # Min
```

    ## [1] 0

``` r
max(table(a1$rejected, a1$participant)[2,]) # Max
```

    ## [1] 3

``` r
# Remove rejected ERPs from the data
a1 <- a1[!a1$rejected,]

# Check number of errors per participant
mean(table(a1$error, a1$participant)[2,]) # Mean
```

    ## [1] 23.9

``` r
mean(table(a1$error, a1$participant)[2,])/366 # Percent
```

    ## [1] 0.06530055

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
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] Rmisc_1.5       plyr_1.8.6      lattice_0.20-41 huxtable_5.0.0 
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] minqa_1.2.4          colorspace_1.4-1     ellipsis_0.3.1      
    ##   [4] rio_0.5.16           estimability_1.3     flextable_0.5.10    
    ##   [7] base64enc_0.1-3      rstudioapi_0.11      listenv_0.8.0       
    ##  [10] R.matlab_3.6.2       mvtnorm_1.1-1        xml2_1.3.2          
    ##  [13] codetools_0.2-16     splines_4.0.2        R.methodsS3_1.8.0   
    ##  [16] knitr_1.29           eegUtils_0.5.0       afex_0.27-2         
    ##  [19] jsonlite_1.7.0       nloptr_1.2.2.2       packrat_0.5.0       
    ##  [22] R.oo_1.23.0          shinydashboard_0.7.1 shiny_1.5.0         
    ##  [25] compiler_4.0.2       httr_1.4.1           emmeans_1.4.8       
    ##  [28] assertthat_0.2.1     Matrix_1.2-18        fastmap_1.0.1       
    ##  [31] lazyeval_0.2.2       later_1.1.0.1        htmltools_0.5.0     
    ##  [34] tools_4.0.2          lmerTest_3.1-2       coda_0.19-3         
    ##  [37] gtable_0.3.0         glue_1.4.1           reshape2_1.4.4      
    ##  [40] dplyr_1.0.0          Rcpp_1.0.5           carData_3.0-4       
    ##  [43] cellranger_1.1.0     vctrs_0.3.2          nlme_3.1-148        
    ##  [46] xfun_0.15            stringr_1.4.0        globals_0.12.5      
    ##  [49] openxlsx_4.1.5       lme4_1.1-23          mime_0.9            
    ##  [52] miniUI_0.1.1.1       lifecycle_0.2.0      edfReader_1.2.1     
    ##  [55] statmod_1.4.34       future_1.18.0        MASS_7.3-51.6       
    ##  [58] scales_1.1.1         hms_0.5.3            promises_1.1.1      
    ##  [61] parallel_4.0.2       RColorBrewer_1.1-2   yaml_2.2.1          
    ##  [64] curl_4.3             gridExtra_2.3        ggplot2_3.3.2       
    ##  [67] gdtools_0.2.2        stringi_1.4.6        highr_0.8           
    ##  [70] boot_1.3-25          zip_2.0.4            commonmark_1.7      
    ##  [73] systemfonts_0.2.3    rlang_0.4.7          pkgconfig_2.0.3     
    ##  [76] matrixStats_0.56.0   pracma_2.2.9         evaluate_0.14       
    ##  [79] purrr_0.3.4          htmlwidgets_1.5.1    tidyselect_1.1.0    
    ##  [82] magrittr_1.5         R6_2.4.1             generics_0.0.2      
    ##  [85] ini_0.3.1            pillar_1.4.6         haven_2.3.1         
    ##  [88] foreign_0.8-80       mgcv_1.8-31          abind_1.4-5         
    ##  [91] tibble_3.0.3         future.apply_1.6.0   crayon_1.3.4        
    ##  [94] car_3.0-8            uuid_0.1-4           plotly_4.9.2.1      
    ##  [97] rmarkdown_2.3        officer_0.3.12       viridis_0.5.1       
    ## [100] grid_4.0.2           readxl_1.3.1         data.table_1.12.8   
    ## [103] forcats_0.5.0        digest_0.6.25        xtable_1.8-4        
    ## [106] tidyr_1.1.0          httpuv_1.5.4         numDeriv_2016.8-1.1 
    ## [109] R.utils_2.9.2        signal_0.7-6         munsell_0.5.0       
    ## [112] viridisLite_0.3.0
