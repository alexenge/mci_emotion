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
    ##  [1] huxtable_5.0.0  magrittr_1.5    forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0    
    ##  [6] purrr_0.3.4     readr_1.3.1     tidyr_1.1.0     tibble_3.0.3    ggplot2_3.3.2  
    ## [11] tidyverse_1.3.0 Rmisc_1.5       plyr_1.8.6      lattice_0.20-41
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-148        fs_1.4.2            lubridate_1.7.9     httr_1.4.2         
    ##  [5] numDeriv_2016.8-1.1 tools_4.0.2         backports_1.1.8     R6_2.4.1           
    ##  [9] afex_0.27-2         DBI_1.1.0           colorspace_1.4-1    withr_2.2.0        
    ## [13] tidyselect_1.1.0    emmeans_1.4.8       curl_4.3            compiler_4.0.2     
    ## [17] cli_2.0.2           rvest_0.3.5         flextable_0.5.11    xml2_1.3.2         
    ## [21] officer_0.3.14      scales_1.1.1        mvtnorm_1.1-1       commonmark_1.7     
    ## [25] systemfonts_0.3.1   digest_0.6.25       foreign_0.8-80      minqa_1.2.4        
    ## [29] rmarkdown_2.3       rio_0.5.16          base64enc_0.1-3     pkgconfig_2.0.3    
    ## [33] htmltools_0.5.0     lme4_1.1-23         dbplyr_1.4.4        highr_0.8          
    ## [37] rlang_0.4.7         readxl_1.3.1        rstudioapi_0.11     generics_0.0.2     
    ## [41] jsonlite_1.7.0      zip_2.1.1           car_3.0-8           Matrix_1.2-18      
    ## [45] Rcpp_1.0.5          munsell_0.5.0       fansi_0.4.1         gdtools_0.2.2      
    ## [49] abind_1.4-5         lifecycle_0.2.0     stringi_1.4.6       yaml_2.2.1         
    ## [53] carData_3.0-4       MASS_7.3-51.6       grid_4.0.2          blob_1.2.1         
    ## [57] parallel_4.0.2      crayon_1.3.4        haven_2.3.1         splines_4.0.2      
    ## [61] hms_0.5.3           knitr_1.29          pillar_1.4.6        uuid_0.1-4         
    ## [65] boot_1.3-25         estimability_1.3    reshape2_1.4.4      reprex_0.3.0       
    ## [69] glue_1.4.1          evaluate_0.14       data.table_1.13.0   renv_0.12.0        
    ## [73] modelr_0.1.8        vctrs_0.3.2         nloptr_1.2.2.2      cellranger_1.1.0   
    ## [77] gtable_0.3.0        assertthat_0.2.1    cpp11_0.2.1         xfun_0.16          
    ## [81] openxlsx_4.1.5      xtable_1.8-4        broom_0.7.0.9001    lmerTest_3.1-2     
    ## [85] statmod_1.4.34      ellipsis_0.3.1
