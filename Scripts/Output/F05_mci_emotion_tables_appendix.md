F05\_mci\_emotion\_tables\_appendix.R
================
kirstenstark
2020-09-14

``` r
### MCI EMO TABLES SCRIPT - APPENDIX (P600) ###

# Creates a table for the output of our linear mixed-effects models on P600 amplitudes. The 
# upper half of the table includes ANOVA-style type III tests (F-tests), the bottom half 
# contains planned follow-up contrasts. For the F-tests, F-values, degrees of freedom, and 
# p-values are printed, whereas for the contrasts, regression estimates, 95% confidence 
# intervals, and p-values are printed.

## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(huxtable)     # version 5.0.0

# Load output from mixed models
load("EEG/export/stats_appendix.RData")

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

## P600 - 500-900 ms ## ---------------------------------------------------------------------------

# Add model names (dependent variables) as the first row
tab <- rbind(c("Verb-Related P600 (500-700 ms)", ""), tab)

# Add a stub column
tab <- cbind(c("", "**Model output**", "Semantics", "Context", "Semantics × context",
               "**Planned contrasts**", "Vio. - int.<br/>(neutral)", "MCI - int.<br/>(neutral)",
               "Vio. - int.<br/>(negative)", "MCI - int.<br/>(negative)"), tab)

# Remove old column names
names(tab) <- NULL

# Create a huxtable and output as markdown
huxt <- huxtable(tab, add_colnames = FALSE)
print_md(huxt, max_width = Inf)
```

|                            | Verb-Related P600 (500-700 ms) |         |
| -------------------------- | ------------------------------ | ------- |
| **Model output**           | ***F*** (***df***)             | ***p*** |
| Semantics                  | 1.10<br/>(2, 103.6)            | 0.335   |
| Context                    | 0.04<br/>(1, 29.6)             | 0.847   |
| Semantics × context        | 0.72<br/>(2, 62.2)             | 0.492   |
| **Planned contrasts**      | **Est. \[95% CI\]**            | ***p*** |
| Vio. - int.<br/>(neutral)  | \-0.02<br/>\[-0.37, 0.33\]     | 1.000   |
| MCI - int.<br/>(neutral)   | \-0.11<br/>\[-0.50, 0.27\]     | 1.000   |
| Vio. - int.<br/>(negative) | 0.24<br/>\[-0.11, 0.58\]       | 0.254   |
| MCI - int.<br/>(negative)  | \-0.04<br/>\[-0.43, 0.34\]     | 1.000   |

``` r
# Export as a word file (after some re-formatting)
tab_word <- data.frame(lapply(tab, function(x){gsub("<br/>", "\n", x)}))
tab_word <- data.frame(lapply(tab_word, function(x){gsub("\\*|\\_", "", x)}))
huxt_word <- huxtable(tab_word, add_colnames = FALSE)
quick_docx(huxt_word, file = "EEG/tables/lmm_table_appendixP600_500-900ms.docx", open = FALSE)
```

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
    ##  [1] magrittr_1.5    forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0     purrr_0.3.4    
    ##  [6] readr_1.3.1     tidyr_1.1.0     tibble_3.0.3    ggplot2_3.3.2   tidyverse_1.3.0
    ## [11] Rmisc_1.5       plyr_1.8.6      lattice_0.20-41 huxtable_5.0.0 
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] minqa_1.2.4         colorspace_1.4-1    ellipsis_0.3.1      rio_0.5.16         
    ##   [5] estimability_1.3    flextable_0.5.11    fs_1.4.2            base64enc_0.1-3    
    ##   [9] rstudioapi_0.11     listenv_0.8.0       R.matlab_3.6.2      fansi_0.4.1        
    ##  [13] lubridate_1.7.9     mvtnorm_1.1-1       xml2_1.3.2          codetools_0.2-16   
    ##  [17] splines_4.0.2       R.methodsS3_1.8.0   knitr_1.29          eegUtils_0.5.0.9000
    ##  [21] afex_0.27-2         jsonlite_1.7.0      nloptr_1.2.2.2      broom_0.7.0.9001   
    ##  [25] dbplyr_1.4.4        R.oo_1.23.0         shiny_1.5.0         compiler_4.0.2     
    ##  [29] httr_1.4.2          backports_1.1.8     emmeans_1.4.8       assertthat_0.2.1   
    ##  [33] Matrix_1.2-18       fastmap_1.0.1       lazyeval_0.2.2      cli_2.0.2          
    ##  [37] later_1.1.0.1       htmltools_0.5.0     tools_4.0.2         lmerTest_3.1-2     
    ##  [41] gtable_0.3.0        glue_1.4.1          reshape2_1.4.4      Rcpp_1.0.5         
    ##  [45] carData_3.0-4       cellranger_1.1.0    vctrs_0.3.2         nlme_3.1-148       
    ##  [49] xfun_0.16           globals_0.12.5      rvest_0.3.5         openxlsx_4.1.5     
    ##  [53] lme4_1.1-23         mime_0.9            miniUI_0.1.1.1      lifecycle_0.2.0    
    ##  [57] renv_0.12.0         statmod_1.4.34      future_1.18.0       MASS_7.3-51.6      
    ##  [61] scales_1.1.1        hms_0.5.3           promises_1.1.1      parallel_4.0.2     
    ##  [65] RColorBrewer_1.1-2  yaml_2.2.1          curl_4.3            gdtools_0.2.2      
    ##  [69] stringi_1.4.6       highr_0.8           boot_1.3-25         zip_2.1.1          
    ##  [73] cpp11_0.2.1         rlang_0.4.7         pkgconfig_2.0.3     systemfonts_0.3.1  
    ##  [77] commonmark_1.7      matrixStats_0.56.0  pracma_2.2.9        evaluate_0.14      
    ##  [81] htmlwidgets_1.5.1   tidyselect_1.1.0    R6_2.4.1            generics_0.0.2     
    ##  [85] DBI_1.1.0           ini_0.3.1           withr_2.2.0         pillar_1.4.6       
    ##  [89] haven_2.3.1         foreign_0.8-80      mgcv_1.8-31         abind_1.4-5        
    ##  [93] future.apply_1.6.0  modelr_0.1.8        crayon_1.3.4        car_3.0-8          
    ##  [97] uuid_0.1-4          plotly_4.9.2.1      rmarkdown_2.3       officer_0.3.14     
    ## [101] grid_4.0.2          readxl_1.3.1        data.table_1.13.0   blob_1.2.1         
    ## [105] reprex_0.3.0        digest_0.6.25       xtable_1.8-4        httpuv_1.5.4       
    ## [109] numDeriv_2016.8-1.1 R.utils_2.9.2       signal_0.7-6        munsell_0.5.0      
    ## [113] viridisLite_0.3.0
