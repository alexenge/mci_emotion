F04\_mci\_emotion\_tables\_lag.R
================
kirstenstark
2020-08-10

``` r
### MCI EMO TABLES SCRIPT - APPENDIX (EFFECT OF LAG1 SEMANTICS) ###

# The expected effect of semantic violations on N400 amplitudes was not found in the main analyses.
# Additional "sequence analyses" were conducted in order to test whether the type of semantic
# manipulation in the previous trial affected present N400 amplitudes. 
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
  coefs[,2] <- substr(coefs[,2], 2, 5)
  coefs[coefs[,2] == ".000", 2] <- "< .001"
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
```

|                                     | Verb-Related N400           |         | Picture-Related N400        |         |
| ----------------------------------- | :-------------------------- | ------- | :-------------------------- | ------- |
| **Model output**                    | ***F*** (***df***)          | ***p*** | ***F*** (***df***)          | ***p*** |
| Lag1Semantics                       | 1.09<br/>(2, 10073.0)       | .336    | 0.88<br/>(2, 10042.1)       | .416    |
| Semantics                           | 7.78<br/>(2, 299.0)         | .001    | 1.08<br/>(2, 93.6)          | .344    |
| Context                             | 0.12<br/>(1, 32.7)          | .727    | 0.03<br/>(1, 34.2)          | .854    |
| Lag1Semantics x semantics           | 0.13<br/>(4, 10062.9)       | .970    | 1.73<br/>(4, 10027.0)       | .139    |
| Lag1Semantics x context             | 2.30<br/>(2, 10055.9)       | .100    | 0.24<br/>(2, 10036.5)       | .784    |
| Semantics × context                 | 0.96<br/>(2, 94.9)          | .388    | 5.29<br/>(2, 99.8)          | .007    |
| lag1Semantics x semantics x context | 0.74<br/>(4, 10058.2)       | .567    | 0.89<br/>(4, 10030.0)       | .467    |
| **Planned contrasts**               | **Est. \[95% CI\]**         | ***p*** | **Est. \[95% CI\]**         | ***p*** |
| Vio. - int.<br/>(lag1Int, neutral)  | \-0.12<br/>\[-0.59, 0.34\]  | 1.000   | 0.27<br/>\[-0.19, 0.73\]    | 0.364   |
| MCI - int.<br/>(lag1Int, neutral)   | \-0.59<br/>\[-1.04, -0.14\] | 0.007   | \-0.11<br/>\[-0.58, 0.36\]  | 1.000   |
| Vio. - int.<br/>(lag1Vio, neutral)  | \-0.16<br/>\[-0.87, 0.54\]  | 1.000   | \-0.59<br/>\[-1.27, 0.09\]  | 0.106   |
| MCI - int.<br/>(lag1Vio, neutral)   | \-0.49<br/>\[-1.19, 0.21\]  | 0.231   | \-0.74<br/>\[-1.43, -0.04\] | 0.035   |
| Vio. - int.<br/>(lag1MCI, neutral)  | \-0.32<br/>\[-1.04, 0.41\]  | 0.661   | \-0.25<br/>\[-0.95, 0.46\]  | 0.860   |
| MCI - int.<br/>(lag1MCI, neutral)   | \-0.44<br/>\[-1.13, 0.25\]  | 0.314   | \-0.81<br/>\[-1.50, -0.12\] | 0.017   |
| Vio. - int.<br/>(lag1Int, negative) | 0.07<br/>\[-0.40, 0.54\]    | 1.000   | 0.31<br/>\[-0.15, 0.78\]    | 0.255   |
| MCI - int.<br/>(lag1Int, negative)  | \-0.11<br/>\[-0.56, 0.34\]  | 1.000   | 0.22<br/>\[-0.25, 0.69\]    | 0.591   |
| Vio. - int.<br/>(lag1Vio, negative) | 0.02<br/>\[-0.69, 0.72\]    | 1.000   | 0.13<br/>\[-0.55, 0.82\]    | 1.000   |
| MCI - int.<br/>(lag1Vio, negative)  | \-0.56<br/>\[-1.27, 0.14\]  | 0.149   | 0.09<br/>\[-0.62, 0.79\]    | 1.000   |
| Vio. - int.<br/>(lag1MCI, negative) | 0.30<br/>\[-0.40, 1.00\]    | 0.669   | \-0.10<br/>\[-0.77, 0.58\]  | 1.000   |
| MCI - int.<br/>(lag1MCI, negative)  | \-0.26<br/>\[-0.94, 0.42\]  | 0.776   | 0.12<br/>\[-0.56, 0.80\]    | 1.000   |

``` r
# Export as a word file (after some re-formatting)
tab_word <- data.frame(lapply(tab, function(x){gsub("<br/>", "\n", x)}))
tab_word <- data.frame(lapply(tab_word, function(x){gsub("\\*|\\_", "", x)}))
huxt_word <- huxtable(tab_word, add_colnames = FALSE)
quick_docx(huxt_word, file = "EEG/tables/lmm_table_appendixLag.docx", open = FALSE)
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
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] huxtable_5.0.0 emmeans_1.4.8  afex_0.27-2    lmerTest_3.1-2 lme4_1.1-23   
    ## [6] Matrix_1.2-18  MASS_7.3-51.6 
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] minqa_1.2.4          colorspace_1.4-1     ellipsis_0.3.1      
    ##   [4] rio_0.5.16           estimability_1.3     flextable_0.5.10    
    ##   [7] base64enc_0.1-3      rstudioapi_0.11      listenv_0.8.0       
    ##  [10] R.matlab_3.6.2       mvtnorm_1.1-1        xml2_1.3.2          
    ##  [13] codetools_0.2-16     splines_4.0.2        R.methodsS3_1.8.0   
    ##  [16] knitr_1.29           eegUtils_0.5.0       jsonlite_1.7.0      
    ##  [19] nloptr_1.2.2.2       packrat_0.5.0        R.oo_1.23.0         
    ##  [22] shinydashboard_0.7.1 shiny_1.5.0          compiler_4.0.2      
    ##  [25] httr_1.4.1           assertthat_0.2.1     fastmap_1.0.1       
    ##  [28] lazyeval_0.2.2       later_1.1.0.1        htmltools_0.5.0     
    ##  [31] tools_4.0.2          coda_0.19-3          gtable_0.3.0        
    ##  [34] glue_1.4.1           reshape2_1.4.4       dplyr_1.0.0         
    ##  [37] Rcpp_1.0.5           carData_3.0-4        cellranger_1.1.0    
    ##  [40] vctrs_0.3.2          nlme_3.1-148         xfun_0.15           
    ##  [43] stringr_1.4.0        globals_0.12.5       Rmisc_1.5           
    ##  [46] openxlsx_4.1.5       mime_0.9             miniUI_0.1.1.1      
    ##  [49] lifecycle_0.2.0      edfReader_1.2.1      statmod_1.4.34      
    ##  [52] future_1.18.0        scales_1.1.1         hms_0.5.3           
    ##  [55] promises_1.1.1       parallel_4.0.2       RColorBrewer_1.1-2  
    ##  [58] yaml_2.2.1           curl_4.3             gridExtra_2.3       
    ##  [61] ggplot2_3.3.2        gdtools_0.2.2        stringi_1.4.6       
    ##  [64] highr_0.8            boot_1.3-25          zip_2.0.4           
    ##  [67] commonmark_1.7       systemfonts_0.2.3    rlang_0.4.7         
    ##  [70] pkgconfig_2.0.3      matrixStats_0.56.0   pracma_2.2.9        
    ##  [73] evaluate_0.14        lattice_0.20-41      purrr_0.3.4         
    ##  [76] htmlwidgets_1.5.1    tidyselect_1.1.0     plyr_1.8.6          
    ##  [79] magrittr_1.5         R6_2.4.1             generics_0.0.2      
    ##  [82] ini_0.3.1            pillar_1.4.6         haven_2.3.1         
    ##  [85] foreign_0.8-80       mgcv_1.8-31          abind_1.4-5         
    ##  [88] tibble_3.0.3         future.apply_1.6.0   crayon_1.3.4        
    ##  [91] car_3.0-8            uuid_0.1-4           plotly_4.9.2.1      
    ##  [94] rmarkdown_2.3        officer_0.3.12       viridis_0.5.1       
    ##  [97] grid_4.0.2           readxl_1.3.1         data.table_1.12.8   
    ## [100] forcats_0.5.0        digest_0.6.25        xtable_1.8-4        
    ## [103] tidyr_1.1.0          httpuv_1.5.4         numDeriv_2016.8-1.1 
    ## [106] R.utils_2.9.2        signal_0.7-6         munsell_0.5.0       
    ## [109] viridisLite_0.3.0
