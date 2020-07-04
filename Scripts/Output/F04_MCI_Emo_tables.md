F04\_MCI\_Emo\_tables.R
================
alexander
2020-07-04

``` r
### MCI EMO TABLES SCRIPT ###

# Creates tables for linear mixed effect model outputs, including ANOVA-style type III tests (F-tests) and
# planned contrasts. To render these tables to a word document, run (only) the following line of code:

# Load packages
library(huxtable)     # version 5.0.0

# Load output from mixed models
load("EEG/export/stats.RData")

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

# Extract a table for the planned contrasts for each model (columns: estimate (CI), p-value)
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

# Bind both data frames (F-tests and contrats) below one another
tab <- rbind(anovas, conts)

# Add model names (dependent varibales) as the first row
tab <- rbind(c("Valence Rating", "", "Arousal Rating", "", "Verb-Related N400", "", "Picture-Related N400", ""), tab)

# Add a stub column
tab <- cbind(c("", "**Model output**", "Semantics", "Context", "Semantics × context",
               "**Planned contrasts**", "Vio. - int.<br/>(neutral)", "MCI - int.<br/>(neutral)",
               "Vio. - int.<br/>(negative)", "MCI - int.<br/>(negative)"), tab)

# Removeo old column names
names(tab) <- NULL

# Create a huxtable and output as markdown
huxt <- huxtable(tab, add_colnames = FALSE)
print_md(huxt, max_width = Inf)
```

|                            | Valence Rating           |         | Arousal Rating             |         | Verb-Related N400           |         | Picture-Related N400       |         |
| -------------------------- | :----------------------- | ------- | :------------------------- | ------- | :-------------------------- | ------- | :------------------------- | ------- |
| **Model output**           | ***F*** (***df***)       | ***p*** | ***F*** (***df***)         | ***p*** | ***F*** (***df***)          | ***p*** | ***F*** (***df***)         | ***p*** |
| Semantics                  | 0.01<br/>(2, 60.1)       | 0.991   | 0.12<br/>(2, 65.9)         | 0.887   | 7.09<br/>(2, 63.1)          | 0.002   | 0.73<br/>(2, 37.0)         | 0.490   |
| Context                    | 164.29<br/>(1, 38.0)     | \< .001 | 84.53<br/>(1, 37.9)        | \< .001 | 0.02<br/>(1, 34.9)          | 0.891   | 0.01<br/>(1, 44.1)         | 0.943   |
| Semantics × context        | 0.02<br/>(2, 60.4)       | 0.984   | 0.05<br/>(2, 87.9)         | 0.952   | 1.25<br/>(2, 69.6)          | 0.292   | 3.89<br/>(2, 52.1)         | 0.027   |
| **Planned contrasts**      | **Est. \[95% CI\]**      | ***p*** | **Est. \[95% CI\]**        | ***p*** | **Est. \[95% CI\]**         | ***p*** | **Est. \[95% CI\]**        | ***p*** |
| Vio. - int.<br/>(neutral)  | 0.01<br/>\[-0.21, 0.24\] | 1.000   | 0.02<br/>\[-0.13, 0.17\]   | 1.000   | \-0.17<br/>\[-0.61, 0.27\]  | 0.762   | \-0.04<br/>\[-0.42, 0.35\] | 1.000   |
| MCI - int.<br/>(neutral)   | 0.00<br/>\[-0.21, 0.21\] | 1.000   | 0.01<br/>\[-0.13, 0.16\]   | 1.000   | \-0.53<br/>\[-0.89, -0.17\] | 0.002   | \-0.41<br/>\[-0.81, 0.00\] | 0.049   |
| Vio. - int.<br/>(negative) | 0.00<br/>\[-0.16, 0.17\] | 1.000   | 0.03<br/>\[-0.12, 0.18\]   | 1.000   | 0.11<br/>\[-0.25, 0.47\]    | 0.946   | 0.18<br/>\[-0.23, 0.59\]   | 0.620   |
| MCI - int.<br/>(negative)  | 0.01<br/>\[-0.16, 0.18\] | 1.000   | \-0.01<br/>\[-0.15, 0.13\] | 1.000   | \-0.24<br/>\[-0.60, 0.13\]  | 0.290   | 0.16<br/>\[-0.23, 0.56\]   | 0.678   |

``` r
# Export as a word file (after some re-formatting)
tab_word <- data.frame(lapply(tab, function(x){gsub("<br/>", "\n", x)}))
tab_word <- data.frame(lapply(tab_word, function(x){gsub("\\*|\\_", "", x)}))
huxt_word <- huxtable(tab_word, add_colnames = FALSE)
quick_docx(huxt_word, file = "EEG/tables/lmm_table.docx", open = FALSE)
```

``` r
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
    ## [1] huxtable_5.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] minqa_1.2.4          colorspace_1.4-1     ellipsis_0.3.1       rio_0.5.16           estimability_1.3     flextable_0.5.10     base64enc_0.1-3     
    ##   [8] rstudioapi_0.11      listenv_0.8.0        R.matlab_3.6.2       mvtnorm_1.1-1        xml2_1.3.2           codetools_0.2-16     splines_4.0.2       
    ##  [15] R.methodsS3_1.8.0    knitr_1.29           eegUtils_0.5.0       afex_0.27-2          jsonlite_1.7.0       nloptr_1.2.2.2       R.oo_1.23.0         
    ##  [22] shinydashboard_0.7.1 shiny_1.5.0          compiler_4.0.2       httr_1.4.1           emmeans_1.4.8        assertthat_0.2.1     Matrix_1.2-18       
    ##  [29] fastmap_1.0.1        lazyeval_0.2.2       later_1.1.0.1        htmltools_0.5.0      tools_4.0.2          lmerTest_3.1-2       coda_0.19-3         
    ##  [36] gtable_0.3.0         glue_1.4.1           reshape2_1.4.4       dplyr_1.0.0          tinytex_0.24         Rcpp_1.0.4.6         carData_3.0-4       
    ##  [43] cellranger_1.1.0     vctrs_0.3.1          nlme_3.1-148         xfun_0.15            stringr_1.4.0        globals_0.12.5       Rmisc_1.5           
    ##  [50] openxlsx_4.1.5       lme4_1.1-23          mime_0.9             miniUI_0.1.1.1       lifecycle_0.2.0      edfReader_1.2.1      statmod_1.4.34      
    ##  [57] future_1.17.0        MASS_7.3-51.6        scales_1.1.1         hms_0.5.3            promises_1.1.1       parallel_4.0.2       RColorBrewer_1.1-2  
    ##  [64] yaml_2.2.1           curl_4.3             gridExtra_2.3        ggplot2_3.3.2        gdtools_0.2.2        stringi_1.4.6        highr_0.8           
    ##  [71] boot_1.3-25          zip_2.0.4            commonmark_1.7       rlang_0.4.6          pkgconfig_2.0.3      systemfonts_0.2.3    matrixStats_0.56.0  
    ##  [78] pracma_2.2.9         evaluate_0.14        lattice_0.20-41      purrr_0.3.4          htmlwidgets_1.5.1    tidyselect_1.1.0     plyr_1.8.6          
    ##  [85] magrittr_1.5         R6_2.4.1             generics_0.0.2       ini_0.3.1            pillar_1.4.4         haven_2.3.1          foreign_0.8-80      
    ##  [92] mgcv_1.8-31          abind_1.4-5          tibble_3.0.1         future.apply_1.6.0   crayon_1.3.4         car_3.0-8            uuid_0.1-4          
    ##  [99] plotly_4.9.2.1       rmarkdown_2.3        officer_0.3.12       viridis_0.5.1        grid_4.0.2           readxl_1.3.1         data.table_1.12.8   
    ## [106] forcats_0.5.0        digest_0.6.25        xtable_1.8-4         tidyr_1.1.0          httpuv_1.5.4         numDeriv_2016.8-1.1  R.utils_2.9.2       
    ## [113] signal_0.7-6         munsell_0.5.0        viridisLite_0.3.0
