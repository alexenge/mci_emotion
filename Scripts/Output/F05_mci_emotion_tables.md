F05\_mci\_emotion\_tables.R
================
kirstenstark
2020-09-15

``` r
### MCI EMO TABLES SCRIPT ###

# Creates a table for the output of our four linear mixed-effects models. The upper half of
# the table includes ANOVA-style type III tests (F-tests), the bottom half contains planned
# follow-up contrasts. For the F-tests, F-values, degrees of freedom, and p-values are
# printed, whereas for the contrasts, regression estimates, 95% confidence intervals, and
# p-values are printed.

## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(Rmisc)        # Version 1.5
library(tidyverse)    # Version 1.3.0
library(magrittr)     # Version 1.5
library(huxtable)     # version 5.0.0

## TABLE 2: MEAN RATINGS ## -----------------------------------------------------------------------

# Load single-trial data
a1 <- readRDS("EEG/export/a1.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 <- na.omit(a1[!a1$error,])

# Adjust range of response scalse
a1$Valence <- a1$ValenzResp + 3
a1$Arousal <- a1$ArousalResp + 3

# Compute mean ratings
tab2 <- summarySEwithin(a1, measurevar = "Valence", withinvars = "context") %>%
  select(m_val = Valence, sd_val = sd) %>%
  bind_cols(summarySEwithin(a1, measurevar = "Arousal", withinvars = "context") %>% 
              select(m_aro = Arousal, sd_aro = sd)) %>%
  set_rownames(c("Neutral", "Negative")) %>%
  huxtable(add_rownames = "", add_colnames = FALSE) %>%
  add_rows(c("Context", "M", "SD", "M", "SD"), after = 0) %>%
  add_rows(c("", "Valence Rating", "", "Arousal Rating", ""), after = 0)

# Output as markdown for GitHub
suppressWarnings(print_md(tab2, max_width = Inf))
```

|          | Valence Rating |      | Arousal Rating |      |
| -------- | -------------- | ---- | -------------- | ---- |
| Context  | M              | SD   | M              | SD   |
| Neutral  | 3.24           | 1.18 | 2.37           | 1.52 |
| Negative | 1.82           | 1.21 | 3.42           | 1.61 |

``` r
# Save as Word document
tab2 %>% quick_docx(file = "EEG/tables/table_ratings.docx", open = FALSE)

## TABLE 3: LMMS FOR N400 ## ----------------------------------------------------------------------

# Load output of linear mixed-effects models
load("EEG/export/stats.RData")

# Extract a table for the F tests for each model (columns: F value (df), p-value)
anovas <- lapply(tests[c("N400.VERB", "N400.PICT")], function(x){
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
conts <- lapply(means.nested[c("N400.VERB", "N400.PICT")], function(x){
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
tab3 <- rbind(anovas, conts)

# Add model names (dependent variables) as the first row
tab3 <- rbind(c("Verb-Related N400", "", "Picture-Related N400", ""), tab3)

# Add a stub column
tab3 <- cbind(c("", "**Model output**", "Semantics", "Context", "Semantics × context",
                "**Planned contrasts**", "Vio. - int.<br/>(neutral)", "MCI - int.<br/>(neutral)",
                "Vio. - int.<br/>(negative)", "MCI - int.<br/>(negative)"), tab3)

# Remove old column names
names(tab3) <- NULL

# Create a huxtable and output as markdown
huxt <- huxtable(tab3, add_colnames = FALSE)
print_md(huxt, max_width = Inf)
```

|                            | Verb-Related N400           |         | Picture-Related N400       |         |
| -------------------------- | :-------------------------- | ------- | :------------------------- | ------- |
| **Model output**           | ***F*** (***df***)          | ***p*** | ***F*** (***df***)         | ***p*** |
| Semantics                  | 8.26<br/>(2, 101.9)         | \< .001 | 0.73<br/>(2, 37.0)         | 0.490   |
| Context                    | 0.02<br/>(1, 24.3)          | 0.888   | 0.01<br/>(1, 44.1)         | 0.942   |
| Semantics × context        | 1.20<br/>(2, 71.7)          | 0.307   | 3.89<br/>(2, 52.1)         | 0.027   |
| **Planned contrasts**      | **Est. \[95% CI\]**         | ***p*** | **Est. \[95% CI\]**        | ***p*** |
| Vio. - int.<br/>(neutral)  | \-0.17<br/>\[-0.54, 0.20\]  | 0.579   | \-0.04<br/>\[-0.42, 0.35\] | 1.000   |
| MCI - int.<br/>(neutral)   | \-0.53<br/>\[-0.87, -0.19\] | 0.001   | \-0.41<br/>\[-0.81, 0.00\] | 0.049   |
| Vio. - int.<br/>(negative) | 0.12<br/>\[-0.25, 0.49\]    | 0.956   | 0.18<br/>\[-0.23, 0.59\]   | 0.621   |
| MCI - int.<br/>(negative)  | \-0.24<br/>\[-0.58, 0.10\]  | 0.234   | 0.16<br/>\[-0.23, 0.56\]   | 0.678   |

``` r
# Export as a word file (after some re-formatting)
tab3_word <- data.frame(lapply(tab3, function(x){gsub("<br/>", "\n", x)}))
tab3_word <- data.frame(lapply(tab3_word, function(x){gsub("\\*|\\_", "", x)}))
huxt_word <- huxtable(tab3_word, add_colnames = FALSE)
quick_docx(huxt_word, file = "EEG/tables/table_N400.docx", open = FALSE)

## TABLE A1: LMM FOR P600 ## ----------------------------------------------------------------------
# Load output from mixed models
load("EEG/export/stats_appendix.RData")

# Extract a table for the F tests for each model (columns: F value (df), p-value)
anovas <- lapply(tests_app, function(x){
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
conts <- lapply(means.nested_app, function(x){
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
tab <- rbind(c("Verb-Related P600", ""), tab)

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

|                            | Verb-Related P600          |         |
| -------------------------- | :------------------------- | ------- |
| **Model output**           | ***F*** (***df***)         | ***p*** |
| Semantics                  | 1.10<br/>(2, 103.6)        | 0.335   |
| Context                    | 0.04<br/>(1, 29.6)         | 0.847   |
| Semantics × context        | 0.72<br/>(2, 62.2)         | 0.492   |
| **Planned contrasts**      | **Est. \[95% CI\]**        | ***p*** |
| Vio. - int.<br/>(neutral)  | \-0.02<br/>\[-0.37, 0.33\] | 1.000   |
| MCI - int.<br/>(neutral)   | \-0.11<br/>\[-0.50, 0.27\] | 1.000   |
| Vio. - int.<br/>(negative) | 0.24<br/>\[-0.11, 0.58\]   | 0.254   |
| MCI - int.<br/>(negative)  | \-0.04<br/>\[-0.43, 0.34\] | 1.000   |

``` r
# Export as a word file (after some re-formatting)
tab_word <- data.frame(lapply(tab, function(x){gsub("<br/>", "\n", x)}))
tab_word <- data.frame(lapply(tab_word, function(x){gsub("\\*|\\_", "", x)}))
huxt_word <- huxtable(tab_word, add_colnames = FALSE)
quick_docx(huxt_word, file = "EEG/tables/lmm_table_appendixP600_500-900ms.docx", open = FALSE)
```

``` r
# ## INTERACTION EFFECTS ## -------------------------------------------------------------------------
# 
# # Checking the MCI-intuitive x context and SEV-intuitive x context interactions separetely (verb)
# summary(models$N400.VERB)$coefficients %>%
#   set_colnames(c("Est.", "SE", "df", "t", "p")) %>%
#   set_rownames(c("(Intercept)", "Semantics: Vio. - int.", "Semantics: MCI - int", "Context",
#                  "(Vio. - int.) × context", "(MCI - int.) × context")) %>%
#   huxtable(add_rownames = "Verb-related N400") %>% add_colnames() %>%
#   set_number_format(value = "%3.3f") %>%
#   quick_docx(file = "EEG/tables/table_ias_verb.docx", open = FALSE)
# 
# # Checking the MCI-intuitive x context and SEV-intuitive x context interactions separetely (verb)
# summary(models$N400.PICT)$coefficients %>%
#   set_colnames(c("Est.", "SE", "df", "t", "p")) %>%
#   set_rownames(c("(Intercept)", "Semantics: Vio. - int.", "Semantics: MCI - int", "Context",
#                  "(Vio. - int.) × context", "(MCI - int.) × context")) %>%
#   huxtable(add_rownames = "Picture-related N400") %>% add_colnames() %>%
#   set_number_format(value = "%3.3f") %>%
#   quick_docx(file = "EEG/tables/table_ias_pict.docx", open = FALSE)


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
    ##   [1] minqa_1.2.4         colorspace_1.4-1    ellipsis_0.3.1      rio_0.5.16         
    ##   [5] flextable_0.5.11    estimability_1.3    base64enc_0.1-3     fs_1.4.2           
    ##   [9] rstudioapi_0.11     listenv_0.8.0       R.matlab_3.6.2      fansi_0.4.1        
    ##  [13] mvtnorm_1.1-1       lubridate_1.7.9     xml2_1.3.2          codetools_0.2-16   
    ##  [17] splines_4.0.2       R.methodsS3_1.8.0   knitr_1.29          eegUtils_0.5.0.9000
    ##  [21] afex_0.27-2         jsonlite_1.7.0      nloptr_1.2.2.2      broom_0.7.0.9001   
    ##  [25] dbplyr_1.4.4        R.oo_1.23.0         shiny_1.5.0         compiler_4.0.2     
    ##  [29] httr_1.4.2          emmeans_1.4.8       backports_1.1.8     assertthat_0.2.1   
    ##  [33] Matrix_1.2-18       fastmap_1.0.1       lazyeval_0.2.2      cli_2.0.2          
    ##  [37] later_1.1.0.1       htmltools_0.5.0     tools_4.0.2         lmerTest_3.1-2     
    ##  [41] gtable_0.3.0        glue_1.4.1          reshape2_1.4.4      Rcpp_1.0.5         
    ##  [45] carData_3.0-4       cellranger_1.1.0    vctrs_0.3.2         nlme_3.1-148       
    ##  [49] xfun_0.16           globals_0.12.5      openxlsx_4.1.5      lme4_1.1-23        
    ##  [53] rvest_0.3.5         mime_0.9            miniUI_0.1.1.1      lifecycle_0.2.0    
    ##  [57] renv_0.12.0         statmod_1.4.34      future_1.18.0       MASS_7.3-51.6      
    ##  [61] scales_1.1.1        hms_0.5.3           promises_1.1.1      parallel_4.0.2     
    ##  [65] yaml_2.2.1          curl_4.3            gdtools_0.2.2       stringi_1.4.6      
    ##  [69] highr_0.8           boot_1.3-25         zip_2.1.1           cpp11_0.2.1        
    ##  [73] commonmark_1.7      systemfonts_0.3.1   rlang_0.4.7         pkgconfig_2.0.3    
    ##  [77] matrixStats_0.56.0  pracma_2.2.9        evaluate_0.14       htmlwidgets_1.5.1  
    ##  [81] tidyselect_1.1.0    R6_2.4.1            generics_0.0.2      ini_0.3.1          
    ##  [85] DBI_1.1.0           withr_2.2.0         pillar_1.4.6        haven_2.3.1        
    ##  [89] foreign_0.8-80      mgcv_1.8-31         abind_1.4-5         future.apply_1.6.0 
    ##  [93] modelr_0.1.8        crayon_1.3.4        car_3.0-8           uuid_0.1-4         
    ##  [97] plotly_4.9.2.1      officer_0.3.14      rmarkdown_2.3       grid_4.0.2         
    ## [101] readxl_1.3.1        data.table_1.13.0   blob_1.2.1          reprex_0.3.0       
    ## [105] digest_0.6.25       xtable_1.8-4        httpuv_1.5.4        numDeriv_2016.8-1.1
    ## [109] R.utils_2.9.2       signal_0.7-6        munsell_0.5.0       viridisLite_0.3.0
