F05\_mci\_emotion\_descriptives.R
================
alexander
2020-07-07

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
    ## [1] Rmisc_1.5       plyr_1.8.6      lattice_0.20-41
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.5      digest_0.6.25   grid_4.0.2      magrittr_1.5    evaluate_0.14   highr_0.8       stringi_1.4.6   rlang_0.4.6     rstudioapi_0.11
    ## [10] rmarkdown_2.3   tools_4.0.2     stringr_1.4.0   xfun_0.15       yaml_2.2.1      compiler_4.0.2  htmltools_0.5.0 knitr_1.29
