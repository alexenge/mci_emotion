F05\_MCI\_Emo\_descriptives.R
================
alexander
2020-07-03

``` r
### MCI EMO DESCRIPTIVES SCRIPT ###

# Gives descriptive information about the number of (a) rejected ERP epochs and (b) errors or
# unrealistically short reaction times per participant. For the latter, an ANOVA also checks whether
# the number of errors differs between experimental conditions.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(Rmisc)     # version 1.5
```

    ## Loading required package: lattice

    ## Loading required package: plyr

``` r
# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Add a column for rejected ERPs
a1$rejected <- is.na(a1$N400.verb) | is.na(a1$N400.pict)

# Check number of rejected ERPs participant (mean, percent, median, min, max)
mean(table(a1$rejected, a1$participant)[2,])
```

    ## [1] 3.6

``` r
mean(table(a1$rejected, a1$participant)[2,])/366
```

    ## [1] 0.009836066

``` r
median(table(a1$rejected, a1$participant)[2,])
```

    ## [1] 1

``` r
min(table(a1$rejected, a1$participant)[2,])
```

    ## [1] 0

``` r
max(table(a1$rejected, a1$participant)[2,])
```

    ## [1] 27

``` r
# Remove rejected ERPs from the data
a1 <- a1[!a1$rejected,]

# Check number of errors per participant (mean, percent, median, min, max)
mean(table(a1$error, a1$participant)[2,])
```

    ## [1] 23.86667

``` r
mean(table(a1$error, a1$participant)[2,])/366
```

    ## [1] 0.06520947

``` r
median(table(a1$error, a1$participant)[2,])
```

    ## [1] 20

``` r
min(table(a1$error, a1$participant)[2,])
```

    ## [1] 5

``` r
max(table(a1$error, a1$participant)[2,])
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
