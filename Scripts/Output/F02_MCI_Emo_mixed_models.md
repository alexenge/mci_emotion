F02\_MCI\_Emo\_mixed\_models.R
================
alexander
2020-07-03

``` r
### MCI EMO MIXED MODELS SCRIPT ###

# Computes linear mixed-effects regression models with simple contrast coding for the fixed effects
# of semantics and emotional context. Thus, in each model, the estimate of the intercept is the
# grand mean, while the estimates of the slopes contrast "treatment" levels to their respective
# reference levels (semantics: violation - intuitive, mci - intuitive; emotional context (negative 
# - neutral). The maximal random effects structure is used with all by-participant and by-item random 
# slopes and random intercepts. Correlations between random effects are removed if the model fails two 
# converge with two different numerical optimizers. Planned follow-up contrasts are computed for the
# main effects and the effects of semantics separately within each type of emotional context.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(MASS)         # version 7.3-51.6
library(lme4)         # version 1.1-23
```

    ## Loading required package: Matrix

``` r
library(lmerTest)     # version 3.1-2
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(afex)         # version 0.27-2
```

    ## Registered S3 methods overwritten by 'car':
    ##   method                          from
    ##   influence.merMod                lme4
    ##   cooks.distance.influence.merMod lme4
    ##   dfbeta.influence.merMod         lme4
    ##   dfbetas.influence.merMod        lme4

    ## ************
    ## Welcome to afex. For support visit: http://afex.singmann.science/

    ## - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
    ## - Methods for calculating p-values with mixed(): 'KR', 'S', 'LRT', and 'PB'
    ## - 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests
    ## - NEWS: library('emmeans') now needs to be called explicitly!
    ## - Get and set global package options with: afex_options()
    ## - Set orthogonal sum-to-zero contrasts globally: set_sum_contrasts()
    ## - For example analyses see: browseVignettes("afex")
    ## ************

    ## 
    ## Attaching package: 'afex'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

``` r
library(emmeans)      # version 1.4.8

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 <- na.omit(a1[!a1$error,])

# Define simple contrast coding for context emotionality (negative - neutral)
    # HO(Intercept): (mu1+mu2)/2 = 0 <-> mu1+mu2 = 0
    # H0(Slope): -mu1 + mu2 = 0
    # with mu1 = mean of the neutral contexts and mu2 = mean of the neg contexts
t(contrasts.context <- t(cbind(c("neu" = -1, "neg" = 1))))
```

    ##     [,1]
    ## neu   -1
    ## neg    1

``` r
contrasts(a1$context) <- ginv(contrasts.context)

# Define simple contrast coding for semantics (violation - intuitive, mci - intuitive)
    # H0(Intercept): (mu1+mu2+mu3)/3 = 0 <-> mu1+mu2+mu3 = 0
    # H0(Slope1): -1*mu1 +1*mu2 + 0*mu3 = 0
    # H0(Slope2): -1*mu1 +0*mu2 + 1*mu3 = 0
    # with mu1 = mean of intuitive concepts, mu2 = mean of violations, mu3 = mean of MCIs
t(contrasts.semantics <- t(cbind(c("int" = -1, "vio" = 1, "mci" = 0),
                                 c("int" = -1, "vio" = 0, "mci" = 1))))
```

    ##     [,1] [,2]
    ## int   -1   -1
    ## vio    1    0
    ## mci    0    1

``` r
contrasts(a1$semantics) <- ginv(contrasts.semantics)

## LINEAR MIXED-EFFECTS MODELS ## -----------------------------------------------------------------

# LMM for valence ratings (converged on first attempt)
mod.valence <- lmer(ValenzResp ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                 data = a1, control = lmerControl(calc.derivs = FALSE))

# LMM for arousal ratings (converged after changing the optimizer + removing correlations between REs)
mod.aroursal <- lmer_alt(ArousalResp ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                     data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related N400 (converged on first attempt)
mod.N400.verb <- lmer(N400.verb ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                 data = a1, control = lmerControl(calc.derivs = FALSE))

# LMM for picture-related N400 (converged after changing the optimizer)
mod.N400.pict <- lmer(N400.pict ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                 data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Create list of models
models <- list("VALENCE" = mod.valence, "AROUSAL" = mod.aroursal, "N400.VERB" = mod.N400.verb, "N400.PICT" = mod.N400.pict)

# F-tests (type III tests)
(tests <- lapply(models, anova))
```

    ## $VALENCE
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
    ## semantics          0.007   0.003     2 60.057   0.0088    0.9912    
    ## context           64.894  64.894     1 37.997 164.2915 2.263e-15 ***
    ## semantics:context  0.013   0.006     2 60.419   0.0162    0.9839    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $AROUSAL
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## semantics          0.131   0.065     2 65.905  0.1205    0.8867    
    ## context           45.853  45.853     1 37.924 84.5312 3.408e-11 ***
    ## semantics:context  0.054   0.027     2 87.907  0.0494    0.9518    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $N400.VERB
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                    Sum Sq Mean Sq NumDF  DenDF F value   Pr(>F)   
    ## semantics         248.274 124.137     2 63.107  7.0927 0.001665 **
    ## context             0.332   0.332     1 34.906  0.0190 0.891227   
    ## semantics:context  43.824  21.912     2 69.569  1.2520 0.292305   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $N400.PICT
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                    Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
    ## semantics          23.539  11.769     2 37.017  0.7279 0.48970  
    ## context             0.085   0.085     1 44.145  0.0052 0.94259  
    ## semantics:context 125.890  62.945     2 52.115  3.8931 0.02656 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## PLANNED FOLLOW-UP CONTRASTS ## -----------------------------------------------------------------

# Allow emmeans to compute Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics <- lapply(models, function(x){emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")}))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $VALENCE
    ## $emmeans
    ##  semantics emmean     SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -0.482 0.0614 78.3   -0.632   -0.332 -7.854  <.0001 
    ##  vio       -0.473 0.0617 80.9   -0.624   -0.322 -7.658  <.0001 
    ##  mci       -0.477 0.0618 80.6   -0.628   -0.326 -7.719  <.0001 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  0.00922 0.0694 60.2   -0.150    0.169 0.133   1.0000 
    ##  mci - int  0.00478 0.0676 60.2   -0.151    0.160 0.071   1.0000 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## 
    ## $AROUSAL
    ## $emmeans
    ##  semantics  emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -0.1138 0.132 32.8   -0.446    0.219 -0.863  1.0000 
    ##  vio       -0.0884 0.135 35.8   -0.427    0.250 -0.656  1.0000 
    ##  mci       -0.1117 0.134 34.9   -0.449    0.225 -0.834  1.0000 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  0.02541 0.0518 60.5  -0.0936    0.144 0.491   1.0000 
    ##  mci - int  0.00209 0.0449 60.1  -0.1012    0.105 0.046   1.0000 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## 
    ## $N400.VERB
    ## $emmeans
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -0.438 0.189 31.6   -0.916   0.0392 -2.320  0.0808 
    ##  vio       -0.467 0.197 33.2   -0.964   0.0312 -2.363  0.0724 
    ##  mci       -0.821 0.180 33.5   -1.274  -0.3687 -4.574  0.0002 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0283 0.131 61.3   -0.329    0.272 -0.216  1.0000 
    ##  mci - int  -0.3830 0.114 65.9   -0.643   -0.123 -3.374  0.0025 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## 
    ## $N400.PICT
    ## $emmeans
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int        -2.80 0.310 30.4    -3.59    -2.02 -9.037  <.0001 
    ##  vio        -2.73 0.312 32.4    -3.52    -1.94 -8.727  <.0001 
    ##  mci        -2.92 0.306 31.5    -3.69    -2.15 -9.548  <.0001 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.073 0.129 35.5   -0.228    0.374  0.568  1.0000 
    ##  mci - int   -0.121 0.138 37.7   -0.444    0.202 -0.872  0.7774 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Follow-up contrasts for the main effect of context
(means.context <- lapply(models, function(x){emmeans(x, trt.vs.ctrl ~ context, infer = TRUE, adjust = "bonferroni")}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $VALENCE
    ## $emmeans
    ##  context emmean     SE   df lower.CL upper.CL t.ratio p.value
    ##  neu      0.227 0.0515 67.5    0.109    0.346   4.413 0.0001 
    ##  neg     -1.182 0.0890 37.3   -1.390   -0.974 -13.287 <.0001 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate   SE df lower.CL upper.CL t.ratio p.value
    ##  neg - neu    -1.41 0.11 38    -1.63    -1.19 -12.818 <.0001 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## 
    ## $AROUSAL
    ## $emmeans
    ##  context emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  neu     -0.626 0.142 42.4   -0.955   -0.297 -4.424  0.0001 
    ##  neg      0.417 0.142 42.4    0.088    0.746  2.946  0.0104 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu     1.04 0.113 37.9    0.813     1.27 9.194   <.0001 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## 
    ## $N400.VERB
    ## $emmeans
    ##  context emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  neu     -0.583 0.189 31.4   -1.028   -0.137 -3.080  0.0086 
    ##  neg     -0.568 0.175 30.1   -0.982   -0.155 -3.242  0.0058 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0142 0.103 34.9   -0.195    0.223 0.138   0.8912 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## 
    ## $N400.PICT
    ## $emmeans
    ##  context emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  neu      -2.81 0.300 29.7    -3.52     -2.1 -9.369  <.0001 
    ##  neg      -2.82 0.304 30.5    -3.54     -2.1 -9.277  <.0001 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu -0.00704 0.0972 44.1   -0.203    0.189 -0.072  0.9426 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95

``` r
# Follow-up contrasts for semantics within each contexts
(means.nested <- lapply(models, function(x){emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")}))
```

    ## $VALENCE
    ## $emmeans
    ## context = neu:
    ##  semantics emmean     SE   df lower.CL upper.CL t.ratio p.value
    ##  int        0.223 0.0737 81.0   0.0423    0.403   3.019 0.0102 
    ##  vio        0.237 0.0763 81.9   0.0506    0.423   3.107 0.0078 
    ##  mci        0.223 0.0772 82.4   0.0341    0.411   2.885 0.0150 
    ## 
    ## context = neg:
    ##  semantics emmean     SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -1.187 0.0986 48.5  -1.4310   -0.942 -12.037 <.0001 
    ##  vio       -1.183 0.1008 49.3  -1.4324   -0.933 -11.731 <.0001 
    ##  mci       -1.177 0.0957 52.8  -1.4138   -0.941 -12.303 <.0001 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ## context = neu:
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int 0.014478 0.0960 60.3   -0.206    0.235 0.151   1.0000 
    ##  mci - int 0.000172 0.0919 60.4   -0.211    0.211 0.002   1.0000 
    ## 
    ## context = neg:
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int 0.003969 0.0728 59.8   -0.164    0.171 0.054   1.0000 
    ##  mci - int 0.009384 0.0754 60.1   -0.164    0.183 0.124   1.0000 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## 
    ## $AROUSAL
    ## $emmeans
    ## context = neu:
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -0.638 0.145 46.6  -0.9985   -0.278 -4.404  0.0002 
    ##  vio       -0.616 0.149 52.1  -0.9854   -0.247 -4.132  0.0004 
    ##  mci       -0.624 0.149 51.4  -0.9916   -0.256 -4.195  0.0003 
    ## 
    ## context = neg:
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int        0.411 0.145 46.6   0.0509    0.771  2.835  0.0203 
    ##  vio        0.440 0.149 52.1   0.0707    0.809  2.947  0.0144 
    ##  mci        0.400 0.149 51.4   0.0324    0.768  2.693  0.0286 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ## context = neu:
    ##  contrast  estimate     SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.0221 0.0658 115   -0.127    0.172  0.336  1.0000 
    ##  mci - int   0.0148 0.0623 121   -0.127    0.156  0.238  1.0000 
    ## 
    ## context = neg:
    ##  contrast  estimate     SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.0287 0.0658 115   -0.121    0.178  0.436  1.0000 
    ##  mci - int  -0.0107 0.0623 121   -0.152    0.131 -0.171  1.0000 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## 
    ## $N400.VERB
    ## $emmeans
    ## context = neu:
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -0.349 0.222 34.5   -0.908   0.2101 -1.571  0.3762 
    ##  vio       -0.519 0.238 35.9   -1.118   0.0797 -2.177  0.1084 
    ##  mci       -0.880 0.185 37.0   -1.344  -0.4151 -4.748  0.0001 
    ## 
    ## context = neg:
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -0.528 0.189 30.4   -1.008  -0.0478 -2.786  0.0273 
    ##  vio       -0.414 0.186 32.6   -0.884   0.0558 -2.225  0.0995 
    ##  mci       -0.763 0.219 32.5   -1.316  -0.2105 -3.486  0.0043 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ## context = neu:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   -0.170 0.193 63.4   -0.613    0.273 -0.882  0.7621 
    ##  mci - int   -0.531 0.156 98.1   -0.887   -0.175 -3.396  0.0020 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.113 0.157 69.1   -0.247    0.474  0.721  0.9464 
    ##  mci - int   -0.235 0.160 70.4   -0.601    0.130 -1.474  0.2896 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## 
    ## $N400.PICT
    ## $emmeans
    ## context = neu:
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int        -2.67 0.313 31.5    -3.46    -1.87 -8.524  <.0001 
    ##  vio        -2.70 0.326 31.3    -3.53    -1.88 -8.292  <.0001 
    ##  mci        -3.07 0.309 30.2    -3.85    -2.29 -9.953  <.0001 
    ## 
    ## context = neg:
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int        -2.94 0.328 30.6    -3.77    -2.10 -8.941  <.0001 
    ##  vio        -2.75 0.315 33.4    -3.55    -1.96 -8.732  <.0001 
    ##  mci        -2.77 0.326 33.5    -3.59    -1.95 -8.492  <.0001 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ## context = neu:
    ##  contrast  estimate    SE   df lower.CL  upper.CL t.ratio p.value
    ##  vio - int  -0.0363 0.163 34.4   -0.419  0.346628 -0.222  1.0000 
    ##  mci - int  -0.4060 0.175 47.1   -0.811 -0.000841 -2.320  0.0494 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE   df lower.CL  upper.CL t.ratio p.value
    ##  vio - int   0.1824 0.178 43.2   -0.230  0.594854  1.027  0.6204 
    ##  mci - int   0.1645 0.170 35.8   -0.233  0.561698  0.969  0.6778 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Backup results
save(models, tests, means.semantics, means.context, means.nested, file = "EEG/export/stats.RData")
```
