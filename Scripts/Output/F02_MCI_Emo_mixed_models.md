F02\_MCI\_Emo\_mixed\_models.R
================
kirstenstark
2020-07-03

``` r
### MCI EMO MIXED MODELS SCRIPT ###

# Computes linear mixed-effects regression models with simple contrast coding for the fixed effects
# of semantics and emotional context. Thus, in each model, the estimate of the intercept is the
# grand mean, while the estimates of the slopes contrast "treatment" levels to their respective
# reference levels (semantics: violation - intuitive, mci - intuitive; emotional context (negative 
# - neutral).
#
# The maximal random effects structure is used with all by-participant and by-item random slopes and
# random intercepts. Correlations between random effects are removed if the model fails two converge
# with two different numerical optimizers. Planned follow-up contrasts are computed for the main
# effects and the effects of semantics separately within each type of emotional context.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(MASS)         # version 7.3-51.6
library(lme4)         # version 1.1-23
library(lmerTest)     # version 3.1-2
library(afex)         # version 0.27-2
library(emmeans)      # version 1.4.8

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 <- na.omit(a1[!a1$error,])

# Log-transform reaction times
a1$logRT <- log(a1$BildRT)

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
```

    ## Warning: Model failed to converge with 1 negative eigenvalue: -1.6e+00

``` r
# LMM for arousal ratings (converged after changing the optimizer + removing correlations between REs)
mod.aroursal <- lmer_alt(ArousalResp ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                     data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related N400 (converged on first attempt)
mod.N400.verb <- lmer(N400.verb ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                 data = a1, control = lmerControl(calc.derivs = FALSE))
```

    ## Warning: Model failed to converge with 4 negative eigenvalues: -4.9e+00 -5.7e+01 -1.0e+02
    ## -1.3e+02

``` r
# LMM for picture-related N400 (converged after changing the optimizer)
mod.N400.pict <- lmer(N400.pict ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                 data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Create list of models
models <- list(mod.valence, mod.aroursal, mod.N400.verb, mod.N400.pict)

# F-tests (type III tests)
(tests <- lapply(models, anova))
```

    ## [[1]]
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
    ## semantics          0.007   0.003     2 60.039   0.0089    0.9912    
    ## context           64.822  64.822     1 37.863 164.1031 2.444e-15 ***
    ## semantics:context  0.013   0.006     2 60.458   0.0162    0.9839    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [[2]]
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## semantics          0.131   0.065     2 65.905  0.1205    0.8867    
    ## context           45.853  45.853     1 37.924 84.5306 3.408e-11 ***
    ## semantics:context  0.054   0.027     2 87.908  0.0494    0.9518    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [[3]]
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                    Sum Sq Mean Sq NumDF  DenDF F value   Pr(>F)   
    ## semantics         248.272 124.136     2 63.185  7.0927 0.001664 **
    ## context             0.332   0.332     1 35.083  0.0190 0.891271   
    ## semantics:context  43.824  21.912     2 69.695  1.2520 0.292294   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [[4]]
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                    Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
    ## semantics          23.538  11.769     2 37.018  0.7279 0.48970  
    ## context             0.085   0.085     1 44.146  0.0052 0.94259  
    ## semantics:context 125.889  62.944     2 52.116  3.8931 0.02656 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## POST-HOC CONTRASTS ## --------------------------------------------------------------------------

# Allow emmeans to compute Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics <- lapply(models, function(x){emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## [[1]]
    ## $emmeans
    ##  semantics emmean     SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -0.482 0.0614 78.3   -0.632   -0.332 -7.853  <.0001 
    ##  vio       -0.473 0.0617 80.9   -0.624   -0.322 -7.658  <.0001 
    ##  mci       -0.477 0.0618 80.5   -0.628   -0.326 -7.718  <.0001 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  0.00923 0.0694 60.2   -0.150    0.169 0.133   1.0000 
    ##  mci - int  0.00477 0.0676 60.0   -0.151    0.160 0.071   1.0000 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## 
    ## [[2]]
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
    ## [[3]]
    ## $emmeans
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -0.438 0.189 31.6   -0.916   0.0393 -2.320  0.0809 
    ##  vio       -0.467 0.197 33.1   -0.965   0.0314 -2.363  0.0725 
    ##  mci       -0.821 0.180 33.4   -1.274  -0.3687 -4.574  0.0002 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0283 0.131 61.4   -0.329    0.273 -0.216  1.0000 
    ##  mci - int  -0.3830 0.114 66.0   -0.643   -0.123 -3.374  0.0025 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## 
    ## [[4]]
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

    ## [[1]]
    ## $emmeans
    ##  context emmean     SE   df lower.CL upper.CL t.ratio p.value
    ##  neu      0.227 0.0515 67.4    0.109    0.346   4.415 0.0001 
    ##  neg     -1.182 0.0890 37.2   -1.390   -0.974 -13.278 <.0001 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate   SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu    -1.41 0.11 37.9    -1.63    -1.19 -12.810 <.0001 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## 
    ## [[2]]
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
    ## [[3]]
    ## $emmeans
    ##  context emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  neu     -0.583 0.189 31.4   -1.028   -0.137 -3.080  0.0086 
    ##  neg     -0.568 0.175 30.1   -0.982   -0.155 -3.241  0.0058 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $contrasts
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0142 0.103 35.1   -0.195    0.223 0.138   0.8913 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## 
    ## [[4]]
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
# Follow-up contrasts for semantics nested within contexts
(means.nested <- lapply(models, function(x){emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")}))
```

    ## [[1]]
    ## $emmeans
    ## context = neu:
    ##  semantics emmean     SE   df lower.CL upper.CL t.ratio p.value
    ##  int        0.223 0.0740 80.7   0.0416    0.404   3.007 0.0106 
    ##  vio        0.237 0.0759 82.1   0.0515    0.423   3.122 0.0074 
    ##  mci        0.223 0.0772 82.3   0.0341    0.411   2.885 0.0150 
    ## 
    ## context = neg:
    ##  semantics emmean     SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -1.187 0.0984 48.5  -1.4305   -0.943 -12.060 <.0001 
    ##  vio       -1.183 0.1011 49.0  -1.4332   -0.932 -11.698 <.0001 
    ##  mci       -1.177 0.0957 52.7  -1.4139   -0.940 -12.297 <.0001 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 3 estimates 
    ## P value adjustment: bonferroni method for 3 tests 
    ## 
    ## $contrasts
    ## context = neu:
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int 0.014481 0.0960 60.3   -0.206    0.235 0.151   1.0000 
    ##  mci - int 0.000147 0.0919 60.3   -0.211    0.211 0.002   1.0000 
    ## 
    ## context = neg:
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int 0.003983 0.0729 59.9   -0.164    0.171 0.055   1.0000 
    ##  mci - int 0.009388 0.0754 60.1   -0.164    0.183 0.125   1.0000 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## 
    ## [[2]]
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
    ## [[3]]
    ## $emmeans
    ## context = neu:
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -0.349 0.222 34.5   -0.908   0.2101 -1.570  0.3763 
    ##  vio       -0.519 0.238 35.8   -1.118   0.0799 -2.177  0.1085 
    ##  mci       -0.880 0.185 37.0   -1.344  -0.4151 -4.748  0.0001 
    ## 
    ## context = neg:
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int       -0.528 0.189 30.5   -1.008  -0.0478 -2.786  0.0273 
    ##  vio       -0.414 0.186 32.5   -0.884   0.0559 -2.224  0.0996 
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
    ##  vio - int   -0.170 0.193 63.7   -0.613    0.273 -0.882  0.7622 
    ##  mci - int   -0.531 0.156 97.9   -0.887   -0.175 -3.395  0.0020 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.113 0.157 69.2   -0.247    0.474  0.721  0.9463 
    ##  mci - int   -0.235 0.160 70.6   -0.601    0.130 -1.474  0.2897 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## 
    ## [[4]]
    ## $emmeans
    ## context = neu:
    ##  semantics emmean    SE   df lower.CL upper.CL t.ratio p.value
    ##  int        -2.67 0.313 31.5    -3.46    -1.87 -8.524  <.0001 
    ##  vio        -2.70 0.326 31.4    -3.53    -1.88 -8.292  <.0001 
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
    ##  vio - int  -0.0363 0.163 34.4   -0.419  0.346627 -0.222  1.0000 
    ##  mci - int  -0.4060 0.175 47.1   -0.811 -0.000842 -2.320  0.0494 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE   df lower.CL  upper.CL t.ratio p.value
    ##  vio - int   0.1824 0.178 43.2   -0.230  0.594853  1.027  0.6204 
    ##  mci - int   0.1645 0.170 35.8   -0.233  0.561700  0.969  0.6778 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Backup results
save(models, tests, means.semantics, means.context, means.nested, file = "EEG/export/stats.RData")

# SessionInfo
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
    ## [1] de_DE.UTF-8/de_DE.UTF-8/de_DE.UTF-8/C/de_DE.UTF-8/de_DE.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] afex_0.27-2    lmerTest_3.1-2 lme4_1.1-23    Matrix_1.2-18  MASS_7.3-51.6 
    ## [6] emmeans_1.4.8 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.4.6        mvtnorm_1.1-1       lattice_0.20-41     digest_0.6.25      
    ##  [5] R6_2.4.1            cellranger_1.1.0    plyr_1.8.6          evaluate_0.14      
    ##  [9] ggplot2_3.3.2       highr_0.8           pillar_1.4.4        rlang_0.4.6        
    ## [13] curl_4.3            readxl_1.3.1        rstudioapi_0.11     minqa_1.2.4        
    ## [17] data.table_1.12.8   car_3.0-8           nloptr_1.2.2.1      rmarkdown_2.3      
    ## [21] splines_4.0.2       statmod_1.4.34      stringr_1.4.0       foreign_0.8-80     
    ## [25] munsell_0.5.0       compiler_4.0.2      numDeriv_2016.8-1.1 xfun_0.15          
    ## [29] pkgconfig_2.0.3     htmltools_0.5.0     tibble_3.0.1        rio_0.5.16         
    ## [33] crayon_1.3.4        grid_4.0.2          nlme_3.1-148        xtable_1.8-4       
    ## [37] gtable_0.3.0        lifecycle_0.2.0     magrittr_1.5        scales_1.1.1       
    ## [41] zip_2.0.4           estimability_1.3    stringi_1.4.6       carData_3.0-4      
    ## [45] reshape2_1.4.4      ellipsis_0.3.1      vctrs_0.3.1         boot_1.3-25        
    ## [49] openxlsx_4.1.5      tools_4.0.2         forcats_0.5.0       glue_1.4.1         
    ## [53] hms_0.5.3           abind_1.4-5         parallel_4.0.2      yaml_2.2.1         
    ## [57] colorspace_1.4-1    knitr_1.29          haven_2.3.1
