F02\_mci\_emotion\_mixed\_models\_lag.R
================
kirstenstark
2020-08-10

``` r
### MCI EMO MIXED MODELS SCRIPT - APPENDIX (EFFECT OF LAG1 SEMANTICS) ###

# The expected effect of semantic violations on N400 amplitudes was not found in the main analyses.
# Additional "sequence analyses" were conducted in order to test whether the type of semantic
# violation in the previous trial affected present N400 amplitudes. 
# Script computes linear mixed-effects regression models with simple contrast coding for the fixed 
# effects of semantics, lag1Semantics, and emotional context. Thus, in each model, the estimate 
# of the intercept is the grand mean, while the estimates of the slopes contrast "treatment" levels 
# to their respective reference levels (semantics & lag1Semantics: violation - intuitive, mci - 
# intuitive; emotional context: negative - neutral). The maximal random effects structure is used 
# with all by-participant and by-item random slopes and random intercepts. Correlations between random 
# effects are removed if the model fails two converge with two different numerical optimizers. Planned 
# follow-up contrasts are computed for the main effects and the effects of semantics separately within
# each type of lag1 semantics and emotional context.

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

# Define simple contrast coding for semantics and lag1Semantics
# (violation - intuitive, mci - intuitive)
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
contrasts(a1$lag1Semantics) <- ginv(contrasts.semantics)

## LINEAR MIXED-EFFECTS MODELS ## -----------------------------------------------------------------

# LMM for verb-related N400: Controlling for sequence of trials (lag)
    # same random structure as for mod.N400.verb (main analyses) intended
    # however, model converged only after bobyqa optimizer was used and correlation parameters were excluded
mod.N400.verb.lag <- lmer_alt(N400.verb ~ lag1Semantics*semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # to compare the two models, the N400.verb random structure needs to be identical
mod.N400.verb.comp <- lmer_alt(N400.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                              data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # model comparison
anova(mod.N400.verb.comp, mod.N400.verb.lag)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: data
    ## Models:
    ## mod.N400.verb.comp: N400.verb ~ semantics * context + (1 + re1.semantics1 + re1.semantics2 + 
    ## mod.N400.verb.comp:     re1.context1 + re1.semantics1_by_context1 + re1.semantics2_by_context1 || 
    ## mod.N400.verb.comp:     participant) + (1 + re2.semantics1 + re2.semantics2 + re2.context1 + 
    ## mod.N400.verb.comp:     re2.semantics1_by_context1 + re2.semantics2_by_context1 || 
    ## mod.N400.verb.comp:     item)
    ## mod.N400.verb.lag: N400.verb ~ lag1Semantics * semantics * context + (1 + re1.semantics1 + 
    ## mod.N400.verb.lag:     re1.semantics2 + re1.context1 + re1.semantics1_by_context1 + 
    ## mod.N400.verb.lag:     re1.semantics2_by_context1 || participant) + (1 + re2.semantics1 + 
    ## mod.N400.verb.lag:     re2.semantics2 + re2.context1 + re2.semantics1_by_context1 + 
    ## mod.N400.verb.lag:     re2.semantics2_by_context1 || item)
    ##                    npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
    ## mod.N400.verb.comp   19 58013 58150 -28987    57975                     
    ## mod.N400.verb.lag    31 58026 58250 -28982    57964 10.529 12     0.5696

``` r
# LMM for picture-related N400: Controlling for sequence of trials
    # same random structure as for mod.N400.pict (main analyses) intended
    # however, model converged only after exclusion of correlation parameters
mod.N400.pict.lag <- lmer_alt(N400.pict ~ lag1Semantics*semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # to compare the two models, N400.pict random structure needs to be identical
mod.N400.pict.comp <- lmer_alt(N400.pict ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# model comparison
anova(mod.N400.pict.comp, mod.N400.pict.lag)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: data
    ## Models:
    ## mod.N400.pict.comp: N400.pict ~ semantics * context + (1 + re1.semantics1 + re1.semantics2 + 
    ## mod.N400.pict.comp:     re1.context1 + re1.semantics1_by_context1 + re1.semantics2_by_context1 || 
    ## mod.N400.pict.comp:     participant) + (1 + re2.semantics1 + re2.semantics2 + re2.context1 + 
    ## mod.N400.pict.comp:     re2.semantics1_by_context1 + re2.semantics2_by_context1 || 
    ## mod.N400.pict.comp:     item)
    ## mod.N400.pict.lag: N400.pict ~ lag1Semantics * semantics * context + (1 + re1.semantics1 + 
    ## mod.N400.pict.lag:     re1.semantics2 + re1.context1 + re1.semantics1_by_context1 + 
    ## mod.N400.pict.lag:     re1.semantics2_by_context1 || participant) + (1 + re2.semantics1 + 
    ## mod.N400.pict.lag:     re2.semantics2 + re2.context1 + re2.semantics1_by_context1 + 
    ## mod.N400.pict.lag:     re2.semantics2_by_context1 || item)
    ##                    npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
    ## mod.N400.pict.comp   19 57285 57422 -28623    57247                     
    ## mod.N400.pict.lag    31 57296 57520 -28617    57234 12.696 12     0.3915

``` r
# Create a list of models
models <- list("N400.VERB.LAG" = mod.N400.verb.lag, "N400.PICT.LAG" = mod.N400.pict.lag)

# F-tests (type III tests)
(tests <- lapply(models, anova))
```

    ## $N400.VERB.LAG
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                                  Sum Sq Mean Sq NumDF   DenDF F value
    ## lag1Semantics                    38.315  19.158     2 10073.0  1.0895
    ## semantics                       273.729 136.864     2   299.0  7.7837
    ## context                           2.185   2.185     1    32.7  0.1243
    ## lag1Semantics:semantics           9.432   2.358     4 10062.9  0.1341
    ## lag1Semantics:context            80.997  40.498     2 10055.9  2.3032
    ## semantics:context                33.663  16.831     2    94.9  0.9572
    ## lag1Semantics:semantics:context  51.779  12.945     4 10058.2  0.7362
    ##                                    Pr(>F)    
    ## lag1Semantics                   0.3364154    
    ## semantics                       0.0005066 ***
    ## context                         0.7267273    
    ## lag1Semantics:semantics         0.9698562    
    ## lag1Semantics:context           0.0999915 .  
    ## semantics:context               0.3876354    
    ## lag1Semantics:semantics:context 0.5671346    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $N400.PICT.LAG
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                                  Sum Sq Mean Sq NumDF   DenDF F value   Pr(>F)
    ## lag1Semantics                    28.441  14.220     2 10042.1  0.8764 0.416292
    ## semantics                        35.002  17.501     2    93.6  1.0786 0.344244
    ## context                           0.556   0.556     1    34.2  0.0343 0.854223
    ## lag1Semantics:semantics         112.558  28.139     4 10027.0  1.7343 0.139331
    ## lag1Semantics:context             7.900   3.950     2 10036.5  0.2434 0.783934
    ## semantics:context               171.525  85.762     2    99.8  5.2858 0.006577
    ## lag1Semantics:semantics:context  57.934  14.484     4 10030.0  0.8927 0.467255
    ##                                   
    ## lag1Semantics                     
    ## semantics                         
    ## context                           
    ## lag1Semantics:semantics           
    ## lag1Semantics:context             
    ## semantics:context               **
    ## lag1Semantics:semantics:context   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## PLANNED FOLLOW-UP CONTRASTS ## -----------------------------------------------------------------

# Allow emmeans to compute Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics <- lapply(models,function(x){
    emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $N400.VERB.LAG
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0364 0.127  120   -0.325    0.252 -0.287  1.0000 
    ##  mci - int  -0.4091 0.112 9875   -0.659   -0.159 -3.665  0.0005 
    ## 
    ## Results are averaged over the levels of: lag1Semantics, context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.PICT.LAG
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0364 0.131 55.8   -0.338    0.265 -0.278  1.0000 
    ##  mci - int  -0.2058 0.141 61.5   -0.530    0.119 -1.457  0.3001 
    ## 
    ## Results are averaged over the levels of: lag1Semantics, context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Follow-up contrasts for the main effect of context
(means.context <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $N400.VERB.LAG
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   -0.036 0.102 32.7   -0.244    0.172 -0.352  0.7267 
    ## 
    ## Results are averaged over the levels of: lag1Semantics, semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $N400.PICT.LAG
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu  -0.0178 0.0961 34.2   -0.213    0.177 -0.185  0.8542 
    ## 
    ## Results are averaged over the levels of: lag1Semantics, semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95

``` r
# Follow-up contrasts for the main effect of lag
(means.lag <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ lag1Semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $N400.VERB.LAG
    ##  contrast  estimate    SE    df lower.CL upper.CL t.ratio p.value
    ##  vio - int  0.00276 0.105 10063   -0.233   0.2383  0.026  1.0000 
    ##  mci - int -0.14749 0.105 10079   -0.383   0.0875 -1.407  0.3191 
    ## 
    ## Results are averaged over the levels of: semantics, context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.PICT.LAG
    ##  contrast  estimate    SE    df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.0508 0.101 10035  -0.1757    0.277 0.502   1.0000 
    ##  mci - int   0.1329 0.101 10046  -0.0932    0.359 1.317   0.3754 
    ## 
    ## Results are averaged over the levels of: semantics, context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Follow-up contrasts for semantics within each contexts
(means.nested.context <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $N400.VERB.LAG
    ## context = neu:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   -0.201 0.177 248   -0.600   0.1970 -1.140  0.5108 
    ##  mci - int   -0.505 0.164 213   -0.876  -0.1347 -3.078  0.0047 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.129 0.174 237   -0.265   0.5218  0.737  0.9235 
    ##  mci - int   -0.313 0.164 214   -0.684   0.0584 -1.902  0.1170 
    ## 
    ## Results are averaged over the levels of: lag1Semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.PICT.LAG
    ## context = neu:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   -0.189 0.173 106   -0.583    0.205 -1.092  0.5545 
    ##  mci - int   -0.554 0.177 152   -0.955   -0.153 -3.125  0.0043 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.116 0.171 101   -0.273    0.506  0.680  0.9956 
    ##  mci - int    0.142 0.177 153   -0.259    0.544  0.802  0.8477 
    ## 
    ## Results are averaged over the levels of: lag1Semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Follow-up contrasts for semantics within each lag1 semantic condition
(means.nested.lag <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|lag1Semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $N400.VERB.LAG
    ## lag1Semantics = int:
    ##  contrast  estimate    SE    df lower.CL upper.CL t.ratio p.value
    ##  vio - int -0.02864 0.150   230   -0.367   0.3097 -0.191  1.0000 
    ##  mci - int -0.35166 0.137  9943   -0.660  -0.0437 -2.560  0.0210 
    ## 
    ## lag1Semantics = vio:
    ##  contrast  estimate    SE    df lower.CL upper.CL t.ratio p.value
    ##  vio - int -0.07278 0.224  1063   -0.576   0.4303 -0.325  1.0000 
    ##  mci - int -0.52618 0.219 10013   -1.017  -0.0353 -2.403  0.0326 
    ## 
    ## lag1Semantics = mci:
    ##  contrast  estimate    SE    df lower.CL upper.CL t.ratio p.value
    ##  vio - int -0.00788 0.226  1090   -0.516   0.5004 -0.035  1.0000 
    ##  mci - int -0.34931 0.214 10006   -0.829   0.1303 -1.633  0.2051 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.PICT.LAG
    ## lag1Semantics = int:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.2920 0.152 100  -0.0532    0.637  1.925  0.1141 
    ##  mci - int   0.0516 0.161 103  -0.3143    0.418  0.321  1.0000 
    ## 
    ## lag1Semantics = vio:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.2284 0.221 436  -0.7245    0.268 -1.036  0.6020 
    ##  mci - int  -0.3230 0.230 418  -0.8399    0.194 -1.406  0.3212 
    ## 
    ## lag1Semantics = mci:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.1728 0.223 452  -0.6739    0.328 -0.776  0.8768 
    ##  mci - int  -0.3462 0.225 388  -0.8534    0.161 -1.536  0.2508 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Follow-up contrasts for semantics within each lag1Semantics and each context condition
(means.nested <- lapply(models, function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|lag1Semantics*context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## $N400.VERB.LAG
    ## lag1Semantics = int, context = neu:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.1245 0.207  463   -0.590    0.341 -0.601  1.0000 
    ##  mci - int  -0.5911 0.200  457   -1.041   -0.141 -2.956  0.0066 
    ## 
    ## lag1Semantics = vio, context = neu:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.1637 0.315 2048   -0.869    0.542 -0.520  1.0000 
    ##  mci - int  -0.4889 0.310 2166   -1.185    0.207 -1.575  0.2307 
    ## 
    ## lag1Semantics = mci, context = neu:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.3160 0.325 2248   -1.044    0.412 -0.973  0.6611 
    ##  mci - int  -0.4362 0.308 2149   -1.127    0.255 -1.416  0.3136 
    ## 
    ## lag1Semantics = int, context = neg:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.0672 0.210  485   -0.405    0.539  0.320  1.0000 
    ##  mci - int  -0.1122 0.199  450   -0.560    0.336 -0.564  1.0000 
    ## 
    ## lag1Semantics = vio, context = neg:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.0181 0.315 2104   -0.688    0.724  0.058  1.0000 
    ##  mci - int  -0.5634 0.316 2272   -1.271    0.145 -1.785  0.1488 
    ## 
    ## lag1Semantics = mci, context = neg:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.3002 0.311 1974   -0.398    0.998  0.965  0.6693 
    ##  mci - int  -0.2624 0.304 2031   -0.944    0.419 -0.863  0.7761 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.PICT.LAG
    ## lag1Semantics = int, context = neu:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.2706 0.202  196   -0.186   0.7271  1.339  0.3643 
    ##  mci - int  -0.1146 0.209  287   -0.585   0.3554 -0.549  1.0000 
    ## 
    ## lag1Semantics = vio, context = neu:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.5902 0.304  921   -1.274   0.0934 -1.938  0.1058 
    ##  mci - int  -0.7354 0.309 1257   -1.430  -0.0411 -2.377  0.0352 
    ## 
    ## lag1Semantics = mci, context = neu:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.2480 0.314 1031   -0.953   0.4569 -0.790  0.8599 
    ##  mci - int  -0.8119 0.307 1234   -1.501  -0.1226 -2.643  0.0166 
    ## 
    ## lag1Semantics = int, context = neg:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.3134 0.205  205   -0.149   0.7757  1.531  0.2548 
    ##  mci - int   0.2179 0.208  284   -0.250   0.6861  1.048  0.5906 
    ## 
    ## lag1Semantics = vio, context = neg:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.1333 0.305  937   -0.550   0.8171  0.438  1.0000 
    ##  mci - int   0.0894 0.314 1327   -0.616   0.7944  0.285  1.0000 
    ## 
    ## lag1Semantics = mci, context = neg:
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0977 0.301  898   -0.773   0.5781 -0.324  1.0000 
    ##  mci - int   0.1196 0.303 1172   -0.561   0.8005  0.394  1.0000 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Backup results
save(models, tests, means.semantics, means.context, means.lag, 
     means.nested.context, means.nested.lag,means.nested, file = "EEG/export/stats_appendixLag.RData")

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
    ## [1] emmeans_1.4.8  afex_0.27-2    lmerTest_3.1-2 lme4_1.1-23    Matrix_1.2-18 
    ## [6] MASS_7.3-51.6 
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] nlme_3.1-148         matrixStats_0.56.0   RColorBrewer_1.1-2  
    ##   [4] httr_1.4.1           numDeriv_2016.8-1.1  tools_4.0.2         
    ##   [7] R6_2.4.1             lazyeval_0.2.2       mgcv_1.8-31         
    ##  [10] colorspace_1.4-1     tidyselect_1.1.0     gridExtra_2.3       
    ##  [13] curl_4.3             compiler_4.0.2       edfReader_1.2.1     
    ##  [16] eegUtils_0.5.0       plotly_4.9.2.1       scales_1.1.1        
    ##  [19] mvtnorm_1.1-1        stringr_1.4.0        digest_0.6.25       
    ##  [22] foreign_0.8-80       minqa_1.2.4          rmarkdown_2.3       
    ##  [25] R.utils_2.9.2        rio_0.5.16           ini_0.3.1           
    ##  [28] pkgconfig_2.0.3      htmltools_0.5.0      highr_0.8           
    ##  [31] fastmap_1.0.1        htmlwidgets_1.5.1    Rmisc_1.5           
    ##  [34] rlang_0.4.7          readxl_1.3.1         rstudioapi_0.11     
    ##  [37] shiny_1.5.0          generics_0.0.2       jsonlite_1.7.0      
    ##  [40] dplyr_1.0.0          zip_2.0.4            car_3.0-8           
    ##  [43] R.oo_1.23.0          magrittr_1.5         R.matlab_3.6.2      
    ##  [46] Rcpp_1.0.5           munsell_0.5.0        abind_1.4-5         
    ##  [49] viridis_0.5.1        lifecycle_0.2.0      R.methodsS3_1.8.0   
    ##  [52] yaml_2.2.1           stringi_1.4.6        carData_3.0-4       
    ##  [55] plyr_1.8.6           grid_4.0.2           parallel_4.0.2      
    ##  [58] listenv_0.8.0        promises_1.1.1       shinydashboard_0.7.1
    ##  [61] forcats_0.5.0        crayon_1.3.4         miniUI_0.1.1.1      
    ##  [64] lattice_0.20-41      haven_2.3.1          splines_4.0.2       
    ##  [67] hms_0.5.3            knitr_1.29           pillar_1.4.6        
    ##  [70] boot_1.3-25          estimability_1.3     reshape2_1.4.4      
    ##  [73] future.apply_1.6.0   codetools_0.2-16     glue_1.4.1          
    ##  [76] packrat_0.5.0        evaluate_0.14        data.table_1.12.8   
    ##  [79] vctrs_0.3.2          nloptr_1.2.2.2       httpuv_1.5.4        
    ##  [82] cellranger_1.1.0     gtable_0.3.0         purrr_0.3.4         
    ##  [85] tidyr_1.1.0          future_1.18.0        ggplot2_3.3.2       
    ##  [88] xfun_0.15            openxlsx_4.1.5       mime_0.9            
    ##  [91] xtable_1.8-4         pracma_2.2.9         coda_0.19-3         
    ##  [94] later_1.1.0.1        viridisLite_0.3.0    signal_0.7-6        
    ##  [97] tibble_3.0.3         globals_0.12.5       statmod_1.4.34      
    ## [100] ellipsis_0.3.1
