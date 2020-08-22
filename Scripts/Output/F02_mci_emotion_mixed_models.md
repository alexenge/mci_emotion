F02\_mci\_emotion\_mixed\_models.R
================
alexander
2020-08-22

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
library(lmerTest)     # version 3.1-2
library(afex)         # version 0.27-2
library(emmeans)      # version 1.4.8

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 <- na.omit(a1[!a1$error,])

# Define simple contrast coding for context emotionality (negative - neutral)
#     H0(Intercept): (mu1+mu2)/2 = 0 <-> mu1+mu2 = 0
#     H0(Slope): -mu1 + mu2 = 0
#     with mu1 = mean of the neutral contexts and mu2 = mean of the neg contexts
t(contrasts.context <- t(cbind(c("neu" = -1, "neg" = 1))))
```

    ##     [,1]
    ## neu   -1
    ## neg    1

``` r
contrasts(a1$context) <- ginv(contrasts.context)

# Define simple contrast coding for semantics (violation - intuitive, mci - intuitive)
#     H0(Intercept): (mu1+mu2+mu3)/3 = 0 <-> mu1+mu2+mu3 = 0
#     H0(Slope1): -1*mu1 +1*mu2 + 0*mu3 = 0
#     H0(Slope2): -1*mu1 +0*mu2 + 1*mu3 = 0
#     with mu1 = mean of intuitive concepts, mu2 = mean of violations, mu3 = mean of MCIs
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
mod.valence <- lmer(ValenzResp ~ context + (context|participant) + (context|item),
                    data = a1, control = lmerControl(calc.derivs = FALSE))

# LMM for arousal ratings (converged after changing the optimizer + removing correlations between REs)
mod.aroursal <- lmer(ArousalResp ~ context + (context|participant) + (context|item),
                     data = a1, control = lmerControl(calc.derivs = FALSE))

# LMM for verb-related N400 (converged on first attempt)
mod.N400.verb <- lmer_alt(N400.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                          data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for picture-related N400 (converged after changing the optimizer)
mod.N400.pict <- lmer(N400.pict ~ semantics*context + (semantics*context|participant) + (semantics*context|item),
                      data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related P600 (short time window, old ROI) (converged after changing the optimizer + removing correlations between REs)
mod.P600_1.verb <- lmer_alt(P600_1.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                            data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related P600 (long time window, old ROI) (converged after changing the optimizer + removing correlations between REs)
mod.P600_2.verb <- lmer_alt(P600_2.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                            data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related P600 (short time window, new ROI) (converged after changing the optimizer + removing correlations between REs)
mod.P600_3.verb <- lmer_alt(P600_3.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                            data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# LMM for verb-related P600 (long time window, new ROI) (converged after changing the optimizer + removing correlations between REs)
mod.P600_4.verb <- lmer_alt(P600_4.verb ~ semantics*context + (semantics*context||participant) + (semantics*context||item),
                            data = a1, control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Create a list of models
models <- list("VALENCE" = mod.valence, "AROUSAL" = mod.aroursal, "N400.VERB" = mod.N400.verb, "N400.PICT" = mod.N400.pict,
               "P600_1" = mod.P600_1.verb, "P600_2" = mod.P600_2.verb, "P600_3" = mod.P600_3.verb, "P600_4" = mod.P600_4.verb)

# F-tests (type III tests)
(tests <- lapply(models, anova))
```

    ## $VALENCE
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##         Sum Sq Mean Sq NumDF  DenDF F value   Pr(>F)    
    ## context 87.278  87.278     1 37.778  164.13 2.53e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $AROUSAL
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##         Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## context 51.535  51.535     1 37.674  83.286 4.411e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $N400.VERB
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                    Sum Sq Mean Sq NumDF   DenDF F value   Pr(>F)    
    ## semantics         290.556 145.278     2 102.359  8.2636 0.000471 ***
    ## context             0.355   0.355     1  24.294  0.0202 0.888110    
    ## semantics:context  42.207  21.104     2  71.656  1.2004 0.307054    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $N400.PICT
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                    Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
    ## semantics          23.533  11.766     2 37.017  0.7277 0.48981  
    ## context             0.085   0.085     1 44.147  0.0053 0.94243  
    ## semantics:context 125.895  62.948     2 52.128  3.8929 0.02656 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $P600_1
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF   DenDF F value  Pr(>F)  
    ## semantics         99.830  49.915     2 111.794  2.3609 0.09902 .
    ## context            1.836   1.836     1  26.115  0.0868 0.77059  
    ## semantics:context 76.582  38.291     2  64.937  1.8111 0.17163  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $P600_2
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF   DenDF F value Pr(>F)
    ## semantics         47.810 23.9051     2 102.050  1.2009 0.3051
    ## context            0.207  0.2066     1  29.669  0.0104 0.9195
    ## semantics:context 31.597 15.7984     2 192.922  0.7936 0.4537
    ## 
    ## $P600_3
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
    ## semantics         55.724  27.862     2 118.09  1.3880 0.25360  
    ## context            0.729   0.729     1  28.09  0.0363 0.85020  
    ## semantics:context 98.899  49.449     2 202.05  2.4635 0.08769 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $P600_4
    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                   Sum Sq Mean Sq NumDF   DenDF F value Pr(>F)
    ## semantics         78.314  39.157     2  81.099  2.0688 0.1330
    ## context            1.143   1.143     1  27.164  0.0604 0.8078
    ## semantics:context 52.496  26.248     2 191.164  1.3867 0.2524

``` r
## PLANNED FOLLOW-UP CONTRASTS ## -----------------------------------------------------------------

# Allow emmeans to use Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics <- lapply(models[-1:-2],function(x){
    emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $N400.VERB
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0288 0.118 90.5   -0.299    0.241 -0.243  1.0000 
    ##  mci - int  -0.3847 0.102 93.2   -0.617   -0.152 -3.771  0.0006 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.PICT
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.073 0.129 35.5   -0.228    0.374  0.567  1.0000 
    ##  mci - int   -0.121 0.138 37.7   -0.444    0.202 -0.872  0.7774 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $P600_1
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.0721 0.120 80.2   -0.202   0.3464  0.601  1.0000 
    ##  mci - int  -0.2274 0.134 79.1   -0.534   0.0793 -1.694  0.1884 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $P600_2
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.1149 0.109 85.1   -0.133    0.363  1.056  0.5877 
    ##  mci - int  -0.0794 0.132 83.2   -0.380    0.221 -0.603  1.0000 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $P600_3
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.1864 0.130 79.9   -0.110    0.482  1.438  0.3086 
    ##  mci - int  -0.0284 0.121 81.7   -0.304    0.247 -0.235  1.0000 
    ## 
    ## Results are averaged over the levels of: context 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $P600_4
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  vio - int  0.23993 0.129 44.6  -0.0587    0.539  1.864  0.1380 
    ##  mci - int -0.00807 0.117 85.1  -0.2755    0.259 -0.069  1.0000 
    ## 
    ## Results are averaged over the levels of: context 
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
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions
    ## NOTE: Results may be misleading due to involvement in interactions

    ## $VALENCE
    ##  contrast  estimate   SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu    -1.41 0.11 37.8    -1.63    -1.19 -12.811 <.0001 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $AROUSAL
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu     1.04 0.114 37.7    0.809     1.27 9.126   <.0001 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $N400.VERB
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0135 0.0946 24.3   -0.182    0.209 0.142   0.8881 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $N400.PICT
    ##  contrast  estimate     SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu -0.00706 0.0972 44.1   -0.203    0.189 -0.073  0.9424 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $P600_1
    ##  contrast  estimate   SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0355 0.12 26.1   -0.212    0.283 0.295   0.7706 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $P600_2
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu    0.012 0.118 29.7   -0.229    0.253 0.102   0.9195 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $P600_3
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0206 0.108 28.1     -0.2    0.241 0.191   0.8502 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## 
    ## $P600_4
    ##  contrast  estimate    SE   df lower.CL upper.CL t.ratio p.value
    ##  neg - neu   0.0268 0.109 27.2   -0.197     0.25 0.246   0.8078 
    ## 
    ## Results are averaged over the levels of: semantics 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95

``` r
# Follow-up contrasts for semantics within each contexts
(means.nested <- lapply(models[-1:-2], function(x){
    emmeans(x, trt.vs.ctrl ~ semantics|context, infer = TRUE, adjust = "bonferroni")$contrasts}))
```

    ## $N400.VERB
    ## context = neu:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   -0.174 0.163 182   -0.543    0.196 -1.062  0.5791 
    ##  mci - int   -0.531 0.151 117   -0.874   -0.188 -3.517  0.0012 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.116 0.163 182   -0.253    0.485  0.711  0.9560 
    ##  mci - int   -0.238 0.151 117   -0.581    0.104 -1.579  0.2341 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $N400.PICT
    ## context = neu:
    ##  contrast  estimate    SE   df lower.CL  upper.CL t.ratio p.value
    ##  vio - int  -0.0363 0.163 34.4   -0.419  0.346617 -0.222  1.0000 
    ##  mci - int  -0.4060 0.175 47.1   -0.811 -0.000838 -2.320  0.0494 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE   df lower.CL  upper.CL t.ratio p.value
    ##  vio - int   0.1823 0.178 43.2   -0.230  0.594863  1.026  0.6209 
    ##  mci - int   0.1645 0.170 35.8   -0.233  0.561694  0.969  0.6778 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $P600_1
    ## context = neu:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   -0.154 0.171 174  -0.5402    0.233 -0.899  0.7397 
    ##  mci - int   -0.281 0.175 131  -0.6786    0.116 -1.604  0.2222 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int    0.298 0.171 173  -0.0882    0.684  1.745  0.1656 
    ##  mci - int   -0.174 0.175 130  -0.5706    0.223 -0.991  0.6467 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $P600_2
    ## context = neu:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0214 0.156 184   -0.374    0.331 -0.137  1.0000 
    ##  mci - int  -0.1145 0.171 233   -0.500    0.271 -0.671  1.0000 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.2511 0.156 183   -0.101    0.603  1.613  0.2168 
    ##  mci - int  -0.0443 0.171 232   -0.429    0.341 -0.260  1.0000 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $P600_3
    ## context = neu:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int  -0.0402 0.172 170  -0.4297    0.349 -0.233  1.0000 
    ##  mci - int  -0.0404 0.163 267  -0.4071    0.326 -0.248  1.0000 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.4129 0.172 170   0.0238    0.802  2.400  0.0350 
    ##  mci - int  -0.0164 0.162 266  -0.3827    0.350 -0.101  1.0000 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests 
    ## 
    ## $P600_4
    ## context = neu:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.0718 0.168 106   -0.310    0.453  0.428  1.0000 
    ##  mci - int  -0.0368 0.158 278   -0.393    0.319 -0.233  1.0000 
    ## 
    ## context = neg:
    ##  contrast  estimate    SE  df lower.CL upper.CL t.ratio p.value
    ##  vio - int   0.4080 0.168 106    0.027    0.789  2.435  0.0332 
    ##  mci - int   0.0206 0.158 277   -0.335    0.376  0.131  1.0000 
    ## 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: bonferroni method for 2 estimates 
    ## P value adjustment: bonferroni method for 2 tests

``` r
# Backup results
save(models, tests, means.semantics, means.context, means.nested, file = "EEG/export/stats.RData")

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
    ## [1] emmeans_1.4.8  afex_0.27-2    lmerTest_3.1-2 lme4_1.1-23    Matrix_1.2-18  MASS_7.3-51.6 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] statmod_1.4.34      tidyselect_1.1.0    xfun_0.16           reshape2_1.4.4      purrr_0.3.4         splines_4.0.2      
    ##  [7] haven_2.3.1         lattice_0.20-41     carData_3.0-4       colorspace_1.4-1    vctrs_0.3.2         generics_0.0.2     
    ## [13] htmltools_0.5.0     yaml_2.2.1          rlang_0.4.7         nloptr_1.2.2.2      pillar_1.4.6        foreign_0.8-80     
    ## [19] glue_1.4.1          readxl_1.3.1        plyr_1.8.6          lifecycle_0.2.0     stringr_1.4.0       munsell_0.5.0      
    ## [25] gtable_0.3.0        cellranger_1.1.0    zip_2.0.4           mvtnorm_1.1-1       coda_0.19-3         evaluate_0.14      
    ## [31] knitr_1.29          rio_0.5.16          forcats_0.5.0       parallel_4.0.2      curl_4.3            highr_0.8          
    ## [37] Rcpp_1.0.5          xtable_1.8-4        scales_1.1.1        abind_1.4-5         ggplot2_3.3.2       hms_0.5.3          
    ## [43] digest_0.6.25       stringi_1.4.6       openxlsx_4.1.5      dplyr_1.0.0         numDeriv_2016.8-1.1 grid_4.0.2         
    ## [49] tools_4.0.2         magrittr_1.5        tibble_3.0.3        crayon_1.3.4        car_3.0-8           pkgconfig_2.0.3    
    ## [55] ellipsis_0.3.1      data.table_1.13.0   estimability_1.3    minqa_1.2.4         rmarkdown_2.3       rstudioapi_0.11    
    ## [61] R6_2.4.1            boot_1.3-25         nlme_3.1-148        compiler_4.0.2
