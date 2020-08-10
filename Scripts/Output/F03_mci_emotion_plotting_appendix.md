F03\_mci\_emotion\_plotting\_appendix.R
================
kirstenstark
2020-08-10

``` r
### MCI EMO PLOTTING SCRIPT - APPENDIX (P600) ###

# Creates a bar plot, an ERP waveform, and scalp topographies for the verb-related P600 effect 
# different semantic conditions (intuitive, violation, MCI) within each type of emotional 
# context (neutral, negative).

## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(dplyr)   # Version 1.0.0
library(tidyr)   # Version 1.1.0
library(eeguana) # Version 0.1.4.9000
library(ggplot2) # Version 3.3.2
library(cowplot) # Version 1.0.0

# Load preprocessed data
a1 <- readRDS("EEG/export/a1_appendix.RDS")
avgs <- readRDS("EEG/export/avgs_verb_appendix.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 <- na.omit(a1[!a1$error,])

# Define ggplot theme
styling <-   theme(panel.grid = element_blank(),
                   panel.border = element_rect(colour = "black", size = 1),
                   legend.position = "right",
                   axis.ticks = element_line(colour = "black"),
                   axis.title = element_text(color = "black", family = "Helvetica", size = 10),
                   axis.text = element_text(color = "black", family = "Helvetica", size = 10),
                   legend.title = element_text(color = "black", family = "Helvetica", size = 10, face = "bold"),
                   legend.text = element_text(color = "black", family = "Helvetica", size = 10),
                   strip.background = element_blank(),
                   strip.text = element_text(color = "black", family = "Helvetica", size = 10))

# Rename some factor levels
a1$semantics <- factor(a1$semantics, levels = c("int", "vio", "mci"), labels = c("Intuitive", "Violation", "MCI"))
a1$context <- factor(a1$context, levels = c("neu", "neg"), labels = c("Neutral context", "Negative context"))
avgs$.segments$semantics <- factor(avgs$.segments$semantics, levels = c("int", "vio", "mci"), labels = c("Intuitive", "Violation", "MCI"))
avgs$.segments$context <- factor(avgs$.segments$context, levels = c("neu", "neg"), labels = c("Neutral context", "Negative context"))

# Define colours for conditions
condition.colors <- RColorBrewer::brewer.pal(3, name = "Set1")[c(3, 1, 2)]
names(condition.colors) <- c("Intuitive", "Violation", "MCI")

## BAR PLOTS ## -----------------------------------------------------------------------------------

# Convert dependent variables to long format
a1.long <- a1 %>% gather(key = "dv", value = "value", P600_1.verb, P600_2.verb, factor_key = TRUE)

# Compute summary statistics (means and confidence intervals) for verb-related and picture-related N400
summs <- sapply(c("P600_1.verb", "P600_2.verb"), function(dv){
  summ <- Rmisc::summarySEwithin(a1, measurevar = dv, withinvars = c("semantics", "context"), idvar = "participant", na.rm = TRUE)
  colnames(summ)[colnames(summ) == dv] <- "value"
  summ$dv <- dv
  return(summ)}, simplify = FALSE)

# Bar plots for verb-related and picture-related N400
bars <- sapply(c("P600_1.verb", "P600_2.verb"), function(what){
  # Actual plotting
  ggplot(summs[names(summs) == what][[1]], aes(x = context, y = value, fill = semantics)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), position = position_dodge(width = 0.9), width = 0.5) +
    scale_fill_manual(values = condition.colors) +
    labs(fill = "Semantics") +
    coord_cartesian(ylim = c(-1, 2)) +
    scale_x_discrete(labels = c("Neutral\ncontext", "Negative\ncontext")) +
    scale_y_continuous(name = "ROI amplitude (µV)", breaks = seq(-1, 2, 1)) +
    geom_hline(yintercept = 0) +
    theme_bw() + styling + theme(axis.title.x = element_blank(), legend.position = "none")
}, simplify = FALSE)

## EXAMPLE TRIAL ## -------------------------------------------------------------------------------

# Example for one sentence with verbs in three conditions
stim <- ggplot() + theme_void() + theme(plot.background = element_rect(fill = "white", color = "white")) +
  coord_cartesian(xlim = c(0, 1.2), ylim = c(0, 1)) + 
  geom_text(aes(x = 0.5, y = 0.5, label = '"The old barren birch tree'), size = 4.939, family = "Helvetica", hjust = 1) +
  geom_segment(aes(x = 0.51, xend = 0.54, y = 0.5, yend = 0.8)) +
  geom_segment(aes(x = 0.51, xend = 0.54, y = 0.5, yend = 0.5)) +
  geom_segment(aes(x = 0.51, xend = 0.54, y = 0.5, yend = 0.2)) +
  geom_text(aes(x = 0.55, y = 0.8, label = "creaks"), size = 4.939, family = "Helvetica", fontface = "bold", color = condition.colors[1], hjust = 0) +
  geom_text(aes(x = 0.55, y = 0.5, label = "talks"), size = 4.939, family = "Helvetica", fontface = "bold", color = condition.colors[2], hjust = 0) +
  geom_text(aes(x = 0.55, y = 0.2, label = "blossoms"), size = 4.939, family = "Helvetica", fontface = "bold", color = condition.colors[3], hjust = 0) +
  geom_text(aes(x = 0.675, y = 0.8, label = 'in the wind"'), size = 4.939, family = "Helvetica", hjust = 0) +
  geom_text(aes(x = 0.643, y = 0.5, label = 'to the girl"'), size = 4.939, family = "Helvetica", hjust = 0) +
  geom_text(aes(x = 0.730, y = 0.2, label = 'above the girl"'), size = 4.939, family = "Helvetica", hjust = 0) +
  draw_plot(get_legend(bars$P600_1.verb + theme(legend.position = "right", legend.title = element_blank())), x = 0.65, y = 0.5, vjust = 0.48)

## WAVEFORMS ## -----------------------------------------------------------------------------------

# ERP waveforms for P600
waves <- sapply(c("Verb-related (500-700 ms)", "Verb-related (500-900 ms)"), function(what){
  tmin <- 0.500
  tmax <- ifelse(what == "Verb-related (500-700 ms)", 0.700, 0.900)
  x_lim <- ifelse(what == "Verb-related (500-700 ms)", 0.8, 1.0)
  x_break <- ifelse(what == "Verb-related (500-700 ms)", 0.7, 0.9)
  x_label <- ifelse(what == "Verb-related (500-700 ms)", 700, 900)
    # Actual plotting
    avgs %>%
      select(ROI) %>%
      ggplot(aes(x = .time, y = .value, color = semantics)) +
      geom_rect(aes(xmin = tmin, xmax = tmax, ymin = -Inf, ymax = Inf), fill = "gray90", inherit.aes = FALSE) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_vline(xintercept = 0, linetype = "dotted") +
      stat_summary(fun = "mean", geom = "line") +
      scale_color_manual(values = condition.colors) +
      coord_cartesian(xlim = c(-0.2, x_lim), ylim = c(-2.5, 2.5), expand = FALSE) +
      scale_x_continuous(breaks = seq(-0.1, x_break, 0.2), labels = seq(-100, x_label, 200)) +
      scale_y_continuous(breaks = seq(-2, 2, 1)) +
      xlab("Time (ms)") + ylab("ROI amplitude (µV)") +
      labs(color = NULL) +
      theme_bw() + styling + theme(legend.position = "none") +
      facet_grid(.~context)
}, simplify = FALSE)
```

    ## Adding missing grouping variables: .sample
    ## Adding missing grouping variables: .sample

``` r
## TOPOGRAPHIES ## --------------------------------------------------------------------------------

# Create scalp topographies for P600
topos <- sapply(c("Verb-related (500-700 ms)", "Verb-related (500-900 ms)"), function(what){
  if(what == "Verb-related (500-700 ms)"){
    tmp <- avgs %>% filter(between(as_time(.sample), 0.500, 0.700))
  } else{
    tmp <- avgs %>% filter(between(as_time(.sample), 0.500, 0.900))}
  tmp <- tmp %>%
    group_by(semantics, context) %>% summarise_at(channel_names(.), mean) %>%
    signal_tbl() %>% select(Fp1:A1) %>% t() %>% as.data.frame()
  names(tmp) <- c("int.neu", "int.neg", "vio.neu", "vio.neg", "mci.neu", "mci.neg")
  tmp <- data.frame("diff.vio.neu" = tmp$vio.neu - tmp$int.neu,
                    "diff.vio.neg" = tmp$vio.neg - tmp$int.neg,
                    "diff.mci.neu" = tmp$mci.neu - tmp$int.neu,
                    "diff.mci.neg" = tmp$mci.neg - tmp$int.neg,
                    "electrode" = rownames(tmp))
  topos <- lapply(1:4, function(x){
    p <- eegUtils::topoplot(data = tmp, quantity = colnames(tmp)[x], limits = c(-0.7, 0.7), r = 0.9, palette = "RdBu", interp_limit = "skirt", 
                            contour = FALSE, highlights = c("C1", "C2", "CZ", "CP1", "CP2", "CPZ"), scaling = 0.5)
    p$layers[[6]]$aes_params$size <- 0.1
    p$layers[[7]]$aes_params$colour <- "black"
    p <- p + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, family = "Helvetica"))})
}, simplify = FALSE)
```

    ## Attempting to add standard electrode locations...

    ## Attempting to add standard electrode locations...
    ## Attempting to add standard electrode locations...
    ## Attempting to add standard electrode locations...
    ## Attempting to add standard electrode locations...
    ## Attempting to add standard electrode locations...
    ## Attempting to add standard electrode locations...
    ## Attempting to add standard electrode locations...

``` r
# Create a colorbar
simdat1 <- data.frame(a = 1:10, b = 1:10, c = seq(-0.7, 0.7, length.out = 10))
colbar <- get_legend(ggplot(simdat1, aes(x = a, y = b, fill = c)) + geom_raster() + geom_line() +
                       scale_fill_distiller(palette = "RdBu", guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 1), breaks = c(-0.7, 0, 0.7)) +
                       labs(fill = "Ampl.\n(µV)") +
                       theme(legend.position = "right",
                             legend.background = element_blank(),
                             legend.key.height = unit(0.3, "cm"),
                             legend.title = element_text(family = "Helvetica", size = 10, color = "black"),
                             legend.text = element_text(family = "Helvetica", size = 10, color = "black"),
                             legend.title.align = 0.5))

# Create one plot combining all four topographies (time window 500-700 ms)
simdat2 <- data.frame("semantics" = factor(c("Violation - Intuitive", "MCI - Intuitive")),
                      "context" = factor(c("Neutral\ncontext", "Negative\ncontext"), levels = c("Neutral\ncontext", "Negative\ncontext")))
topos.P600_1 <- ggplot(simdat2, aes(x = context, y = semantics)) +
    geom_point() +
    draw_plot(topos$`Verb-related (500-700 ms)`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
    draw_plot(topos$`Verb-related (500-700 ms)`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
    draw_plot(topos$`Verb-related (500-700 ms)`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
    draw_plot(topos$`Verb-related (500-700 ms)`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
    annotate("text", x = 1.5, y = 0.53, label = "500-700 ms", size = 3.528, family = "Helvetica") + 
    draw_plot(colbar, x = 0.95, y = 1) +
    styling +
    theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

# Create one plot combining all four topographies (time window 500-900 ms)
topos.P600_2 <- ggplot(simdat2, aes(x = context, y = semantics)) +
  geom_point() +
  draw_plot(topos$`Verb-related (500-900 ms)`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
  draw_plot(topos$`Verb-related (500-900 ms)`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
  draw_plot(topos$`Verb-related (500-900 ms)`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
  draw_plot(topos$`Verb-related (500-900 ms)`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
  annotate("text", x = 1.5, y = 0.53, label = "500-900 ms", size = 3.528, family = "Helvetica") +
  draw_plot(colbar, x = 0.95, y = 1) +
  styling +
  theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

## PUBLICATION-READY FIGURES ## -------------------------------------------------------------------

# Figure 1: Verb-related potentials (ROI 500-700 ms)
plot_grid(stim, waves$`Verb-related (500-700 ms)`,
          plot_grid(bars$P600_1.verb, topos.P600_1, nrow = 1, rel_widths = c(0.6, 1), labels = c("C", "D"),
                    label_fontfamily = "Helvetica", label_y = 1.03),
          nrow = 3, rel_heights = c(0.2, 0.8, 1), labels = c("A", "B", NULL), label_fontfamily = "Helvetica") %>%
  ggsave(filename = "EEG/figures/appendix_P600_500-700ms.pdf", width = 18, height = 22, units = "cm")

# Figure 1: Verb-related potentials (ROI 500-900 ms)
plot_grid(stim, waves$`Verb-related (500-900 ms)`,
          plot_grid(bars$P600_2.verb, topos.P600_2, nrow = 1, rel_widths = c(0.6, 1), labels = c("C", "D"),
                    label_fontfamily = "Helvetica", label_y = 1.03),
          nrow = 3, rel_heights = c(0.2, 0.8, 1), labels = c("A", "B", NULL), label_fontfamily = "Helvetica") %>%
  ggsave(filename = "EEG/figures/appendix_P600_500-900ms.pdf", width = 18, height = 22, units = "cm")

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
    ## [1] cowplot_1.0.0      ggplot2_3.3.2      eeguana_0.1.4.9000 tidyr_1.1.0       
    ## [5] dplyr_1.0.0        Rmisc_1.5          plyr_1.8.6         lattice_0.20-41   
    ## [9] huxtable_5.0.0    
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] minqa_1.2.4          colorspace_1.4-1     ellipsis_0.3.1      
    ##   [4] rio_0.5.16           estimability_1.3     flextable_0.5.10    
    ##   [7] base64enc_0.1-3      rstudioapi_0.11      farver_2.0.3        
    ##  [10] listenv_0.8.0        R.matlab_3.6.2       fansi_0.4.1         
    ##  [13] mvtnorm_1.1-1        xml2_1.3.2           codetools_0.2-16    
    ##  [16] splines_4.0.2        R.methodsS3_1.8.0    knitr_1.29          
    ##  [19] eegUtils_0.5.0       afex_0.27-2          jsonlite_1.7.0      
    ##  [22] nloptr_1.2.2.2       packrat_0.5.0        R.oo_1.23.0         
    ##  [25] shinydashboard_0.7.1 shiny_1.5.0          compiler_4.0.2      
    ##  [28] httr_1.4.1           emmeans_1.4.8        assertthat_0.2.1    
    ##  [31] Matrix_1.2-18        fastmap_1.0.1        lazyeval_0.2.2      
    ##  [34] cli_2.0.2            later_1.1.0.1        htmltools_0.5.0     
    ##  [37] tools_4.0.2          lmerTest_3.1-2       coda_0.19-3         
    ##  [40] gtable_0.3.0         glue_1.4.1           reshape2_1.4.4      
    ##  [43] Rcpp_1.0.5           carData_3.0-4        cellranger_1.1.0    
    ##  [46] vctrs_0.3.2          nlme_3.1-148         xfun_0.15           
    ##  [49] stringr_1.4.0        globals_0.12.5       openxlsx_4.1.5      
    ##  [52] lme4_1.1-23          mime_0.9             miniUI_0.1.1.1      
    ##  [55] lifecycle_0.2.0      edfReader_1.2.1      statmod_1.4.34      
    ##  [58] future_1.18.0        MASS_7.3-51.6        scales_1.1.1        
    ##  [61] hms_0.5.3            promises_1.1.1       parallel_4.0.2      
    ##  [64] RColorBrewer_1.1-2   yaml_2.2.1           curl_4.3            
    ##  [67] gridExtra_2.3        gdtools_0.2.2        stringi_1.4.6       
    ##  [70] highr_0.8            boot_1.3-25          zip_2.0.4           
    ##  [73] commonmark_1.7       systemfonts_0.2.3    rlang_0.4.7         
    ##  [76] pkgconfig_2.0.3      matrixStats_0.56.0   pracma_2.2.9        
    ##  [79] evaluate_0.14        purrr_0.3.4          labeling_0.3        
    ##  [82] htmlwidgets_1.5.1    tidyselect_1.1.0     magrittr_1.5        
    ##  [85] R6_2.4.1             generics_0.0.2       ini_0.3.1           
    ##  [88] withr_2.2.0          pillar_1.4.6         haven_2.3.1         
    ##  [91] foreign_0.8-80       mgcv_1.8-31          abind_1.4-5         
    ##  [94] tibble_3.0.3         future.apply_1.6.0   crayon_1.3.4        
    ##  [97] car_3.0-8            utf8_1.1.4           uuid_0.1-4          
    ## [100] plotly_4.9.2.1       rmarkdown_2.3        officer_0.3.12      
    ## [103] viridis_0.5.1        grid_4.0.2           readxl_1.3.1        
    ## [106] data.table_1.12.8    forcats_0.5.0        digest_0.6.25       
    ## [109] xtable_1.8-4         httpuv_1.5.4         numDeriv_2016.8-1.1 
    ## [112] R.utils_2.9.2        signal_0.7-6         munsell_0.5.0       
    ## [115] viridisLite_0.3.0
