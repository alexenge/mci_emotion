F03\_MCI\_Emo\_plotting.R
================
alexander
2020-07-04

``` r
### MCI EMO PLOTTING SCRIPT ###

## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(dplyr)
library(tidyr)
library(eeguana)
library(ggplot2)
library(cowplot)

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")
avgs.verb <- readRDS("EEG/export/avgs_verb.RDS")
avgs.pict <- readRDS("EEG/export/avgs_pict.RDS")

# Combine verb-related and picture-related potentials
avgs.verb <- avgs.verb %>% mutate(type = "Verb-related")
avgs.pict <- avgs.pict %>% mutate(type = "Picture-related")
avgs <- bind(avgs.verb, avgs.pict)
```

    ## # Object size in memory 110.5 Mb

``` r
avgs$.segments$type <- factor(avgs$.segments$type, levels = c("Verb-related", "Picture-related"))

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

## FIGURES FOR PUBLIACTION ## ---------------------------------------------------------------------

# Rename some factor leves
a1$semantics <- factor(a1$semantics, levels = c("int", "vio", "mci"), labels = c("Intuitive", "Violation", "MCI"))
a1$context <- factor(a1$context, levels = c("neu", "neg"), labels = c("Neutral context", "Negative context"))
avgs$.segments$semantics <- factor(avgs$.segments$semantics, levels = c("int", "vio", "mci"), labels = c("Intuitive", "Violation", "MCI"))
avgs$.segments$context <- factor(avgs$.segments$context, levels = c("neu", "neg"), labels = c("Neutral context", "Negative context"))

# Define colours for conditions
condition.colors <- RColorBrewer::brewer.pal(3, name = "Set1")[c(3, 1, 2)]
names(condition.colors) <- c("Intuitive", "Violation", "MCI")

# Convert dependent variables to long format
a1.long <- a1 %>% gather(key = "dv", value = "value", N400.verb, N400.pict, factor_key = TRUE)

# Compute summery statistics (means and confidence intervals) for verb-related and picture-related N400
summs <- sapply(c("N400.verb", "N400.pict"), function(dv){
  summ <- Rmisc::summarySEwithin(a1, measurevar = dv, withinvars = c("semantics", "context"), idvar = "participant", na.rm = TRUE)
  colnames(summ)[colnames(summ) == dv] <- "value"
  summ$dv <- dv
  return(summ)}, simplify = FALSE)

# Bar plots for verb-related and picture-related N400
bars <- sapply(c("N400.verb", "N400.pict"), function(what){
  # Brackets and stars for statistical significance
  bracket <- data.frame(context = as.factor("Neutral context"),
                        ymin = c(0.2, 0.5, 0.2),
                        ymax = c(0.5, 0.5, 0.5),
                        xmin = c(0.7, 0.7, 1.3),
                        xmax = c(0.7, 1.3, 1.3))
  star <- data.frame("type" = as.factor(c("N400.verb", "N400.pict")), "context" = as.factor("Neutral context"), "stars" = c("**", "*"))
  star <- subset(star, type == what)
  # Actual plotting
  ggplot(summs[names(summs) == what][[1]], aes(x = context, y = value, fill = semantics)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), position = position_dodge(width = 0.9), width = 0.5) +
    geom_segment(data = bracket, aes(x = xmin, y = ymin, xend = xmax, yend = ymax), inherit.aes = FALSE) +
    geom_label(data = star, aes(x = context, y = 0.43, label = stars), inherit.aes = FALSE, size = 6, label.size = 0) +
    scale_fill_manual(values = condition.colors) +
    labs(fill = "Semantics") +
    coord_cartesian(ylim = c(-4, 1)) +
    scale_x_discrete(labels = c("Neutral\ncontext", "Negative\ncontext")) +
    scale_y_continuous(name = "ROI amplitude (µV)", breaks = seq(-4, 1, 1)) +
    geom_hline(yintercept = 0) +
    theme_bw() + styling + theme(axis.title.x = element_blank(), legend.position = "none")
}, simplify = FALSE)

# Trial structure
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
  draw_plot(get_legend(bars$N400.verb + theme(legend.position = "right", legend.title = element_blank())), x = 0.65, y = 0.5, vjust = 0.48)

# Waveforms for verb-related and picture-related N400
waves <- sapply(c("Verb-related", "Picture-related"), function(what){
  # Which time window to shade
  tmin <- ifelse(what == "Verb-related", 0.300, 0.150)
  tmax <- ifelse(what == "Verb-related", 0.500, 0.350)
  # Significant area to highlight (MCI - intuitive in the neutral context)
  highlight <- avgs %>%
    select(ROI) %>% filter(between(as_time(.sample), !!tmin, !!tmax)) %>% 
    group_by(semantics, context, type, .sample) %>% summarise_at(channel_names(.), mean, na.rm = TRUE)
  highlight <- data.frame(seq(tmin, tmax, 0.002),
                          highlight %>% filter(type == what, semantics == "MCI", context == "Neutral context") %>% signal_tbl %>% select(ROI),
                          highlight %>% filter(type == what, semantics == "Intuitive", context == "Neutral context") %>% signal_tbl %>% select(ROI))
  names(highlight) <- c(".time", "mci", "int")
  highlight$context <- as.factor("Neutral context")
  # Stars for significance levels
  star <- data.frame("type" = as.factor(c("Verb-related", "Picture-related")), "context" = as.factor("Neutral context"), "stars" = c("**", "*"))
  star <- subset(star, type == what)
  # Actual plotting
  avgs %>%
    filter(type == what) %>%
    select(ROI) %>%
    ggplot(aes(x = .time, y = .value, color = semantics)) +
    geom_rect(aes(xmin = tmin, xmax = tmax, ymin = -Inf, ymax = Inf), fill = "gray90", inherit.aes = FALSE) +
    geom_ribbon(data = highlight, aes(x = .time, ymin = mci, ymax = int), fill = "#ffff33", inherit.aes = FALSE) +
    geom_text(data = star, aes(x = tmin+(tmax-tmin)/2, y = 3.5, label = stars), inherit.aes = FALSE, size = 6) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    stat_summary(fun = "mean", geom = "line") +
    scale_color_manual(values = condition.colors) +
    coord_cartesian(xlim = c(-0.2, 0.8), ylim = c(-4.5, 4.5), expand = FALSE) +
    scale_x_continuous(breaks = seq(-0.1, 0.7, 0.2), labels = seq(-100, 700, 200)) +
    scale_y_continuous(breaks = seq(-4, 4, 2)) +
    xlab("Time (ms)") + ylab("ROI amplitude (µV)") +
    labs(color = NULL) +
    theme_bw() + styling + theme(legend.position = "none") +
    facet_grid(.~context)
}, simplify = FALSE)

# Topographies for verb-related and picture-related N400
topos <- sapply(c("Verb-related", "Picture-related"), function(what){
  if(what == "Verb-related"){
    tmp <- avgs %>% filter(between(as_time(.sample), 0.300, 0.500), type == "Verb-related")
  } else{
    tmp <- avgs %>% filter(between(as_time(.sample), 0.150, 0.350), type == "Picture-related")}
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
# Colorbar
simdat1 <- data.frame(a = 1:10, b = 1:10, c = seq(-0.7, 0.7, length.out = 10))
colbar <- get_legend(ggplot(simdat1, aes(x = a, y = b, fill = c)) + geom_raster() + geom_line() +
                       scale_fill_distiller(palette = "RdBu", guide = guide_colorbar(ticks = FALSE, title.position = "left"), breaks = c(-0.7, 0, 0.7)) +
                       labs(fill = "Ampl.\n(µV)") +
                       theme(legend.position = "right",
                             legend.background = element_blank(),
                             legend.key.height = unit(0.3, "cm"),
                             legend.title = element_text(family = "Helvetica", size = 10, color = "black"),
                             legend.text = element_text(family = "Helvetica", size = 10, color = "black"),
                             legend.title.align = 0.5))

# Create one plot with four topographies (verb-related)
simdat2 <- data.frame("semantics" = as.factor(c("Violation - Intuitive", "MCI - Intuitive")),
                      "context" = as.factor(c("Neutral\ncontext", "Negative\ncontext")))
topos.verb <- ggplot(simdat2, aes(x = context, y = semantics)) +
  geom_point() +
  draw_plot(topos$`Verb-related`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
  draw_plot(topos$`Verb-related`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
  draw_plot(topos$`Verb-related`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
  draw_plot(topos$`Verb-related`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
  annotate("text", x = 1.5, y = 0.53, label = "300-500 ms", size = 3.528, family = "Helvetica") + 
  draw_plot(colbar, x = 0.95, y = 1) +
  styling +
  theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

# Create one plot with four topographies (picture-related)
topos.pict <- ggplot(simdat2, aes(x = context, y = semantics)) +
  geom_point() +
  draw_plot(topos$`Picture-related`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
  draw_plot(topos$`Picture-related`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
  draw_plot(topos$`Picture-related`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
  draw_plot(topos$`Picture-related`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
  annotate("text", x = 1.5, y = 0.53, label = "150-350 ms", size = 3.528, family = "Helvetica") +
  draw_plot(colbar, x = 0.95, y = 1) +
  styling +
  theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

# Combine everything and save (verb-related)
plot_grid(stim, waves$`Verb-related`,
          plot_grid(bars$N400.verb, topos.verb, nrow = 1, rel_widths = c(0.6, 1), labels = c("C", "D"),
                    label_fontfamily = "Helvetica", label_y = 1.03),
          nrow = 3, rel_heights = c(0.2, 0.8, 1), labels = c("A", "B", NULL), label_fontfamily = "Helvetica") %>%
  ggsave(filename = "EEG/figures/N400_verb.pdf", width = 18, height = 22, units = "cm") +
  ggsave(filename = "Scripts/Output/Figures/N400_verb.png", width = 18, height = 22, units = "cm")
```

    ## integer(0)

![Figure 1: Verb-Related N400 Effects](Figures/N400_verb.png)

``` r
# Combine everything and save (picture-related)
plot_grid(plot_grid(waves$`Picture-related`) +
            draw_plot(get_legend(bars$N400.pict +
                                   theme(legend.position = "right", legend.title = element_blank(), legend.background = element_blank())),
                      x = 0.41, y = -0.22),
          plot_grid(bars$N400.pict, topos.pict, nrow = 1, rel_widths = c(0.6, 1), labels = c("B", "C"),
                    label_fontfamily = "Helvetica", label_y = 1.03),
          nrow = 2, rel_heights = c(0.8, 1), labels = c("A", NULL), label_fontfamily = "Helvetica") %>%
  ggsave(filename = "EEG/figures/N400_pict.pdf", width = 18, height = 19.8, units = "cm") +
  ggsave(filename = "Scripts/Output/Figures/N400_pict.png", width = 18, height = 22, units = "cm")
```

    ## integer(0)

![Figure 2: Picture-Related N400 Effects](Figures/N400_pict.png)

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
    ## [1] huxtable_5.0.0     cowplot_1.0.0      ggplot2_3.3.2      eeguana_0.1.4.9000 tidyr_1.1.0        dplyr_1.0.0        Rmisc_1.5          plyr_1.8.6        
    ## [9] lattice_0.20-41   
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] nlme_3.1-148         matrixStats_0.56.0   RColorBrewer_1.1-2   httr_1.4.1           numDeriv_2016.8-1.1  tools_4.0.2          R6_2.4.1            
    ##   [8] afex_0.27-2          lazyeval_0.2.2       mgcv_1.8-31          colorspace_1.4-1     withr_2.2.0          tidyselect_1.1.0     gridExtra_2.3       
    ##  [15] emmeans_1.4.8        curl_4.3             compiler_4.0.2       edfReader_1.2.1      eegUtils_0.5.0       plotly_4.9.2.1       labeling_0.3        
    ##  [22] scales_1.1.1         mvtnorm_1.1-1        stringr_1.4.0        digest_0.6.25        foreign_0.8-80       minqa_1.2.4          rmarkdown_2.3       
    ##  [29] R.utils_2.9.2        rio_0.5.16           ini_0.3.1            pkgconfig_2.0.3      htmltools_0.5.0      lme4_1.1-23          fastmap_1.0.1       
    ##  [36] highr_0.8            readxl_1.3.1         htmlwidgets_1.5.1    rlang_0.4.6          rstudioapi_0.11      shiny_1.5.0          generics_0.0.2      
    ##  [43] farver_2.0.3         jsonlite_1.7.0       zip_2.0.4            car_3.0-8            R.oo_1.23.0          magrittr_1.5         R.matlab_3.6.2      
    ##  [50] Matrix_1.2-18        Rcpp_1.0.4.6         munsell_0.5.0        abind_1.4-5          viridis_0.5.1        lifecycle_0.2.0      R.methodsS3_1.8.0   
    ##  [57] stringi_1.4.6        yaml_2.2.1           carData_3.0-4        MASS_7.3-51.6        grid_4.0.2           parallel_4.0.2       listenv_0.8.0       
    ##  [64] promises_1.1.1       forcats_0.5.0        shinydashboard_0.7.1 crayon_1.3.4         miniUI_0.1.1.1       haven_2.3.1          splines_4.0.2       
    ##  [71] hms_0.5.3            knitr_1.29           pillar_1.4.4         boot_1.3-25          estimability_1.3     reshape2_1.4.4       future.apply_1.6.0  
    ##  [78] codetools_0.2-16     glue_1.4.1           evaluate_0.14        data.table_1.12.8    vctrs_0.3.1          nloptr_1.2.2.2       httpuv_1.5.4        
    ##  [85] cellranger_1.1.0     gtable_0.3.0         purrr_0.3.4          future_1.17.0        assertthat_0.2.1     openxlsx_4.1.5       xfun_0.15           
    ##  [92] mime_0.9             xtable_1.8-4         pracma_2.2.9         coda_0.19-3          later_1.1.0.1        viridisLite_0.3.0    signal_0.7-6        
    ##  [99] tibble_3.0.1         lmerTest_3.1-2       globals_0.12.5       statmod_1.4.34       ellipsis_0.3.1