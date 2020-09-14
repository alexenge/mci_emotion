#/* Run this first piece of code only if you want to create a markdown report for GitHub
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd()) #*/

### MCI EMO PLOTTING SCRIPT ###

# Creates a bar plot, an ERP waveform, and scalp topographies for the N400 effect for the
# different semantic conditions (intuitive, violation, MCI) within each type of emotional 
# context (neutral, negative), separately for verb- and picture-related potentials.

## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(tidyverse)   # Version 1.3.0
library(magrittr)    # Version 1.5
library(eeguana)     # Version 0.1.4.9000
library(cowplot)     # Version 1.0.0

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")
avgs.verb <- readRDS("EEG/export/avgs_verb.RDS")
avgs.pict <- readRDS("EEG/export/avgs_pict.RDS")

# Combine verb-related and picture-related potentials
avgs.verb <- avgs.verb %>% mutate(type = "Verb-related")
avgs.pict <- avgs.pict %>% mutate(type = "Picture-related")
avgs <- suppressWarnings(bind(avgs.verb, avgs.pict))
avgs$.segments$type <- factor(avgs$.segments$type, levels = c("Verb-related", "Picture-related"))

# Remove trials with errors or invalid RTs/ERPs
a1 <- na.omit(a1[!a1$error,])

# Define ggplot theme
styling <- theme(panel.grid = element_blank(),
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

# # Define color scheme (brewer)
# colors.conditions <- RColorBrewer::brewer.pal(3, name = "Set1")[c(3, 1, 2)]
# colors.highlight <- "#ffff33"
# colors.topo <- "RdBu"
# scale.topo <- scale_fill_distiller(palette = "RdBu", guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 1), breaks = c(-0.7, 0, 0.7))

# Define color scheme for conditions (plasma)
colors.conditions <- viridisLite::plasma(3, end = 0.9, direction = -1)[c(1, 2, 3)]
colors.highlight <- viridisLite::plasma(1, direction = -1)
colors.topo <- "plasma"
scale.topo <- scale_fill_viridis_c(option = "plasma", guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 1), breaks = c(-0.7, 0, 0.7))

# Assign names to colors
names(colors.conditions) <- c("Intuitive", "Violation", "MCI")

## BAR PLOTS ## -----------------------------------------------------------------------------------

# Convert dependent variables to long format
a1.long <- a1 %>% gather(key = "dv", value = "value", N400.verb, N400.pict, factor_key = TRUE)

# Compute summary statistics (means and confidence intervals) for verb-related and picture-related N400
summs <- sapply(c("N400.verb", "N400.pict"), function(dv){
  summ <- Rmisc::summarySEwithin(a1, measurevar = dv, withinvars = c("semantics", "context"), idvar = "participant", na.rm = TRUE)
  colnames(summ)[colnames(summ) == dv] <- "value"
  summ$dv <- dv
  return(summ)}, simplify = FALSE)

# Bar plots for verb-related and picture-related N400
bars <- sapply(c("N400.verb", "N400.pict"), function(what){
  # Different scales for verb-related and picture-related
  if(what == "N400.verb"){
    scaling <- list(ymin = -1.25, ymax = 0.25, step = 0.25)
    bracket <- data.frame(context = as.factor("Neutral context"),
                          ymin = 0.375*c(0.2, 0.5, 0.2),
                          ymax = 0.375*c(0.5, 0.5, 0.5),
                          xmin = c(0.7, 0.7, 1.3),
                          xmax = c(0.7, 1.3, 1.3))
    star <- data.frame("context" = as.factor("Neutral context"), "stars" = c("**"), ypos = 0.375*0.45)}
  else {
    scaling <- list(ymin = -3.5, ymax = 0.5, step = 1)
    bracket <- data.frame(context = as.factor("Neutral context"),
                          ymin = c(0.2, 0.5, 0.2),
                          ymax = c(0.5, 0.5, 0.5),
                          xmin = c(0.7, 0.7, 1.3),
                          xmax = c(0.7, 1.3, 1.3))
    star <- data.frame("context" = as.factor("Neutral context"), "stars" = c("*"), ypos = 0.45)}
  # Actual plotting
  ggplot(summs[names(summs) == what][[1]], aes(x = context, y = value, fill = semantics)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), position = position_dodge(width = 0.9), width = 0.5) +
    geom_segment(data = bracket, aes(x = xmin, y = ymin, xend = xmax, yend = ymax), inherit.aes = FALSE) +
    geom_label(data = star, aes(x = context, y = ypos, label = stars), inherit.aes = FALSE, size = 6, label.size = 0) +
    scale_fill_manual(values = colors.conditions) +
    labs(fill = "Semantics") +
    coord_cartesian(ylim = c(scaling$ymin, scaling$ymax)) +
    scale_x_discrete(labels = c("Neutral\ncontext", "Negative\ncontext")) +
    scale_y_continuous(name = "ROI amplitude (µV)", breaks = seq(scaling$ymin, scaling$ymax, scaling$step)) +
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
  geom_text(aes(x = 0.55, y = 0.8, label = "creaks"), size = 4.939, family = "Helvetica", fontface = "bold", color = colors.conditions[1], hjust = 0) +
  geom_text(aes(x = 0.55, y = 0.5, label = "blossoms"), size = 4.939, family = "Helvetica", fontface = "bold", color = colors.conditions[2], hjust = 0) +
  geom_text(aes(x = 0.55, y = 0.2, label = "talks"), size = 4.939, family = "Helvetica", fontface = "bold", color = colors.conditions[3], hjust = 0) +
  geom_text(aes(x = 0.675, y = 0.8, label = 'in the wind"'), size = 4.939, family = "Helvetica", hjust = 0) +
  geom_text(aes(x = 0.730, y = 0.5, label = 'above the girl"'), size = 4.939, family = "Helvetica", hjust = 0) +
  geom_text(aes(x = 0.643, y = 0.2, label = 'to the girl"'), size = 4.939, family = "Helvetica", hjust = 0) +
  draw_plot(get_legend(bars$N400.verb + theme(legend.position = "right", legend.title = element_blank())), x = 0.65, y = 0.5, vjust = 0.48)

## WAVEFORMS ## -----------------------------------------------------------------------------------

# ERP waveforms for verb-related and picture-related N400
waves <- sapply(c("Verb-related", "Picture-related"), function(what){
  # Different y-axis limits and shading for both plots
  if (what == "Verb-related"){
    lims <- list(ymin = -1.5, ymax = 1.5, step = 0.5, tmin = 0.300, tmax = 0.500)
  } else {
    lims <- list(ymin = -4, ymax = 3, step = 1, tmin = 0.150, tmax = 0.350)}
  # Significant area to highlight (MCI - intuitive in the neutral context)
  highlight <- avgs %>%
    select(ROI) %>% filter(between(as_time(.sample), !!lims$tmin, !!lims$tmax)) %>% 
    group_by(semantics, context, type, .sample) %>% summarise_at(channel_names(.), mean, na.rm = TRUE)
  highlight <- data.frame(seq(lims$tmin, lims$tmax, 0.002),
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
    geom_rect(aes(xmin = lims$tmin, xmax = lims$tmax, ymin = -Inf, ymax = Inf), fill = "gray90", inherit.aes = FALSE) +
    geom_ribbon(data = highlight, aes(x = .time, ymin = mci, ymax = int), fill = colors.highlight, inherit.aes = FALSE) +
    geom_text(data = star, aes(x = lims$tmin+(lims$tmax-lims$tmin)/2, y = lims$ymax-lims$step/2, label = stars), inherit.aes = FALSE, size = 6) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    stat_summary(fun = "mean", geom = "line") +
    scale_color_manual(values = colors.conditions) +
    coord_cartesian(xlim = c(-0.2, 0.8), ylim = c(lims$ymin-lims$step/2, lims$ymax+lims$step/2), expand = FALSE) +
    scale_x_continuous(breaks = seq(-0.1, 0.7, 0.2), labels = seq(-100, 700, 200)) +
    scale_y_continuous(breaks = seq(lims$ymin, lims$ymax, lims$step)) +
    xlab("Time (ms)") + ylab("ROI amplitude (µV)") +
    labs(color = NULL) +
    theme_bw() + styling + theme(legend.position = "none") +
    facet_grid(.~context)
}, simplify = FALSE)

## TOPOGRAPHIES ## --------------------------------------------------------------------------------

# Create scalp topographies for verb-related and picture-related N400
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
    p <- eegUtils::topoplot(data = tmp, quantity = colnames(tmp)[x], limits = c(-0.7, 0.7), r = 0.9, palette = colors.topo,
                            interp_limit = "skirt", contour = FALSE, highlights = c("C1", "C2", "CZ", "CP1", "CP2", "CPZ"), scaling = 0.5)
    p$layers[[6]]$aes_params$size <- 0.1
    p$layers[[7]]$aes_params$colour <- "black"
    p <- p + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, family = "Helvetica"))})
}, simplify = FALSE)

# Create a colorbar
simdat1 <- data.frame(a = 1:10, b = 1:10, c = seq(-0.7, 0.7, length.out = 10))
colbar <- get_legend(ggplot(simdat1, aes(x = a, y = b, fill = c)) + geom_raster() + geom_line() +
                       scale.topo +
                       labs(fill = "Ampl.\n(µV)") +
                       theme(legend.position = "right",
                             legend.background = element_blank(),
                             legend.key.height = unit(0.3, "cm"),
                             legend.title = element_text(family = "Helvetica", size = 10, color = "black"),
                             legend.text = element_text(family = "Helvetica", size = 10, color = "black"),
                             legend.title.align = 0.5))

# Create one plot combining all four topographies (verb-related)
simdat2 <- data.frame("semantics" = factor(c("Violation - intuitive", "MCI - intuitive")),
                      "context" = factor(c("Neutral\ncontext", "Negative\ncontext"), levels = c("Neutral\ncontext", "Negative\ncontext")))
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

# Create one plot combining all four topographies (picture-related)
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

## PUBLICATION-READY FIGURES ## -------------------------------------------------------------------

# Figure 1: Verb-Related N400 Effects
plot_grid(stim, waves$`Verb-related`,
          plot_grid(bars$N400.verb, topos.verb, nrow = 1, rel_widths = c(0.6, 1), labels = c("C", "D"),
                    label_fontfamily = "Helvetica", label_y = 1.03),
          nrow = 3, rel_heights = c(0.2, 0.8, 1), labels = c("A", "B", NULL), label_fontfamily = "Helvetica") %>%
  ggsave(filename = "EEG/figures/N400_verb.pdf", width = 18, height = 22, units = "cm")

# Figure 2: Picture-Related N400 Effects
plot_grid(plot_grid(waves$`Picture-related`) +
            draw_plot(get_legend(bars$N400.pict +
                                   theme(legend.position = "right", legend.title = element_blank(), legend.background = element_blank())),
                      x = 0.41, y = -0.22),
          plot_grid(bars$N400.pict, topos.pict, nrow = 1, rel_widths = c(0.6, 1), labels = c("B", "C"),
                    label_fontfamily = "Helvetica", label_y = 1.03),
          nrow = 2, rel_heights = c(0.8, 1), labels = c("A", NULL), label_fontfamily = "Helvetica") %>%
  ggsave(filename = "EEG/figures/N400_pict.pdf", width = 18, height = 19.8, units = "cm")

# Full system specs and package versions
sessionInfo()

