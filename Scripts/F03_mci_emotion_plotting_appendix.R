#/* Run this first piece of code only if you want to create a markdown report for GitHub
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd()) #*/

### MCI EMO PLOTTING SCRIPT - APPENDIX (P600) ###

# Creates a bar plot, an ERP waveform, and scalp topographies for the verb-related P600 effect 
# different semantic conditions (intuitive, violation, MCI) within each type of emotional 
# context (neutral, negative).

## PREPARATION ## ---------------------------------------------------------------------------------

# Load packages
library(tidyverse) # Version 1.3
library(magrittr)  # Version 1.5
library(eeguana)   # Version 0.1.4.9000
library(cowplot)   # Version 1.0.0

# Load preprocessed data
a1 <- readRDS("EEG/export/a1_appendix.RDS")
avgs <- readRDS("EEG/export/avgs_verb_appendix.RDS")

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

# Define colours for conditions
condition.colors <- RColorBrewer::brewer.pal(3, name = "Set1")[c(3, 1, 2)]
names(condition.colors) <- c("Intuitive", "Violation", "MCI")

## TOPOGRAPHIES ONLY ## ---------------------------------------------------------------------------

# Define color scales
colors.topo <- "RdBu"
scales.topo <- scale_fill_distiller(palette = "RdBu", breaks = c(-0.7, 0, 0.7),
                                    guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 0.5, title.vjust = 1))
# colors.topo <- "plasma"
# scales.topo <- scale_fill_viridis_c(option = "plasma", breaks = c(-0.7, 0, 0.7),
#                                     guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 0.5, title.vjust = 1))

# Create a color bar for the topographies
colbar <- get_legend(ggplot(data.frame(x = c(0, 0), y = c(0, 0), fill = c(-0.7, 0.7)), aes(x = x, y = y, fill = fill)) + geom_raster() +
                       scales.topo +
                       labs(fill = "Ampl.\n(µV)") +
                       theme(legend.direction = "horizontal",
                             legend.key.width = unit(0.3, "cm"),
                             legend.title = element_text(family = "Helvetica", size = 10, color = "black"),
                             legend.text = element_text(family = "Helvetica", size = 10, color = "black"),
                             legend.title.align = 0.5))

# Create scalp topographies for P600 (new ROI)
map(c(0.5, 0.6, 0.7, 0.8), function(tmin){
  tmax <- tmin + 0.1
  # Compute differences between conditions in time window at all electrodes
  avgs %>% filter(between(as_time(.sample), !!tmin, !!tmax)) %>%
    group_by(semantics, context) %>% summarise_at(channel_names(.), mean) %>%
    signal_tbl() %>% select(Fp1:A1) %>% t() %>% as.data.frame() %>%
    set_colnames(c("int.neu", "int.neg", "vio.neu", "vio.neg", "mci.neu", "mci.neg")) %>%
    transmute(diff.vio.neu = vio.neu - int.neu,
              diff.vio.neg = vio.neg - int.neg,
              diff.mci.neu = mci.neu - int.neu,
              diff.mci.neg = mci.neg - int.neg) %>%
    # Create four topoplots for the current time window
    map(function(amplitudes){
      dat <- data.frame(amplitude = amplitudes, electrode = avgs %>% select(Fp1:A1) %>% channel_names())
      p <- eegUtils::topoplot(data = dat,
                              limits = c(-0.7, 0.7),
                              r = 0.9,
                              palette = colors.topo,
                              interp_limit = "skirt", 
                              contour = FALSE,
                              highlights = c("C1", "C2", "CZ", "FC1", "FC2", "FZ"),
                              scaling = 0.4)
      p$layers[[6]]$aes_params$size <- 0.1
      p$layers[[7]]$aes_params$colour <- "black"
      p + theme(legend.position = "none")}) %>%
    # Add another plot denoting the current time window in ms
    prepend(list(ggplot() + theme_void() +
                   annotate("text", x = 0, y = 0, label = paste0(tmin*1000, "-", tmax*1000, " ms"), size = 3.528, family = "Helvetica"))) %>%
    # Combine the five plots below one another
    plot_grid(plotlist = ., nrow = 5, rel_heights = c(1, 3, 3, 3, 3))}) %>%
  prepend(list(plot_grid(NULL,
                         ggplot() + theme_void() +
                           annotate("text", x = 0, y = 0, label = paste0("Violation - intuitive\n(neutral context)"), size = 3.528, family = "Helvetica"),
                         ggplot() + theme_void() +
                           annotate("text", x = 0, y = 0, label = paste0("Violation - intuitive\n(negative context)"), size = 3.528, family = "Helvetica"),
                         ggplot() + theme_void() +
                           annotate("text", x = 0, y = 0, label = paste0("MCI - intuitive\n(neutral context)"), size = 3.528, family = "Helvetica"),
                         ggplot() + theme_void() +
                           annotate("text", x = 0, y = 0, label = paste0("MCI - intuitive\n(negative context)"), size = 3.528, family = "Helvetica"),
                         nrow = 5, rel_heights = c(1, 3, 3, 3, 3)))) %>%
  # Combine all time windows next to each other
  plot_grid(plotlist = ., nrow = 1) +
  # Add colorbar
  draw_plot(colbar, x = -0.403, y = 0.42) +
  # Save plot
  ggsave(filename = "EEG/figures/P600_appendix.pdf", width = 18, height = 15, units = "cm")

# ## BAR PLOTS ## -----------------------------------------------------------------------------------
# 
# # Convert dependent variables to long format
# a1.long <- a1 %>% gather(key = "dv", value = "value", P600_1.verb, P600_2.verb, P600_3.verb, P600_4.verb, factor_key = TRUE)
# 
# # Compute summary statistics (means and confidence intervals) for verb-related and picture-related N400
# summs <- sapply(c("P600_1.verb", "P600_2.verb", "P600_3.verb", "P600_4.verb"), function(dv){
#   summ <- Rmisc::summarySEwithin(a1, measurevar = dv, withinvars = c("semantics", "context"), idvar = "participant", na.rm = TRUE)
#   colnames(summ)[colnames(summ) == dv] <- "value"
#   summ$dv <- dv
#   return(summ)}, simplify = FALSE)
# 
# # Bar plots for verb-related P600 (old ROI)
# bars_old <- sapply(c("P600_1.verb", "P600_2.verb"), function(what){
#   # Actual plotting
#   ggplot(summs[names(summs) == what][[1]], aes(x = context, y = value, fill = semantics)) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#     geom_errorbar(aes(ymin = value - ci, ymax = value + ci), position = position_dodge(width = 0.9), width = 0.5) +
#     scale_fill_manual(values = condition.colors) +
#     labs(fill = "Semantics") +
#     coord_cartesian(ylim = c(-2, 3)) +
#     scale_x_discrete(labels = c("Neutral\ncontext", "Negative\ncontext")) +
#     scale_y_continuous(name = "ROI amplitude (µV)", breaks = seq(-2, 2, 2)) +
#     geom_hline(yintercept = 0) +
#     theme_bw() + styling + theme(axis.title.x = element_blank(), legend.position = "none")
# }, simplify = FALSE)
# 
# # Bar plots for verb-related P600 (new ROI)
# bars_new <- sapply(c("P600_3.verb", "P600_4.verb"), function(what){
#   # Brackets and stars for statistical significance
#   bracket <- data.frame(context = as.factor("Negative context"),
#                         ymin = c(-0.4, -0.6, -0.2),
#                         ymax = c(-0.6, -0.6, -0.6),
#                         xmin = c(1.7, 1.7, 2.0),
#                         xmax = c(1.7, 2.0, 2.0))
#   # Actual plotting
#   ggplot(summs[names(summs) == what][[1]], aes(x = context, y = value, fill = semantics)) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#     geom_errorbar(aes(ymin = value - ci, ymax = value + ci), position = position_dodge(width = 0.9), width = 0.5) +
#     geom_segment(data = bracket, aes(x = xmin, y = ymin, xend = xmax, yend = ymax), inherit.aes = FALSE) +
#     geom_label(aes(x = 1.85, y = -0.67, label = "*"), inherit.aes = FALSE, size = 6, label.size = 0) +
#     scale_fill_manual(values = condition.colors) +
#     labs(fill = "Semantics") +
#     coord_cartesian(ylim = c(-2, 3)) +
#     scale_x_discrete(labels = c("Neutral\ncontext", "Negative\ncontext")) +
#     scale_y_continuous(name = "ROI amplitude (µV)", breaks = seq(-2, 3, 1)) +
#     geom_hline(yintercept = 0) +
#     theme_bw() + styling + theme(axis.title.x = element_blank(), legend.position = "none")
# }, simplify = FALSE)
# 
# ## WAVEFORMS ## -----------------------------------------------------------------------------------
# 
# # ERP waveforms for P600 (old ROI)
# waves_old <- sapply(c("Verb-related (500-700 ms)", "Verb-related (500-900 ms)"), function(what){
#   tmin <- 0.500
#   tmax <- ifelse(what == "Verb-related (500-700 ms)", 0.700, 0.900)
#   x_lim <- ifelse(what == "Verb-related (500-700 ms)", 0.8, 1.0)
#   x_break <- ifelse(what == "Verb-related (500-700 ms)", 0.7, 0.9)
#   x_label <- ifelse(what == "Verb-related (500-700 ms)", 700, 900)
#   # Actual plotting
#   avgs %>%
#     select(ROI) %>%
#     ggplot(aes(x = .time, y = .value, color = semantics)) +
#     geom_rect(aes(xmin = tmin, xmax = tmax, ymin = -Inf, ymax = Inf), fill = "gray90", inherit.aes = FALSE) +
#     geom_hline(yintercept = 0, linetype = "dotted") +
#     geom_vline(xintercept = 0, linetype = "dotted") +
#     stat_summary(fun = "mean", geom = "line") +
#     scale_color_manual(values = condition.colors) +
#     coord_cartesian(xlim = c(-0.2, x_lim), ylim = c(-4.5, 4.5), expand = FALSE) +
#     scale_x_continuous(breaks = seq(-0.1, x_break, 0.2), labels = seq(-100, x_label, 200)) +
#     scale_y_continuous(breaks = seq(-4, 4, 2)) +
#     xlab("Time (ms)") + ylab("ROI amplitude (µV)") +
#     labs(color = NULL) +
#     theme_bw() + styling + theme(legend.position = "none") +
#     facet_grid(.~context)
# }, simplify = FALSE)
# 
# # ERP waveforms for P600 (new ROI)
# waves_new <- sapply(c("Verb-related (500-700 ms)", "Verb-related (500-900 ms)"), function(what){
#   tmin <- 0.500
#   tmax <- ifelse(what == "Verb-related (500-700 ms)", 0.700, 0.900)
#   x_lim <- ifelse(what == "Verb-related (500-700 ms)", 0.8, 1.0)
#   x_break <- ifelse(what == "Verb-related (500-700 ms)", 0.7, 0.9)
#   x_label <- ifelse(what == "Verb-related (500-700 ms)", 700, 900)
#   # Significant area to highlight (MCI - intuitive in the neutral context)
#   highlight <- avgs %>%
#     select(ROI2) %>% filter(between(as_time(.sample), !!tmin, !!tmax)) %>% 
#     group_by(semantics, context, .sample) %>% summarise_at(channel_names(.), mean, na.rm = TRUE)
#   highlight <- data.frame(seq(tmin, tmax, 0.002),
#                           highlight %>% filter(semantics == "Violation", context == "Negative context") %>% signal_tbl %>% select(ROI2),
#                           highlight %>% filter(semantics == "Intuitive", context == "Negative context") %>% signal_tbl %>% select(ROI2))
#   names(highlight) <- c(".time", "vio", "int")
#   highlight$context <- as.factor("Negative context")
#   # Stars for significance levels
#   star <- data.frame(context = as.factor("Negative context"), stars = "*")
#   # Actual plotting
#   avgs %>%
#     select(ROI2) %>%
#     ggplot(aes(x = .time, y = .value, color = semantics)) +
#     geom_rect(aes(xmin = tmin, xmax = tmax, ymin = -Inf, ymax = Inf), fill = "gray90", inherit.aes = FALSE) +
#     geom_ribbon(data = highlight, aes(x = .time, ymin = vio, ymax = int), fill = "#ffff33", inherit.aes = FALSE) +
#     geom_text(data = star, aes(x = tmin+(tmax-tmin)/2, y = 3.5, label = stars), inherit.aes = FALSE, size = 6) +
#     geom_hline(yintercept = 0, linetype = "dotted") +
#     geom_vline(xintercept = 0, linetype = "dotted") +
#     stat_summary(fun = "mean", geom = "line") +
#     scale_color_manual(values = condition.colors) +
#     coord_cartesian(xlim = c(-0.2, x_lim), ylim = c(-4.5, 4.5), expand = FALSE) +
#     scale_x_continuous(breaks = seq(-0.1, x_break, 0.2), labels = seq(-100, x_label, 200)) +
#     scale_y_continuous(breaks = seq(-4, 4, 2)) +
#     xlab("Time (ms)") + ylab("ROI amplitude (µV)") +
#     labs(color = NULL) +
#     theme_bw() + styling + theme(legend.position = "none") +
#     facet_grid(.~context)
# }, simplify = FALSE)
# 
# ## TOPOGRAPHIES ## --------------------------------------------------------------------------------
# 
# # Create scalp topographies for P600 (old ROI)
# topos_old <- sapply(c("Verb-related (500-700 ms)", "Verb-related (500-900 ms)"), function(what){
#   if(what == "Verb-related (500-700 ms)"){
#     tmp <- avgs %>% filter(between(as_time(.sample), 0.500, 0.700))
#   } else{
#     tmp <- avgs %>% filter(between(as_time(.sample), 0.500, 0.900))}
#   tmp <- tmp %>%
#     group_by(semantics, context) %>% summarise_at(channel_names(.), mean) %>%
#     signal_tbl() %>% select(Fp1:A1) %>% t() %>% as.data.frame()
#   names(tmp) <- c("int.neu", "int.neg", "vio.neu", "vio.neg", "mci.neu", "mci.neg")
#   tmp <- data.frame("diff.vio.neu" = tmp$vio.neu - tmp$int.neu,
#                     "diff.vio.neg" = tmp$vio.neg - tmp$int.neg,
#                     "diff.mci.neu" = tmp$mci.neu - tmp$int.neu,
#                     "diff.mci.neg" = tmp$mci.neg - tmp$int.neg,
#                     "electrode" = rownames(tmp))
#   topos <- lapply(1:4, function(x){
#     p <- eegUtils::topoplot(data = tmp, quantity = colnames(tmp)[x], limits = c(-0.7, 0.7), r = 0.9, palette = "RdBu", interp_limit = "skirt", 
#                             contour = FALSE, highlights = c("C1", "C2", "CZ", "CP1", "CP2", "CPZ"), scaling = 0.5)
#     p$layers[[6]]$aes_params$size <- 0.1
#     p$layers[[7]]$aes_params$colour <- "black"
#     p <- p + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, family = "Helvetica"))})
# }, simplify = FALSE)
# 
# # Create scalp topographies for P600 (new ROI)
# topos_new <- sapply(c("Verb-related (500-700 ms)", "Verb-related (500-900 ms)"), function(what){
#   if(what == "Verb-related (500-700 ms)"){
#     tmp <- avgs %>% filter(between(as_time(.sample), 0.500, 0.700))
#   } else{
#     tmp <- avgs %>% filter(between(as_time(.sample), 0.500, 0.900))}
#   tmp <- tmp %>%
#     group_by(semantics, context) %>% summarise_at(channel_names(.), mean) %>%
#     signal_tbl() %>% select(Fp1:A1) %>% t() %>% as.data.frame()
#   names(tmp) <- c("int.neu", "int.neg", "vio.neu", "vio.neg", "mci.neu", "mci.neg")
#   tmp <- data.frame("diff.vio.neu" = tmp$vio.neu - tmp$int.neu,
#                     "diff.vio.neg" = tmp$vio.neg - tmp$int.neg,
#                     "diff.mci.neu" = tmp$mci.neu - tmp$int.neu,
#                     "diff.mci.neg" = tmp$mci.neg - tmp$int.neg,
#                     "electrode" = rownames(tmp))
#   topos <- lapply(1:4, function(x){
#     p <- eegUtils::topoplot(data = tmp, quantity = colnames(tmp)[x], limits = c(-0.7, 0.7), r = 0.9, palette = "RdBu", interp_limit = "skirt", 
#                             contour = FALSE, highlights = c("C1", "C2", "CZ", "FC1", "FC2", "FZ"), scaling = 0.5)
#     p$layers[[6]]$aes_params$size <- 0.1
#     p$layers[[7]]$aes_params$colour <- "black"
#     p <- p + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10, family = "Helvetica"))})
# }, simplify = FALSE)
# 
# # Create a colorbar
# simdat1 <- data.frame(a = 1:10, b = 1:10, c = seq(-0.7, 0.7, length.out = 10))
# colbar <- get_legend(ggplot(simdat1, aes(x = a, y = b, fill = c)) + geom_raster() + geom_line() +
#                        scale_fill_distiller(palette = "RdBu", guide = guide_colorbar(ticks = FALSE, title.position = "left", label.hjust = 1), breaks = c(-0.7, 0, 0.7)) +
#                        labs(fill = "Ampl.\n(µV)") +
#                        theme(legend.position = "right",
#                              legend.background = element_blank(),
#                              legend.key.height = unit(0.3, "cm"),
#                              legend.title = element_text(family = "Helvetica", size = 10, color = "black"),
#                              legend.text = element_text(family = "Helvetica", size = 10, color = "black"),
#                              legend.title.align = 0.5))
# 
# # Create one plot combining all four topographies (time window 500-700 ms, old ROI)
# simdat2 <- data.frame("semantics" = factor(c("Violation - Intuitive", "MCI - Intuitive")),
#                       "context" = factor(c("Neutral\ncontext", "Negative\ncontext"), levels = c("Neutral\ncontext", "Negative\ncontext")))
# topos.P600_1 <- ggplot(simdat2, aes(x = context, y = semantics)) +
#   geom_point() +
#   draw_plot(topos_old$`Verb-related (500-700 ms)`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-700 ms)`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-700 ms)`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-700 ms)`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
#   annotate("text", x = 1.5, y = 0.53, label = "500-700 ms", size = 3.528, family = "Helvetica") + 
#   draw_plot(colbar, x = 0.95, y = 1) +
#   styling +
#   theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
#         panel.background = element_rect(fill = "white"),
#         axis.title = element_blank(),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# # Create one plot combining all four topographies (time window 500-900 ms, old ROI)
# topos.P600_2 <- ggplot(simdat2, aes(x = context, y = semantics)) +
#   geom_point() +
#   draw_plot(topos_old$`Verb-related (500-900 ms)`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-900 ms)`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-900 ms)`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
#   draw_plot(topos_old$`Verb-related (500-900 ms)`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
#   annotate("text", x = 1.5, y = 0.53, label = "500-900 ms", size = 3.528, family = "Helvetica") +
#   draw_plot(colbar, x = 0.95, y = 1) +
#   styling +
#   theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
#         panel.background = element_rect(fill = "white"),
#         axis.title = element_blank(),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# # Create one plot combining all four topographies (time window 500-700 ms, new ROI)
# topos.P600_3 <- ggplot(simdat2, aes(x = context, y = semantics)) +
#   geom_point() +
#   draw_plot(topos_new$`Verb-related (500-700 ms)`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-700 ms)`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-700 ms)`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-700 ms)`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
#   annotate("text", x = 1.5, y = 0.53, label = "500-700 ms", size = 3.528, family = "Helvetica") + 
#   draw_plot(colbar, x = 0.95, y = 1) +
#   styling +
#   theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
#         panel.background = element_rect(fill = "white"),
#         axis.title = element_blank(),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# # Create one plot combining all four topographies (time window 500-900 ms, new ROI)
# topos.P600_4 <- ggplot(simdat2, aes(x = context, y = semantics)) +
#   geom_point() +
#   draw_plot(topos_new$`Verb-related (500-900 ms)`[[1]], x = 0.4, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-900 ms)`[[2]], x = 1.5, y = 1.5, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-900 ms)`[[3]], x = 0.4, y = 0.4, width = 1.1, height = 1.1) +
#   draw_plot(topos_new$`Verb-related (500-900 ms)`[[4]], x = 1.5, y = 0.4, width = 1.1, height = 1.1) +
#   annotate("text", x = 1.5, y = 0.53, label = "500-900 ms", size = 3.528, family = "Helvetica") +
#   draw_plot(colbar, x = 0.95, y = 1) +
#   styling +
#   theme(panel.border = element_rect(colour = "black", size = 1, fill = alpha("white", 0)),
#         panel.background = element_rect(fill = "white"),
#         axis.title = element_blank(),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# ## PUBLICATION-READY FIGURES ## -------------------------------------------------------------------
# 
# # Figure 1: Verb-related potentials (ROI 500-700 ms, old ROI)
# plot_grid(plot_grid(waves_old$`Verb-related (500-700 ms)`) +
#             draw_plot(get_legend(bars_old$P600_1.verb +
#                                    theme(legend.position = "right", legend.title = element_blank(), legend.background = element_blank())),
#                       x = 0.41, y = -0.22),
#           plot_grid(bars_old$P600_1.verb, topos.P600_1, nrow = 1, rel_widths = c(0.6, 1), labels = c("B", "C"),
#                     label_fontfamily = "Helvetica", label_y = 1.03),
#           nrow = 2, rel_heights = c(0.8, 1), labels = c("A", NULL), label_fontfamily = "Helvetica") %>%
#   ggsave(filename = "EEG/figures/appendix_P600_500-700ms_old_ROI.pdf", width = 18, height = 19.8, units = "cm")
# 
# # Figure 1: Verb-related potentials (ROI 500-900 ms, old ROI)
# plot_grid(plot_grid(waves_old$`Verb-related (500-900 ms)`) +
#             draw_plot(get_legend(bars_old$P600_2.verb +
#                                    theme(legend.position = "right", legend.title = element_blank(), legend.background = element_blank())),
#                       x = 0.41, y = -0.22),
#           plot_grid(bars_old$P600_2.verb, topos.P600_2, nrow = 1, rel_widths = c(0.6, 1), labels = c("B", "C"),
#                     label_fontfamily = "Helvetica", label_y = 1.03),
#           nrow = 2, rel_heights = c(0.8, 1), labels = c("A", NULL), label_fontfamily = "Helvetica") %>%
#   ggsave(filename = "EEG/figures/appendix_P600_500-900ms_old_ROI.pdf", width = 18, height = 19.8, units = "cm")
# 
# # Figure 1: Verb-related potentials (ROI 500-700 ms, new ROI)
# plot_grid(plot_grid(waves_new$`Verb-related (500-700 ms)`) +
#             draw_plot(get_legend(bars_new$P600_3.verb +
#                                    theme(legend.position = "right", legend.title = element_blank(), legend.background = element_blank())),
#                       x = 0.41, y = -0.22),
#           plot_grid(bars_new$P600_3.verb, topos.P600_3, nrow = 1, rel_widths = c(0.6, 1), labels = c("B", "C"),
#                     label_fontfamily = "Helvetica", label_y = 1.03),
#           nrow = 2, rel_heights = c(0.8, 1), labels = c("A", NULL), label_fontfamily = "Helvetica") %>%
#   ggsave(filename = "EEG/figures/appendix_P600_500-700ms_new_ROI.pdf", width = 18, height = 19.8, units = "cm")
# 
# # Figure 1: Verb-related potentials (ROI 500-900 ms, new ROI)
# plot_grid(plot_grid(waves_new$`Verb-related (500-900 ms)`) +
#             draw_plot(get_legend(bars_new$P600_4.verb +
#                                    theme(legend.position = "right", legend.title = element_blank(), legend.background = element_blank())),
#                       x = 0.41, y = -0.22),
#           plot_grid(bars_new$P600_4.verb, topos.P600_4, nrow = 1, rel_widths = c(0.6, 1), labels = c("B", "C"),
#                     label_fontfamily = "Helvetica", label_y = 1.03),
#           nrow = 2, rel_heights = c(0.8, 1), labels = c("A", NULL), label_fontfamily = "Helvetica") %>%
#   ggsave(filename = "EEG/figures/appendix_P600_500-900ms_new_ROI.pdf", width = 18, height = 19.8, units = "cm")
# 
# # Full system specs and package versions
# sessionInfo()
# 
