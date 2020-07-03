#/* Run this first piece of code only if you want to create a markdown report for GitHub:
#+ eval = FALSE
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd())
#*/
#+

### MCI EMO PREPROCESSING SCRIPT ###

# Reads behavioral log files for all participants and binds them together. Performs EEG preprocessing
# including re-refercing, ocular artifact correction, filtering, epoching, baseline correction and
# automatic artifact rejection, separetly for verb- and picture-related potentials. Computes single-
# trial mean ERP amplitudes for the N400 component and exports by-participant averaged waveforms for
# plotting.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(naturalsort)  # Version 0.1.3
library(dplyr)        # Version 1.0.0
library(eeguana)      # Version 0.1.4.9000

# Make sure we have enough RAM available
memory.limit(size = 64000)

## BEHAVIORAL DATA ## -----------------------------------------------------------------------------

# List behavioral log files
filenames.rt <- list.files("RT", pattern = ".txt", full.names = TRUE)
filenames.rt <- naturalsort(filenames.rt)

# Read behavioral data into one data frame (if umlaute can't be read, try: Sys.setlocale("LC_ALL", "C"))
a1 <- lapply(filenames.rt, read.delim2)
a1 <- do.call(rbind, a1)

# Remove empty lines
a1 <- na.omit(a1)

# For exploratory analyses: Add factorized columns for lag 1 semantics and context manipulations
    # Fillers are coded as intuitive semantic condition and neutral context condition
a1 <- a1 %>% mutate(lag1Semantics = factor(lag(SatzBed, n=1), levels = c("filler", "neutral", "sem", "mci"),
                                           labels = c("int", "int", "vio", "mci")))
a1 <- a1 %>% mutate(lag1Context = factor(lag(EmoBed, n=1), levels =c(1,2), labels = c("neu", "neg")))

# Remove fillers
a1 <- subset(a1, SatzBed != "filler")

# Add factorized columns for semantics and context manipulations (fixed effects)
a1$semantics <- factor(a1$SatzBed, levels = c("neutral", "sem", "mci"), labels = c("int", "vio", "mci"))
a1$context <- factor(a1$EmoBed, levels = c(1, 2), labels = c("neu", "neg"))

# Add factorized columns for participants and items (random variables)
a1$participant <- as.factor(a1$VP_nr)
a1$item <- as.factor(a1$Verb)

# Add a column checking if participants made an error or if RTs are unrealistically short (< 200 ms)
a1$error <- a1$Errors == 99 | a1$BildRT <= 200

# Remove trials with missing EEG
a1 <- a1[-c(4002:4011),]
a1 <- a1[-7013,]
a1 <- a1[-c(9488:9492),]
a1 <- a1[-c(10300:10303),]

## EEG DATA ## ------------------------------------------------------------------------------------

# List EEG header files and BESA matrices
filenames.eeg <- list.files("EEG/raw", pattern = ".vhdr", full.names = TRUE)
filenames.besa <- list.files("EEG/cali", pattern = ".matrix", full.names = TRUE)

# Preprocessing for verb-related potentials
eeg.verb <- mapply(function(vhdr.filename, besa.filename){
  message(paste("## PREPROCESSING", vhdr.filename, "WITH", besa.filename, "(VERB-RELATED)"))
  dat <- read_vhdr(vhdr.filename)
  message("## FIXING CHANNEL SETUP...")
  eog <- dat$.signal$Auge_u
  dat$.signal$Auge_u <- 0
  dat <- dat %>% rename(A2 = Mastoid, A1 = Auge_u)
  message("## RE-REFERENCING...")
  channames <- dat %>% channel_names(.)
  dat <- dat %>% eeg_rereference(ref = channames)
  message("## OCCULAR CORRECTION...")
  besa <- as.matrix(read.delim(besa.filename, row.names = 1))
  tmp <- t(dat$.signal %>% select(all_of(channames)))
  tmp <- besa %*% tmp # This is the actual OC, above and below is just to transform the sginal table
  tmp <- split(tmp, row(tmp))
  tmp <- lapply(tmp, channel_dbl)
  dat$.signal[,channames] <- tmp[1:length(channames)]
  dat$.signal$IO1 <- eog
  message("## FILTERING...")
  dat <- dat %>% eeg_filt_band_pass(freq = c(0.1, 30))
  message("## EPOCHING...")
  dat <- dat %>% eeg_segment(.description %in% c("S211", "S212", "S213", "S221", "S222", "S223"),
                             lim = c(-200, 998), unit = "ms")
  message("## BASELINE CORRECTION...")
  dat <- dat %>% eeg_baseline()
  message("## ARTIFACT REJECTION...")
  dat <- dat %>%
    eeg_artif_amplitude(-IO1, threshold = c(-200, 200)) %>%
    eeg_artif_minmax(-IO1, threshold = 50, window = 2, unit = "ms") %>%
    eeg_artif_minmax(-IO1, threshold = 200, window = 200, unit = "ms") %>%
    eeg_events_to_NA(.type == "artifact", all_chs = TRUE)
  message("## DONE\n")
  return(dat)
}, filenames.eeg, filenames.besa, SIMPLIFY = FALSE)

# Preprocessing for picture-related potentials
eeg.pict <- mapply(function(vhdr.filename, besa.filename){
  message(paste("## PREPROCESSING", vhdr.filename, "WITH", besa.filename, "(PICTURE-RELATED)"))
  dat <- read_vhdr(vhdr.filename)
  message("## FIXING CHANNEL SETUP...")
  eog <- dat$.signal$Auge_u
  dat$.signal$Auge_u <- 0
  dat <- dat %>% rename(A2 = Mastoid, A1 = Auge_u)
  dat$.signal <- dat$.signal %>% select(.id:A1)
  message("## RE-REFERENCING...")
  channames <- dat %>% channel_names(.)
  dat <- dat %>% eeg_rereference(ref = channames)
  message("## OCCULAR CORRECTION...")
  besa <- as.matrix(read.delim(besa.filename, row.names = 1))
  tmp <- t(dat$.signal %>% select(all_of(channames)))
  tmp <- besa %*% tmp # This is the actual OC, above and below is just to transform the sginal table
  tmp <- split(tmp, row(tmp))
  tmp <- lapply(tmp, channel_dbl)
  dat$.signal[,channames] <- tmp[1:length(channames)]
  dat$.signal$IO1 <- eog
  message("## FILTERING...")
  dat <- dat %>% eeg_filt_band_pass(freq = c(0.1, 30))
  message("## EPOCHING...")
  dat <- dat %>% eeg_segment(.description %in% c("S181", "S182", "S183", "S191", "S192", "S193"),
                             lim = c(-200, 998), unit = "ms")
  message("## BASELINE CORRECTION...")
  dat <- dat %>% eeg_baseline()
  message("## ARTIFACT REJECTION...")
  dat <- dat %>%
    eeg_artif_amplitude(-IO1, threshold = c(-200, 200)) %>%
    eeg_artif_minmax(-IO1, threshold = 50, window = 2, unit = "ms") %>%
    eeg_artif_minmax(-IO1, threshold = 200, window = 200, unit = "ms") %>%
    eeg_events_to_NA(.type == "artifact", all_chs = TRUE)
  message("## DONE\n")
  return(dat)
}, filenames.eeg, filenames.besa, SIMPLIFY = FALSE)

# Bind all participants together
eeg.verb <- do.call(bind, eeg.verb)
eeg.pict <- do.call(bind, eeg.pict)

# Extract experimental factors from EEG triggers
eeg.verb <- eeg.verb %>% mutate(semantics = if_else(description %in% c("S211", "S221"), "int",
                                                    if_else(description %in% c("S212", "S222"), "vio", "mci")),
                                context = if_else(description %in% c("S211", "S212", "S213"), "neu", "neg"))
eeg.pict <- eeg.pict %>% mutate(semantics = if_else(description %in% c("S181", "S191"), "int",
                                                    if_else(description %in% c("S182", "S192"), "vio", "mci")),
                                context = if_else(description %in% c("S181", "S182", "S183"), "neu", "neg"))

# Factorize these columns
eeg.verb <- eeg.verb %>% mutate(semantics = factor(semantics, levels = c("int", "vio", "mci")),
                                context = factor(context, levels = c("neu", "neg")))
eeg.pict <- eeg.pict %>% mutate(semantics = factor(semantics, levels = c("int", "vio", "mci")),
                                context = factor(context, levels = c("neu", "neg")))

# Compute mean amplitude accross the N400/P600 ROI
eeg.verb <- eeg.verb %>% mutate(ROI = chs_mean(C1, C2, Cz, CP1, CP2, CPz))
eeg.pict <- eeg.pict %>% mutate(ROI = chs_mean(C1, C2, Cz, CP1, CP2, CPz))

# Average single trial ERPs across ROI electrodes in the relevant time window (and bind to behavioral data)
a1$N400.verb <- aggregate(ROI ~ .id, eeg.verb$.signal[between(as_time(.sample), 0.300, 0.500)], mean, na.action = NULL)$ROI
a1$N400.pict <- aggregate(ROI ~ .id, eeg.pict$.signal[between(as_time(.sample), 0.150, 0.350)], mean, na.action = NULL)$ROI

# Export behavioral data with ERPs for LMMs
saveRDS(a1, file = "EEG/export/a1.RDS")

## PREPARE FOR PLOTTING ## ------------------------------------------------------------------------

# Compute averaged waveforms for plotting
avgs.verb <- eeg.verb %>%
  mutate(participant = a1$participant, error = a1$error) %>%
  filter(!error) %>%
  group_by(.sample, semantics, context, participant) %>%
  summarize_at(channel_names(.), mean, na.rm = TRUE)
avgs.pict <- eeg.pict %>%
  mutate(participant = a1$participant, error = a1$error) %>%
  filter(!error) %>%
  group_by(.sample, semantics, context, participant) %>%
  summarize_at(channel_names(.), mean, na.rm = TRUE)

# Export averaged waveforms for plotting
saveRDS(avgs.verb, "EEG/export/avgs_verb.RDS")
saveRDS(avgs.pict, "EEG/export/avgs_pict.RDS")

