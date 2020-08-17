F01\_mci\_emotion\_preprocessing.R
================
neuro-lab
2020-07-14

``` r
### MCI EMO PREPROCESSING SCRIPT ###

# Reads behavioral log files for all participants and binds them together. Performs EEG preprocessing
# including re-refercing, ocular artifact correction, filtering, epoching, baseline correction, and
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
```

    ## [1] 64000

``` r
## BEHAVIORAL DATA ## -----------------------------------------------------------------------------

# List behavioral log files
filenames.rt <- list.files("RT", pattern = ".txt", full.names = TRUE)
filenames.rt <- naturalsort(filenames.rt)

# Read behavioral data into one data frame (if umlaute can't be read, try: Sys.setlocale("LC_ALL", "C"))
a1 <- lapply(filenames.rt, read.delim2)
a1 <- do.call(rbind, a1)

# Remove empty lines
a1 <- na.omit(a1)

# For exploratory analyses: Add factorized columns for semantics and context manipulations at lag 1
# Fillers are coded as belonging to the intuitive semantic and neutral context condition
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

# Add a column which checks if participants made an error or if RTs are unrealistically short (< 200 ms)
a1$error <- a1$Errors == 99 | a1$BildRT <= 200

# Remove trials with missing EEG (due to technical issues)
a1 <- a1[-c(4002:4011),]
a1 <- a1[-7013,]
a1 <- a1[-c(9488:9492),]
a1 <- a1[-c(10300:10303),]

## EEG DATA ## ------------------------------------------------------------------------------------

# List EEG header files and BESA matrices (for ocular correction)
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
  tmp <- besa %*% tmp # This is the actual OC; lines above and below are just transforming the sginal table
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
```

    ## ## PREPROCESSING EEG/raw/Vp0001.vhdr WITH EEG/cali/Vp0001.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0001.vhdr...

    ## # Data from EEG/raw/Vp0001.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.2 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0002.vhdr WITH EEG/cali/Vp0002.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0002.vhdr...

    ## # Data from EEG/raw/Vp0002.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.2 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0003.vhdr WITH EEG/cali/Vp0003.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0003.vhdr...

    ## # Data from EEG/raw/Vp0003.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0004.vhdr WITH EEG/cali/Vp0004.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0004.vhdr...

    ## # Data from EEG/raw/Vp0004.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0005.vhdr WITH EEG/cali/Vp0005.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0005.vhdr...

    ## # Data from EEG/raw/Vp0005.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0006.vhdr WITH EEG/cali/Vp0006.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0006.vhdr...

    ## # Data from EEG/raw/Vp0006.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.6 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0007.vhdr WITH EEG/cali/Vp0007.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0007.vhdr...

    ## # Data from EEG/raw/Vp0007.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0008.vhdr WITH EEG/cali/Vp0008.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0008.vhdr...

    ## # Data from EEG/raw/Vp0008.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0009.vhdr WITH EEG/cali/Vp0009.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0009.vhdr...

    ## # Data from EEG/raw/Vp0009.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0010.vhdr WITH EEG/cali/Vp0010.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0010.vhdr...

    ## # Data from EEG/raw/Vp0010.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0011.vhdr WITH EEG/cali/Vp0011.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0011.vhdr...

    ## # Data from EEG/raw/Vp0011.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 341 segments found.

    ## # Object size in memory 103.1 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 1

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0011bis.vhdr WITH EEG/cali/Vp0011bis.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0011bis.vhdr...

    ## # Data from EEG/raw/Vp0011bis.eeg was read.

    ## # Data from 1 segment(s) and 64 channels was loaded.

    ## # Object size in memory 105.9 Mb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 15 segments found.

    ## # Object size in memory 4.6 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 1

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0012.vhdr WITH EEG/cali/Vp0012.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0012.vhdr...

    ## # Data from EEG/raw/Vp0012.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0013.vhdr WITH EEG/cali/Vp0013.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0013.vhdr...

    ## # Data from EEG/raw/Vp0013.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.7 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 1

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0014.vhdr WITH EEG/cali/Vp0014.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0014.vhdr...

    ## # Data from EEG/raw/Vp0014.eeg was read.

    ## # Data from 7 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0015.vhdr WITH EEG/cali/Vp0015.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0015.vhdr...

    ## # Data from EEG/raw/Vp0015.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0016.vhdr WITH EEG/cali/Vp0016.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0016.vhdr...

    ## # Data from EEG/raw/Vp0016.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 1

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0017.vhdr WITH EEG/cali/Vp0017.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0017.vhdr...

    ## # Data from EEG/raw/Vp0017.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 1

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0018.vhdr WITH EEG/cali/Vp0018.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0018.vhdr...

    ## # Data from EEG/raw/Vp0018.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.1 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0019.vhdr WITH EEG/cali/Vp0019.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0019.vhdr...

    ## # Data from EEG/raw/Vp0019.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 2

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0020.vhdr WITH EEG/cali/Vp0020.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0020.vhdr...

    ## # Data from EEG/raw/Vp0020.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 365 segments found.

    ## # Object size in memory 110.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0021.vhdr WITH EEG/cali/Vp0021.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0021.vhdr...

    ## # Data from EEG/raw/Vp0021.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 2

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0022.vhdr WITH EEG/cali/Vp0022.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0022.vhdr...

    ## # Data from EEG/raw/Vp0022.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.7 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0023.vhdr WITH EEG/cali/Vp0023.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0023.vhdr...

    ## # Data from EEG/raw/Vp0023.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.4 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0024.vhdr WITH EEG/cali/Vp0024.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0024.vhdr...

    ## # Data from EEG/raw/Vp0024.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0025.vhdr WITH EEG/cali/Vp0025.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0025.vhdr...

    ## # Data from EEG/raw/Vp0025.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.7 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0026.vhdr WITH EEG/cali/Vp0026.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0026.vhdr...

    ## # Data from EEG/raw/Vp0026.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.6 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 348 segments found.

    ## # Object size in memory 105.2 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0026a.vhdr WITH EEG/cali/Vp0026a.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0026a.vhdr...

    ## # Data from EEG/raw/Vp0026a.eeg was read.

    ## # Data from 1 segment(s) and 64 channels was loaded.

    ## # Object size in memory 103.5 Mb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 13 segments found.

    ## # Object size in memory 4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0027.vhdr WITH EEG/cali/Vp0027.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0027.vhdr...

    ## # Data from EEG/raw/Vp0027.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0028.vhdr WITH EEG/cali/Vp0028.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0028.vhdr...

    ## # Data from EEG/raw/Vp0028.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.2 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 5

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 17

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0029.vhdr WITH EEG/cali/Vp0029.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0029.vhdr...

    ## # Data from EEG/raw/Vp0029.eeg was read.

    ## # Data from 6 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.4 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 362 segments found.

    ## # Object size in memory 109.5 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 4

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0030.vhdr WITH EEG/cali/Vp0030.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0030.vhdr...

    ## # Data from EEG/raw/Vp0030.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

``` r
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
  tmp <- besa %*% tmp # This is the actual OC; lines above and below are just transforming the sginal table
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
```

    ## ## PREPROCESSING EEG/raw/Vp0001.vhdr WITH EEG/cali/Vp0001.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0001.vhdr...

    ## # Data from EEG/raw/Vp0001.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.2 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 4

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 5

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0002.vhdr WITH EEG/cali/Vp0002.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0002.vhdr...

    ## # Data from EEG/raw/Vp0002.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.2 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 1

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 4

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0003.vhdr WITH EEG/cali/Vp0003.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0003.vhdr...

    ## # Data from EEG/raw/Vp0003.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0004.vhdr WITH EEG/cali/Vp0004.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0004.vhdr...

    ## # Data from EEG/raw/Vp0004.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0005.vhdr WITH EEG/cali/Vp0005.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0005.vhdr...

    ## # Data from EEG/raw/Vp0005.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 10

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0006.vhdr WITH EEG/cali/Vp0006.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0006.vhdr...

    ## # Data from EEG/raw/Vp0006.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.6 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0007.vhdr WITH EEG/cali/Vp0007.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0007.vhdr...

    ## # Data from EEG/raw/Vp0007.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0008.vhdr WITH EEG/cali/Vp0008.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0008.vhdr...

    ## # Data from EEG/raw/Vp0008.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 4

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0009.vhdr WITH EEG/cali/Vp0009.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0009.vhdr...

    ## # Data from EEG/raw/Vp0009.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0010.vhdr WITH EEG/cali/Vp0010.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0010.vhdr...

    ## # Data from EEG/raw/Vp0010.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0011.vhdr WITH EEG/cali/Vp0011.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0011.vhdr...

    ## # Data from EEG/raw/Vp0011.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 341 segments found.

    ## # Object size in memory 103.1 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0011bis.vhdr WITH EEG/cali/Vp0011bis.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0011bis.vhdr...

    ## # Data from EEG/raw/Vp0011bis.eeg was read.

    ## # Data from 1 segment(s) and 64 channels was loaded.

    ## # Object size in memory 105.9 Mb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 15 segments found.

    ## # Object size in memory 4.6 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0012.vhdr WITH EEG/cali/Vp0012.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0012.vhdr...

    ## # Data from EEG/raw/Vp0012.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 1

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0013.vhdr WITH EEG/cali/Vp0013.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0013.vhdr...

    ## # Data from EEG/raw/Vp0013.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.7 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 2

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 5

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0014.vhdr WITH EEG/cali/Vp0014.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0014.vhdr...

    ## # Data from EEG/raw/Vp0014.eeg was read.

    ## # Data from 7 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0015.vhdr WITH EEG/cali/Vp0015.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0015.vhdr...

    ## # Data from EEG/raw/Vp0015.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 3

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 8

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0016.vhdr WITH EEG/cali/Vp0016.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0016.vhdr...

    ## # Data from EEG/raw/Vp0016.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0017.vhdr WITH EEG/cali/Vp0017.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0017.vhdr...

    ## # Data from EEG/raw/Vp0017.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0018.vhdr WITH EEG/cali/Vp0018.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0018.vhdr...

    ## # Data from EEG/raw/Vp0018.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.1 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0019.vhdr WITH EEG/cali/Vp0019.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0019.vhdr...

    ## # Data from EEG/raw/Vp0019.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 1

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0020.vhdr WITH EEG/cali/Vp0020.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0020.vhdr...

    ## # Data from EEG/raw/Vp0020.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 365 segments found.

    ## # Object size in memory 110.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 15

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 64

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0021.vhdr WITH EEG/cali/Vp0021.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0021.vhdr...

    ## # Data from EEG/raw/Vp0021.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 2

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 2

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0022.vhdr WITH EEG/cali/Vp0022.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0022.vhdr...

    ## # Data from EEG/raw/Vp0022.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.7 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 26

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 18

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0023.vhdr WITH EEG/cali/Vp0023.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0023.vhdr...

    ## # Data from EEG/raw/Vp0023.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.4 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0024.vhdr WITH EEG/cali/Vp0024.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0024.vhdr...

    ## # Data from EEG/raw/Vp0024.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0025.vhdr WITH EEG/cali/Vp0025.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0025.vhdr...

    ## # Data from EEG/raw/Vp0025.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.7 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0026.vhdr WITH EEG/cali/Vp0026.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0026.vhdr...

    ## # Data from EEG/raw/Vp0026.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.6 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 348 segments found.

    ## # Object size in memory 105.3 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0026a.vhdr WITH EEG/cali/Vp0026a.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0026a.vhdr...

    ## # Data from EEG/raw/Vp0026a.eeg was read.

    ## # Data from 1 segment(s) and 64 channels was loaded.

    ## # Object size in memory 103.5 Mb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 13 segments found.

    ## # Object size in memory 4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0027.vhdr WITH EEG/cali/Vp0027.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0027.vhdr...

    ## # Data from EEG/raw/Vp0027.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 4

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 29

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0028.vhdr WITH EEG/cali/Vp0028.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0028.vhdr...

    ## # Data from EEG/raw/Vp0028.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.2 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0029.vhdr WITH EEG/cali/Vp0029.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0029.vhdr...

    ## # Data from EEG/raw/Vp0029.eeg was read.

    ## # Data from 6 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.4 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 362 segments found.

    ## # Object size in memory 109.5 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 5

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 3

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0030.vhdr WITH EEG/cali/Vp0030.matrix (PICTURE-RELATED)

    ## Reading file EEG/raw/Vp0030.vhdr...

    ## # Data from EEG/raw/Vp0030.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## Warning: `keys` of signal table are missing.

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 366 segments found.

    ## # Object size in memory 110.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 1

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 2

    ## ## DONE

``` r
# Bind data from all participants together
eeg.verb <- do.call(bind, eeg.verb)
```

    ## # Object size in memory 3.2 Gb

``` r
eeg.pict <- do.call(bind, eeg.pict)
```

    ## # Object size in memory 3.2 Gb

``` r
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

# Compute mean amplitude accross electrodes in the N400 ROI
eeg.verb <- eeg.verb %>% mutate(ROI = chs_mean(C1, C2, Cz, CP1, CP2, CPz))
eeg.pict <- eeg.pict %>% mutate(ROI = chs_mean(C1, C2, Cz, CP1, CP2, CPz))

# Average single trial ERPs in the ROI across the relevant time window (and bind to behavioral data)
a1$N400.verb <- aggregate(ROI ~ .id, eeg.verb$.signal[between(as_time(.sample), 0.300, 0.500)], mean, na.action = NULL)$ROI
a1$N400.pict <- aggregate(ROI ~ .id, eeg.pict$.signal[between(as_time(.sample), 0.150, 0.350)], mean, na.action = NULL)$ROI

# Export behavioral data and ERPs for mixed models
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

# Full system specs and package versions
sessionInfo()
```

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 18363)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252
    ## [4] LC_NUMERIC=C                    LC_TIME=German_Germany.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] eeguana_0.1.4.9000 dplyr_1.0.0        naturalsort_0.1.3 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.5        pillar_1.4.6      compiler_4.0.2    highr_0.8         tools_4.0.2      
    ##  [6] digest_0.6.25     evaluate_0.14     lifecycle_0.2.0   tibble_3.0.3      gtable_0.3.0     
    ## [11] pkgconfig_2.0.3   rlang_0.4.7       rstudioapi_0.11   yaml_2.2.1        RcppRoll_0.3.0   
    ## [16] xfun_0.15         stringr_1.4.0     knitr_1.29        generics_0.0.2    vctrs_0.3.1      
    ## [21] grid_4.0.2        tidyselect_1.1.0  glue_1.4.1        data.table_1.12.8 R6_2.4.1         
    ## [26] rmarkdown_2.3     ggplot2_3.3.2     purrr_0.3.4       magrittr_1.5      scales_1.1.1     
    ## [31] ellipsis_0.3.1    htmltools_0.5.0   MASS_7.3-51.6     colorspace_1.4-1  stringi_1.4.6    
    ## [36] ini_0.3.1         signal_0.7-6      munsell_0.5.0     crayon_1.3.4
