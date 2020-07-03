#/* Run this first piece of code only if you want to create a markdown report for GitHub:
#+ eval = FALSE
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd())
#*/
#+

### MCI EMO DESCRIPTIVES SCRIPT ###

# Gives descriptive information about the number of (a) rejected ERP epochs and (b) errors or
# unrealistically short reaction times per participant. For the latter, an ANOVA also checks whether
# the number of errors differs between experimental conditions.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(Rmisc)     # version 1.5

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Add a column for rejected ERPs
a1$rejected <- is.na(a1$N400.verb) | is.na(a1$N400.pict)

# Check number of rejected ERPs participant (mean, percent, median, min, max)
mean(table(a1$rejected, a1$participant)[2,])
mean(table(a1$rejected, a1$participant)[2,])/366
median(table(a1$rejected, a1$participant)[2,])
min(table(a1$rejected, a1$participant)[2,])
max(table(a1$rejected, a1$participant)[2,])

# Remove rejected ERPs from the data
a1 <- a1[!a1$rejected,]

# Check number of errors per participant (mean, percent, median, min, max)
mean(table(a1$error, a1$participant)[2,])
mean(table(a1$error, a1$participant)[2,])/366
median(table(a1$error, a1$participant)[2,])
min(table(a1$error, a1$participant)[2,])
max(table(a1$error, a1$participant)[2,])

# Check if error tates differ between conditions
accuracy <- summarySEwithin(a1, measurevar = "error", withinvars = c("semantics", "context"),
                            betweenvars = "participant", idvar = "participant", na.rm = TRUE)
summary(aov(error ~ semantics*context + Error(participant/(semantics*context)), data = accuracy))

# SessionInfo
sessionInfo()