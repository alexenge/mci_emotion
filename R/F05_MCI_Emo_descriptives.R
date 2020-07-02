### MCI EMO DESCRIPTIVES SCRIPT ###

# Gives descriptive information about the number of (a) rejected ERP epochs and (b) errors or
# unrealistically short reaction times per participant. For the latter, an ANOVA also checks whether
# the number of errors differs between experimental conditions.

## SETUP ## ---------------------------------------------------------------------------------------

# Load packages
library(Rmisc)

# Load preprocessed data
a1 <- readRDS("Results/EEG/export/a1.RDS")

# Add a column for rejected ERPs
a1$rejected <- is.na(a1$N400) | is.na(a1$Npic)

# Check number of rejected ERPs participant (mean, median, min, max)
mean(table(a1$rejected, a1$participant)[2,])
median(table(a1$rejected, a1$participant)[2,])
min(table(a1$rejected, a1$participant)[2,])
max(table(a1$rejected, a1$participant)[2,])

# Remove rejected ERPs from the data
a1 <- a1[!a1$rejected,]

# Check number of errors per participant (mean, median, min, max)
mean(table(a1$error, a1$participant)[2,])
median(table(a1$error, a1$participant)[2,])
min(table(a1$error, a1$participant)[2,])
max(table(a1$error, a1$participant)[2,])

# Check if error tates differ between conditions
accuracy <- summarySEwithin(a1, measurevar = "error", withinvars = c("semantics", "context"),
                            betweenvars = "participant", idvar = "participant", na.rm = TRUE)
summary(aov(error ~ semantics*context + Error(participant/(semantics*context)), data = accuracy))

