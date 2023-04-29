# Project:   plotmipca
# Objective: Prepare and deploy fdd results for app
# Author:    Edoardo Costantini
# Created:   2023-04-29
# Modified:  2023-04-29
# Notes: 

# Load data --------------------------------------------------------------------

dataFdd <- readRDS("./data/dataFdd.rds")

# Case study: results ----------------------------------------------------------

# Rename methods to match what is used in the paper
dataFdd$imp <- factor(dataFdd$imp,
    levels = c(
        "expert",
        "pcr.aux",
        "pcr.all",
        "pcr.vbv",
        "default"
    ),
    labels = c(
        "Expert",
        "MI-PCR-AUX",
        "MI-PCR-ALL",
        "MI-PCR-VBV",
        "Default"
    )
)

# Give meaningful names to the variables
names(dataFdd)[4] <- "Time"
names(dataFdd)[5] <- "PTSD-RI parent score"
names(dataFdd)[6] <- "PTSD-RI children score"

# Rename outcome measures as used in the paper
levels(dataFdd$trt) <- c("EMDR", "CBT")

# Rename time points as used in the paper
dataFdd$Time <- factor(dataFdd$Time, labels = c("T1", "T2", "T3"))

# Use the data
usethis::use_data(dataFdd, overwrite = TRUE)