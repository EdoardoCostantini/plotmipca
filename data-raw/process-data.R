# Project:   plotmipca
# Objective: Prepare and deploy fdd results for app
# Author:    Edoardo Costantini
# Created:   2023-04-29
# Modified:  2023-04-29
# Notes:

# Load data --------------------------------------------------------------------

# Load mids for convergence checks
mids_sim <- readRDS("./data-raw/convergence-sim2-p242.rds")
mids_caseStudy_expert <- readRDS("./data-raw/convergence-expert.rds")
mids_caseStudy_si4auxall <- readRDS("./data-raw/convergence-si-4-aux-all.rds")
mids_caseStudy_pcraux <- readRDS("./data-raw/convergence-pcraux.rds")
mids_caseStudy_vbv <- readRDS("./data-raw/convergence-vbv.rds")
mids_caseStudy_default <- readRDS("./data-raw/convergence-default.rds")

# results for FDD
dataFdd <- readRDS("./data-raw/20211220_144954_res.rds")

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

# Mids -------------------------------------------------------------------------

# Replace names of methods in mids sim 
names(mids_sim) <- gsub("MIMI", "MI-MI", names(mids_sim))
names(mids_sim) <- gsub("MIOP", "MI-OP", names(mids_sim))
names(mids_sim) <- gsub("MIOR", "MI-OR", names(mids_sim))
names(mids_sim) <- gsub("aux", "MI-PCA-AUX", names(mids_sim))
names(mids_sim) <- gsub("vbv", "MI-PCA-VBV", names(mids_sim))

# Subset important elements from mids sim
for (i in 1:length(mids_sim)) {
    temp <- mids_sim[[i]][c("chainMean", "chainVar", "m", "iteration")]
    temp[["chainMean"]] <- temp[["chainMean"]][1:4, , ]
    temp[["chainVar"]] <- temp[["chainVar"]][1:4, , ]
    mids_sim[[i]] <- temp
}

# Subset important elements from mids case study
mids_case <- list(
    expert = mids_caseStudy_expert[c("chainMean", "chainVar", "m", "iteration")],
    si4auxall = mids_caseStudy_si4auxall[c("chainMean", "chainVar", "m", "iteration")],
    pcraux = mids_caseStudy_pcraux$mids[c("chainMean", "chainVar", "m", "iteration")],
    vbv = mids_caseStudy_vbv[c("chainMean", "chainVar", "m", "iteration")],
    deafult = mids_caseStudy_default[c("chainMean", "chainVar", "m", "iteration")]
)

for (i in 1:length(mids_case)) {
    temp <- mids_case[[i]]
    
    # Subset correct variables
    temp[["chainMean"]] <- temp[["chainMean"]][c("yp1", "yp2", "yp3", "yc1", "yc2", "yc3"), , ]
    temp[["chainVar"]] <- temp[["chainVar"]][c("yp1", "yp2", "yp3", "yc1", "yc2", "yc3"), , ]

    # Store output
    mids_case[[i]] <- temp
}

# Replace names of methods in mids sim
names(mids_case) <- gsub("expert", "Expert", names(mids_case))
names(mids_case) <- gsub("si4auxall", "SI-PCA-ALL", names(mids_case))
names(mids_case) <- gsub("pcraux", "MI-PCA-AUX", names(mids_case))
names(mids_case) <- gsub("vbv", "MI-PCA-VBV", names(mids_case))
names(mids_case) <- gsub("deafult", "Default", names(mids_case))

# Use data in the package
usethis::use_data(mids_sim, overwrite = TRUE)
usethis::use_data(mids_case, overwrite = TRUE)
