# Project:   plotmipca
# Objective: Prepare and deploy fdd results for app
# Author:    Edoardo Costantini
# Created:   2023-04-29
# Modified:  2024-03-02
# Notes:

# Load data --------------------------------------------------------------------

# Results for simulation study
ggshapeld <- readRDS("./data-raw/9950505_main_gg_shape.rds") # 1st BRM submission w/ correct MI-OP
ggshapehd <- readRDS("./data-raw/9987321_main_gg_shape.rds") # HD version with full pj

# Results for FDD
dataFdd <- readRDS("./data-raw/20211220_144954_res.rds")

# Load mids for convergence checks
mids_sim <- readRDS("./data-raw/convergence-sim2-p242.rds")
mids_caseStudy_expert <- readRDS("./data-raw/convergence-expert.rds")
mids_caseStudy_si4auxall <- readRDS("./data-raw/convergence-si-4-aux-all.rds")
mids_caseStudy_pcraux <- readRDS("./data-raw/convergence-pcraux.rds")
mids_caseStudy_vbv <- readRDS("./data-raw/convergence-vbv.rds")
mids_caseStudy_default <- readRDS("./data-raw/convergence-default.rds")

# Result data ------------------------------------------------------------------

# > Simulation studies ---------------------------------------------------------

# Combine data
ggshape <- rbind(
    cbind(j = 56, ggshapeld),
    cbind(j = 242, ggshapehd)
)

# Map names of methods
methods_names <- data.frame(
    original = c(
        "all", "all_oracle", "aux", "vbv",
        "MIOP", "MIOR", "MIMI", "CC", "OG"
    ),
    plot = c(
        "MI-PCR-ALL", "MI-PCR-ALL (oracle)", "MI-PCR-AUX", "MI-PCR-VBV",
        "MI-QP", "MI-OR", "MI-MI", "CC", "OG"
    )
)

# Change names of factors for plot
methods_names <- methods_names[methods_names$original %in% levels(ggshape$method), ]
levels(ggshape$method) <- methods_names$plot

# Round pj
ggshape$pj <- round(ggshape$pj, 2)

# Make a different factor for labelling npcs
ggshape$NPC <- ggshape[, "npc"]
levels(ggshape$NPC) <- list(
    "7+" = as.character(7:max(as.numeric(levels(ggshape[, "npc"])))),
    "1 to 6" = as.character(1:6),
    "0" = c("0")
)

# Make npc names better
ggshape$npc <- as.numeric(as.character(ggshape$npc))

# Make Parameter names better
current_levels <- levels(ggshape$par)

current_levels <- gsub("~1", " mean", current_levels)
current_levels <- gsub("r", " correlation ", current_levels)
current_levels <- gsub("~~", " covariance ", current_levels)
levels(ggshape$par) <- current_levels

# Use data in the package
usethis::use_data(dataResults, overwrite = TRUE)

# > Case study -----------------------------------------------------------------

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

# MIDS data --------------------------------------------------------------------

# > Simulation studies ---------------------------------------------------------

# Compress mids for simulation study
for (i in 1:length(mids_sim)) {
    # Keep the only two objects you need for the trace plots
    mids_sim[[i]] <- list(
        chainMean = mids_sim[[i]]$chainMean,
        chainVar = mids_sim[[i]]$chainVar,
        m = mids_sim[[i]]$m,
        iteration = mids_sim[[i]]$iteration
    )

    # Get rid of non-imputed values
    mids_sim[[i]]$chainMean <- mids_sim[[i]]$chainMean[1:4, , ]
    mids_sim[[i]]$chainVar <- mids_sim[[i]]$chainVar[1:4, , ]
}

# Replace names of methods in mids sim 
names(mids_sim) <- gsub("MIMI", "MI-MI", names(mids_sim))
names(mids_sim) <- gsub("MIOP", "MI-OP", names(mids_sim))
names(mids_sim) <- gsub("MIOR", "MI-OR", names(mids_sim))
names(mids_sim) <- gsub("aux", "MI-PCA-AUX", names(mids_sim))
names(mids_sim) <- gsub("vbv", "MI-PCA-VBV", names(mids_sim))

# Use the data
usethis::use_data(mids_sim, overwrite = TRUE)

# > Case study -----------------------------------------------------------------

# Collect mids for data application

mids_casestudy <- list(
    expert = mids_caseStudy_expert,
    si4auxall = mids_caseStudy_si4auxall,
    pcraux = mids_caseStudy_pcraux$mids,
    vbv = mids_caseStudy_vbv,
    default = mids_caseStudy_default
)

# Compress mids for data application

for (i in 1:length(mids_casestudy)) {
    # Keep the only two objects you need for the trace plots
    mids_casestudy[[i]] <- list(
        chainMean = mids_casestudy[[i]]$chainMean,
        chainVar = mids_casestudy[[i]]$chainVar,
        m = mids_casestudy[[i]]$m,
        iteration = mids_casestudy[[i]]$iteration
    )

    # Which variables are we interested in?
    rowindex <- c("yp1", "yp2", "yp3", "yc1", "yc2", "yc3")

    # Get rid of non-imputed values
    mids_casestudy[[i]]$chainMean <- mids_casestudy[[i]]$chainMean[rowindex, , ]
    mids_casestudy[[i]]$chainVar <- mids_casestudy[[i]]$chainVar[rowindex, , ]
}

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
usethis::use_data(mids_case, overwrite = TRUE)
