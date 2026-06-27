# ==============================================================================
#
#   SPECIES DISTRIBUTION MODELLING
#   Leioproctus launcestonensis (Cockerell, 1910)
#   Master Run Script
#
#
#   Date     : 2026
#
# ------------------------------------------------------------------------------
#   DESCRIPTION
#   -----------
#   Runs the complete SDM pipeline from start to finish.
#   Calls each script in sequence with error handling and timing.
#
#   USAGE
#   -----
#   1. Open R or RStudio
#   2. Set working directory:
#        setwd("path/to/SDM_Leioproctus")
#   3. Set GBIF credentials (if using Method B — session only):
#        Sys.setenv(GBIF_USER="...", GBIF_PWD="...", GBIF_EMAIL="...")
#   4. Run this script:
#        source("run_all.R")
#
#   ESTIMATED RUNTIME
#   -----------------
#   Part 0 (setup):        2-10 min
#   Part 1 (main):         30-60 min  (dominated by model fitting)
#   Part 2 (extended):     5-15 min
#   Part 3 (tables):       1-2 min
#   Part 4 (sensitivity):  60-120 min (optional — see SKIP_SENSITIVITY)
#   Part 5 (figures):      5-10 min
#   TOTAL:                 ~1.5-3 hours (without sensitivity)
#                          ~3-5 hours   (with sensitivity)
#
#   SKIP OPTIONS
#   ------------
#   Set to TRUE to skip optional/slow sections:
#     SKIP_SENSITIVITY = TRUE   Skip Part 4 sensitivity analyses
#     SKIP_SETUP       = FALSE  Skip Part 0 if already verified

SKIP_SENSITIVITY <- TRUE    # Set FALSE to run all sensitivity analyses
SKIP_SETUP       <- FALSE   # Set TRUE if already run setup

# ==============================================================================

cat("\n", strrep("=", 65), "\n")
cat("  SDM Pipeline: Leioproctus launcestonensis\n")
cat("  Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 65), "\n\n")

# Check working directory
cat("Working directory:", getwd(), "\n")
required_scripts <- c(
  "00_SDM_Leioproctus_setup.R",
  "01_SDM_Leioproctus_main.R",
  "02_SDM_Leioproctus_extended.R",
  "03_SDM_Leioproctus_manuscript_tables.R",
  "04_SDM_Leioproctus_sensitivity.R",
  "05_SDM_Leioproctus_figures_final.R",
  "utils_SDM_helpers.R"
)

missing_scripts <- required_scripts[!file.exists(required_scripts)]
if (length(missing_scripts) > 0) {
  stop("\nMissing scripts — ensure working directory is correct:\n  ",
       paste(missing_scripts, collapse = "\n  "),
       "\n\nRun: setwd('path/to/SDM_Leioproctus')\n")
}
cat("All scripts found.\n\n")

# Timing log
pipeline_log <- data.frame(
  script    = character(0),
  status    = character(0),
  start     = as.POSIXct(character(0)),
  end       = as.POSIXct(character(0)),
  duration_min = numeric(0),
  stringsAsFactors = FALSE
)

# Helper: run one script with timing and error handling
run_script <- function(script_name, skip = FALSE) {
  if (skip) {
    cat(strrep("-", 65), "\n")
    cat("SKIPPED:", script_name, "\n\n")
    return(data.frame(
      script = script_name, status = "SKIPPED",
      start = Sys.time(), end = Sys.time(), duration_min = 0
    ))
  }

  cat(strrep("-", 65), "\n")
  cat("RUNNING:", script_name, "\n")
  cat("Started:", format(Sys.time(), "%H:%M:%S"), "\n\n")

  t_start <- Sys.time()
  result  <- tryCatch({
    source(script_name, echo = FALSE, local = FALSE)
    "SUCCESS"
  }, error = function(e) {
    cat("\n!!! ERROR in", script_name, "!!!\n")
    cat("Message:", conditionMessage(e), "\n")
    cat("Traceback:\n")
    traceback()
    paste0("ERROR: ", conditionMessage(e))
  }, warning = function(w) {
    cat("WARNING:", conditionMessage(w), "\n")
    "SUCCESS (with warnings)"
  })

  t_end    <- Sys.time()
  duration <- as.numeric(difftime(t_end, t_start, units = "mins"))

  cat("\n")
  cat("Status:   ", result, "\n")
  cat("Duration: ", round(duration, 1), "minutes\n\n")

  data.frame(
    script       = script_name,
    status       = result,
    start        = t_start,
    end          = t_end,
    duration_min = round(duration, 2),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# RUN PIPELINE
# ==============================================================================

# Part 0: Setup
pipeline_log <- rbind(pipeline_log,
  run_script("00_SDM_Leioproctus_setup.R", skip = SKIP_SETUP)
)

# Check GBIF credentials before proceeding
if (nchar(Sys.getenv("GBIF_USER")) == 0) {
  cat(strrep("!", 65), "\n")
  cat("GBIF credentials not set!\n")
  cat("Set them now and re-run from Part 1:\n\n")
  cat('  Sys.setenv(GBIF_USER  = "your_username")\n')
  cat('  Sys.setenv(GBIF_PWD   = "your_password")\n')
  cat('  Sys.setenv(GBIF_EMAIL = "your@email.com")\n\n')
  cat("Or see Section 3.1 of 01_SDM_Leioproctus_main.R for Method A.\n")
  cat(strrep("!", 65), "\n")
  stop("Pipeline halted — GBIF credentials required.")
}

# Part 1: Main pipeline
pipeline_log <- rbind(pipeline_log,
  run_script("01_SDM_Leioproctus_main.R")
)

# Check Part 1 succeeded before continuing
if (grepl("ERROR", pipeline_log$status[pipeline_log$script ==
                                         "01_SDM_Leioproctus_main.R"])) {
  cat("Part 1 failed — stopping pipeline.\n")
  cat("Fix the error above and re-run.\n")
  stop("Pipeline halted after Part 1 failure.")
}

# Part 2: Extended analyses
pipeline_log <- rbind(pipeline_log,
  run_script("02_SDM_Leioproctus_extended.R")
)

# Part 3: Manuscript tables
pipeline_log <- rbind(pipeline_log,
  run_script("03_SDM_Leioproctus_manuscript_tables.R")
)

# Part 4: Sensitivity analyses (optional)
pipeline_log <- rbind(pipeline_log,
  run_script("04_SDM_Leioproctus_sensitivity.R",
             skip = SKIP_SENSITIVITY)
)

# Part 5: Final figures
pipeline_log <- rbind(pipeline_log,
  run_script("05_SDM_Leioproctus_figures_final.R")
)

# ==============================================================================
# PIPELINE SUMMARY
# ==============================================================================

total_time <- sum(pipeline_log$duration_min, na.rm = TRUE)

cat("\n", strrep("=", 65), "\n")
cat("  PIPELINE COMPLETE\n")
cat("  Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("  Total runtime:", round(total_time, 1), "minutes\n")
cat(strrep("=", 65), "\n\n")

cat("Script summary:\n")
for (i in seq_len(nrow(pipeline_log))) {
  row <- pipeline_log[i, ]
  status_icon <- dplyr::case_when(
    row$status == "SUCCESS"           ~ "✓",
    row$status == "SKIPPED"           ~ "-",
    grepl("ERROR",   row$status)      ~ "✗",
    grepl("WARNING", row$status)      ~ "~",
    TRUE                              ~ "?"
  )
  cat(sprintf("  %s  %-48s  %5.1f min  [%s]\n",
              status_icon,
              row$script,
              row$duration_min,
              row$status))
}

# Count outputs
n_maps    <- length(list.files("outputs/maps",    pattern="\\.tif$"))
n_figs    <- length(list.files("outputs/figures/paper", pattern="\\.pdf$"))
n_tables  <- length(list.files("outputs/evaluation",    pattern="\\.csv$"))

cat("\nOutputs generated:\n")
cat(sprintf("  Maps (GeoTIFF): %d\n", n_maps))
cat(sprintf("  Figures (PDF):  %d\n", n_figs))
cat(sprintf("  Tables (CSV):   %d\n", n_tables))

# Save log
dir.create("outputs", recursive=TRUE, showWarnings=FALSE)
write.csv(pipeline_log, "outputs/pipeline_run_log.csv", row.names=FALSE)
cat("\nRun log saved: outputs/pipeline_run_log.csv\n")

# Check for any failures
failed <- pipeline_log$script[grepl("ERROR", pipeline_log$status)]
if (length(failed) > 0) {
  cat("\n!!! FAILURES DETECTED !!!\n")
  cat("The following scripts encountered errors:\n")
  for (f in failed) cat("  -", f, "\n")
  cat("Review the error messages above and re-run failed scripts.\n\n")
} else {
  cat("\nAll scripts completed successfully.\n")
  cat("Results are in: outputs/\n\n")
}

