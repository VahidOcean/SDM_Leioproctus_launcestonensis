# ==============================================================================
#
#   SPECIES DISTRIBUTION MODELLING
#   Leioproctus launcestonensis (Cockerell, 1910)
#   Part 0 — Project Setup and Environment Check
#
#   Date     : 2026
#   R version: >= 4.3.0
#
# ------------------------------------------------------------------------------
#   DESCRIPTION
#   -----------
#   Run this script FIRST before any other script in the pipeline.
#   It installs all required packages, verifies GBIF credentials,
#   tests internet connectivity, and prints a full environment report.
#   This script does not download any data or fit any models.
#
#   Run time: 2-10 minutes (depending on how many packages need installing)
#
# ==============================================================================


# ==============================================================================
# SECTION 1: R VERSION CHECK
# ==============================================================================

r_ver <- getRversion()
message("R version: ", r_ver)

if (r_ver < "4.3.0") {
  warning(
    "\nR version ", r_ver, " detected.\n",
    "This pipeline was developed and tested under R >= 4.3.0.\n",
    "Some package features may not work correctly on older versions.\n",
    "Please consider updating R from: https://cran.r-project.org\n"
  )
} else {
  message("R version OK (>= 4.3.0)")
}


# ==============================================================================
# SECTION 2: INSTALL ALL REQUIRED PACKAGES
# ==============================================================================

# Complete list of all packages used across Parts 1-5
all_packages <- list(

  # --- SDM core ---
  sdm = c(
    "biomod2",     # Ensemble SDM framework (v4+)
    "maxnet",      # MaxEnt via glmnet (required by biomd2 MAXNET)
    "ENMeval"      # MaxEnt regularisation tuning
  ),

  # --- Spatial ---
  spatial = c(
    "terra",             # Modern raster/vector handling
    "sf",                # Simple features vector data
    "rnaturalearth",     # Country boundary polygons
    "rnaturalearthdata", # Data for rnaturalearth
    "geodata"            # WorldClim and CMIP6 downloads
  ),

  # --- Occurrence data ---
  occurrence = c(
    "rgbif",   # GBIF programmatic download
    "rinat"    # iNaturalist API access
  ),

  # --- Data cleaning ---
  cleaning = c(
    "spThin",  # Spatial thinning
    "usdm"     # Variance inflation factor analysis
  ),

  # --- Extended analyses ---
  analyses = c(
    "ecospat",           # Boyce index + niche overlap
    "landscapemetrics"   # Landscape connectivity metrics
  ),

  # --- Visualisation ---
  viz = c(
    "ggplot2",      # Core plotting
    "ggpubr",       # Multi-panel plots
    "ggtext",       # Rich text in ggplot
    "viridis",      # Colour-blind-safe palettes
    "RColorBrewer", # Colour palettes
    "corrplot",     # Correlation matrix plots
    "patchwork",    # Combining ggplot panels
    "scales",       # Axis formatting
    "gridExtra",    # Additional plot arrangement
    "grid"          # Low-level graphics
  ),

  # --- Data wrangling ---
  wrangling = c(
    "dplyr",  # Data manipulation
    "tidyr"   # Data reshaping
  ),

  # --- Tables and reporting ---
  tables = c(
    "knitr",      # R Markdown tables
    "kableExtra", # Enhanced knitr tables
    "flextable",  # Publication tables
    "officer"     # Word document creation
  ),

  # --- Legacy (required by some biomod2 internals) ---
  legacy = c(
    "raster"   # Legacy raster package
  )
)

# Flatten to single vector
all_pkgs_flat <- unlist(all_packages, use.names=FALSE)

# Check installation status
pkg_status <- data.frame(
  package    = all_pkgs_flat,
  installed  = sapply(all_pkgs_flat,
                       requireNamespace, quietly=TRUE),
  stringsAsFactors = FALSE
)

message("\nPackage installation status:")
message("  Total required: ", nrow(pkg_status))
message("  Already installed: ", sum(pkg_status$installed))
message("  Missing: ", sum(!pkg_status$installed))

# Install missing packages
missing_pkgs <- pkg_status$package[!pkg_status$installed]
if (length(missing_pkgs) > 0) {
  message("\nInstalling ", length(missing_pkgs), " missing packages:")
  message("  ", paste(missing_pkgs, collapse=", "))
  install.packages(missing_pkgs, repos="https://cloud.r-project.org")
  message("Installation complete.")
} else {
  message("\nAll packages already installed.")
}

# Verify installation
pkg_status$installed_after <- sapply(
  all_pkgs_flat, requireNamespace, quietly=TRUE
)
failed_install <- pkg_status$package[!pkg_status$installed_after]

if (length(failed_install) > 0) {
  warning(
    "\nThe following packages failed to install:\n  ",
    paste(failed_install, collapse=", "),
    "\nTry installing manually or check for system dependencies."
  )
} else {
  message("All packages successfully installed.")
}


# ==============================================================================
# SECTION 3: LOAD AND VERSION CHECK
# ==============================================================================

message("\n--- Loading packages and checking versions ---")

pkg_versions <- data.frame(
  package = all_pkgs_flat,
  stringsAsFactors = FALSE
)

pkg_versions$version <- sapply(all_pkgs_flat, function(pkg) {
  tryCatch(
    as.character(packageVersion(pkg)),
    error = function(e) "NOT INSTALLED"
  )
})

# Key packages with known version requirements
version_requirements <- list(
  "biomod2"  = "4.0.0",    # v4 has different API from v3
  "terra"    = "1.7.0",    # SpatRaster class
  "sf"       = "1.0.0",
  "ENMeval"  = "2.0.0"
)

message("\nKey package versions:")
for (pkg in names(version_requirements)) {
  installed_ver <- pkg_versions$version[pkg_versions$package == pkg]
  required_ver  <- version_requirements[[pkg]]
  ok <- tryCatch(
    package_version(installed_ver) >= package_version(required_ver),
    error = function(e) FALSE
  )
  status <- if (ok) "OK" else paste0("WARNING: requires >= ", required_ver)
  message(sprintf("  %-20s %s  [%s]", pkg, installed_ver, status))
}

# Full version table
write.csv(pkg_versions,
          "outputs/session_packages.csv",
          row.names=FALSE)


# ==============================================================================
# SECTION 4: GBIF CREDENTIALS CHECK
# ==============================================================================

message("\n--- GBIF credentials check ---")

gbif_user  <- Sys.getenv("GBIF_USER")
gbif_pwd   <- Sys.getenv("GBIF_PWD")
gbif_email <- Sys.getenv("GBIF_EMAIL")

if (nchar(gbif_user) == 0 || nchar(gbif_pwd) == 0 ||
    nchar(gbif_email) == 0) {
  message("  GBIF credentials: NOT SET")
  message("\n  To set GBIF credentials:")
  message("  METHOD A — Permanent (recommended):")
  message("    Run: usethis::edit_r_environ()")
  message("    Add these lines, then SAVE and RESTART R:")
  message('      GBIF_USER="your_username"')
  message('      GBIF_PWD="your_password"')
  message('      GBIF_EMAIL="your_email@example.com"')
  message("\n  METHOD B — Session only (run before Part 1):")
  message('    Sys.setenv(GBIF_USER  = "your_username")')
  message('    Sys.setenv(GBIF_PWD   = "your_password")')
  message('    Sys.setenv(GBIF_EMAIL = "your_email@example.com")')
  message("\n  Register free account at: https://www.gbif.org/user/profile")
} else {
  message("  GBIF_USER:  ", gbif_user, "  [SET]")
  message("  GBIF_PWD:   ", paste(rep("*", nchar(gbif_pwd)), collapse=""),
          "  [SET]")
  message("  GBIF_EMAIL: ", gbif_email, "  [SET]")

  # Test GBIF connectivity
  message("\n  Testing GBIF API connection...")
  test_result <- tryCatch({
    result <- rgbif::occ_count(taxonKey=1, georeferenced=TRUE)
    paste0("SUCCESS (", format(result, big.mark=","),
           " georeferenced records available)")
  }, error = function(e) {
    paste0("FAILED: ", conditionMessage(e))
  })
  message("  GBIF API: ", test_result)
}


# ==============================================================================
# SECTION 5: INTERNET CONNECTIVITY TEST
# ==============================================================================

message("\n--- Internet connectivity ---")

urls_to_test <- list(
  "GBIF API"    = "https://api.gbif.org",
  "WorldClim"   = "https://worldclim.org",
  "iNaturalist" = "https://api.inaturalist.org",
  "CRAN"        = "https://cloud.r-project.org"
)

for (name in names(urls_to_test)) {
  url    <- urls_to_test[[name]]
  status <- tryCatch({
    con <- url(url, open="rb", timeout=5)
    close(con)
    "OK"
  }, warning = function(w) "OK (with warning)",
     error   = function(e) paste0("FAILED: ", conditionMessage(e)))
  message(sprintf("  %-20s %s", name, status))
}


# ==============================================================================
# SECTION 6: DISK SPACE CHECK
# ==============================================================================

message("\n--- Disk space ---")

# Estimated disk requirements
requirements <- data.frame(
  item = c(
    "WorldClim current (2.5 arcmin)",
    "WorldClim future (3 GCMs × 4 scenarios)",
    "Occurrence data",
    "biomd2 model objects",
    "Projection rasters (5 scenarios)",
    "Figures (PDF + PNG)",
    "TOTAL ESTIMATED"
  ),
  size_mb = c(350, 1200, 1, 500, 200, 150, 2400)
)

message("  Estimated disk requirements:")
for (i in 1:nrow(requirements)) {
  message(sprintf("  %-45s ~%s MB",
                  requirements$item[i],
                  format(requirements$size_mb[i], big.mark=",")))
}

# Check available disk space
tryCatch({
  disk_info <- system("df -m . | tail -1", intern=TRUE)
  available_mb <- as.numeric(strsplit(disk_info, "\\s+")[[1]][4])
  message(sprintf("\n  Available disk space: ~%s MB",
                  format(available_mb, big.mark=",")))
  if (available_mb < 3000) {
    warning("Available disk space may be insufficient (< 3 GB recommended).")
  } else {
    message("  Disk space OK.")
  }
}, error = function(e) {
  message("  Could not check disk space on this system.")
})


# ==============================================================================
# SECTION 7: PROJECT DIRECTORY STRUCTURE
# ==============================================================================

message("\n--- Creating project directory structure ---")

dirs <- c(
  "data/raw",
  "data/processed",
  "data/climate/worldclim",
  "data/climate/worldclim/ensemble",
  "outputs/maps",
  "outputs/models",
  "outputs/evaluation",
  "outputs/tables",
  "outputs/figures/paper",
  "outputs/sensitivity"
)

for (d in dirs) {
  dir.create(d, recursive=TRUE, showWarnings=FALSE)
  status <- if (dir.exists(d)) "OK" else "FAILED"
  message(sprintf("  %-45s %s", d, status))
}


# ==============================================================================
# SECTION 8: ENVIRONMENT REPORT
# ==============================================================================

message("\n", strrep("=",60))
message("ENVIRONMENT REPORT")
message(strrep("-",60))
message("R version:      ", r_ver)
message("Platform:       ", .Platform$OS.type, " / ", R.version$os)
message("Working dir:    ", getwd())
message("Date:           ", Sys.time())
message("")
message("Packages:       ",
        sum(pkg_versions$version != "NOT INSTALLED"),
        " / ", nrow(pkg_versions), " installed")
message("biomd2 version: ",
        pkg_versions$version[pkg_versions$package=="biomod2"])
message("terra version:  ",
        pkg_versions$version[pkg_versions$package=="terra"])
message("")

# GBIF status summary
if (nchar(Sys.getenv("GBIF_USER")) > 0) {
  message("GBIF:           Credentials set for ", Sys.getenv("GBIF_USER"))
} else {
  message("GBIF:           Credentials NOT set (required for Part 1)")
}

message("")
message("Next step: Run 01_SDM_Leioproctus_main.R")
message(strrep("=",60))

# Save full session info
if (!dir.exists("outputs")) dir.create("outputs", recursive=TRUE)
writeLines(capture.output(sessionInfo()),
           "outputs/session_info_setup.txt")
message("\nFull session info saved: outputs/session_info_setup.txt")

