# ==============================================================================
#
#   SPECIES DISTRIBUTION MODELLING
#   Leioproctus launcestonensis (Cockerell, 1910)
#   Helper Utilities — Shared Functions
#
#   Authors  : Vahid Sepahvand
#   Date     : 2026
#
# ------------------------------------------------------------------------------
#   DESCRIPTION
#   -----------
#   This script defines all shared utility functions used across Parts 1-5.
#   Source this file at the top of any script to access all helpers.
#   Does not install packages or produce outputs by itself.
#
#   Usage:
#     source("utils_SDM_helpers.R")
#
#   FUNCTIONS
#   ---------
#   RASTER UTILITIES
#     get_wmean()          Extract EMwmean suitability layer from ensemble TIF
#     get_binary()         Extract TSS-thresholded binary layer from ensemble TIF
#     rast_to_df()         Convert SpatRaster to masked ggplot-ready data frame
#     range_change_rast()  Compute 4-category range change raster
#     suit_area_km2()      Compute suitable area (km²) from binary raster
#
#   PLOTTING
#     theme_sdm()          Shared ggplot theme for all SDM maps and figures
#     suit_scale()         Shared viridis fill scale for suitability maps
#     change_scale()       Shared fill scale for range change maps
#     save_figure()        Save ggplot as editable PDF + high-res PNG
#     save_pdf_only()      Save ggplot as editable PDF only
#
#   DATA HELPERS
#     load_if_missing()    Load RDS object if not already in environment
#     check_files()        Check required files exist; stop with clear message
#     make_dirs()          Create project directory structure
#
#   EVALUATION
#     compute_boyce()      Compute continuous Boyce index (ecospat)
#     compute_centroid()   Weighted centroid of suitable habitat
#     suit_area_region()   Suitable area within a masked region
#     change_areas()       Range change km² per category
#
# ==============================================================================


# ==============================================================================
# RASTER UTILITIES
# ==============================================================================

#' Extract the EMwmean (weighted mean) suitability layer from an ensemble TIF
#'
#' @param tif_path  Path to ensemble GeoTIFF
#' @return          SpatRaster (single layer, continuous 0-1000 scale)
get_wmean <- function(tif_path) {
  r   <- terra::rast(tif_path)
  lyr <- grep("EMwmean", names(r), value = TRUE)[1]
  if (!is.na(lyr)) return(r[[lyr]])
  message("  EMwmean layer not found in ", tif_path,
          " — using first layer.")
  return(r[[1]])
}


#' Extract TSS-thresholded binary layer from ensemble TIF
#'
#' If a pre-computed binary layer exists in the TIF it is returned directly.
#' Otherwise, the EMwmean layer is thresholded at tss_cutoff.
#'
#' @param tif_path   Path to ensemble GeoTIFF
#' @param tss_cutoff TSS-optimised threshold (default 410; update from eval_em)
#' @return           SpatRaster (binary: 1=suitable, 0=unsuitable)
get_binary <- function(tif_path, tss_cutoff = 410) {
  r       <- terra::rast(tif_path)
  bin_lyr <- grep("EMwmean.*bin|bin.*EMwmean", names(r), value = TRUE)[1]
  if (!is.na(bin_lyr)) return(r[[bin_lyr]])
  wmean_lyr <- grep("EMwmean", names(r), value = TRUE)[1]
  if (is.na(wmean_lyr)) wmean_lyr <- names(r)[1]
  terra::ifel(r[[wmean_lyr]] >= tss_cutoff, 1L, 0L)
}


#' Convert SpatRaster to a ggplot-ready data frame
#'
#' Scales suitability from 0-1000 to 0-1, removes NA values,
#' and masks very low suitability (< threshold) so land background shows through.
#'
#' @param rast_obj    SpatRaster (suitability, 0-1000 scale)
#' @param scale01     Logical; scale values from 0-1000 to 0-1 (default TRUE)
#' @param min_suit    Minimum suitability to retain (default 0.05 on 0-1 scale)
#' @return            data.frame with columns x, y, suit
rast_to_df <- function(rast_obj, scale01 = TRUE, min_suit = 0.05) {
  df <- as.data.frame(rast_obj, xy = TRUE) %>%
    dplyr::rename(suit = 3) %>%
    dplyr::filter(!is.na(suit))
  if (scale01) df$suit <- df$suit / 1000
  df %>% dplyr::filter(suit >= min_suit)
}


#' Compute 4-category range change raster
#'
#' Categories:
#'   0 = Stable absent   (0 in both)
#'   1 = Lost            (1 now, 0 future)
#'   2 = Gained          (0 now, 1 future)
#'   3 = Stable present  (1 in both)
#'
#' @param cur_bin  SpatRaster; current binary suitability
#' @param fut_bin  SpatRaster; future binary suitability
#' @return         SpatRaster with integer values 0-3
range_change_rast <- function(cur_bin, fut_bin) {
  terra::ifel(cur_bin == 0 & fut_bin == 0,  0L,
  terra::ifel(cur_bin == 1 & fut_bin == 0,  1L,
  terra::ifel(cur_bin == 0 & fut_bin == 1,  2L, 3L)))
}


#' Compute suitable area (km²) from a binary raster
#'
#' @param bin_rast   SpatRaster; binary (1=suitable, 0=unsuitable)
#' @param mask_sf    Optional sf object to mask to a region (e.g. NZ only)
#' @param mask_ext   Optional terra::ext() to further crop after masking
#' @return           Numeric; suitable area in km²
suit_area_km2 <- function(bin_rast, mask_sf = NULL, mask_ext = NULL) {
  r <- bin_rast
  if (!is.null(mask_sf))  r <- terra::mask(r, terra::vect(mask_sf))
  if (!is.null(mask_ext)) r <- terra::crop(r, mask_ext)
  cell_area <- terra::cellSize(r, unit = "km")
  sum(terra::values(cell_area)[terra::values(r) == 1], na.rm = TRUE)
}


#' Compute range change areas (km²) for all 4 categories
#'
#' @param rc_rast   SpatRaster; range change raster (values 0-3)
#' @param label     Character label for the scenario
#' @return          data.frame with columns: scenario, lost_km2, gained_km2,
#'                  stable_km2, net_change_km2, pct_change
change_areas <- function(rc_rast, label) {
  cell_km2 <- terra::cellSize(rc_rast, unit = "km")
  vals      <- terra::values(rc_rast)
  areas     <- terra::values(cell_km2)
  data.frame(
    scenario   = label,
    lost_km2   = round(sum(areas[vals == 1], na.rm = TRUE)),
    gained_km2 = round(sum(areas[vals == 2], na.rm = TRUE)),
    stable_km2 = round(sum(areas[vals == 3], na.rm = TRUE))
  ) %>%
    dplyr::mutate(
      net_change_km2 = gained_km2 - lost_km2,
      pct_change     = round(
        (gained_km2 - lost_km2) / (stable_km2 + lost_km2) * 100, 1)
    )
}


# ==============================================================================
# PLOTTING UTILITIES
# ==============================================================================

#' Shared SDM ggplot theme
#'
#' Clean minimal theme with ocean background, white gridlines, and
#' consistent typography.
#'
#' @param base_size  Base font size (default 12)
#' @return           ggplot theme object
theme_sdm <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = "#e8f4f8", colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = "white",
                                                 linewidth = 0.4),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.text         = ggplot2::element_text(colour = "grey40",
                                                 size = base_size - 2),
      axis.title        = ggplot2::element_text(colour = "grey30",
                                                 size = base_size - 1),
      plot.title        = ggplot2::element_text(face = "bold",
                                                 size = base_size + 1,
                                                 colour = "#2E4057"),
      plot.subtitle     = ggplot2::element_text(colour = "grey50",
                                                 size = base_size - 2),
      plot.caption      = ggplot2::element_text(colour = "grey60",
                                                 size = base_size - 3,
                                                 hjust = 0),
      legend.title      = ggplot2::element_text(size = base_size - 1),
      legend.text       = ggplot2::element_text(size = base_size - 2),
      legend.key.height = grid::unit(1.2, "cm")
    )
}


#' Shared viridis fill scale for suitability maps
#'
#' @param limits  Numeric vector of length 2; colour scale limits (default 0-1)
#' @return        ggplot scale_fill object
suit_fill_scale <- function(limits = c(0, 1)) {
  viridis::scale_fill_viridis_c(
    name     = "Suitability",
    option   = "D",
    limits   = limits,
    breaks   = seq(limits[1], limits[2], length.out = 5),
    labels   = scales::number_format(accuracy = 0.01),
    na.value = NA
  )
}


#' Shared fill scale for range change maps
#'
#' @return  ggplot scale_fill_manual object
change_fill_scale <- function() {
  ggplot2::scale_fill_manual(
    values = c("0" = "grey88",
               "1" = "#d73027",
               "2" = "#1a9850",
               "3" = "#4575b4"),
    labels = c("0" = "Stable absent",
               "1" = "Lost",
               "2" = "Gained",
               "3" = "Stable present"),
    name   = "Range change"
  )
}


#' Save ggplot as both editable PDF and high-resolution PNG
#'
#' PDF uses useDingbats = FALSE for Adobe Illustrator compatibility.
#' PNG uses Cairo renderer for crisp text at high resolution.
#'
#' @param plot_obj   ggplot object
#' @param base_name  Filename without extension (relative to out_dir)
#' @param out_dir    Output directory (default "outputs/figures/paper")
#' @param width      Width in inches (default 14)
#' @param height     Height in inches (default 10)
#' @param dpi        PNG resolution (default 300)
#' @return           data.frame with file paths (for figure manifest)
save_figure <- function(plot_obj, base_name,
                         out_dir = "outputs/figures/paper",
                         width = 14, height = 10, dpi = 300) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  pdf_path <- file.path(out_dir, paste0(base_name, ".pdf"))
  png_path <- file.path(out_dir, paste0(base_name, ".png"))

  # Editable PDF
  grDevices::pdf(pdf_path, width = width, height = height,
                  useDingbats = FALSE)
  print(plot_obj)
  grDevices::dev.off()

  # High-res PNG
  grDevices::png(png_path,
                  width  = width  * dpi,
                  height = height * dpi,
                  res    = dpi,
                  type   = "cairo")
  print(plot_obj)
  grDevices::dev.off()

  message("Saved: ", base_name, ".pdf  +  .png")
  invisible(data.frame(
    figure    = base_name,
    pdf_path  = pdf_path,
    png_path  = png_path,
    width_in  = width,
    height_in = height,
    dpi       = dpi,
    saved     = Sys.time()
  ))
}


#' Save ggplot as editable PDF only (faster than save_figure)
#'
#' @param plot_obj   ggplot object
#' @param filepath   Full file path including .pdf extension
#' @param width      Width in inches
#' @param height     Height in inches
save_pdf_only <- function(plot_obj, filepath, width = 14, height = 10) {
  dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(filepath, width = width, height = height,
                  useDingbats = FALSE)
  print(plot_obj)
  grDevices::dev.off()
  message("Saved: ", basename(filepath))
}


# ==============================================================================
# DATA HELPERS
# ==============================================================================

#' Load an RDS object if it is not already in the global environment
#'
#' Avoids redundant loading when running scripts in the same R session
#' as a previous script.
#'
#' @param obj_name  Character; name of object in global environment
#' @param rds_path  Character; path to .rds file
load_if_missing <- function(obj_name, rds_path) {
  if (!exists(obj_name, envir = .GlobalEnv)) {
    if (!file.exists(rds_path)) {
      stop("Cannot find: ", rds_path,
           "\nRun the preceding script to generate this file.")
    }
    message("Loading ", obj_name, " from ", rds_path, "...")
    assign(obj_name, readRDS(rds_path), envir = .GlobalEnv)
  } else {
    message(obj_name, " already in environment — skipping load.")
  }
}


#' Check that all required files exist; stop with informative error if not
#'
#' @param file_paths  Character vector of required file paths
#' @param context     Character; which script requires these files
check_files <- function(file_paths, context = "this script") {
  missing <- file_paths[!file.exists(file_paths)]
  if (length(missing) > 0) {
    stop(
      "\nRequired files for ", context, " are missing:\n  ",
      paste(missing, collapse = "\n  "),
      "\nPlease run the preceding script(s) first.\n"
    )
  }
  message("All required files found.")
  invisible(TRUE)
}


#' Create the full project directory structure
#'
#' @param base_dir  Base directory (default current working directory)
make_dirs <- function(base_dir = ".") {
  dirs <- file.path(base_dir, c(
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
  ))
  invisible(lapply(dirs, dir.create,
                   recursive = TRUE, showWarnings = FALSE))
  message("Project directories ready.")
}


# ==============================================================================
# EVALUATION HELPERS
# ==============================================================================

#' Compute the continuous Boyce index for a suitability raster
#'
#' Uses ecospat::ecospat.boyce(). Values near +1 = excellent model;
#' near 0 = no better than random; negative = counter-prediction.
#'
#' @param suit_rast  SpatRaster; continuous suitability (any scale)
#' @param occ_pts    data.frame with columns lon, lat
#' @param label      Character label for the scenario
#' @return           data.frame with columns: scenario, boyce_index, n_occ
compute_boyce <- function(suit_rast, occ_pts, label = "") {
  if (!requireNamespace("ecospat", quietly = TRUE)) {
    stop("Package 'ecospat' required. Install with: install.packages('ecospat')")
  }
  suit_occ <- terra::extract(suit_rast, as.matrix(occ_pts))[, 1]
  suit_occ <- suit_occ[!is.na(suit_occ)]
  suit_bg  <- terra::values(suit_rast, na.rm = TRUE)

  boyce <- ecospat::ecospat.boyce(
    fit      = suit_bg,
    obs      = suit_occ,
    nclass   = 0,
    window.w = "default",
    res      = 100,
    PEplot   = FALSE
  )
  data.frame(
    scenario    = label,
    boyce_index = round(boyce$cor, 3),
    n_occ       = length(suit_occ)
  )
}


#' Compute the weighted geographic centroid of suitable habitat
#'
#' Centroid coordinates are weighted by suitability value so that
#' high-suitability cells contribute more to the centroid position.
#'
#' @param rast_obj    SpatRaster; continuous suitability
#' @param threshold   Minimum suitability value to include (default 500)
#' @param mask_sf     Optional sf object to restrict to a region
#' @param label       Character label for the scenario
#' @return            data.frame with columns: scenario, centroid_lon,
#'                    centroid_lat, n_cells, mean_suit; or NULL if no cells
compute_centroid <- function(rast_obj, threshold = 500,
                              mask_sf = NULL, label = "") {
  if (!is.null(mask_sf)) {
    rast_obj <- terra::mask(rast_obj, terra::vect(mask_sf))
  }

  df <- as.data.frame(rast_obj, xy = TRUE) %>%
    dplyr::rename(val = 3) %>%
    dplyr::filter(!is.na(val), val >= threshold)

  if (nrow(df) == 0) {
    message("  No cells >= threshold for: ", label)
    return(NULL)
  }

  data.frame(
    scenario     = label,
    centroid_lon = round(stats::weighted.mean(df$x, df$val), 3),
    centroid_lat = round(stats::weighted.mean(df$y, df$val), 3),
    n_cells      = nrow(df),
    mean_suit    = round(mean(df$val), 1)
  )
}


#' Compute suitable area in km² within a region for one scenario TIF
#'
#' @param tif_path   Path to ensemble GeoTIFF
#' @param label      Character label for the scenario
#' @param mask_sf    sf object defining the region (e.g. nz_sf)
#' @param crop_ext   terra::ext() to crop after masking
#' @param tss_cutoff Binarisation threshold
#' @return           data.frame with columns: scenario, suitable_km2
suit_area_region <- function(tif_path, label,
                              mask_sf    = NULL,
                              crop_ext   = NULL,
                              tss_cutoff = 410) {
  r       <- terra::rast(tif_path)
  lyr     <- grep("EMwmean", names(r), value = TRUE)[1]
  if (is.na(lyr)) lyr <- names(r)[1]
  r_suit  <- r[[lyr]]

  if (!is.null(mask_sf))  r_suit <- terra::mask(r_suit, terra::vect(mask_sf))
  if (!is.null(crop_ext)) r_suit <- terra::crop(r_suit, crop_ext)

  bin   <- terra::ifel(r_suit >= tss_cutoff, 1L, 0L)
  area  <- suit_area_km2(bin)
  data.frame(scenario = label, suitable_km2 = round(area))
}


# ==============================================================================
# PACKAGE LOADER
# ==============================================================================

#' Load all packages required by the full pipeline
#'
#' @param install_missing  Logical; install missing packages (default TRUE)
load_all_packages <- function(install_missing = TRUE) {

  pkgs <- c(
    "biomod2","maxnet","ENMeval",
    "terra","sf","rnaturalearth","rnaturalearthdata","geodata",
    "rgbif","rinat",
    "spThin","usdm",
    "ecospat","landscapemetrics",
    "ggplot2","ggpubr","viridis","RColorBrewer","corrplot",
    "patchwork","scales","gridExtra","grid",
    "dplyr","tidyr",
    "knitr","kableExtra","flextable","officer",
    "raster"
  )

  if (install_missing) {
    missing_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    if (length(missing_pkgs) > 0) {
      message("Installing: ", paste(missing_pkgs, collapse = ", "))
      install.packages(missing_pkgs)
    }
  }

  failed <- character(0)
  for (pkg in pkgs) {
    ok <- tryCatch({
      library(pkg, character.only = TRUE, warn.conflicts = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) failed <- c(failed, pkg)
  }

  if (length(failed) > 0) {
    warning("Failed to load: ", paste(failed, collapse = ", "))
  } else {
    message("All packages loaded.")
  }
  invisible(failed)
}


# ==============================================================================
# VERSION INFO
# ==============================================================================

.SDM_UTILS_VERSION <- "1.0.0"
.SDM_UTILS_DATE    <- "2026"
.SDM_SPECIES       <- "Leioproctus launcestonensis"

message("utils_SDM_helpers.R loaded  |  v", .SDM_UTILS_VERSION,
        "  |  ", .SDM_SPECIES)

