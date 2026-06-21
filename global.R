# =============================================================================
# Africa Soil Diversity Atlas — global.R
# Loaded once at startup, shared across all Shiny sessions.
# =============================================================================

library(shiny)
library(leaflet)
library(terra)
library(sf)
library(bslib)

# =============================================================================
# 1. Paths
# =============================================================================

DATA_DIR <- "data"

RASTER_FILES <- list(
  mean = c(
    q0   = file.path(DATA_DIR, "pred_mean_D0.tif"),
    q1   = file.path(DATA_DIR, "pred_mean_D1.tif"),
    q2   = file.path(DATA_DIR, "pred_mean_D2.tif"),
    logE = file.path(DATA_DIR, "pred_mean_logE.tif")
  ),
  ppi90 = c(
    q0   = file.path(DATA_DIR, "pred_ppi90_D0.tif"),
    q1   = file.path(DATA_DIR, "pred_ppi90_D1.tif"),
    q2   = file.path(DATA_DIR, "pred_ppi90_D2.tif"),
    logE = file.path(DATA_DIR, "pred_ppi90_logE.tif")
  )
)

# =============================================================================
# 2. Validate files exist before loading
# =============================================================================

missing_files <- Filter(Negate(file.exists), unlist(RASTER_FILES))

if (length(missing_files) > 0) {
  stop(
    "Missing raster files:\n",
    paste(" -", missing_files, collapse = "\n")
  )
}

# =============================================================================
# 3. Load raster stacks
# =============================================================================

message("[global.R] Loading raster stacks...")

# Each stack is a named SpatRaster with 4 layers (q0, q1, q2, logE).
# terra::rast() on a vector of paths stacks them automatically.
rasters <- list(
  mean  = terra::rast(unname(RASTER_FILES$mean)),
  ppi90 = terra::rast(unname(RASTER_FILES$ppi90))
)

# Assign clean layer names for unambiguous extraction downstream
names(rasters$mean)  <- names(RASTER_FILES$mean)   # q0, q1, q2, logE
names(rasters$ppi90) <- names(RASTER_FILES$ppi90)

message("[global.R] Raster stacks loaded: ",
        paste(names(rasters$mean), collapse = ", "))

# =============================================================================
# 4. Per-layer display metadata
# =============================================================================
# Used by server.R to drive the legend and results panel labels
# without any magic strings scattered across the codebase.

LAYER_META <- list(
  q0 = list(
    label       = "Species Richness",
    order       = "q = 0",
    short       = "Richness",
    unit        = "ASVs",
    palette     = "Greens",
    decimals    = 0
  ),
  q1 = list(
    label       = "Shannon Diversity",
    order       = "q = 1",
    short       = "Shannon",
    unit        = "eff. species",
    palette     = "YlGn",
    decimals    = 1
  ),
  q2 = list(
    label       = "Simpson Diversity",
    order       = "q = 2",
    short       = "Simpson",
    unit        = "eff. species",
    palette     = "BuGn",
    decimals    = 1
  ),
  logE = list(
    label       = "Evenness",
    order       = "logE",
    short       = "Evenness",
    unit        = "",
    palette     = "PuBuGn",
    decimals    = 3
  )
)

# =============================================================================
# 5. Pre-compute per-layer min/max for legend scaling
# =============================================================================
# terra::global() is fast on GeoTIFF with built-in statistics.
# We compute this once here rather than on every reactive event.

message("[global.R] Computing raster range statistics...")

LAYER_RANGES <- lapply(names(LAYER_META), function(lyr) {
  stats <- terra::global(rasters$mean[[lyr]], fun = "range", na.rm = TRUE)
  list(
    min = round(stats$min, LAYER_META[[lyr]]$decimals),
    max = round(stats$max, LAYER_META[[lyr]]$decimals)
  )
})
names(LAYER_RANGES) <- names(LAYER_META)

message("[global.R] Layer ranges computed.")

# =============================================================================
# 6. Country bounding boxes
# =============================================================================
# Used by server.R to fly the map to the selected country.
# Coordinates: list(lng_min, lat_min, lng_max, lat_max)

COUNTRY_BBOX <- list(
  ALL = list(lng1 =  -25, lat1 = -35, lng2 =  55, lat2 =  38),
  CM  = list(lng1 =   8, lat1 =   1, lng2 =  17, lat2 =  13),
  ET  = list(lng1 =  33, lat1 =   3, lng2 =  48, lat2 =  15),
  GH  = list(lng1 =  -4, lat1 =   5, lng2 =   1, lat2 =  11),
  KE  = list(lng1 =  34, lat1 =  -5, lng2 =  42, lat2 =   5),
  MG  = list(lng1 =  43, lat1 = -26, lng2 =  51, lat2 = -12),
  NG  = list(lng1 =   3, lat1 =   4, lng2 =  15, lat2 =  14),
  SN  = list(lng1 = -17, lat1 =  12, lng2 = -11, lat2 =  16),
  ZA  = list(lng1 =  16, lat1 = -35, lng2 =  33, lat2 = -22),
  TZ  = list(lng1 =  29, lat1 = -12, lng2 =  40, lat2 =  -1)
)

# =============================================================================
# 7. Helper: extract diversity values at a clicked point
# =============================================================================
# Returns a named list with mean and ppi90 for all four indices,
# or NULL if the point falls outside the raster extent.

extract_diversity <- function(lng, lat) {
  
  pt <- terra::vect(
    matrix(c(lng, lat), ncol = 2),
    crs = "EPSG:4326"
  )
  
  # Reproject point to raster CRS if needed
  if (!terra::same.crs(pt, rasters$mean)) {
    pt <- terra::project(pt, terra::crs(rasters$mean))
  }
  
  mean_vals  <- terra::extract(rasters$mean,  pt)
  ppi90_vals <- terra::extract(rasters$ppi90, pt)
  
  # Drop the ID column terra adds
  mean_vals  <- terra::extract(rasters$mean,  pt, ID = FALSE)
  ppi90_vals <- terra::extract(rasters$ppi90, pt, ID = FALSE)
  
  if (all(is.na(mean_vals))) return(NULL)
  
  list(
    mean  = as.list(unlist(mean_vals[1, ])),
    ppi90 = as.list(unlist(ppi90_vals[1, ]))
  )
}

# =============================================================================
# 8. Helper: format a single diversity value for display
# =============================================================================

format_diversity <- function(value, layer_key) {
  if (is.null(value) || is.na(value)) return("—")
  formatC(value, digits = LAYER_META[[layer_key]]$decimals, format = "f")
}

message("[global.R] Initialisation complete. Atlas ready.")