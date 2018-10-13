#!/usr/bin/env Rscript

# For parallel implementation of vmf.kerncontour
# library(parallel)
# cl <- makeCluster(2)
# clusterCall(cl, function() { library(Directional) })

library(pds3)        # Load Observation data
library(Directional) # For spherical density functions

## Set up logging utilities
start_time <- as.numeric(Sys.time())
last_time <- as.numeric(Sys.time())
time_log <- function(msg) {
  cur_time <- as.numeric(Sys.time())
  message(sprintf("%8.3f %8.3f - mars_density - %s",
          cur_time - start_time,
          cur_time - last_time,
          msg))
  last_time <<- cur_time
}

time_log(date())

##
# Path of script magic...
# from https://stackoverflow.com/a/1816487
csf <- function() {
    # http://stackoverflow.com/a/32016824/2292993
    cmdArgs = commandArgs(trailingOnly = FALSE)
    needle = "--file="
    match = grep(needle, cmdArgs)
    if (length(match) > 0) {
        # Rscript via command line
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        ls_vars = ls(sys.frames()[[1]])
        if ("fileName" %in% ls_vars) {
            # Source'd via RStudio
            return(normalizePath(sys.frames()[[1]]$fileName))
        } else {
            if (!is.null(sys.frames()[[1]]$ofile)) {
            # Source'd via R console
            return(normalizePath(sys.frames()[[1]]$ofile))
            } else {
                # RStudio Run Selection
                # http://stackoverflow.com/a/35842176/2292993
                # return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
            }
        }
    }
}

PATH <- dirname(csf())

## Function to calculate the cmf density
vmf_density_grid <- function(u, ngrid = 100, func = vmf.kerncontour) {
  # Translate to (0,180) and (0,360)
  u[,1] <- u[,1] + 90
  u[,2] <- u[,2] + 180
  # NOTE: have to use thumb = "rot" as the default is where the huge mem-use
  # comes from
  res <- func(u, thumb = "rot", den.ret = T, full = T,
                             ngrid = ngrid)

  # Translate back to (-90, 90) and (-180, 180) and create a grid of
  # coordinates
  ret <- expand.grid(Lat = res$lat - 90, Long = res$long - 180)
  ret$Density <- c(res$den)
  ret
}
time_log("Setup Done")

position_file <- file.path(PATH, "..", "output", "mars_positions.rds")
if (!file.exists(position_file)) {
  source(file.path(PATH, "parse_positions.R"))
}
positions <- readRDS(position_file)

grid.size <- 100

# For Testing...
# positions <- positions[1:100,]

# Calculate densitites
densities <- vmf_density_grid(positions[,c("lat",
                                           "lon")],
                              ngrid = grid.size)
time_log("Densitites Calculated")

outDir <- file.path(PATH, "..", "output")
if (!dir.exists(outDir)) {
  dir.create(outDir)
}
outFile <- file.path(PATH, "..", "output", "mars_densities.rds")
saveRDS(densities, outFile)
time_log("Densitites Saved")

# vmf.kerncontour.mjw <- function(u, thumb = "none", den.ret = FALSE, full = FALSE,
#                                 ngrid = 100) {
#   ## u contains the data in latitude and longitude
#   ## the first column is the latitude and the
#   ## second column is the longitude
#   ## thumb is either 'none' (defualt), or 'rot' (Garcia-Portugues, 2013)
#   ## den.ret if set to TRUE returns a list with the following components:
#   ##  * Lat - latitudes of densities
#   ##  * Long - longitudes of densities
#   ##  * h - bandwidth used in calculation
#   ##  * den - matrix with densities at each latitude / longitude
#   ## full if set to TRUE calculates densities for the full sphere, otherwise
#   ##   using extents of the data
#   ## ngrid specifies the number of points taken at each axis
#   n <- dim(u)[1]  ## sample size
#   x <- euclid(u)
# 
#   if (thumb == "none") {
#     h <- as.numeric( vmfkde.tune(x, low = 0.1, up = 1)[1] )
#   } else if (thumb == "rot") {
#     k <- vmf(x)$kappa
#     h <- ( (8 * sinh(k)^2) / (k * n * ( (1 + 4 * k^2) * sinh(2 * k) -
#     2 * k * cosh(2 * k)) ) ) ^ ( 1/6 )
#   }
# 
#   if (full) {
#     x1 <- seq( 0, 180, length = ngrid )  ## latitude
#     x2 <- seq( 0, 360, length = ngrid )  ## longitude
#   } else {
#     x1 <- seq( min(u[, 1]) - 5, max(u[, 1]) + 5, length = ngrid )  ## latitude
#     x2 <- seq( min(u[, 2]) - 5, max(u[, 2]) + 5, length = ngrid )  ## longitude
#   }
#   cpk <- 1 / (  ( h^2)^0.5 *(2 * pi)^1.5 * besselI(1/h^2, 0.5) )
#   mat <- matrix(nrow = ngrid, ncol = ngrid)
# 
#   mat <- parApply(cl, expand.grid(1:ngrid, 1:ngrid), 1, function(gridPt) {
#     y <- euclid( c(x1[gridPt[1]], x2[gridPt[2]]) )
#     a <- as.vector( tcrossprod(x, y / h^2) )
#     can <- sum( exp(a + log(cpk)) ) / ngrid
#     if (abs(can) < Inf) can
#   })
# 
#   if (den.ret) {
#     return(list(lat = x1, long = x2, h = h, den = mat))
#   } else {
#     contour(mat$Lat, mat$Long, mat, nlevels = 10, col = 2, xlab = "Latitude",
#             ylab = "Longitude")
#     points(u[, 1], u[, 2])
#   }
# }
# 
# den2 <- vmf_density_grid(positions[,c("lat", "lon")], ngrid = grid.size,
#                          func=vmf.kerncontour.mjw)
# identical(densities, den2)

time_log("DONE")
