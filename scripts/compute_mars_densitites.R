#!/usr/bin/env Rscript
library(pds3)        # Load Observation data
library(Directional) # For spherical density functions

## Set up logging utilities
start_time <- as.numeric(Sys.time())
last_time <- as.numeric(Sys.time())
time_log <- function(msg) {
  cur_time <- as.numeric(Sys.time())
  message(sprintf("%8.3f %8.3f - %s",
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
                return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
            }
        }
    }
}

PATH <- dirname(csf())

## Function to calculate the cmf density
vmf_density_grid <- function(u, ngrid = 100) {
  # Translate to (0,180) and (0,360)
  u[,1] <- u[,1] + 90
  u[,2] <- u[,2] + 180
  res <- vmf.kerncontour(u, thumb = "none", den.ret = T, full = T,
                             ngrid = ngrid)

  # Translate back to (-90, 90) and (-180, 180) and create a grid of
  # coordinates
  ret <- expand.grid(Lat = res$lat - 90, Long = res$long - 180)
  ret$Density <- c(res$den)
  ret
}
time_log("Setup Done")

## Load the headers
header_label <- pds3_read(file.path(PATH, "..", "data", "RDRCUMINDEX.LBL"))
headers <- do.call(rbind, header_label$odl$RDR_INDEX_TABLE$COLUMN)
time_log("Headers Loaded")

## Load the Observations
observations <- read.table(file.path(PATH, "..", "data", "RDRCUMINDEX.TAB.bz2"),
                           sep = ",")
names(observations) <- unlist(headers[,1])
time_log("Observations Loaded")

positions <- observations[,c("CORNER1_LONGITUDE", "CORNER1_LATITUDE")]
names(positions) <- c("lon", "lat")
positions$lon[positions$lon < 0] <- 360 + positions$lon[positions$lon < 0]
positions$lon <- positions$lon - 180

grid.size <- 100

# For Testing...
# positions <- positions[1:1000,]

# Calculate densitites
densities <- vmf_density_grid(positions[,c("lat",
                                           "lon")],
                              ngrid = grid.size)
time_log("Densitites Calculated")

outDir <- file.path(PATH, "..", "output")
if (!dir.exists(outDir)) {
  dir.create(outDir)
}
outFile <- file.path(PATH, "..", "output", "mars.rds")
saveRDS(densities, outFile)
time_log("Densitites Saved")

time_log("DONE")
