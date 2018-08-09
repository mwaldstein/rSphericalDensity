#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

directory <- if (length(args) > 0) {
  args[1]
} else {
  "posts"
}

port <- if (length(args) > 1) {
  as.integer(args[2])
} else {
  8080
}

if (!dir.exists(directory)) {
  stop(paste0("Directory does not exist: ", directory))
}

library(servr)
# Building in-session because when run independently, misses .Rprofile and
# as a result misses the packrat library.
rmdv2(dir = directory, port = port, in_session = T)
