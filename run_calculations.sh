#!/bin/bash

Rscript -e "packrat::restore()"
Rscript scripts/compute_mars_densities.R
