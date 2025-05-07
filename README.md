# Installation

You can install the development version of fars from GitHub using:

library(devtools)
install_github("akbarpunjwani/msdr-rpkgexample")
library(fars)

# About

FARS (Fatality Analysis Reporting System) is a demonstration R package that provides tools for working with traffic accident data from the years 2013, 2014, and 2015. The sample data is included for internal use only.

This package enables:
- Year-wise summaries of monthly accident counts via fars_summarize_years()
- State-specific accident visualizations for a given year using fars_map_state()

It is designed as an educational example for building R packages that import, process, and visualize structured data.

# Build Status
https://travis-ci.com/nziegenbein/courseraRPackage.svg?branch=main
