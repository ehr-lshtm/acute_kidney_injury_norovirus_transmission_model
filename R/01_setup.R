# run file path

# source("paths/00_filepath.R")

# install.packages("remotes")
# remotes::install_github("sbfnk/fitR")

# pacman package first

if (!("pacman" %in% installed.packages()))
  install.packages("pacman")
library(pacman)

# load packages

p_load(
  deSolve,
  socialmixr,
  tidyr,
  dplyr,
  ggplot2,
  data.table,
  lubridate,
  ggpubr,
  arrow,
  readxl,
  fitR,
  coda,
  lattice,
  profvis,
  rmarkdown,
  Rcpp,
  reshape2,
  fitR,
  furrr,
  noromod,
  splines,
  readr,
  bayesplot,
  tmvtnorm,
  posterior,
  hexbin,
  flextable,
  progress,
  officer
)

# if(!require("pak")) install.packages("pak")
# pak::pak("pratikunterwegs/noromod")
# pak::pak("pratikunterwegs/noromod@birth-and-ageing-branch")

# install.packages("posterior")

# ggplot colour palette with grey:
cbPalette <-
  c(
    "#999999",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )

# ggplot colour palette with black:

cbbPalette <-
  c(
    "#920000",
    "#56B4E9",
    "#009E73",
    "#D55E00",
    "#E69F00",
    "#0072B2",
    "#CC79A7",
    "#004949",
    "#009292",
    "#000000",
    "#ff6db6",
    "#ffb6db",
    "#490092",
    "#006ddb",
    "#b66dff",
    "#6db6ff",
    "#b6dbff",
    "#924900",
    "#db6d00",
    "#24ff24",
    "#ffff6d"
  )

# grey color grid

col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

# function for x axis labelling

everyother <- function(x) x[seq_along(x) %% 2 == 0]

