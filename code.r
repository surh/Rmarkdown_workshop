#' ---
#' title: "Workshop example"
#' author: "Sur Herrera Paredes"
#' date: "23/4/2022"
#' output:
#'   html_document:
#'     toc: TRUE
#'     toc_float: TRUE
#' ---
#' 
#' # Setup
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------------
library(tidyverse)

#' 
#' # Read data
#' 
#' Data from phenotypic measurements of *A. thaliana* under different Phosphate & bacterial condition.
#' 
## ----reading_data, echo=FALSE----------------------------------------------------------------------------------------
Dat <- read_tsv("data/phenotypes.tsv")

#' 
## --------------------------------------------------------------------------------------------------------------------
Dat

#' 
#' # Exploratory plots
#' 
## --------------------------------------------------------------------------------------------------------------------
ggplot(Dat, aes(x = Pi_content, y= Elongation, col = EndP)) +
  geom_point() +
  theme_classic()

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------
arrange(Dat, Elongation)

#' We noted that the three plants with smallest elongation are probably outliars due to batch effect
#' 
#' 
## --------------------------------------------------------------------------------------------------------------------
Dat <- filter(Dat, Elongation > 2)
arrange(Dat, Elongation)

#' 
#' 
## --------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(Dat, aes(x = Pi_content, y= Elongation, col = EndP)) +
  geom_point() +
  theme_classic()
p1

#' 
#' 
#' # Session Info
#' 
## --------------------------------------------------------------------------------------------------------------------
sessionInfo()

#' 
#' 
#' 
