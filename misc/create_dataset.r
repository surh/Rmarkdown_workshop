#' # Get data for workshop
#' 
#' Load wheelP & get data

#+ setup
library(tidyverse)
library(wheelP)
data(Elongation)
data(Pi)

args <- list(startP = "-Pi,0.5%Suc",
  syncoms = c("P1P2", "I1I2", "N2N3", "none"))

#' First we subset & aggregate elongation data

#+ elongation data
Elongation <- Elongation %>%
  filter(StartP == args$startP) %>%
  filter(Bacteria %in% args$syncoms) %>%
  group_by(Plate) %>%
  summarise(Experiment = unique(Experiment),
            Plate = unique(Plate),
            StartP = unique(StartP),
            EndP = unique(EndP),
            Bacteria = unique(Bacteria),
            Elongation = mean(Elongation),
            .groups = 'drop')

#' Then we subset Pi data

#+ Pi data
Pi <- Pi %>%
  filter(StartP == args$startP) %>%
  filter(Bacteria %in% args$syncoms) %>%
  select(Pi_content, Elongation, Experiment, EndP, Bacteria, Plate)
Pi

#' I remembered here that I had already combined Pi & Elongation data,
#' so I only need the Pi data
#' 
#' We write the output
#'+ wride data
write_tsv(Pi, "../data/phenotypes.tsv")
