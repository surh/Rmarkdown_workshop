#' # Get data for workshop
#' 
#' Load wheelP & get data

#+ setup
library(tidyverse)
library(wheelP)
data(Elongation)
data(Pi)

args <- list(startP = "-Pi,0.5%Suc",
  syncoms = c("P1P2", "I1I2", "N2N3", "none"),
  exps = c("A", "B", "C", "D"))

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
  filter(Experiment %in% args$exps) %>%
  select(Pi_content, Elongation, Experiment, EndP, Bacteria, Plate)
Pi

#' I remembered here that I had already combined Pi & Elongation data,
#' so I only need the Pi data
#' 
#' We replace none for axenic, which makes things simpler for models
#' during workshop (no need to set the reference level)
Pi$Bacteria <- as.character(Pi$Bacteria)
Pi$Bacteria[ Pi$Bacteria == "none" ] <- "axenic"

#' 
#' We write the output
#'+ wride data
write_tsv(Pi, "../data/phenotypes.tsv")
