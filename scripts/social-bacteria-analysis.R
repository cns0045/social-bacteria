library(dplyr)
library(ggplot2)
require(gridExtra)
library(scales)
library(RColorBrewer)

setwd("C:/msys64/home/schne/social-bacteria")
source("functions.R")

file <- "data/cat-data.csv"
folder <- "logs/"

cat <- readLogData(file, folder)

grouped <- group_by(cat, Cheat, Spite, SubpopCarryingCapacity, MigrationRate, Virulence)
grouped <- na.omit(grouped)

summary_table <- createSummaryTable(grouped)
summary_table <- summary_table[!(summary_table$Cheat==0 & summary_table$Spite==0),]

novir_table <- summary_table[summary_table$Virulence==0,]

plotEndsNoVirulence(novir_table)
