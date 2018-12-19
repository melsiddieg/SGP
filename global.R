library(feather)
library(data.table)
library(dplyr)
library(tidyr)
library(shiny)
library(DT)
library(highcharter)
library(shinythemes)
library(shinyjs)
library(cellbaseR)
library(shinyWidgets)
cb <- CellBaseR()
aug <- read_feather('Final.feather',columns = c(
  'rs', 'Chr', 'Start', 'REF', 'ALT', 'conseq', 'Impact', 'HGNC',
  'ENSEMBL', 'ID', 'Arb_MAF', 'Nub_MAF', 'Eas_MAF', 'Wes_Maf', 'Sot_MAf',
  'NE_MAF', 'SW_MAF'
))
aug <- as.data.table(aug)
setindexv(aug, c('Chr', 'Impact','HGNC','conseq','ENSEMBL','rs'))
