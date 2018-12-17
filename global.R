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
# names(aug)
# aug2 <- aug[,c(1,9:16,2:8,17)]
# write_feather(aug,'Aug.feather')
# names(aug)
# names(aug)[11:17] <- c('Arb_MAF', 'Nub_MAF','Eas_MAF','Wes_Maf','Sot_MAf','NE_MAF','SW_MAF')
# aug[,index:=NULL]
# 
# write_feather(aug, 'Final.feather')
# aug2 <- aug %>% mutate_at(vars(Arb_MAF:SW_MAF),~round(.,3))
# write_feather(aug2, path = 'SGP/Final.feather')  
# ?getVariant
# res <- getVariant(object=cb, ids="19:45411941:T:C", resource="annotation")
# query <- with(aug[1,],paste(Chr,Start, REF, ALT, sep = ":"))
# response <- getVariant(cb, query,resource = 'annotation')
# pops <- response$populationFrequencies[[1]]
# conseq <- response$consequenceTypes[[1]]
# clinvar <- response$variantTraitAssociation$clinvar[[1]]
# cosmic <- response$variantTraitAssociation$cosmic[[1]]
# drugs <- response$geneDrugInteraction[[1]]
