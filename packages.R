

# path to my workstation R packages,
pkgs <- file.path(
  "C:", "Users", "bjaeger", "Documents", "R", "4.1", "workstation"
)

# only use this library if you are bcjaeger
if(dir.exists(pkgs)) .libPaths(pkgs)

library(shiny)
library(rlang)
library(tibble)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(shinyWidgets)
library(data.table)
library(gt)
library(glue)
library(table.glue)
library(rintrojs)
library(cmprsk)
library(tidyfast)
library(DT)
library(conflicted)

conflict_prefer(':=', 'data.table')
conflict_prefer("filter", "dplyr")
conflict_prefer("renderDataTable", "DT")

