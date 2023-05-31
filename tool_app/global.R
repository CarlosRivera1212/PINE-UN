library(shiny)
library(shinycssloaders)

library(readxl)
library(writexl)
library(foreign)
library(tidyverse)
library(reshape2)

library(spatialreg)
library(spdep)
library(sp)


source('../f_be_tc.R')
source('../f_ut_cc.R')
source('../f_crunt.R')
source('../f_ceuro.R')
source('../f_ds_2.R')

reglas_euro = '../df_reg_euro_032823.rda'