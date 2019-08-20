## Global ## 

library(shinydashboard)
library(shiny)
library(googleVis)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(leaflet)

data <- read.csv('crimes_boston.csv')
 

##cancer_stat <- data.frame(cancer[c(-2, -4, -5, -8, -10)])
#choice <- colnames(cancer_stat)[-1]