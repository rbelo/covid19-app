library(data.table)
library(lubridate)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(plotly)

load("covid-data.RData")

countries.to.remove.always <- c("Diamond Princess")
countries.to.include.default <- sort(c("Portugal", "Israel", "Italy", "Spain", "France", "Germany", "US", "United Kingdom", "Netherlands", "Denmark"))
dt.covid <- dt.covid[!(country %in% countries.to.remove.always)]
