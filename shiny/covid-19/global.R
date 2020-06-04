library(data.table)
library(lubridate)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(plotly)

#load("covid-data.RData")

prep.timeseries.data <- function(file) {
  dt.data <- fread(file)
  dt.data[, country_province := ifelse(`Province/State` == "", 
                                     `Country/Region`,
                                     paste0(`Country/Region`, "-", `Province/State`))]
  setnames(dt.data, 
           names(dt.data)[1:4], 
           c("province", "country", "lat", "lon")) 
  dt.data <- 
       melt(dt.data, 
            id.vars = c("country_province", "province", "country", "lat", "lon"),
            value.name = "cases", 
            variable.name = "date")
  dt.data[, date := mdy(date)]
  dt.data <- dt.data[, list(cases = sum(cases)), by = list(country, date)]
  dt.data <- dt.data[order(country, date)]
  dt.data <- rbind(dt.data, dt.data[, list(country = "World", cases = sum(cases)), by = date])
  dt.data
}

dt.confirmed <- prep.timeseries.data("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
dt.deaths    <- prep.timeseries.data("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

dt.covid <- rbind(dt.confirmed[, list(country, date, event_type = "confirmed", cases)],
                  dt.deaths[,    list(country, date, event_type = "deaths", cases)])


countries.to.remove.always <- c("Diamond Princess")
countries.to.include.default <- sort(c("World", "Portugal", "Israel", "Italy", "Spain", "Germany", "US", "United Kingdom", "Netherlands", "Brazil"))
dt.covid <- dt.covid[!(country %in% countries.to.remove.always)]
