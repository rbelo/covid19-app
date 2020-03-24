library(data.table)
library(lubridate)

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
  dt.data
}

setwd("~/project/covid19-app")

dt.confirmed <- prep.timeseries.data("~/project/covid19-app/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
dt.deaths    <- prep.timeseries.data("~/project/covid19-app/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

dt.covid <- rbind(dt.confirmed[, list(country, date, event_type = "confirmed", cases)],
                  dt.deaths[,    list(country, date, event_type = "deaths", cases)])

# Manual adjustments
dt.covid[country == "Portugal" & date == ymd("2020-03-18") & event_type == "confirmed", cases := 642]
dt.covid[country == "Portugal" & date == ymd("2020-03-18") & event_type == "deaths",    cases := 2]

save(dt.covid,  file="./shiny/covid-19/covid-data.RData")
