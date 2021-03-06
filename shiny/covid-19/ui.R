ui <- fluidPage(
  # Application title
  titlePanel(paste0("COVID-19 (Latest Data: ", format(dt.covid[, max(date)], "%B %d, %Y"), ")")),
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      selectInput("countries.to.include", "Include Countries:", 
                  choices = dt.covid[, unique(country)], 
                  multiple = TRUE,
                  selected = countries.to.include.default),      
      actionButton("select.all", "All Countries"),
      actionButton("select.none", "None"),
      actionButton("select.default", "Default"), hr(),
      sliderInput("prediction.period.future",
                  label = "Predict the next days:",
                  value = ymd(dt.covid[, max(date)]),
                  min = ymd(dt.covid[, max(date)]), max = ymd(dt.covid[, max(date)]) + ddays(60),
                  timeFormat = "%d %b"),
      sliderInput("prediction.period.past",
                  label = "Prediction based on growth of the last days:",
                  value = c(5),
                  min = 2, max = 14), hr(),
      radioButtons("cases.count", "Cases:",
                   c("New" = "New",
                     "Cumulative" = "Cumulative"),
                   inline = TRUE),
      radioButtons("scale.type", "Scale Type:",
                   c("Log Scale" = "Log",
                     "Linear Scale" = "Linear"),
                   inline = TRUE),
      radioButtons("growth.type", "Growth Type:",
                   c("Exponential" = "Exp",
                     "Linear" = "Linear"),
                   inline = TRUE), hr(),
      numericInput("start.from.n.cases",
                   label = "Start tracking from the following confirmed cases:",
                   value = c(100)),
      numericInput("start.from.n.deaths",
                   label = "Start tracking from the following deaths:",
                   value = c(10)),
      sliderInput("max.obs.period",
                  label = "Track a country for a maximum of days:",
                  value = c(max(dt.covid[, .N, by = list(country, event_type)][, N])),
                  min = 7, max = max(dt.covid[, .N, by = list(country, event_type)][, N]))
   ),

    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Confirmed Cases", id = "confirmed", plotlyOutput("ft.plot.confirmed", height="500px")), 
        tabPanel("Deaths", id = "deaths", plotlyOutput("ft.plot.deaths", height="500px"))
          ),
      "Inspired by this", a("plot", href="https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441"), 
      "by", a("@jburnmurdoch", href="https://twitter.com/jburnmurdoch"), br(),
      "Data source:", a("Johns Hopkins CSSE", href="https://github.com/CSSEGISandData/COVID-19"), br(),
      "Updated ±daily by", a("@rbelo", href="https://twitter.com/rbelo"), br(),
      "Source code", a("here", href="https://github.com/rbelo/covid19-app")
    )
  )
)

ui
