ui <- fluidPage(
  # Application title
  titlePanel(paste0("COVID-19 (Latest Data: ", format(dt.covid[, max(date)], "%B %d, %Y"), ")")),
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
#shinyWidgets::sliderTextInput("pvalue2","PValue:",
#                            choices=c(0, 0.0001, 0.001, 0.01),
#                            selected=c(0, 0.001), grid = T),
#   numericInput("min.n.cases",
#                label = "Consider only countries that have reached at least the following confirmed cases:",
#                value = 100),
      numericInput("start.from.n.cases",
                label = "Start tracking a country when it reaches at least the following cases:",
                value = c(100)),
      sliderInput("max.obs.period",
                label = "Track a country for a maximum of days:",
                value = c(35),
                min = 7, max = 60),
      sliderInput("prediction.period.future",
                label = "Predict the next days:",
                value = ymd(dt.covid[, max(date)]),
                min = ymd(dt.covid[, max(date)]), max = ymd(dt.covid[, max(date)]) + ddays(14),
                timeFormat = "%d %b"),
      sliderInput("prediction.period.past",
                label = "Prediction based on growth of the lastest days:",
                value = c(5),
                min = 2, max = 14),
      radioButtons("scale.type", "Scale Type:",
               c("Log Scale" = "Log",
                 "Linear Scale" = "Linear"),
                 inline = TRUE),
      radioButtons("growth.type", "Growth Type:",
               c("Exponential" = "Exp",
                 "Linear" = "Linear"),
                 inline = TRUE),
   actionButton("select.all", "All Countries"),
   actionButton("select.none", "None"),
   actionButton("select.default", "Default"),
   selectInput("countries.to.include", "Include Countries:", 
               choices = dt.covid[, unique(country)], 
               multiple = TRUE,
               selected = countries.to.include.default
    )),

    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Confirmed Cases", id = "confirmed", plotlyOutput("ft.plot.confirmed", height="500px")), 
        tabPanel("Deaths", id = "deaths", plotlyOutput("ft.plot.deaths", height="500px"))
          ),
      "Inspired by this", a("plot", href="https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441"), 
      "by", a("@jburnmurdoch", href="https://twitter.com/jburnmurdoch"), br(),
      "Data source:", a("Johns Hopkins CSSE", href="https://github.com/CSSEGISandData/COVID-19"), br(),
      "Updated ±daily by", a("@rbelo", href="https://twitter.com/rbelo")
    )
  )
)

ui
