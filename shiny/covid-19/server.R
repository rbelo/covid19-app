ft.plot <- function(event.type = c("confirmed", "deaths"),
                    start.from.n.cases = 100, 
                    countries = c("Portugal", "Spain", "Italy"),
                    min.date = ymd("2019-11-01"),
                    max.date = today(),
                    max.obs.period = 50,
                    cases.count = c("New", "Cumulative"),
                    scale.type = c("Log", "Linear"),
                    growth.type = c("Exp", "Linear"), 
                    predict.based.on.last.n.days = 7,
                    predict.n.days.ahead = 0) {

  dt.covid.plot <- copy(dt.covid)
  if (cases.count[1] == "New") {
      dt.covid.plot[, cases := round(frollmean(cases - shift(cases, fill=0), 7)), by = list(country)]
   }
  dt.covid.plot <- dt.covid.plot[event_type == event.type[1] & 
                            cases >= start.from.n.cases &
                            country %in% countries &
                            date >= min.date & date <= max.date]
  dt.covid.plot[, ref_date := min(date), by=country]
  dt.covid.plot[, days_from_ref_date := as.numeric(difftime(date, ref_date, units="days"))]
  dt.covid.plot <- dt.covid.plot[days_from_ref_date <= max.obs.period]
  dt.covid.plot[, max_days_from_ref_date := max(days_from_ref_date), by=country]
  
  if(nrow(dt.covid.plot) == 0) {
   return(ggplot() + annotate(geom = "text", 
                              y = 0, x = 0,
                             label = "Please select at least one country.") + 
   theme_minimal() + theme(legend.position="none") + 
   scale_alpha_discrete(name = "Prediction", range = c(1, 0.5)) +
   ylab(paste(cases.count, tools::toTitleCase(event.type[1]))) + 
   xlab(paste("Days since at least", start.from.n.cases, "cases")) 
)
   
   }

  if (growth.type[1] == "Exp") {
    reg.growth.model <- lm(log(cases) ~ days_from_ref_date, data = dt.covid.plot)
 #   dt.covid.ref.growth <- dt.covid.plot[order(days_from_ref_date)][!duplicated(days_from_ref_date)][, 
 #                                   list(days_from_ref_date, 
 #                                        ref_growth = exp(reg.growth.model$coefficients[1] +
 #                                                         reg.growth.model$coefficients[2] * (0:(.N-1))))]
    dt.covid.coeffs <- dt.covid.plot[days_from_ref_date > max_days_from_ref_date - predict.based.on.last.n.days, 
                                     {model <- lm(log(cases) ~ days_from_ref_date)$coefficients; list(coeff_const = model[1], coeff_growth = model[2])}, by=list(country)]
    dt.covid.coeffs <- merge(dt.covid.coeffs, 
                             dt.covid.plot[, list(days_from_ref_date = days_from_ref_date[.N], 
                                                  cases              = cases[.N]), by=country],
                             by="country", all.x = TRUE)
    dt.covid.predict <- dt.covid.coeffs[, list(date               = dt.covid.plot[, max(date)] + ddays(0:predict.n.days.ahead),
                                               days_from_ref_date = days_from_ref_date + 0:predict.n.days.ahead, 
                                               cases              = round(cases * exp(coeff_growth * (0:predict.n.days.ahead)))), 
                                          by=country]
 
  } else {
    reg.growth.model <- lm(cases ~ days_from_ref_date, data = dt.covid.plot)
#    dt.covid.ref.growth <- dt.covid.plot[order(days_from_ref_date)][!duplicated(days_from_ref_date)][, 
#                                    list(days_from_ref_date, 
#                                         ref_growth = reg.growth.model$coefficients[1] +
#                                                      reg.growth.model$coefficients[2] * (0:(.N-1)))]
    dt.covid.coeffs <- dt.covid.plot[days_from_ref_date > max_days_from_ref_date - predict.based.on.last.n.days, 
                                     {model <- lm(cases ~ days_from_ref_date)$coefficients; list(coeff_const = model[1], coeff_growth = model[2])}, by=list(country)]
    dt.covid.coeffs <- merge(dt.covid.coeffs, 
                             dt.covid.plot[, list(days_from_ref_date = days_from_ref_date[.N], 
                                                  cases              = cases[.N]), by=country],
                             by="country", all.x = TRUE)
    dt.covid.predict <- dt.covid.coeffs[, list(date               = dt.covid.plot[, max(date)] + ddays(0:predict.n.days.ahead),
                                               days_from_ref_date = days_from_ref_date + 0:predict.n.days.ahead, 
                                               cases              = round(cases + coeff_growth * (0:predict.n.days.ahead))), 
                                          by=country]
}

  # add predictions 
  dt.covid.plot <- rbind(dt.covid.plot[, list(country, Date=date, event_type, cases, days_from_ref_date,  prediction = "No")], 
                         dt.covid.predict[, list(country, Date=date, event_type = event.type, cases, days_from_ref_date, prediction = "Yes")])
  dt.covid.plot[, Growth := paste0((cases - shift(cases)), " (", round((cases - shift(cases))/shift(cases) * 100, 1), "%)"), by=list(country)]

  # calculate growth refs 
  dt.covid.growth.refs <- 
                CJ(days_from_ref_date = 0:(dt.covid.plot[, max(days_from_ref_date)] + predict.n.days.ahead), 
                   double_every_x_days = c(3,7,14))
  dt.covid.growth.refs[, ref_growth := log(2^(1/double_every_x_days))]
  dt.covid.growth.refs[, ref_cases := exp(log(start.from.n.cases) + days_from_ref_date * ref_growth)]
  dt.covid.growth.refs[, Growth := paste0(round(ref_growth * 100), "%")]
  dt.covid.growth.refs[, Desc := paste0("Doubles every ", double_every_x_days, " days")]
  dt.covid.growth.refs[double_every_x_days == 1, Desc := paste0("Doubles every day")]
  dt.covid.growth.refs <- dt.covid.growth.refs[ref_cases <= dt.covid.plot[, max(cases, na.rm=TRUE)] * 2 & 
                                               days_from_ref_date <= dt.covid.plot[, max(days_from_ref_date, na.rm=TRUE)]]

  # prepare labels
  dt.covid.plot.labels <- dt.covid.plot[, .SD[.N], by=list(country, prediction)]
  dt.covid.plot.labels[, diff_cases := cases[prediction == "Yes"] - cases[prediction == "No"], by=country]
  dt.covid.plot.labels[, diff_cases_desc := paste0(" (", 
                                                   ifelse(diff_cases > 0, "+",""), 
                                                   format(diff_cases, big.mark=",", trim=TRUE), 
                                                   ")"), by=country]
  dt.covid.plot.labels[prediction == "No" | diff_cases == 0, diff_cases_desc := ""]
  dt.covid.plot.labels[, Desc := paste0(country, ": ", format(cases, big.mark=",", trim=TRUE), diff_cases_desc)]

  # main plot
  g <- ggplot(dt.covid.plot) + 
#     geom_line(aes(days_from_ref_date, ref_growth),
#               data = dt.covid.ref.growth, linetype = "dashed", size=0.35) +
     geom_line(aes(days_from_ref_date, ref_cases, series=Growth),
               data = dt.covid.growth.refs, color="gray", linetype = "dotted", size=0.35) +
     geom_text(aes(days_from_ref_date, ref_cases, 
                   label = Desc), 
               color="gray",
               data = dt.covid.growth.refs[, .SD[.N], by=ref_growth],
               hjust = 0, nudge_x = .6, size=3, check_overlap = TRUE) + 
     annotate(geom = "text", 
              x = 0, 
              y = dt.covid.growth.refs[, max(ref_cases)],
              hjust = 0,
              size = 3.5,
              label = ifelse(growth.type[1] == "Exp", 
                             paste0("Avg. daily growth: ", round(reg.growth.model$coefficients[2] * 100, 0), "%"),
                             paste0("Avg. new cases: ", round(reg.growth.model$coefficients[2], 0)))) +
     geom_line(aes(days_from_ref_date, cases, color=country, label=Date, linetype = prediction, alpha=prediction), size=0.35) + 
     geom_point(aes(days_from_ref_date, cases, color=country, label=Date, alpha = prediction, label2=Growth),
                 size=0.5,
                data = dt.covid.plot) + 
     geom_point(aes(days_from_ref_date, cases, color=country, label=Date, alpha = prediction, label2=Growth),
                 size=1.5,
                data = dt.covid.plot[, .SD[.N], by=list(country, prediction)]) + 
     geom_text(aes(days_from_ref_date, cases, alpha=prediction, 
                   label = Desc), 
               data = dt.covid.plot.labels,
               hjust = 0, nudge_x = .6, size=3, check_overlap = TRUE) + 
     xlim(c(0, dt.covid.plot[, max(days_from_ref_date) * 1.3])) +
     theme_minimal() + theme(legend.position="none") + 
     scale_alpha_discrete(name = "Prediction", range = c(1, 0.5)) +
     ylab(paste(cases.count, tools::toTitleCase(event.type[1]))) + 
     xlab(paste("Days since at least", start.from.n.cases, "cases")) 
  
  
  if (scale.type[1] == "Log") {
     g <- g + scale_y_log10()
  }
  g %>% style(textposition="right")
}

server <- function(input, output, session) {

selected.tab <- "confirmed"
start.from.n <- 100

observeEvent(input$tabs, {
  if (input$tabs == "Confirmed Cases") { 
     start.from.n <<- input$start.from.n.cases
     selected.tab <<- "confirmed"
  } else if (input$tabs == "Deaths"){
     start.from.n <<- input$start.from.n.deaths
     selected.tab <<- "deaths"
  }
#  updateNumericInput(session, "start.from.n.cases", value = my.val)
})

observeEvent(input$select.all, {
 updateSelectInput(session, "countries.to.include",
                   choices = dt.covid[event_type == selected.tab & cases >= start.from.n, unique(country)],
                   selected = dt.covid[event_type == selected.tab & cases >= start.from.n, unique(country)])
})

observeEvent(input$select.none, {
 updateSelectInput(session, "countries.to.include",
                   choices = dt.covid[event_type == selected.tab & cases >= start.from.n, unique(country)],
                   selected = c(""))
})

observeEvent(input$select.default, {
 updateSelectInput(session, "countries.to.include",
                   choices = dt.covid[event_type == selected.tab & cases >= start.from.n, unique(country)],
                   selected = countries.to.include.default)
})


observeEvent(input$start.from.n.cases, {
  start.from.n <<- input$start.from.n.cases
  updateSelectInput(session, "countries.to.include",
                   choices = dt.covid[event_type == selected.tab & cases >= start.from.n, unique(country)],
                   selected = input$countries.to.include)
})
observeEvent(input$start.from.n.deaths, {
  start.from.n <<- input$start.from.n.deaths
  updateSelectInput(session, "countries.to.include",
                   choices = dt.covid[event_type == selected.tab & cases >= start.from.n, unique(country)],
                   selected = input$countries.to.include)
})

  output$ft.plot.confirmed <- 
        renderPlotly({ft.plot(event.type = "confirmed", 
                                start.from.n.cases = input$start.from.n.cases, 
                                countries = input$countries.to.include,
                                max.obs.period = input$max.obs.period,
                                cases.count = input$cases.count,
                                scale.type = input$scale.type,
                                growth.type = input$growth.type,
                                predict.based.on.last.n.days = input$prediction.period.past,
                                predict.n.days.ahead = as.numeric(input$prediction.period.future - ymd(dt.covid[, max(date)])))})

  output$ft.plot.deaths <- 
         renderPlotly({ft.plot(event.type = "deaths", 
                                start.from.n.cases = input$start.from.n.deaths, 
                                countries = input$countries.to.include,
                                max.obs.period = input$max.obs.period,
                                cases.count = input$cases.count,
                                scale.type = input$scale.type,
                                growth.type = input$growth.type,
                                predict.based.on.last.n.days = input$prediction.period.past,
                                predict.n.days.ahead = as.numeric(input$prediction.period.future - ymd(dt.covid[, max(date)])))})

}

server
