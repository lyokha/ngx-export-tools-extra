library(shiny)
library(plotly)
library(jsonlite)

ui <- fluidPage(
    fluidRow(
        headerPanel(h1("Nginx workers load arbitration", align = "center"),
                    "Nginx workers load arbitration")),
    fluidRow(
        wellPanel(div(align = "center",
                      div(style = "display: inline-block; margin-right: 20px",
                          textInput("i_url", NULL, "http://127.0.0.1:8020/",
                                    width = "200px")), span(),
                      div(style = "display: inline-block; margin-right: 20px",
                          radioButtons("rb_mode", NULL,
                                       c("Requests" = "reqs",
                                         "Bytes_sent" = "bsent"),
                                       selected = "reqs", inline = TRUE)),
                      div(style = "display: inline-block",
                          actionButton("b_reset", "Reset Traces"))))),
    fluidRow(
        div(plotlyOutput("plot"), id = "graph"))
)

server <- function(input, output, session) {
    values <- reactiveValues()
    values$init <- TRUE
    values$load <- list("", list())
    values$pids_prev <- list()

    observe({
            invalidateLater(5000, session)

            if (class(values$load) != "try-error") {
                values$pids_prev <- names(values$load[[2]])
            }
            values$load <- try(fromJSON(input$i_url))

            m <- `if`(input$rb_mode == "reqs", 2, 1)

            if (class(values$load) == "try-error" ||
                length(values$load[[2]]) == 0) {
                invalidateLater(1000, session)
            } else {
                pids <- names(values$load[[2]])

                if (values$init) {
                    values$init <- FALSE
                    values$p <- plot_ly(type = "scatter",
                                        mode = "lines",
                                        colors = "YlOrRd")

                    for (i in 1:length(values$load[[2]])) {
                        xs <- as.POSIXct(values$load[[2]][[i]][[1]],
                                         format = "%Y-%m-%dT%H:%M:%S")
                        values$p <- add_trace(values$p,
                            name = paste(pids[i],
                                         names(values$load[[2]][[i]][[2]][m])),
                            x = xs,
                            y = values$load[[2]][[i]][[2]][[m]],
                            line = list(width = 2)) %>%
                        add_annotations(
                            x = xs,
                            y = values$load[[2]][[i]][[2]][[m]],
                            text = "<span />",
                            showarrow = TRUE, arrowcolor = "#bbb")
                    }
                    values$p <- layout(values$p, yaxis = list(range = 0))

                    output$plot <- renderPlotly(values$p)
                } else {
                    vs <- list()
                    xs <- list()
                    ts <- list()
                    len <- length(pids)

                    if (length(names(values$load[[2]])) !=
                            length(values$pids_prev) ||
                        length(setdiff(names(values$load[[2]]),
                                       values$pids_prev)) > 0) {
                        invalidateLater(1000, session)
                        values$init <- TRUE
                    } else {
                        i <- 1
                        while (i <= len) {
                            vs[[i]] <- list(values$load[[2]][[i]][[2]][[m]])
                            xs[[i]] <- list(values$load[[2]][[i]][[1]])
                            ts[[i]] <- i
                            i <- i + 1
                        }

                        plotlyProxy("plot", session) %>%
                            plotlyProxyInvoke("extendTraces",
                                              list(x = xs, y = vs), ts)
                    }
                }
            }
        }
    )

    observeEvent(input$b_reset, {
            values$init <- TRUE
        }
    )

    observeEvent(input$rb_mode, {
            values$init <- TRUE
        }
    )
}

