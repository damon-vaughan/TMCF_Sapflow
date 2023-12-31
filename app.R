library(needs)
needs(shiny, tidyverse, lubridate)

options(readr.show_col_types = FALSE)

import.log <- read_csv(file.path("Sapflow_data_supporting", "Sapflow_data_import_log.csv"))

# Read in maintenance actions so they can be applied to the plot
SF.actions <- read_csv(file.path("Sapflow_data_supporting",
                                 "Sapflow_maintenance_actions.csv"))

# Read in baseline flags
baseline.flags <- read_csv(file.path("Sapflow_data_supporting",
                                 "Baseline_flags.csv"))

# Read in zero-ing periods from the microclimate data
zeroes <- read_csv(file.path("Sapflow_data_supporting", "Zeroes_full.csv"))

# plot dimensions
p.width <- 1000
p.height <- 700

# UI ----------------------------------------------------------------------

ui <- basicPage(
  titlePanel("Sapflow data portal"),

  sidebarLayout(
    sidebarPanel(
      # helpText("View Sapflow data"),

      sliderInput("dates",
                  label = h4("Select date range"),
                  min = ymd_hms("2022-09-01 00:00:00", tz = "UTC"),
                  max = max(import.log$Last.import),
                  value = c(ymd_hms("2022-09-01 00:00:00", tz = "UTC"),
                            max(import.log$Last.import))),

      titlePanel("Viewing options"),

      fluidRow(
        column(3,
               radioButtons("Level",
                            label = h4("Select level"),
                            choices = c("L1", "L2", "L3"),
                            selected = "L3")),
        column(3,
               radioButtons("Show_zeroes",
                            label = h4("Show zeroing?"),
                            choices = c("yes", "no"),
                            selected = "no")),
        column(3,
               radioButtons("Show_maintenance",
                            label = h4("Show maintenance?"),
                            choices = c("yes", "no"),
                            selected = "yes"))
      ),

      fluidRow(
        column(2,
               radioButtons("Tree_view",
                            label = h4("Select tree"),
                            choices = c("ET1", "ET2", "ET3", "ET4",
                                        "ET5", "ET6", "ET7", "ET8",
                                        "FB1", "FB2", "FB3", "FB4",
                                        "FB5", "FB6", "FB7", "FB8",
                                        "TV1", "TV2", "TV3", "TV4"),
                            selected = "FB1")),
        column(2, offset = 1,
               radioButtons("Cable_view",
                            label = h4("Select cable"),
                            choices = c("1", "2", "3", "4",
                                        "5", "6", "7", "8"),
                            selected = "1"),
               checkboxGroupInput("Cable_subseries",
                                  label = h4("Select sub-series"),
                                  choices = c("a", "b", "c", "d"),
                                  selected = c("a", "b", "c", "d")))),

      titlePanel("Download options"),

      fluidRow(
        column(2,
               checkboxGroupInput("Tree_DL",
                                  label = h4("Select tree"),
                                  choices = c("ET1", "ET2", "ET3", "ET4",
                                              "ET5", "ET6", "ET7", "ET8",
                                              "FB1", "FB2", "FB3", "FB4",
                                              "FB5", "FB6", "FB7", "FB8",
                                              "TV1", "TV2", "TV3", "TV4"),
                                  selected = "FB1"))),

      radioButtons("Time_format",
                   label = h4("Select time format"),
                   choices = list("ISO", "Excel_ready"),
                   selected = "ISO"),

      downloadButton("downloadData", "Download")
    ),

    mainPanel(
      "Graph shows data until:",
      verbatimTextOutput("maxdate.output"),
      plotOutput("plot", width = p.width, height = p.height,
                 hover = "plot_hover",
                 brush = "plot_brush"),
      verbatimTextOutput("plot_hoverinfo"),
      plotOutput("plot_brushedpoints")
      )
  )
)

# Server -----------------------------------------------------------------

server <- function(input, output, session) {
  dataInput <- reactive({
    if(input$Level == "L1") {
      read_csv(file.path(str_c("Sapflow_data_", input$Level),
                       str_c(input$Tree_view, "_Sapflow_", input$Level, ".csv")),
             show_col_types = F) %>%
      filter(Cable == input$Cable_view) %>%
      filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$dates[2], tz = "UTC"))
      } else {
        read_csv(file.path(str_c("Sapflow_data_", input$Level),
                         str_c(input$Tree_view, "_Sapflow_", input$Level, ".csv")),
               show_col_types = F) %>%
        filter(Cable == str_c(input$Cable_view, input$Cable_subseries)) %>%
        filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
                 Timestamp <= as.POSIXct(input$dates[2], tz = "UTC"))}
    })

  dataInput2 <- reactive({
    dataInput() %>%
      pivot_longer(4:6, names_to = "Position", values_to = "Velocity")
  })

  flagInput <- reactive({
    baseline.flags %>%
      filter(Tree == input$Tree_view) %>%
      filter(Cable == str_c(input$Cable_view, input$Cable_subseries)) %>%
      filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$dates[2], tz = "UTC")) %>%
      left_join(dataInput(), by = c("Tree", "Cable", "Timestamp")) %>%
      select(Tree, Cable, Timestamp, outer_vel, middle_vel, inner_vel) %>%
      pivot_longer(4:6, names_to = "Position", values_to = "Velocity")
  })

  labelInput <- reactive({
    SF.actions %>%
      filter(Tree == input$Tree_view) %>%
      filter(Cable == input$Cable_view) %>%
      filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$dates[2], tz = "UTC"))
  })

  zeroesInput <- reactive({
    zeroes %>%
      filter(Tree == input$Tree_view) %>%
      filter(periodBegin >= as.POSIXct(input$dates[1], tz = "UTC")  &
               periodBegin <= as.POSIXct(input$dates[2], tz = "UTC"))
  })

  output$maxdate.output <- renderText({
    as.character(max(dataInput()$Timestamp))
  })

  output$plot <- renderPlot({
    p <- ggplot() +
      geom_line(data = dataInput2(), aes(x = Timestamp, y = Velocity)) +
      geom_hline(yintercept = 0) +
      labs(y = "cm / hour") +
      scale_x_datetime(
        limits = c(as.POSIXct(input$dates[1], tz = "UTC"),
                   as.POSIXct(input$dates[2], tz = "UTC"))) +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 18),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 18),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$Tree_view, "_", input$Cable_view)) +
      facet_wrap(~Position, ncol = 1, scales = "fixed")
    if(input$Show_zeroes == "yes"){
      p2 <- p +
        geom_vline(data = zeroesInput(), aes(xintercept = periodBegin,
                                             y = min(dataInput2()$Velocity, na.rm = T)),
        color = "blue")} else {
          p2 <- p
        }

    if(input$Level == "L3"){
      p3 <- p2 +
        geom_line(data = flagInput(), aes(x = Timestamp, y = Velocity), color = "blue")
    } else{
      p3 <- p2
    }

    if(input$Show_maintenance == "yes"){
      p4 <- p3 +
        geom_label(data = labelInput(), aes(x = Timestamp, y = -2, label = Action))
    } else {
      p4 <- p3
    }
    p4},
    width = p.width, height = p.height)

  output$plot_hoverinfo <- renderText({
    val <- nearPoints(dataInput2(), input$plot_hover, maxpoints = 1)
    as.character(unique(val$Timestamp))
  })

  output$plot_brushedpoints <- renderPlot({
    dat <- brushedPoints(dataInput2(), input$plot_brush)
    if (nrow(dat) == 0)
      return()
    ggplot(dat) +
      geom_line(aes(x = Timestamp, y = Velocity)) +
      geom_hline(yintercept = 0) +
      labs(y = "cm / hour") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 18),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 18),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$Tree_view, "_", input$Cable_view)) +
      facet_wrap(~Position, ncol = 1, scales = "fixed")
  })

  dataDL <- reactive({
    L3 %>%
      filter(Tree %in% input$Tree_DL)
  })

  dataDL2 <- reactive({
    if(input$Time_format == "ISO"){
      dataDL()
    } else if(input$Time_format == "Excel_ready"){
      dataDL() %>%
        mutate(Timestamp = as.character(Timestamp))
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      str_c(str_c(str_flatten(input$Tree_DL, collapse = "_"), input$dates[1], input$dates[2], sep = "_"),
            ".csv")
    },
    content = function(file) {
      write_csv(dataDL2(), file)
    }
  )
}

# Run app ----------------------------------------------------------------

shinyApp(ui, server)

# Test server -------------------------------------------------------------

# Testing labels
# test <- testServer(server, {
#   session$setInputs(Tree_view = "FB6")
#   session$setInputs(Level = "L2")
#   session$setInputs(Cable_view = "1")
#   session$setInputs(Cable_subseries = "a")
#   # session$setInputs(dates = c(min = ymd("2022-09-01"),
#   #                             max = ymd("2023-04-30")))
#   session$setInputs(dates = c(min = ymd("2022-09-01"),
#                               max = maxdate))
#   print(dataInput())
# })

# Testing Level functioning
# test <- testServer(server, {
#   session$setInputs(Tree_view = "FB6")
#   session$setInputs(Level = "L2")
#   session$setInputs(Cable_view = "1")
#   session$setInputs(Cable_subseries = "a")
#   session$setInputs(dates = c(min = ymd("2022-09-01"),
#                               max = maxdate))
#   # print(file.path(str_c("Sapflow_data_", input$Level),
#   #           str_c(input$Tree_view, "_Sapflow_", input$Level, ".csv")))
#   str(dataInput())
#   # print(output$maxdate.output)
# })
