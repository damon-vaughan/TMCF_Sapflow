library(needs)
needs(shiny, tidyverse, lubridate)

options(readr.show_col_types = FALSE)
source("app_functions_SF.R")

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

      sliderInput("daterange",
                  label = h4("Select date range"),
                  min = ymd_hms("2022-07-01 00:00:00", tz = "UTC"),
                  max = max(import.log$Last.import),
                  value = c(ymd_hms("2022-09-01 00:00:00", tz = "UTC"),
                            max(import.log$Last.import))),
      fluidRow(
        column(3,
               radioButtons("fixed.y",
                            label = h4("Fix y-axis?"),
                            choices = c("Yes", "No"),
                            selected = "No")),
        column(9,
               sliderInput("yrange",
                           label = h4("Select y-axis range"),
                           min = -10,
                           max = 30,
                           value = c(-10, 30)))),

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
        column(3,
               radioButtons("Tree_view",
                            label = h4("Select tree"),
                            choices = c("ET1", "ET2", "ET3", "ET4",
                                        "ET5", "ET6", "ET7", "ET8",
                                        "FB1", "FB2", "FB3", "FB4",
                                        "FB5", "FB6", "FB7", "FB8",
                                        "TV1", "TV2", "TV3", "TV4"),
                            selected = "ET1")),
        column(3, offset = 1,
               radioButtons("Cable_view",
                            label = h4("Select cable"),
                            choices = c("1", "2", "3", "4",
                                        "5", "6", "7", "8"),
                            selected = "1"),
               # checkboxGroupInput("Cable_subseries",
               #                    label = h4("Select sub-series"),
               #                    choices = c("a", "b", "c", "d"),
               #                    selected = c("a", "b", "c", "d")),
               radioButtons("Time.res",
                            label = h4("Select time resolution"),
                            choices = c("15 Min", "Hourly", "Daily", "Weekly"),
                            selected = "15 Min"))),

      titlePanel("Download options"),

      radioButtons("Time_format",
                   label = h4("Select time format"),
                   choices = list("ISO", "Excel_ready"),
                   selected = "ISO"),

      downloadButton("downloadData", "Download")
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot",
                 "Graph shows data until:",
                 verbatimTextOutput("max.date.out"),
                 plotOutput("plot", width = p.width, height = p.height,
                            click = "plot_click",
                            brush = "plot_brush"),
                 verbatimTextOutput("plot_clickinfo"),
                 plotOutput("plot_brushedpoints")),
        tabPanel("Data", tableOutput("data1"), tableOutput("data2")),
        tabPanel("Summary", tableOutput("summary"))))
  )
)

# Server -----------------------------------------------------------------

server <- function(input, output, session) {
  # dataInput <- reactive({
  #   if(input$Level == "L1") {
  #     read_csv(file.path(str_c("Sapflow_data_", input$Level),
  #                      str_c(input$Tree_view, "_Sapflow_", input$Level, ".csv")),
  #            show_col_types = F) %>%
  #     filter(Cable == input$Cable_view) %>%
  #     filter(Timestamp >= as.POSIXct(input$daterange[1], tz = "UTC")  &
  #              Timestamp <= as.POSIXct(input$daterange[2], tz = "UTC"))
  #     } else {
  #       read_csv(file.path(str_c("Sapflow_data_", input$Level),
  #                        str_c(input$Tree_view, "_Sapflow_", input$Level, ".csv")),
  #              show_col_types = F) %>%
  #       filter(Cable == str_c(input$Cable_view, input$Cable_subseries)) %>%
  #       filter(Timestamp >= as.POSIXct(input$daterange[1], tz = "UTC")  &
  #                Timestamp <= as.POSIXct(input$daterange[2], tz = "UTC"))}
  #   })

  dataInput <- reactive({
    read_csv(file.path(str_c("Sapflow_data_", input$Level),
                       str_c(input$Tree_view, "_Sapflow_",
                             input$Level, ".csv"))) %>%
      filter(str_sub(Cable, start = 1, end = 1) == input$Cable_view) %>%
      filter(Timestamp >= as.POSIXct(input$daterange[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$daterange[2], tz = "UTC"))
  })

  dataInput2 <- reactive({
    if(input$Time.res == "15 Min"){
      dataInput()
    } else if(input$Time.res == "Hourly"){
      dataInput() %>%
        group_by(Tree, Cable, Timestamp = floor_date(Timestamp, "hour")) %>%
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>%
        ungroup()
    } else if(input$Time.res == "Daily"){
      dataInput() %>%
        group_by(Tree, Cable, Timestamp = floor_date(Timestamp, "day")) %>%
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>%
        ungroup()
    } else if(input$Time.res == "Weekly"){
      dataInput() %>%
        group_by(Tree, Cable, Timestamp = floor_date(Timestamp, "week")) %>%
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>%
        ungroup()}
  })

  dataInput3 <- reactive({
    dataInput2() %>%
      pivot_longer(4:6, names_to = "Position", values_to = "Velocity")
  })

  flagInput <- reactive({
    baseline.flags %>%
      filter(Tree == input$Tree_view) %>%
      filter(Cable == str_c(input$Cable_view, input$Cable_subseries)) %>%
      filter(Timestamp >= as.POSIXct(input$daterange[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$daterange[2], tz = "UTC")) %>%
      left_join(dataInput(), by = c("Tree", "Cable", "Timestamp")) %>%
      select(Tree, Cable, Timestamp, outer_vel, middle_vel, inner_vel) %>%
      pivot_longer(4:6, names_to = "Position", values_to = "Velocity")
  })

  labelInput <- reactive({
    SF.actions %>%
      filter(Tree == input$Tree_view) %>%
      filter(Cable == input$Cable_view) %>%
      filter(Timestamp >= as.POSIXct(input$daterange[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$daterange[2], tz = "UTC"))
  })

  zeroesInput <- reactive({
    zeroes %>%
      filter(Tree == input$Tree_view) %>%
      filter(periodBegin >= as.POSIXct(input$daterange[1], tz = "UTC")  &
               periodBegin <= as.POSIXct(input$daterange[2], tz = "UTC"))
  })

  output$max.date.out <- renderText({
    as.character(max(dataInput2()$Timestamp))
  })

  output$plot <- renderPlot({
    p <- ggplot() +
      geom_line(data = dataInput3(), aes(x = Timestamp, y = Velocity)) +
      geom_hline(yintercept = 0) +
      labs(y = "cm / hour") +
      scale_x_datetime(
        limits = c(as.POSIXct(input$daterange[1], tz = "UTC"),
                   as.POSIXct(input$daterange[2], tz = "UTC"))) +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 18),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 18),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$Tree_view, "_", input$Cable_view)) +
      facet_wrap(~Position, ncol = 1, scales = "fixed")

    if(input$fixed.y == "Yes"){
      p <- p +
        ylim(input$yrange[1], input$yrange[2])
    }

    if(input$Show_zeroes == "yes"){
      p2 <- p +
        geom_vline(data = zeroesInput(), aes(xintercept = periodBegin,
                                             y = min(dataInput3()$Velocity, na.rm = T)),
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

  output$plot_clickinfo <- renderPrint({
    val <- nearPoints(dataInput3(), input$plot_click, maxpoints = 1)
    as.character(unique(val$Timestamp))
  })

  output$plot_brushedpoints <- renderPlot({
    dat <- brushedPoints(dataInput3(), input$plot_brush)
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

  ## Data and summary --------------------------------------------------------

  data.for.summaries <- reactive({
    dataInput2() %>%
      mutate(Timestamp = as.character(Timestamp))
  })

  output$data1 <- renderTable({
    head(data.for.summaries())
  })

  output$data2 <- renderTable({
    tail(data.for.summaries())
  })

  output$summary <- renderTable({
    summarise_sapflow(data.for.summaries())
  })

  ## Download data options ---------------------------------------------------

  data.for.download <- reactive({
    if(input$Time_format == "ISO"){
      dataInput2()
    } else if(input$Time_format == "Excel_ready"){
      dataInput2() %>%
        mutate(Timestamp = as.character(Timestamp))
    }
  })

  # data.for.download <- dataInput2()

  output$downloadData <- downloadHandler(
    filename = function() {
      str_c(str_c(input$Tree_view, str_c("Cable", input$Cable_view),
                  str_sub(as.character(input$daterange[1]), start = 1, end = 10),
                  str_sub(as.character(input$daterange[2]), start = 1, end = 10),
                  sep = "_"),
            ".csv")
    },
    content = function(file) {
      write_csv(data.for.download(), file)
    }
    # content = function(file) {
    #   write_csv(dataInput2(), file)
    # }
  )
}

# Run app ----------------------------------------------------------------

shinyApp(ui, server)

# Test server -------------------------------------------------------------

test1 <- testServer(server, {
  session$setInputs(Tree_view = "ET1")
  session$setInputs(Level = "L3")
  session$setInputs(Cable_view = "1")
  session$setInputs(Cable_subseries = "a")
  session$setInputs(Time.res = "15 Min")
  # session$setInputs(daterange = c(min = ymd("2022-09-01"),
  #                             max = ymd("2023-04-30")))
  session$setInputs(daterange = c(min = ymd("2023-09-01"),
                              max = ymd("2023-12-01")))
  test1 <<- print(dataInput2())
})

# Testing labels
# test <- testServer(server, {
#   session$setInputs(Tree_view = "FB6")
#   session$setInputs(Level = "L2")
#   session$setInputs(Cable_view = "1")
#   session$setInputs(Cable_subseries = "a")
#   # session$setInputs(daterange = c(min = ymd("2022-09-01"),
#   #                             max = ymd("2023-04-30")))
#   session$setInputs(daterange = c(min = ymd("2022-09-01"),
#                               max = maxdate))
#   print(dataInput())
# })

# Testing Level functioning
# test <- testServer(server, {
#   session$setInputs(Tree_view = "FB6")
#   session$setInputs(Level = "L2")
#   session$setInputs(Cable_view = "1")
#   session$setInputs(Cable_subseries = "a")
#   session$setInputs(daterange = c(min = ymd("2022-09-01"),
#                               max = maxdate))
#   # print(file.path(str_c("Sapflow_data_", input$Level),
#   #           str_c(input$Tree_view, "_Sapflow_", input$Level, ".csv")))
#   str(dataInput())
#   # print(output$max.date.out)
# })
