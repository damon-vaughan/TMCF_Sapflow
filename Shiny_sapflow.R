library(needs)
needs(shiny, tidyverse, readxl, lubridate)

# Find all L2 data
filesFull <- list.files(here("Sapflow_data_L2"), full.names = T)
filesFull.sub <- filesFull[which(str_detect(filesFull, "Combined") == F)]

# Read and bind into one df
L2 <- lapply(filesFull.sub, read_csv, show_col_types = F) %>%
  bind_rows()
# L2 <- read_csv(file.path("Sapflow_data_L2", "Combined_Sapflow_L2.csv"))

maxdate <- ymd_hms(as.character(max(L2$Timestamp)))

# Read in maintenance actions so they can be applied to the plot
SF_actions <- read_csv(file.path("Sapflow_data_supporting", "Sapflow_maintenance_actions.csv"))

# Read in zero-ing periods from the microclimate data
Zeroes <- read_csv(file.path("Sapflow_data_supporting", "Zeroes_long.csv"))

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
                  min = ymd_hms("2022-09-01 00:00:00"),
                  max = maxdate,
                  value = c(ymd_hms("2022-09-01 00:00:00"), maxdate)),

      titlePanel("Viewing options"),

      fluidRow(
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
                            # choices = c("Cable1", "Cable2", "Cable3", "Cable4",
                            #             "Cable5", "Cable6", "Cable7", "Cable8"),
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
      # column(2, offset = 1,
      #        checkboxGroupInput("Cable_DL",
      #                     label = h4("Select cable"),
      #                     choices = c("Cable1", "Cable2", "Cable3", "Cable4",
      #                                 "Cable5", "Cable6", "Cable7", "Cable8"),
      #                     selected = "Cable1"))),

      radioButtons("Time_format",
                   label = h4("Select time format"),
                   choices = list("ISO", "Excel_ready"),
                   selected = "ISO"),

      downloadButton("downloadData", "Download")
    ),

    mainPanel(
      plotOutput("plot", width = p.width, height = p.height,
                 hover = "plot_hover",
                 brush = "plot_brush"),
      verbatimTextOutput("plot_hoverinfo"),
      plotOutput("plot_brushedpoints"))
  )
)

# Server -----------------------------------------------------------------

# * Server for L1, L2, L3 ---------------------------------------

server <- function(input, output, session) {

  dataInput <- reactive({
    # get(input$L) %>%
    L2 %>%
      filter(Tree == input$Tree_view) %>%
      filter(Cable == str_c(input$Cable_view, input$Cable_subseries)) %>%
      # filter(Cable == input$Cable_view) %>%
      filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$dates[2], tz = "UTC")) %>%
      pivot_longer(4:6, names_to = "Position", values_to = "Velocity")
  })

  labelInput <- reactive({
    SF_actions %>%
      filter(Tree == input$Tree_view) %>%
      filter(Cable == input$Cable_view) %>%
      filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$dates[2], tz = "UTC"))
  })

  zeroesInput <- reactive({
    Zeroes %>%
      filter(Tree == input$Tree_view) %>%
      filter(periodBegin >= as.POSIXct(input$dates[1], tz = "UTC")  &
               periodBegin <= as.POSIXct(input$dates[2], tz = "UTC"))
  })

  output$plot <- renderPlot({
    p <- ggplot() +
      geom_line(data = dataInput(), aes(x = Timestamp, y = Velocity)) +
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
      p <- p +
        geom_vline(data = zeroesInput(), aes(xintercept = periodBegin, y = -2), color = "blue")
    }
    if(input$Show_maintenance == "yes"){
      p <- p +
        geom_label(data = labelInput(), aes(x = Timestamp, y = -2, label = Action))
    }
    p},
    width = p.width, height = p.height)

  output$plot_hoverinfo <- renderPrint({
    val <- nearPoints(dataInput(), input$plot_hover, maxpoints = 1)
    unique(val$Timestamp)
  })

  output$plot_brushedpoints <- renderPlot({
    dat <- brushedPoints(dataInput(), input$plot_brush)
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

# # Deploy app --------------------------------------------------------------
# library(rsconnect)
#
# rsconnect::setAccountInfo(name='tmcfdata', token='1C78F1777CCC2DA19A8189627F5ECB3F', secret='RZhSSv4Yv9EYOPxkUlSy1tT6+828LquoaStTs1Qz')
# #
# deployApp()

# Test server -------------------------------------------------------------

# Test multiple selections for download
# test <- testServer(server, {
#   session$setInputs(Tree_DL = c("FB1", "FB2"))
#   session$setInputs(Cable_DL = "Cable1")
#   session$setInputs(dates = c(min = ymd("2022-09-01"),
#                               max = maxdate))
#   # session$setInputs(Download_type = "Excel_ready")
#   print(str(input$Tree_DL))
# })


# Test time format download
# testServer(server, {
#   session$setInputs(Tree_view = "FB1")
#   session$setInputs(Cable_view = "Cable1")
#   session$setInputs(dates = c(min = ymd("2022-09-01"),
#                               max = maxdate))
#   session$setInputs(Download_type = "Excel_ready")
#   str(dataInput2())
# })

# Testing labels
# testServer(server, {
#   session$setInputs(Tree_view = "FB6")
#   session$setInputs(Cable_view = "Cable1")
#   # session$setInputs(dates = c(min = ymd("2022-09-01"),
#   #                             max = ymd("2023-04-30")))
#   session$setInputs(dates = c(min = ymd("2022-09-01"),
#                               max = maxdate))
#   print(labelInput())
# })




