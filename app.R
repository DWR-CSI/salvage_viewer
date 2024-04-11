# Load necessary libraries
library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)


# Define UI for application
ui <- fluidPage(
  titlePanel("SHERLOCK Excel Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Excel File",
        accept = c(".xlsx")
      ),
      selectInput("color", "Select color variable:", choices = NULL)
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  ),
  p("Note: This app is designed to work with the SHERLOCK ONCOR results Excel data file.
  Pending QA/QC samples and samples that are heterozygous pending a Fluidigm run are excluded from the visualization.
  Fluidigm run-type assignments are not distinguished here from normal SHERLOCK data.")
)

# Define server logic
server <- function(input, output, session) {
  # Read in static LAD data
  lad_long <- read_tsv("data/lad_long.txt")

  data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath, sheet = "Working")
    names(df) <- make.names(names(df)) # Make column names valid
    df %>%
      mutate(
        SHERLOCK.Group = str_replace_all(SHERLOCK.Group, "\\*", ""),
        SHERLOCK.Assignment = str_replace_all(SHERLOCK.Assignment, "\\*", "")
      ) %>%
      filter(
        SHERLOCK.Group != "Likely heterozygote",
        SHERLOCK.Group != "Pending QA/QC",
        SHERLOCK.Group != "NA",
        GTSeq.Group != "Steelhead"
      ) %>%
      rename(SH_OTS28 = SH..OTS.28)
  })

  observe({ # Update selectInput choices when data is loaded
    req(data())
    updateSelectInput(session, "color", choices = c(
      "SHERLOCK.Assignment",
      "SHERLOCK.Group",
      "GTSeq.OTS28",
      "sexid",
      "GTSeq.Group",
      "Heterozygote.",
      "Facility",
      "GTseqEarlyAndFall",
      "GTseqLateAndSpring",
      "SH_OTS28"
    ))
  })

  p <- reactive({
    req(data(), input$color)

    p <- plot_ly() %>%
      add_markers(
        data = data(),
        x = ~SampleDate,
        y = ~ForkLength,
        color = ~ get(input$color),
        colors = "Paired",
        text = ~ID,
        size = I(20),
        alpha = 0.8
      ) %>%
      add_ribbons( # Add ribbons for min/max values overlaid on top of fork length
        data = lad_long,
        x = ~date,
        ymin = ~min,
        ymax = ~max,
        color = ~cohort,
        showlegend = TRUE,
        alpha = 0.2,
        size = 0.1,
        line = list(width = 0)
      ) %>%
      layout(
        title = "SHERLOCK Excel Data Visualization",
        xaxis = list(title = "Sample Date", range = c(as.Date("2023-12-25"), NULL)), # not working
        yaxis = list(title = "Fork Length"),
        showlegend = TRUE
      )

    p
  })

  output$plot <- renderPlotly({
    p()
  })
}




# Run the application
shinyApp(ui = ui, server = server)
