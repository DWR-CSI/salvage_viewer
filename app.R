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
  tsv_files <- list.files("data", pattern = "^lad_long.*\\.txt$", full.names = TRUE)
  lad_long <- purrr::map_dfr(tsv_files, read_tsv)

  data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath, sheet = "Working")
    names(df) <- make.names(names(df)) # Make column names valid
    df %>%
      mutate(
        SHERLOCK.Group = str_replace_all(SHERLOCK.Group, "\\*", ""),
        SHERLOCK.Assignment = str_replace_all(SHERLOCK.Assignment, "\\*", ""),
        SampleDate = force_tz(SampleDate, tzone = "America/Los_Angeles")
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
    choices <- c( # Valid and allowable choice column names
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
    )
    valid_choices <- choices[choices %in% names(data())]
    updateSelectInput(session, "color", choices = valid_choices)
  })

  date_range <- reactive({
    req(data())
    min_date <- min(data()$SampleDate, na.rm = TRUE)
    max_date <- max(data()$SampleDate, na.rm = TRUE)
    c(min_date, max_date)
  })

  p <- reactive({
    req(data(), input$color)
    lad_long_filtered <- lad_long %>%
      filter(date >= date_range()[1] - days(2) & date <= date_range()[2] + days(2)) # Filter based on date range

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
        data = lad_long_filtered,
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
        title = "Salvage Chinook Salmon",
        xaxis = list(title = "Sample Date", range = c(date_range()[1] - days(3), date_range()[2] + days(3))),
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
