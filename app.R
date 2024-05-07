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
      dateRangeInput("date_range", "Select date range:",
        start = NULL,
        end = NULL
      ),
      selectInput("model", "Select LAD size classes:", choices = NULL, multiple = TRUE),
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
        SampleDate = force_tz(SampleDate, tzone = "America/Los_Angeles"),
        GTSeq.Assignment = if_else(
          GTSeq.Assignment == "Non-winter",
          "Non-Winter",
          GTSeq.Assignment
          ),
        GTSeq.OTS28 = tools::toTitleCase(GTSeq.OTS28),
        SH_greb1l = case_when(
          SH..OTS.28 == "Early / Early" ~ "Early",
          SH..OTS.28 == "Late / Late" ~ "Late",
          SH..OTS.28 == "Early / Late" ~ "Intermediate",
          TRUE ~ "Other"
          )

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
      "GTSeq.Assignment",
      "GTSeq.Group",
      "Model",
      "Heterozygote.",
      "Facility",
      "GTseqEarlyAndFall",
      "GTseqLateAndSpring",
      "SH_OTS28"
    )
    valid_choices <- choices[choices %in% names(data())]
    updateSelectInput(session, "color", choices = valid_choices)

    model_choices <- data() %>%
      pull(Model) %>%
      unique() %>%
      sort()
    updateSelectInput(session, "model", choices = model_choices, selected = model_choices)

    updateDateRangeInput(
      session,
      "date_range",
      start = as.Date(min(data()$SampleDate, na.rm = TRUE), tz = "America/Los_Angeles"),
      end = as.Date(max(data()$SampleDate, na.rm = TRUE), tz = "America/Los_Angeles"),
      min = as.Date(min(data()$SampleDate, na.rm = TRUE), tz = "America/Los_Angeles"),
      max = as.Date(max(data()$SampleDate, na.rm = TRUE), tz = "America/Los_Angeles")
    )
  })

  p <- reactive({
    req(data(), input$color)
    prerun_color_palette <- RColorBrewer::brewer.pal(9, "Set1")
    run_color_mapping <- c(
      "Fall / Late Fall" = prerun_color_palette[5],
      "Fall" = "#c9b428",
      "Late Fall" = prerun_color_palette[7],
      "Spring" = prerun_color_palette[3],
      "Winter" = prerun_color_palette[2],
      "f22" = prerun_color_palette[6],
      "f23" = prerun_color_palette[6],
      "lf23" = prerun_color_palette[7],
      "s23" = prerun_color_palette[3],
      "w23" = prerun_color_palette[2],
      "lf24" = prerun_color_palette[7],
      "Non-Winter" = prerun_color_palette[9],
      "Early" = prerun_color_palette[2],
      "Intermediate" = "#c9b428",
      "Late" = prerun_color_palette[5],
      "Latefall" = prerun_color_palette[7],
      "female" = prerun_color_palette[1],
      "male" = prerun_color_palette[2],
      "TRUE" = prerun_color_palette[1],
      "FALSE" = prerun_color_palette[9],
      "CVP" = prerun_color_palette[2],
      "SWP" = prerun_color_palette[1],
      "Early / Early" = prerun_color_palette[2],
      "Late / Late" = prerun_color_palette[5],
      "Early / Late" = "#c9b428",
      "NA" = prerun_color_palette[9]
      )
    lad_long_filtered <- lad_long %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2] # Filter based on date range
      )

    p <- plot_ly(colors = run_color_mapping) %>%
      add_ribbons( # Add ribbons for min/max values overlaid on top of fork length
        data = lad_long_filtered,
        x = ~date,
        ymin = ~min,
        ymax = ~max,
        color = ~cohort,
        colors = run_color_mapping,
        showlegend = TRUE,
        alpha = 0.2,
        size = 0.1,
        line = list(width = 0)
      ) %>%
      add_markers(
        data = data() %>% filter(Model %in% input$model),
        x = ~SampleDate,
        y = ~ForkLength,
        color = ~ get(input$color),
        colors = run_color_mapping,
        text = ~ID,
        size = I(20),
        alpha = 0.8
      ) %>%
      layout(
        title = "Salvage Chinook Salmon",
        xaxis = list(title = "Sample Date", range = c(input$date_range[1] - days(3), input$date_range[2] + days(3))),
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
