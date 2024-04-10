# Load necessary libraries
library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)


# Define UI for application
ui <- fluidPage(
  titlePanel("SHERLOCK Excel Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Excel File",
        accept = c(".xlsx")
      ),
      selectInput("x", "Select x-axis variable:", choices = NULL),
      selectInput("y", "Select y-axis variable:", choices = NULL),
      selectInput("color", "Select color variable:", choices = NULL),
      downloadButton("downloadPlot", "Download Plot")
    ),
    mainPanel(
      plotOutput("plot")
    )
  ),
  p("Note: This app is designed to work with the SHERLOCK ONCOR results Excel data file. Pending QA/QC samples and samples that are heterozygous pending a Fluidigm run are excluded from the visualization. Fluidigm run-type assignments are not distinguished here from normal SHERLOCK data.")
)

# Define server logic
server <- function(input, output, session) {
  # Read in static LAD data
  lad <- read_tsv("data\\LengthCriteriaDelta_sheet1.txt") %>%
    mutate_all(~ ifelse(. == "*", NA, .)) %>%
    mutate(year = ifelse(row_number() <= 184, 2023, 2024)) %>%
    select(
      month = 1,
      day = 2,
      w23_min = 3,
      w23_max = 4,
      w22_min = 5,
      w22_max = 6,
      lf_min = 8,
      lf_max = 9,
      f22_min = 11,
      f22_max = 12,
      f23_min = 13,
      f23_max = 14,
      s22_min = 16,
      s22_max = 17,
      s23_min = 18,
      s23_max = 19,
      year = year
    ) %>%
    mutate(
      date = lubridate::ymd(paste(year, month, day, sep = "-")),
      w23_min = as.numeric(w23_min),
      w23_max = as.numeric(w23_max),
      w22_min = as.numeric(w22_min),
      w22_max = as.numeric(w22_max),
      lf_min = as.numeric(lf_min),
      lf_max = as.numeric(lf_max),
      f22_min = as.numeric(f22_min),
      f22_max = as.numeric(f22_max),
      f23_min = as.numeric(f23_min),
      f23_max = as.numeric(f23_max),
      s22_min = as.numeric(s22_min),
      s22_max = as.numeric(s22_max),
      s23_min = as.numeric(s23_min),
      s23_max = as.numeric(s23_max)
    )

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
    updateSelectInput(session, "x", choices = names(data()), select = "Sample.Date")
    updateSelectInput(session, "y", choices = names(data()), select = "Fork.Length")
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
    req(data(), input$x, input$y, input$color)
    p <- ggplot(data(), aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(size = 3, alpha = 0.6) +
      scale_color_brewer(palette = "Set1") +
      theme_bw()

    ggplotly(p)
  })

  output$plot <- renderPlot({
    p()
  })

  # Download the plot as a PNG file
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot", ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = p(), device = "png", width = 8, height = 6, units = "in", dpi = 300)
    }
  )
}




# Run the application
shinyApp(ui = ui, server = server)
