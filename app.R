# Load necessary libraries
library(shiny)
library(tidyverse)
library(readxl)


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
    updateSelectInput(session, "x", choices = names(data()), select = "Sample.Date") # nolint: line_length_linter.
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
    ggplot(data(), aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(size = 3, alpha = 0.6) +
      scale_color_brewer(palette = "Set1") +
      theme_bw()
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
