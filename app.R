library(shiny)
library(DT)
library(plotly)

source("R/data_import.R")
source("R/metadata_profile.R")
source("R/filter_engine.R")
source("R/phyloseq_builder.R")
source("R/plots_ordination.R")
source("R/export_reproduce.R")

ui <- fluidPage(
  titlePanel("better-renacapa (MVP)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("tax_file", "Taxonomy table (csv/tsv)", accept = c(".csv", ".tsv", ".txt")),
      fileInput("meta_file", "Metadata table (csv/tsv)", accept = c(".csv", ".tsv", ".txt")),
      textInput("sample_id_col", "Sample ID column name in metadata", value = "SampleID"),

      hr(),
      h4("Metadata filters"),
      uiOutput("filter_ui"),

      hr(),
      h4("Ordination mapping"),
      uiOutput("mapping_ui"),

      hr(),
      numericInput("topN_levels", "Max levels for categorical variables (Top N + Other)", value = 12, min = 3, max = 50),
      numericInput("min_level_n", "Min samples per level (below -> Other)", value = 5, min = 1, max = 100),

      hr(),
      downloadButton("dl_reproduce", "Download reproduce.R")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Metadata Explorer", DTOutput("meta_profile")),
        tabPanel("Filtered Samples", DTOutput("filtered_meta")),
        tabPanel("Ordination", plotlyOutput("ord_plot", height = 600))
      )
    )
  )
)

server <- function(input, output, session){

  raw <- reactive({
    req(input$tax_file, input$meta_file)
    tax <- read_table_auto(input$tax_file$datapath)
    meta <- read_table_auto(input$meta_file$datapath)
    list(tax = tax, meta = meta)
  })

  meta_profile_df <- reactive({
    req(raw())
    profile_metadata(raw()$meta, input$sample_id_col)
  })

  output$meta_profile <- renderDT({
    datatable(meta_profile_df(), options = list(pageLength = 25, scrollX = TRUE))
  })

  # Build filter UI dynamically from metadata profile
  output$filter_ui <- renderUI({
    req(raw())
    build_filter_ui(raw()$meta, input$sample_id_col, session)
  })

  filtered <- reactive({
    req(raw())
    apply_filters(raw()$meta, input$sample_id_col, input)
  })

  output$filtered_meta <- renderDT({
    datatable(filtered()$meta, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$mapping_ui <- renderUI({
    req(filtered())
    cols <- setdiff(names(filtered()$meta), input$sample_id_col)
    tagList(
      selectInput("color_by", "Color by", choices = c("None", cols), selected = "None"),
      selectInput("shape_by", "Shape by", choices = c("None", cols), selected = "None")
    )
  })

  ord_plot_obj <- reactive({
    req(raw(), filtered())
    ps <- build_phyloseq_from_tables(
      tax = raw()$tax,
      meta = filtered()$meta,
      sample_id_col = input$sample_id_col
    )

    make_ordination_plot(
      ps,
      color_by = input$color_by,
      shape_by = input$shape_by,
      topN_levels = input$topN_levels,
      min_level_n = input$min_level_n
    )
  })

  output$ord_plot <- renderPlotly({
    req(ord_plot_obj())
    ggplotly(ord_plot_obj(), tooltip = c("text"))
  })

  output$dl_reproduce <- downloadHandler(
    filename = function() "reproduce.R",
    content = function(file){
      writeLines(
        build_reproduce_script(input = input),
        con = file
      )
    }
  )
}

shinyApp(ui, server)