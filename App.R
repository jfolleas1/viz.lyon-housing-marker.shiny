# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(maps)
library(mapproj)

source(file = "MapHelper.R", local = TRUE)
source(file = "DataFilteringHelper.R", local = TRUE)
source(file = "PlotHelper.R", local = TRUE)

# Load data --------------------------------------------------------------------

house_sale_df <- read.csv("data/lyon_housing.csv")
n_house_sales <- nrow(house_sale_df)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs:
    sidebarPanel(
      get_data_filtering_inputs(house_sale_df)
      ),
    # Output:
    mainPanel(
      textOutput(outputId = "nb_sell_loaded"),
      tabsetPanel(
        type = "tabs",
        tabPanel("Data table",
                 dataTableOutput(outputId = "sale_data_table")
                 ),
        tabPanel("Map of the city",
                 leafletOutput("map")),
        tabPanel("Density and scatter plot",
          get_scatter_dist_plot_pannel(house_sale_df)
        )
      )
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  ## Reactive
  # Selected sub dataset
  sub_house_sale_df <- reactive({
    data_filter_application(house_sale_df, input)
  })
  n_sub_house_sale_df <- reactive({
    nrow(sub_house_sale_df())
  })
  ## Output buildings
  output$nb_sell_loaded <- renderText({
    paste("There is in total ", n_house_sales, " house sales loaded, ",
          n_sub_house_sale_df(), " were selected")
  })
  output$sale_data_table <- renderDataTable({
    DT::datatable(data = sub_house_sale_df(), 
                  options = list(pageLength = 10, scrollX = TRUE), 
                  rownames = FALSE)
  })
  output$map <- renderLeaflet({
    get_lyon_map(sub_house_sale_df())
  })
  build_plot_panel_outputs(input, output, sub_house_sale_df)
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
