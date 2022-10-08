# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(maps)
library(mapproj)

source(file = "MapHelper.R", local = TRUE)
source(file = "DataFilteringHelper.R", local = TRUE)

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
                 flowLayout(selectInput(
                   inputId = "scatterplotx",
                   label = "Scatter plot X:",
                   choices = c("Nombre de pieces"="nombre_pieces",
                               "Surface logement"="surface_logement", 
                               "Surface carrez"="surface_carrez_logement",
                               "Surface terrain"="surface_terrain",
                               "Prix"="prix",
                               "Anciennete"="anciennete"),
                   selected = "surface_logement"),
                 selectInput(
                   inputId = "scatterploty",
                   label = "Scatter plot Y:",
                   choices = c("Nombre de pieces"="nombre_pieces",
                               "Surface logement"="surface_logement", 
                               "Surface carrez"="surface_carrez_logement",
                               "Surface terrain"="surface_terrain",
                               "Prix"="prix",
                               "Anciennete"="anciennete"),
                   selected = "prix"),
                 selectInput(
                   inputId = "scatterplotcolor",
                   label = "Coloring:",
                   choices = c("Type d'achat"="type_achat",
                               "Type de bien"="type_bien",
                               "Nombre de pieces"="nombre_pieces",
                               "Nombre de parking"="nombre_parkings",
                               "Commune"="commune"),
                   selected = "prix")),
                 plotOutput(outputId = "scatterplot", height = 400),
                 selectInput(
                   inputId = "densityplotvar",
                   label = "Variable to display:",
                   choices = c("Nombre de pieces"="nombre_pieces",
                               "Surface logement"="surface_logement", 
                               "Surface carrez"="surface_carrez_logement",
                               "Surface terrain"="surface_terrain",
                               "Prix"="prix",
                               "Anciennete"="anciennete"),
                   selected = "prix"),
                 plotOutput(outputId = "densityplot", height = 200))
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
  output$densityplot <- renderPlot({
    ggplot(data = sub_house_sale_df(), aes_string(x = input$densityplotvar)) +
      geom_density()
  })
  output$scatterplot <- renderPlot({
    ggplot(data = sub_house_sale_df(), aes_string(
      x=input$scatterplotx,
      y=input$scatterploty,
      color=isolate(input$scatterplotcolor))) +
      geom_point()
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
