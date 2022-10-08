library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

get_scatter_dist_plot_pannel <- function(house_sale_df){
  scatter_dist_plot_pannel <- verticalLayout(
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
    plotOutput(outputId = "densityplot", height = 200)
  )
}

build_plot_panel_outputs <- function(input, output, sub_house_sale_df){
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