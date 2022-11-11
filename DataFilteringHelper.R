library(shiny)


get_data_filtering_inputs <- function(house_sale_df){
  data_filtering_inputs <- verticalLayout(h2("Dataset filtering : "),
                 checkboxGroupInput(inputId = "cb_buy_type",
                                    label = "Buy type :", 
                                    choices = unique(house_sale_df$type_achat),
                                    selected = unique(house_sale_df$type_achat)),
                 checkboxGroupInput(inputId = "cb_property_type",
                                    label = "Property type :", 
                                    choices = unique(house_sale_df$type_bien),
                                    selected = unique(house_sale_df$type_bien)),
                 selectInput(inputId = "city_selector",
                             label = "City :",
                             choices = unique(house_sale_df$commune),
                             selected = unique(house_sale_df$commune),
                             multiple = TRUE,
                             selectize = FALSE),
                 sliderInput(
                   inputId = 'nb_room_selector',
                   label = 'Number of rooms',
                   min = 1,
                   max = max(house_sale_df$nombre_pieces),
                   value = c(1, max(house_sale_df$nombre_pieces)),
                   step = 1),
                 sliderInput(
                   inputId = 'dwelling_size_selector',
                   label = 'Dwelling size (m2)',
                   min = 0,
                   max = max(house_sale_df$surface_logement),
                   value = c(0, max(house_sale_df$surface_logement)),
                   step = 5),
                 sliderInput(
                   inputId = 'garden_size_selector',
                   label = 'Garden size (m2)',
                   min = 0,
                   max = max(house_sale_df$surface_terrain),
                   value = c(0, max(house_sale_df$surface_terrain)),
                   step = 5),
                 sliderInput(
                   inputId = 'nb_car_spaces_selector',
                   label = 'Number of car space',
                   min = 0,
                   max = max(house_sale_df$nombre_parkings),
                   value = c(0, max(house_sale_df$nombre_parkings)),
                   step = 1),
                 sliderInput(
                   inputId = 'building_age_selector',
                   label = 'Building age',
                   min = 0,
                   max = trunc(max(house_sale_df$anciennete))+1,
                   value = c(0, trunc(max(house_sale_df$anciennete))+1),
                   step = 1),
                 numericInput(
                   inputId = 'min_price_selector',
                   label = 'Min price',
                   value = 0,
                   min = 0,
                   max = trunc(max(house_sale_df$prix))+1,
                   step = 1),
                 numericInput(
                   inputId = 'max_price_selector',
                   label = 'Max price',
                   value = trunc(max(house_sale_df$prix))+1,
                   min = 0,
                   max = trunc(max(house_sale_df$prix))+1,
                   step = 1))
}

data_filter_application <- function(house_sale_df, input){
  house_sale_df %>%
    filter(type_achat %in% input$cb_buy_type) %>%
    filter(type_bien %in% input$cb_property_type) %>%
    filter(commune %in% input$city_selector) %>%
    filter(nombre_pieces >= input$nb_room_selector[1]) %>%
    filter(nombre_pieces <= input$nb_room_selector[2]) %>%
    filter(surface_logement >= input$dwelling_size_selector[1]) %>%
    filter(surface_logement <= input$dwelling_size_selector[2]) %>%
    filter(surface_terrain >= input$garden_size_selector[1]) %>%
    filter(surface_terrain <= input$garden_size_selector[2]) %>%
    filter(nombre_parkings >= input$nb_car_spaces_selector[1]) %>%
    filter(nombre_parkings <= input$nb_car_spaces_selector[2]) %>%
    filter(anciennete >= input$building_age_selector[1]) %>%
    filter(anciennete <= input$building_age_selector[2]) %>%
    filter(prix >= input$min_price_selector) %>%
    filter(prix <= input$max_price_selector)
}