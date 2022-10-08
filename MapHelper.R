library(leaflet)
library(jsonlite)

# house_sale <- read.csv("data/lyon_housing.csv")



get_lyon_map <- function(sales_data){
  
  

  
  # legend("bottomleft", 
  #        legend = legend.text, 
  #        fill = shades[c(1, 25, 50, 75, 100)], 
  #        title = legend.title)
  
  # Sub-sample sales_data if necessary
  if (nrow(sales_data) > 4000){
    displayed_sales_data <- sales_data[sample(nrow(sales_data), 4000), ]
  } else {
    displayed_sales_data <- sales_data
  }
  displayed_sales_data <- displayed_sales_data[complete.cases(
    displayed_sales_data[c("latitude","longitude", "prix")]),]
  
  # # generate vector of fill colors for map
  # shades <- colorRampPalette(c("blue", "red"))(100)
  # norm_price <- as.integer(100 * (displayed_sales_data$prix - min(displayed_sales_data$prix)) /
  #                            (quantile(displayed_sales_data$prix, 0.95) - min(displayed_sales_data$prix)))
  # norm_price <- pmax(norm_price, 1)
  # norm_price <- pmin(norm_price, 100)
  displayed_sales_data$prix_trunc <- pmin(displayed_sales_data$prix,
                                    1200000)
  #quantile(displayed_sales_data$prix, 0.95))
  
  
  pal <- colorNumeric(
    palette = "RdYlBu",
    domain = displayed_sales_data$prix_trunc
  )
  
  leaflet(data = displayed_sales_data) %>% addTiles() %>%
    addLegend("bottomright", pal = pal, values = ~prix_trunc,
              title = "Prix",
              labFormat = labelFormat(prefix = "€"),
              opacity = 1) %>%
    addCircleMarkers(lng = ~longitude, lat = ~latitude,
                     color = ~pal(prix_trunc), radius=1, opacity = 0.5, fillOpacity = 0.2) 
}





