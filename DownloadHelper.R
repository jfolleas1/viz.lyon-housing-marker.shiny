library(shiny)


get_download_buton <- function(){
  downloadButton("download_sel_data", "Download selected data")
}

set_download_action <- function(input, output, sub_house_sale_df){
  output$download_sel_data <- downloadHandler(
    filename = function() {
      'housing_market_data.csv'
      },
    content = function(file) { 
      write.csv(sub_house_sale_df(), file) 
    }
  )
}