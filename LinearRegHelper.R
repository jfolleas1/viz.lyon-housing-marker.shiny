library(shiny)

get_linear_reg_pannel <- function(){
  verbatimTextOutput(outputId = 'lmoutput')
}

set_linear_reg_pannel_output <- function(input, output, sub_house_sale_df){
  output$lmoutput <- renderPrint({
    lm_instance <- lm(
      prix ~ nombre_parkings + nombre_pieces + surface_logement + anciennete,
      data=sub_house_sale_df()
    )
    summ <- summary(lm_instance) 
    print(summ, digits = 3, signif.stars = FALSE)
  })
}