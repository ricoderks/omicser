#' linking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_linking_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' linking Server Functions
#'
#' @noRd 
mod_linking_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_linking_ui("linking_ui_1")
    
## To be copied in the server
# mod_linking_server("linking_ui_1")
