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
    fluidRow(
      p("The idea is to link 2 datasets here. The first dataset (dataset A) is
      selected in the ingest tab. The second dataset (dataset B) will be selected
      here. A table providing links between e.g. transcriptomics and proteomics
      will be loaded here. In the future this should be a pull down menu selecting
      which linking file you want or automatically depending on the datasets you
      load. ")
    ),
    fluidRow(
      htmlOutput(outputId = ns("UI_dataset_a")),
      htmlOutput(outputId = ns("UI_dataset_b")),
      selectizeInput(inputId = ns("SI_database_B"),
                     label = "Choose dataset B",
                     choices = "",
                     multiple = FALSE,
                     options = list(placeholder = "Dataset B")),
      shinyjs::disabled(
        actionButton(inputId = ns("AB_load_B"),
                     label = "Load Data",
                     class = "btn btn-large btn-danger")
      )
      # verbatimTextOutput(outputId = ns("temp"))
    )
  )
}

#' linking Server Functions
#'
#' @param id id of the instance
#' @param rv_data contains all the data loaded
#' @param rv_selections
#'
#' @noRd
mod_linking_server <- function(id, rv_data, rv_selections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ###### reactive values
    # database info
    db <- reactiveValues(name_a = NULL, # current, dataset A
                         name_b = NULL, # dataset B
                         dir_a = NULL, # current
                         dir_b = NULL, # directory of dataset B
                         root = NULL, # db_root
                         list = NULL, # all databases
                         regenerate = TRUE) # always regenerate on initial load...

    ##### outputs

    output$temp <- renderText({
      req(rv_data)

      # What omic type is loaded
      rv_data$db_meta$name
    })

    output$UI_dataset_a <- renderUI({
      req(rv_data$db_meta$name)

      # Show which database is loaded and from which omic type.
      if (is.null(rv_data$db_meta$name) ) {
        out_text <- "No data loaded"
      } else {
        out_text <- paste("<i>", rv_data$db_meta$omics_type,
                          "</i> database:  <b>", rv_data$db_meta$name,
                          "</b>")
      }
      out_text <- HTML("<b>Dataset A :</b> ", out_text)
      return(out_text)
    })

    output$UI_dataset_b <- renderUI({
      req(db$name_b)

      # Show which database is loaded and from which omic type.
      if (is.null(db$name_b) ) {
        out_text <- "No data loaded"
      } else {
        out_text <- paste("<i>", #rv_data$db_meta$omics_type,
                          "</i> database:  <b>", db$name_b,
                          "</b>")
      }
      out_text <- HTML("<b>Dataset B :</b> ", out_text)
      return(out_text)
    })


    ##### observers
    # update all database info
    observe({
      req(rv_data$db_meta)

      db$name_a <- rv_data$db_meta$name
      db$list <- rv_data$db_meta$db_list
    })

    # load dataset
    observe({
      req(input$SI_database_B)

      # retrieve all database info
      db$dir_b <- input$SI_database_B
      db$name_b <- names(which(db$list == input$SI_database_B))

      print("Rico: dataset B")
    })

    # activate button
    observe({
      if (!is.null(db$name_b) ) {
        shinyjs::enable("AB_load_B")
      } else {
        shinyjs::disable("AB_load_B")
      }
    })

    # update the pull down selection with the database list
    observe({
      req(db$list)

      # update the pull down selector
      updateSelectizeInput(session = session,
                           inputId = "SI_database_B",
                           choices = db$list,
                           selected = db$list[1],
                           server = TRUE)
    })

  }) # end of moduleServer
}

## To be copied in the UI
# mod_linking_ui("linking_ui_1")

## To be copied in the server
# mod_linking_server("linking_ui_1")
