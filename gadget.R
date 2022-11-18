library(shiny)
library(dplyr)
library(tibble)
library(tidyr)
library(DT)
library(stringr)
library(glue)
library(miniUI)

data_manipulation_gadget <- function(dataset){
  
  ui <-
    miniPage(
      
      shiny::tags$head(
        shiny::tags$script("function copy_text_function() {
      var text = document.querySelector('#actions').textContent;
      navigator.clipboard.writeText(text);
        }")
      ),
      
      miniContentPanel(
      
      fluidRow(
        
        actionButton(inputId = "select_button", label = "SELECT"),
        
        actionButton(inputId = "unnest_button", label = "UNNEST"),
        
        actionButton(inputId = "deselect_button", label = "DE-SELECT")
        
      ),
      
      fluidRow(
        DT::dataTableOutput(outputId = "table_output")
      ),
      
      
      fluidRow(
        textOutput(outputId = "test_text")
      ),
      
      fluidRow(
        actionButton(inputId = "copy_test", label = "Copy text", onclick = "copy_text_function()"),
      ),
      
      fluidRow(
        textOutput(outputId = "actions")
      )
      )
      
    )
  
  server <- function(input, output , session){
    
    
    table_list <- 
      reactiveValues("tables" = list(),
                     "action_string" = "")
    
    observe({
      
      table_list$tables[[1]] <- 
        dataset %>% 
        tibble::enframe() %>% 
        tibble::column_to_rownames(var = "name") %>% 
        t() %>% 
        tibble::as_tibble()
      
      table_list$action_string <- 
        glue::glue("{dataset} %>% 
                 tibble::enframe() %>% 
                 tibble::column_to_rownames(var = 'name') %>% 
                 t() %>% 
                 tibble::as_tibble()")
      
    }) %>% 
      bindEvent(dataset)
    
    
    output$table_output <-
      renderDataTable({
        
        table_list$tables %>% 
          dplyr::last() %>% 
          DT::datatable(selection = list(target = "column"), options = list(scrollX = T) )
        
        
      }
      )
    
    output$actions <- 
      renderText({
        
        table_list$action_string
        
      }) 
    
    output$test_text <- 
      renderText({
        
        input$table_output_columns_selected
        
      }) 
    
    
    observe({
      
      table_list$tables[[1]] <- 
        table_list$tables[[1]] %>% 
        select(input$table_output_columns_selected)
      
      action_string_to_append <- 
        glue::glue("
                select({glue::glue_collapse(input$table_output_columns_selected, sep = ',')})
                ")
      
      table_list$action_string <-
        stringr::str_c(table_list$action_string, " %>% ", action_string_to_append)
      
    }) %>% 
      bindEvent(input$select_button)
    
    observe({
      
      table_list$tables[[1]] <- 
        table_list$tables[[1]] %>% 
        tidyr::unnest(input$table_output_columns_selected, names_sep = "_" )
      
      action_string_to_append <- 
        glue::glue("
                tidyr::unnest({glue::glue_collapse(input$table_output_columns_selected, sep = ',')}, names_sep = '_' )
                ")
      
      table_list$action_string <-
        stringr::str_c(table_list$action_string, " %>% ", action_string_to_append)
      
      
      
    }) %>% 
      bindEvent(input$unnest_button)
    
    observe({
      
      table_list$tables[[1]] <- 
        table_list$tables[[1]] %>% 
        select(- input$table_output_columns_selected)
      
      action_string_to_append <- 
        glue::glue("
                select(- c( {glue::glue_collapse(input$table_output_columns_selected, sep = ',')} ) )
                ")
      
      table_list$action_string <-
        stringr::str_c(table_list$action_string, " %>% ", action_string_to_append)
      
      
    }) %>% 
      bindEvent(input$deselect_button)
    
    
  }
  
  runGadget(ui, server)
}
