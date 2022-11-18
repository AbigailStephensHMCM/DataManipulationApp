shinyServer(function(input, output){
  

# Global ------------------------------------------------------------------

  

# Tab: Dimension transformation --------------------------------------------


## Tab Global --------------------------------------------------------------


### ReactiveVal: update_counter --------------------------------------------


  update_counter <- 
    reactiveVal(1)
  

### ReactiveValues: table_list ------------------------------------------------



  table_list <- 
    reactiveValues("tables" = list(),
                   "action_string" = list())
  
  
## Box: Structure Tools -----------------------------------------------------


### Observe: select_button --------------------------------------------------

  observe({
    
    req(input$file_upload)
    
    table_list$tables[[update_counter() + 1]] <- 
      table_list$tables %>% 
      purrr::pluck(update_counter()) %>% 
      select(input$table_output_columns_selected)

    
    action_string_to_append <- 
      glue::glue("
                select({glue::glue_collapse(colnames(table_list$tables[[update_counter()]])[input$table_output_columns_selected], sep = ',')})
                ")
    
    table_list$action_string[[update_counter() + 1]] <-
      stringr::str_c(table_list$action_string[[update_counter()]], " %>% \n", action_string_to_append)
    
    update_counter(update_counter() + 1)
    
  }) %>% 
    bindEvent(input$select_button)
  

### Observe: deselect_button ---------------------------------------------------------


  observe({
    
    req(input$file_upload)
    
    table_list$tables[[update_counter() + 1]] <- 
      table_list$tables %>% 
      purrr::pluck(update_counter()) %>%  
      select(- input$table_output_columns_selected)
    
    action_string_to_append <- 
      glue::glue("
                select(- c({glue::glue_collapse(colnames(table_list$tables[[update_counter()]])[input$table_output_columns_selected], sep = ',')} ) )
                ")
    
    table_list$action_string[[update_counter() + 1]] <-
      stringr::str_c(table_list$action_string[[update_counter()]], " %>% ", action_string_to_append)
    
    update_counter(update_counter() + 1)
    
  }) %>% 
    bindEvent(input$deselect_button)
  


### Observe: nest_button ----------------------------------------------------

  observe({
    
    req(input$file_upload)
    
    table_list$tables[[update_counter() + 1]] <- 
      table_list$tables %>% 
      purrr::pluck(update_counter()) %>%  
      tidyr::nest(!!input$nest_new_colname_text := eval(input$table_output_columns_selected))
    
    action_string_to_append <- 
      glue::glue("
                tidyr::nest('{input$nest_new_colname_text}' = c({glue::glue_collapse(colnames(table_list$tables[[update_counter()]])[input$table_output_columns_selected], sep = ',') }))
                ")
    
    table_list$action_string[[update_counter() + 1]] <-
      stringr::str_c(table_list$action_string[[update_counter()]], " %>% ", action_string_to_append)
    
    update_counter(update_counter() + 1)
    
  }) %>% 
    bindEvent(input$nest_button)
  
### Observe: unnest_wider_button --------------------------------------------------
  
  
  observe({
    
    req(input$file_upload)
    
    table_list$tables[[update_counter() + 1]] <- 
      table_list$tables %>% 
      purrr::pluck(update_counter()) %>% 
      tidyr::unnest_wider(input$table_output_columns_selected, names_sep = "_" )
    
    action_string_to_append <- 
      glue::glue("
                tidyr::unnest_wider({colnames(table_list$tables[[update_counter()]])[input$table_output_columns_selected]}, names_sep = '_')
                ")
    
    table_list$action_string[[update_counter() + 1]] <-
      stringr::str_c(table_list$action_string[[update_counter()]], " %>% ", action_string_to_append)
    
    update_counter(update_counter() + 1)
    
  }) %>% 
    bindEvent(input$unnest_wider_button)
  

### Observe: unnest_longer_button -------------------------------------------
  
  observe({
    
    req(input$file_upload)
    
    table_list$tables[[update_counter() + 1]] <- 
      table_list$tables %>% 
      purrr::pluck(update_counter()) %>% 
      tidyr::unnest_longer(input$table_output_columns_selected)
    
    action_string_to_append <- 
      glue::glue("
                tidyr::unnest_longer({colnames(table_list$tables[[update_counter()]])[input$table_output_columns_selected]})
                ")
    
    table_list$action_string[[update_counter() + 1]] <-
      stringr::str_c(table_list$action_string[[update_counter()]], " %>% ", action_string_to_append)
    
    update_counter(update_counter() + 1)
    
  }) %>% 
    bindEvent(input$unnest_longer_button)
  
### Observe: pivot_longer_button --------------------------------------------

  observe({
    
    req(input$file_upload)
    
    table_list$tables[[update_counter() + 1]] <- 
      table_list$tables %>% 
      purrr::pluck(update_counter()) %>%  
      tidyr::pivot_longer(cols = eval(input$table_output_columns_selected), 
                          names_to = input$pivot_longer_names_to_text, 
                          values_to = input$pivot_longer_values_to_text)
    
    action_string_to_append <- 
      glue::glue("
                tidyr::pivot_longer(cols = c({glue::glue_collapse(colnames(table_list$tables[[update_counter()]])[input$table_output_columns_selected], sep = ',')}), 
                          names_to = '{input$pivot_longer_names_to_text}', 
                          values_to = '{input$pivot_longer_values_to_text}')
                ")
    
    table_list$action_string[[update_counter() + 1]] <-
      stringr::str_c(table_list$action_string[[update_counter()]], " %>% ", action_string_to_append)
    
    update_counter(update_counter() + 1)
    
  }) %>% 
    bindEvent(input$pivot_longer_button)

### Observe: undo_button -------------------------------------------------------------


  observe({
    
    req(update_counter() > 1)
    
    update_counter(update_counter() - 1)
    
  }) %>% 
    bindEvent(input$undo_button)
  
  

## Box: Data Types ---------------------------------------------------------
### Observe: convert_character_button ---------------------------------------

  observe({
    
    req(input$file_upload)
    
    table_list$tables[[update_counter() + 1]] <- 
      table_list$tables %>% 
      purrr::pluck(update_counter()) %>% 
      mutate(across(.cols = input$table_output_columns_selected, .fns = as.character) )
    
    
    action_string_to_append <- 
      glue::glue("
                 mutate(across(.cols = c({glue::glue_collapse(colnames(table_list$tables[[update_counter()]])[input$table_output_columns_selected], sep = ',')}), .fns = as.character))
                ")
    
    table_list$action_string[[update_counter() + 1]] <-
      stringr::str_c(table_list$action_string[[update_counter()]], " %>% \n", action_string_to_append)
    
    update_counter(update_counter() + 1)
    
    
  }) %>% 
    bindEvent(input$convert_character_button)

  

### Observe: convert_numeric_button ------------------------------------------

  observe({
    
    req(input$file_upload)
    
   tryCatch({
     
     table_list$tables[[update_counter() + 1]] <- 
       table_list$tables %>% 
       purrr::pluck(update_counter()) %>% 
       mutate(across(.cols = input$table_output_columns_selected, .fns = as.numeric) )
     
     action_string_to_append <- 
       glue::glue("
                mutate(across(.cols = c({glue::glue_collapse(colnames(table_list$tables[[update_counter()]])[input$table_output_columns_selected], sep = ',')}), .fns = as.numeric ))
                ")
     
     table_list$action_string[[update_counter() + 1]] <-
       stringr::str_c(table_list$action_string[[update_counter()]], " %>% \n ", action_string_to_append)
     
     update_counter(update_counter() + 1)
     
     
     
   }, warning = function(w){
     
     showNotification(w, type = "warning", closeButton = T, duration = NULL )
     
   }, error = function(e){
     
     showNotification(e, type = "error", closeButton = T, duration = NULL)
     
   }) 
    
    
  }) %>% 
    bindEvent(input$convert_numeric_button )
  
  

### Observe: convert_factor_button ------------------------------------------

  observe({
    
    req(input$file_upload)
    
    tryCatch({
      
      table_list$tables[[update_counter() + 1]] <- 
        table_list$tables %>% 
        purrr::pluck(update_counter()) %>% 
        mutate(across(.cols = input$table_output_columns_selected, .fns = as.factor) )
      
      action_string_to_append <- 
        glue::glue("
                mutate(across(.cols = c({glue::glue_collapse(colnames(table_list$tables[[update_counter()]])[input$table_output_columns_selected], sep = ',')}), .fns = as.factor ))
                ")
      
      table_list$action_string[[update_counter() + 1]] <-
        stringr::str_c(table_list$action_string[[update_counter()]], " %>% \n ", action_string_to_append)
      
      update_counter(update_counter() + 1)
      
      
      
    }, warning = function(w){
      
      showNotification(w, type = "warning", closeButton = T, duration = NULL )
      
    }, error = function(e){
      
      showNotification(e, type = "error", closeButton = T, duration = NULL)
      
    }) 
    
    
  }) %>% 
    bindEvent(input$convert_factor_button )
  

# Observe: convert_logical_button -----------------------------------------

  observe({
    
    req(input$file_upload)
    
    tryCatch({
      
      table_list$tables[[update_counter() + 1]] <- 
        table_list$tables %>% 
        purrr::pluck(update_counter()) %>% 
        mutate(across(.cols = input$table_output_columns_selected, .fns = as.logical) )
      
      action_string_to_append <- 
        glue::glue("
                mutate(across(.cols = c({glue::glue_collapse(colnames(table_list$tables[[update_counter()]])[input$table_output_columns_selected], sep = ',')}), .fns = as.logical ))
                ")
      
      table_list$action_string[[update_counter() + 1]] <-
        stringr::str_c(table_list$action_string[[update_counter()]], " %>% \n ", action_string_to_append)
      
      update_counter(update_counter() + 1)
      
      
      
    }, warning = function(w){
      
      showNotification(w, type = "warning", closeButton = T, duration = NULL )
      
    }, error = function(e){
      
      showNotification(e, type = "error", closeButton = T, duration = NULL)
      
    }) 
    
    
  }) %>% 
    bindEvent(input$convert_logical_button )
  
## Box: Data Display--------------------------------------------------------
### Box-tab: Data --------------------------------------------------------
#### Reactive: input_file -------------------------------------------------------

  
  input_file <- reactive ({
    
    req(input$file_upload)
    
    file_extension <- tools::file_ext(input$file_upload$datapath)
    
   
    if (file_extension == "rds"){

      readRDS(input$file_upload$datapath)

    } else if (file_extension == "csv"){

      read.csv(input$file_upload$datapath)

    } else {

      stop("file extension invalid")

    }
    
     #readRDS(input$file_upload$datapath)
      #read.csv(input$file_upload$datapath)
    
  })
  

#### Observe: input_file -----------------------------------------------------


  observe({

    input_file_type <-
      input_file() %>%
      class()

    if (input_file_type == "list"){

      table_list$tables[[1]] <-
        input_file() %>%
        tibble::enframe() %>%
        tibble::column_to_rownames(var = "name") %>%
        t() %>%
        tibble::as_tibble()

      table_list$action_string[[1]] <-
        glue::glue("readRDS('{input$file_upload$name}') %>%
                 tibble::enframe() %>%
                 tibble::column_to_rownames(var = 'name') %>%
                 t() %>%
                 tibble::as_tibble()")

    } else if (input_file_type == "data.frame") {

      table_list$tables[[1]] <-
        input_file() %>%
        tibble::as_tibble()

      table_list$action_string[[1]] <-
        glue::glue("read.csv('{input$file_upload$name}') %>%
                 tibble::column_to_rownames(var = 'name') %>%
                 t() %>%
                 tibble::as_tibble()")

    } else {

      stop("data type not accepted")
    }


  }) %>%
    bindEvent(input$file_upload)


#### Output: table_output ----------------------------------------------------


  output$table_output <-
    renderDataTable({
      
      req(input$file_upload)
      
      table_list$tables[[update_counter()]] %>% 
        DT::datatable(selection = list(target = "column"), options = list(scrollX = T) )
      
      
    }
    )
  

### Box-tab: Data types -----------------------------------------------------
#### Output: data_types_output -----------------------------------------------
  
  output$data_types_output <-
    renderDataTable({
      
      req(input$file_upload)
      
      table_list$tables[[update_counter()]] %>% 
        purrr::map(class) %>% 
        enframe() %>% 
        DT::datatable(options = list(scrollX = T) )
      
      
    }
    )
  
  
## Box: Code Output ---------------------------------------------------------


  output$actions <- 
    renderText({
      
      req(input$file_upload)
      
      table_list$action_string[[update_counter()]]
      
    }) 


 



 

 
 
 
  

# Tab: Testing ------------------------------------------------------------
  
## Test: test_text_output ---------------------------------------------------------
  
  output$test_text_output <-
    renderText({
      
      req(input$file_upload)
      
      tools::file_ext(input$file_upload$datapath)
      
    })
  
  

# Test: test_datatable_output ---------------------------------------------

  output$test_datatable_output <-
    renderDataTable({
      
      req(input$file_upload)
      
      table_list$tables[[update_counter()]] %>% 
        lapply(class) %>% 
        enframe() %>% 
        unnest(cols = value) %>% 
        DT::datatable(options = list(scrollX = T) )
      
      
    })
  
  
})




