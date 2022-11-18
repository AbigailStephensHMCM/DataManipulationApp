# Dashboard Header ------------------------------------------------------------
dash_head <- dashboardHeader(title = "Data Manipulation App" )

# Dashboard Sidebar ------------------------------------------------------------
dash_sidebar <- 
  dashboardSidebar(collapsed = T,
    
    sidebarMenu(
      menuItem(tabName = "dimension_transform_tab", text = "Dimension Transformation"),
      menuItem(tabName = "data_type_tab", text = "Data Types"),
      menuItem(tabName = "testing_tab", text = "Testing")
    )
  
  
)








# Dashboard Body ------------------------------------------------------------



## Head: Page head ---------------------------------------------------------------
page_head <-
  shiny::tags$head(
  
    shiny::tags$link(rel = "stylesheet", href = "my_stylesheet.css"),
    
    includeScript(path = here::here("script.js") ),
  
  
  insert_html('.copy-box .box-header',
              actionButton(inputId = "copy_button", 
                           label = "Copy text", 
                           onclick = "copy_text_function()")
  ),
  add_class('#copy_button', 'pull-right'),
  
  insert_html('#undo_box .nav-tabs',
              actionButton(inputId = "undo_button", label = "Undo")),
  
  add_class('#undo_button', 'pull-right'),
  
  
  insert_html('#input-tools-box .box-header',
              shiny::tags$button(id = "input-tools-collapse",
                                 class = "btn",
                                 onClick = "collapse_box_function('input-tools-box')",
                                 shiny::tags$i(class = "fa fa-plus")) %>% 
                stringr::str_replace_all("[\r\n]", "")

  ),
  
  add_class('#input-tools-collapse', 'pull-right'),
  
  insert_html('#data-types-box .box-header',
              shiny::tags$button(id = "data-types-collapse",
                                 class = "btn",
                                 onClick = "collapse_box_function('data-types-box')",
                                 shiny::tags$i(class = "fa fa-plus")) %>% 
                stringr::str_replace_all("[\r\n]", "")
             ),
  
  add_class('#data-types-collapse', 'pull-right'),
  
  insert_html('#structure-tools-box .box-header',
                          shiny::tags$button(id = "structure-tools-collapse",
                                             class = "btn",
                                             onClick = "collapse_box_function('structure-tools-box')",
                                             shiny::tags$i(class = "fa fa-plus")) %>% 
                            stringr::str_replace_all("[\r\n]", "")
              ),
  
  add_class('#structure-tools-collapse', 'pull-right')


)

  


## Box: input_tools -----------------------------------------

box_input_tools <- 
  box( width = "100%", title = "Input Tools",
    
      
            shiny::tags$div(style = "display:inline-block",
              
              textInput(inputId = "nest_new_colname_text", 
                        label = "Enter a name for the new nest column",
                        value = "new_col",
                        placeholder = "new_col",
                        width = "100%"),
            ),
            
            shiny::tags$div(style = "display:inline-block",
              
              actionButton(inputId = "nest_button", label = "NEST")
              
            
          ),
      
         shiny::tags$div(style = "display:inline-block",
                         
                         textInput(inputId = "pivot_longer_names_to_text", 
                                   label = "Enter a name for the names column",
                                   value = "new_names",
                                   placeholder = "new_names",
                                   width = "100%"),
         ),
       
       shiny::tags$div(style = "display:inline-block",
                       
                       textInput(inputId = "pivot_longer_values_to_text", 
                                 label = "Enter a name for the values column",
                                 value = "new_values",
                                 placeholder = "new_values",
                                 width = "100%"),
       ),
         
         shiny::tags$div(style = "display:inline-block",
                         
                         actionButton(inputId = "pivot_longer_button", label = "PIVOT LONGER")
                         
                         
         )
       
  )

## Box: Structure tools ------------------------------------------------------------
box_structure_tools <-
  box(title = "Structure Tools", width = "100%",
    
    actionButton(inputId = "select_button", label = "SELECT", width = "25%"),
    
    actionButton(inputId = "unnest_longer_button", label = "UNNEST LONGER", width = "25%"),
    
    actionButton(inputId = "unnest_wider_button", label = "UNNEST WIDER", width = "25%"),
    
    actionButton(inputId = "deselect_button", label = "DE-SELECT", width = "24%")
    
)


## Box: Data types ---------------------------------------------------------

box_data_types <-
  box(width = "100%", title = "Data Types",
  
    actionButton(inputId = "convert_character_button", label = "Character", width = str_c(99/4, "%")),
    
    actionButton(inputId = "convert_numeric_button", label = "Numeric", width = str_c(99/4, "%")),
    
    actionButton(inputId = "convert_factor_button", label = "Factor", width = str_c(99/4, "%")),
    
    actionButton(inputId = "convert_logical_button", label = "Logical", width = str_c(99/4, "%"))
  
)

## Box: Data display ------------------------------------------------------------

box_data_display <-
  tabBox(width = "100%",
         tabPanel(title = "Data",
            fileInput(inputId = "file_upload", label = "Upload a file"),
            DT::dataTableOutput(outputId = "table_output") 
         ),
         tabPanel(title = "Types",
                  DT::dataTableOutput(outputId = "data_types_output")
                  )
)



## Box: Code output ---------------------------------------------------------

box_code_output <- 
  box(title = "Code Output", width = "100%",
      verbatimTextOutput(outputId = "actions")
      )

### Tab: Dimension ------------------------------------------------------------
dimensions_tab <-
  tabItem(tabName = "dimension_transform_tab",
          
    fluidPage(
      
    shiny::tags$div(class = "sticky-row",  
      fluidRow(
       
      shiny::tags$div(class = "tools-box", 
                      
        shiny::tags$div(id = "input-tools-box",
                        box_input_tools),
        shiny::tags$div(id = "data-types-box",
                        box_data_types),
        shiny::tags$div(id = "structure-tools-box",
                        box_structure_tools)
      )
      
    )
  ),
            
      
      fluidRow(
        shiny::tags$div(id = "undo_box",
          box_data_display
        )
               ),
      
      fluidRow(
        shiny::tags$div(class = "copy-box",
                        box_code_output,
                        )
        
               )
      
        )
  )






# Box: Test Text Output ---------------------------------------------------

test_text_box <-
  box(width = "100%",
      
      verbatimTextOutput(outputId = "test_text_output")
    
    
  )


# Box: Test Datatable Output ----------------------------------------------

test_datatable_box <-
  box(width = "100%",
    
    dataTableOutput(outputId = "test_datatable_output" )
    
  )

### Tab: Testing ------------------------------------------------------------

testing_tab <- 
  tabItem(tabName = "testing_tab",
    
    test_text_box,
    test_datatable_box
    
  )

#### Body: Dashboard compile ------------------------------------------------------------
dash_body <- 
  dashboardBody(
    
    page_head,
    
    tabItems(
      dimensions_tab,
      testing_tab
    )
 
)
  


##### Page: Dashboard Page ----------------------------------------------------------


dashboardPage(dash_head, dash_sidebar, dash_body)


