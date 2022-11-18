insert_html <- 
  function(insert_location, html_to_insert){
    
   
     shiny::tags$script(
      
       HTML(
      glue::glue(
        "document.addEventListener('DOMContentLoaded', function() {{ 
        var location_html = document.querySelector('{insert_location}').innerHTML; 
       document.querySelector('{insert_location}').innerHTML = 
       location_html.concat('{html_to_insert}'); 
       }}
       )"
      )
       )
      
    )
   
    
  }

add_class <- function(html_object, class_to_add){
  
  shiny::tags$script(
    
    HTML(
      glue::glue(
        "document.addEventListener('DOMContentLoaded', function() {{ 
       document.querySelector('{html_object}').classList.add('{class_to_add}')
       }}
       )"
      )
    )
    
  )
  
}

