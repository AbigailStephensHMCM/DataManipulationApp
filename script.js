function copy_text_function() {
      var text = document.querySelector('#actions').textContent;
      navigator.clipboard.writeText(text);
        }
        

function collapse_box_function(selected_box){ 
  if (document.getElementById(selected_box).querySelector(".box-body").style.display == "none")
  {document.querySelectorAll('.tools-box .box-body:not([style = "display: none;"])').forEach(function(item){ item.style.display = 'none'});
    
    document.getElementById(selected_box).querySelector(".box-body").style.display = "block";
    
  } else 
  {document.getElementById(selected_box).querySelector(".box-body").style.display = "none"}
  
}
