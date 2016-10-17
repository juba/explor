if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("Type", "Contrib.x", "Contrib.y", "Cos2.x", "Cos2.y", 
                           "Level", "Variable", "Coord.x", "Coord.y", "Name", 
                           "P.value", "Class", "Cor", "Cor.x", "Cor.y", "Coord", 
                           "starts_with", "Contrib", "Cos2", "varname", "modname", 
                           "V.test", "eta2", "con.tra", "name", "pos", "Axis"))

##' @import shiny

explor_multi_css <- function() {
  shiny::HTML("
  .well label, 
  .well input, 
  .well select, 
  .well option,
  .well button,
  .well a,
  div.option,
  input, label, select, option, .selectize-input {
      font-size: 11px !important;
      height: auto !important;
  }
  .well .checkbox { margin-left: 5px !important; }
  .well {padding: 5px !important;}
  .well .btn { padding: 6px 10px; }
  .dataTable th, 
  .dataTable td {
      font-size: 11px !important;
      padding: 3px 5px !important; 
  }
  .dataTable th { padding-right: 18px !important }
  .dataTables_wrapper {
    max-width: 850px;
    margin-bottom: 2em;
  }
  .dataTables_info, .dataTables_length, 
  .dataTables_filter, .dataTables_paginate {
      font-size: 11px !important;
  }
  #varplot, #indplot { height: 90vh !important}
  #eigplot { max-width: 850px; }
  .legend .label { font-weight: normal !important; font-size: 10px !important;}
  .navbar-nav>li>a { font-size: 13px; padding: 15px 10px;}
  #lasso-mod-content {
    max-height: 700px;
    overflow: auto;
    padding: 10px;
  }
  ")
}


explor_multi_lasso_callback <- function() {
    "function(sel) {
        var selected = sel.data().map(function(d) {return d.key_var});
        var values = selected.join('<br />');       
        var r_code = 'c(\"' + selected.join('\", \"') + '\")';
        var out = '<h4>IDs</h4><p><pre>'+values+'</pre></p>';
        out += '<h4>R vector</h4>';
        out += '<p><pre>'+r_code+'</pre></p>';
        Shiny.onInputChange('show_lasso_modal', out);
     }"
}
