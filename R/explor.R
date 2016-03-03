if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("Type", "Contrib.x", "Contrib.y", "Cos2.x", "Cos2.y", 
                           "Level", "Variable", "Coord.x", "Coord.y", "Name", 
                           "P.value", "Class", "Cor", "Cor.x", "Cor.y", "Coord", 
                           "starts_with", "Contrib", "Cos2", "varname", "modname", 
                           "V.test", "eta2", "con.tra", "name", "pos", "Axis"))


##' Interface for analysis results exploration
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive visualisation and exploration of an analysis results.
##'
##' @param obj object containing analysis results
##' @return
##' The function launches a shiny app in the system web browser.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @export
##' @examples
##' \dontrun{
##' 
##' require(FactoMineR)
##' 
##' ## FactoMineR::MCA exploration
##' data(hobbies)
##' mca <- MCA(hobbies[1:1000,c(1:8,21:23)], quali.sup = 9:10, 
##'            quanti.sup = 11, ind.sup = 1:100, graph = FALSE)
##' explor(mca)
##' 
##' ## FactoMineR::PCA exploration
##' data(decathlon)
##' d <- decathlon[,1:12]
##' pca <- PCA(d, quanti.sup = 11:12, graph = FALSE)
##' explor(pca)
##' }

explor <- function(obj) {
  UseMethod("explor")
}


##' @import shiny

explor_css <- function() {
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


explor_lasso_callback <- function() {
  paste("function(sel) {",
        "var selected = sel.data().map(function(d) {return d.key_var});",
        "var values = selected.join('<br />');",        
        "var r_code = 'c(\"' + selected.join('\", \"') + '\")';",
        "var out = '<h4>", gettext("Identifiers", domain = "R-explor"), "</h4>",
                  "<p><pre>'+values+'</pre></p>",
                  "<h4>",gettext("R vector", domain = "R-explor"), "</h4>",
                  "<p><pre>'+r_code+'</pre></p>';",
        "$('#lasso-mod-content').html(out);",
        "$('#lasso-modal').modal();",
        "}")
}
