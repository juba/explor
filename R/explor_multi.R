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


## Generate correct datatable order option from a column name
order_option <- function(table, name, order="desc") {
    index <- which(names(table) == name) - 1
    list(order = list(list(index, order)))
}


## INDIVIDUAL DATA SHINY MODULE

explor_multi_ind_dataUI <- function(id, has_sup_ind, axes) {
    ns <- NS(id)
    fluidRow(
        column(2,
               wellPanel(
                   selectInput(ns("inddim"), 
                               gettext("Dimension", domain = "R-explor"),
                               choices = axes, selected = "Axis 1"))),
        column(10,
               h4(gettext("Active individuals", domain = "R-explor")),
               DT::dataTableOutput(ns("indtable")),
               if (has_sup_ind) {
                   list(h4(gettext("Supplementary individuals", domain = "R-explor")),
                        DT::dataTableOutput(ns("indtablesup")))
               }
               ))
}


explor_multi_ind_data <- function(input, output, session, ind, settings) {

    tableOptions_ind <- list(lengthMenu = c(10,20,50,100), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = TRUE)

    ## Active individuals
    indTable <- reactive({
        ind() %>%
            filter(Type == "Active", Axis == input$inddim) %>%
            select_(.dots = settings()$ind_columns)
    })
    output$indtable <- DT::renderDataTable(
                               DT::datatable({indTable()},
                                             options = c(tableOptions_ind, order_option(indTable(), "Coord")), rownames = FALSE))

    ## Supplementary individuals
    indTableSup <- reactive({
        ind() %>%
            filter(Type == "Supplementary", Axis == input$inddim) %>%
            select_(.dots = settings()$indsup_columns)
    })
    output$indtablesup <- DT::renderDataTable(
                                  DT::datatable({indTableSup()},
                                                options = c(tableOptions_ind, order_option(indTableSup(), "Coord")), rownames = FALSE))
    
}

