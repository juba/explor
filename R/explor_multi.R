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

explor_multi_table <- function(tab, options, sort_column) {
    DT::renderDataTable(
            DT::datatable(tab,
                          options = c(options, order_option(tab, sort_column)),
                          rownames = FALSE))
}


## INDIVIDUAL DATA SHINY MODULE ---------------------------------------------------------

## UI for individual data panel
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

## Server for individual data panel
explor_multi_ind_data <- function(input, output, session, res, settings) {

    table_options <- list(lengthMenu = c(10,20,50,100),
                          pageLength = 10, orderClasses = TRUE,
                          autoWidth = TRUE, searching = TRUE)

    ## Active individuals
    indTable <- reactive({
        res()$ind %>%
            filter(Type == "Active", Axis == input$inddim) %>%
            select_(.dots = settings()$ind_columns)
    })
    output$indtable <- explor_multi_table(indTable(), table_options, "Coord")

    ## Supplementary individuals
    indTableSup <- reactive({
        res()$ind %>%
            filter(Type == "Supplementary", Axis == input$inddim) %>%
            select_(.dots = settings()$indsup_columns)
    })
    output$indtablesup <- explor_multi_table(indTableSup(), table_options, "Coord")
}


## VARIABLE DATA SHINY MODULE ---------------------------------------------------------

## UI for variable data panel
explor_multi_var_dataUI <- function(id, has_sup_var, axes, has_eta2) {
    ns <- NS(id)
    fluidRow(
        column(2,
               wellPanel(
                   selectInput(ns("vardim"), 
                               gettext("Dimension", domain = "R-explor"),
                               choices = axes, selected = "1")
               )),
        column(10,
               h4(gettext("Active variables", domain = "R-explor")),                   
               DT::dataTableOutput(ns("vartable")),
               if (has_sup_var) {
                   list(h4(gettext("Supplementary variables", domain = "R-explor")),
                        DT::dataTableOutput("vartablesup"))
               },
               if (has_eta2) {
                   h4(withMathJax(gettext("Variables \\(\\eta^2\\)", domain = "R-explor"))),
                   DT::dataTableOutput(ns("vartableeta2")),
                   if (has_sup_var) {
                       list(h4(gettext("Supplementary variables \\(\\eta^2\\)", domain = "R-explor")),
                            DT::dataTableOutput("vartablesupeta2"))
                   }
               }
               ))
}


## Server for variable data panel
explor_multi_var_data <- function(input, output, session, res, settings, has_eta2) {

    table_options <- list(lengthMenu = c(10,20,50,100),
                          pageLength = 10, orderClasses = TRUE,
                          autoWidth = TRUE, searching = TRUE)
    ## Active variables
    varTable <- reactive({
        res()$var %>% 
            filter(Type == "Active", Axis == input$vardim) %>%
            select_(.dots = settings()$var_columns)
    })
    output$vartable <- explor_multi_table(varTable(), table_options, "Contrib")
      
    ## Supplementary variables
    varTableSup <- reactive({
        res()$var %>% 
            filter(Type == "Supplementary", Axis == input$vardim) %>%
            mutate(Level = ifelse(Class == "Quantitative", "-", Level)) %>%
            select_(.dots = settings()$varsup_columns)
    })
    output$vartablesup <- explor_multi_table(varTableSup(), table_options, "Coord")

    if (has_eta2) {
                  
        ## Variables eta2
        varTableEta2 <- reactive({
            res()$vareta2 %>% filter(Type == "Active", Axis == input$vardim) %>%
                    select_(.dots = settings()$vareta2_columns) %>%
                    arrange(eta2)
        })
        output$vartableeta2 <- explor_multi_table(varTableEta2(), table_options, "eta2")

        ## Supplementary variables eta2
        varTableSupEta2 <- reactive({
            res()$vareta2 %>% filter(Type == "Supplementary",
                                     Class == "Qualitative",
                                     Axis == input$vardim) %>%
                    select_(.dots = settings()$varsupeta2_columns) %>%
                    arrange(eta2)
        })
        output$vartablesupeta2 <- explor_multi_table(varTableSupEta2(), table_options, "eta2")
    }

}
