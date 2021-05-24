if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("Type", "Contrib.x", "Contrib.y", "Cos2.x", "Cos2.y", 
                           "Level", "Variable", "Coord.x", "Coord.y", "Name", 
                           "P.value", "Class", "Cor", "Cor.x", "Cor.y", "Coord", 
                           "starts_with", "Contrib", "Cos2", "varname", "modname", 
                           "V.test", "eta2", "con.tra", "name", "pos", "Axis", "Count"))


##' @importFrom formatR tidy_source

code_modal <- function(obj, plot_code, zoom_code) {
  code <- paste0("res <- explor::prepare_results(", obj, ")\n")
  code <- paste0(code, plot_code)
  code <- paste0(code, zoom_code, ")")
  
  code <- formatR::tidy_source(text = code, 
    width.cutoff = 75, 
    output = FALSE)$text.tidy
  
  modalDialog(
    title = gettext("Export R code"),
    size = "l",
    HTML(paste0(explor_multi_export_code_message(),
      "<pre><code>",
      paste(highr::hi_html(code), collapse="\n"),
      "</code></pre>")),
    easyClose = TRUE)
}


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
  .well #var_sup_choice .shiny-options-group {
    max-height: 200px;
    margin-top: 5px;
    margin-left: 15px;
    overflow-y: scroll;
    overflow-x: hidden;
  }
  .well #var_sup_choice .checkbox {
    margin-top: 0px;
    margin-bottom: 0px;
  } 
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
  #varplot, #indplot, #biplot { height: 90vh !important}
  #eigplot { max-width: 850px; }
  .legend .label { font-weight: normal !important; font-size: 10px !important;}
  .navbar-nav>li>a { font-size: 13px; padding: 15px 10px;}
  #lasso-mod-content {
    max-height: 700px;
    overflow: auto;
    padding: 10px;
  }

  /* Syntax highlighting */
  span.hl.str { color: #d14;}
  span.hl.kwa { color: #099;}
  span.hl.num { color: #099;}
  span.hl.kwd { color: #333; font-weight: bold;}
  span.hl.com { color: #888; font-style: italic;}
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

explor_multi_zoom_callback <- function(type = "var") {
    paste0(
        "function(xmin, xmax, ymin, ymax) {
            Shiny.onInputChange('", type, "_zoom_range', {xmin:xmin, xmax:xmax, ymin:ymin, ymax:ymax});
         }")
}


explor_multi_sidebar_footer <- function(type = "var") {
    list(
        checkboxInput(paste0(type, "_transitions"), 
                      HTML(gettext("Animations")),
                      value = TRUE),
        if (type != "bi") {
            tags$p(actionButton(paste0("explor-", type, "-lasso-toggle"),
                          icon = icon("crosshairs"),
                          label = gettext("Lasso selection")))
        },
        tags$p(actionButton(paste0("explor_", type, "_plot_code"),
                          icon = icon("code"),
                          label = gettext("Get R code"))),
        tags$p(tags$a(id = paste0("explor-", type, "-svg-export"),
                          class = "btn btn-default",
                          HTML(paste(icon("file-image-o"), gettext("Export as SVG"))))))
               
}


## Generate correct datatable order option from a column name
order_option <- function(table, name, order="desc") {
    index <- which(names(table) == name) - 1
    list(order = list(list(index, order)))
}

## Generate a DataTable for numerical results
explor_multi_table <- function(tab, options, sort_column) {
    dt <- DT::datatable(tab,
                        options = c(options, order_option(tab, sort_column)),
                        rownames = FALSE)
    indices_3 <- which(names(tab) %in% c("Coord", "Cos2", "V.test", "eta2", "P.value"))
    indices_2 <- which(names(tab) %in% c("Contrib"))
    if (length(indices_3) > 0) {
      dt <- dt %>%
        DT::formatRound(indices_3, digits = 3)
    }
    if (length(indices_2) > 0) {
      dt <- dt %>%
        DT::formatRound(indices_2, digits = 2)
    }
    dt
}



## Generate the xlim and ylim from a zoom range for R code export
explor_multi_zoom_code <- function(zoom_range) {
    if (is.null(zoom_range)) return("")
    xmin <- signif(zoom_range$xmin, 3)
    xmax <- signif(zoom_range$xmax, 3)
    ymin <- signif(zoom_range$ymin, 3)
    ymax <- signif(zoom_range$ymax, 3)
    return(sprintf(",\n    xlim = c(%s, %s), ylim = c(%s, %s)", xmin, xmax, ymin, ymax))

}

## Message displyed in the R code export dialog
explor_multi_export_code_message <- function () {
    gettext("<p>Copy/paste the following code to reproduce the displayed plot. Note that custom label positions are not taken into account, use the <em>export label positions</em> menu entry to save  them and add the file content to the <tt>labels_positions</tt> argument.</p>")
}

## INDIVIDUAL DATA SHINY MODULE ---------------------------------------------------------

## UI for individual data panel
explor_multi_ind_dataUI <- function(id, settings, axes) {
    ns <- NS(id)
    fluidRow(
        column(2,
               wellPanel(
                   selectInput(ns("inddim"), 
                               gettext("Dimension"),
                               choices = axes, selected = "Axis 1"))),
        column(10,
               h4(gettext("Active individuals")),
               DT::DTOutput(ns("indtable")),
               if (settings$has_sup_ind) {
                   list(h4(gettext("Supplementary individuals")),
                        DT::DTOutput(ns("indtablesup")))
               }
               ))
}

## Server for individual data panel
explor_multi_ind_data <- function(input, output, session, res, settings) {

    table_options <- list(lengthMenu = c(10,20,50,100),
                          pageLength = 20, orderClasses = TRUE,
                          autoWidth = FALSE, searching = TRUE)

    ## Active individuals
    indTable <- reactive({
        res()$ind %>%
            filter(Type == "Active", Axis == input$inddim) %>%
            select(all_of(settings()$ind_columns))
    })
    output$indtable <- DT::renderDT(
                               explor_multi_table(indTable(), table_options, "Coord"))

    ## Supplementary individuals
    indTableSup <- reactive({
        res()$ind %>%
            filter(Type == "Supplementary", Axis == input$inddim) %>%
            select(all_of(settings()$indsup_columns))
    })
    output$indtablesup <- DT::renderDT(
                                  explor_multi_table(indTableSup(), table_options, "Coord"))
}


## INPUTS -----------------------------------------------------------------------------

## Axes inputs
explor_multi_axes_input <- function(res, type) {
    x_input <- selectInput(paste0(type, "_x"), 
                            gettext("X axis"), 
                            choices = res$axes, selected = "1")
    y_input <- selectInput(paste0(type, "_y"), 
                            gettext("Y axis"), 
                            choices = res$axes, selected = "2")
    return(list(x_input, y_input))
}

## Variable size input for MCA and CA
explor_multi_var_size_input <- function(settings) {
    var_size_choices <- "None"
    names <- gettext("None")
    if (settings$has_contrib) {
        var_size_choices <- append(var_size_choices, "Contrib")
        names <- append(names, gettext("Contribution"))
    }
    if (settings$has_cos2) {
        var_size_choices <- append(var_size_choices, "Cos2")
        names <- append(names, gettext("Squared cosinus"))
    }
    if (settings$has_count) {
        var_size_choices <- append(var_size_choices, "Count")
        names <- append(names, gettext("Count"))
    }
    names(var_size_choices) <- names
    var_size_input <- if (length(var_size_choices) > 1) {
        selectInput("var_size", 
                    gettext("Points size :"),
                    choices = var_size_choices,
                    selected = "None")
                      } else NULL
    return(var_size_input)
}

## Variable color input
explor_multi_var_col_input <- function(settings) {
    if (settings$type == "PCA" && !settings$has_sup_vars) return(NULL)
    if (settings$type == "PCA" && settings$has_quali_sup_vars) {
        choices <- c("None", "Type", "Variable")
        selected <- "Type"
    }
    if (settings$type == "PCA" && !settings$has_quali_sup_vars) {
        choices <- c("None", "Type")
        selected <- "Type"
    }
    if (settings$type == "MCA" && settings$has_sup_vars) {
        choices <- c("None", "Variable", "Type")
        selected <- "Variable"
    }
    if (settings$type == "MCA" && !settings$has_sup_vars) {
        choices <- c("None", "Variable")
        selected <- "Variable"
    }
    if (settings$type == "CA" && (settings$has_sup_vars || settings$has_sup_levels)) {
        choices <- c("None", "Position", "Type")
        selected <- "Position"
    }
    if (settings$type == "CA" && !(settings$has_sup_vars || settings$has_sup_levels)) {
        choices <- c("None", "Position")
        selected <- "Position"
    }
    names(choices)[choices == "None"] <- gettext("None")
    names(choices)[choices == "Variable"] <- gettext("Variable name")
    names(choices)[choices == "Type"] <- gettext("Variable type")
    names(choices)[choices == "Position"] <- gettext("Variable position")
    
    selectInput("var_col", gettext("Points color :"),
                choices = choices,  selected = selected)
}

## Variable symbol input
explor_multi_var_symbol_input <- function(settings) {
    if (settings$type == "MCA" && settings$has_sup_vars) {
        choices <- c("None", "Variable", "Type")
        selected <- "Type"
    }
    if (settings$type == "MCA" && !settings$has_sup_vars) {
        choices <- c("None", "Variable")
        selected <- "None"
    }
    if (settings$type == "CA" && (settings$has_sup_vars || settings$has_sup_levels)) {
        choices <- c("None", "Position", "Type")
        selected <- "Type"
    }
    if (settings$type == "CA" && !(settings$has_sup_vars || settings$has_sup_levels)) {
        choices <- c("None", "Position")
        selected <- "None"
    }
    names(choices)[choices == "None"] <- gettext("None")
    names(choices)[choices == "Variable"] <- gettext("Variable name")
    names(choices)[choices == "Type"] <- gettext("Variable type")
    names(choices)[choices == "Position"] <- gettext("Variable position")

    selectInput("var_symbol", gettext("Points symbol :"),
                choices = choices, selected = selected)   
}

## Individual color input
explor_multi_ind_col_input <- function(settings, res) {
    ind_col_choices <- c("None", "Type")
    names(ind_col_choices) <- c(gettext("None"),
                                gettext("Individual type"))
    ind_col_choices <- c(ind_col_choices, names(res$quali_data))
    ind_col_choices <- setdiff(ind_col_choices, "Name")

    selectInput("ind_col", 
                gettext("Points color :"),
                choices = ind_col_choices,
                selected = "None")
}

## Individuals opacity input
explor_multi_ind_opacity_input <- function(settings) {
  ind_opacity_choices <- "Fixed"
  names <- gettext("Fixed")
  if (settings$has_contrib) {
    ind_opacity_choices <- append(ind_opacity_choices, "Contrib")
    names <- append(names, gettext("Contribution"))
  }
  if (settings$has_cos2) {
    ind_opacity_choices <- append(ind_opacity_choices, "Cos2")
    names <- append(names, gettext("Squared cosinus"))
  }
  names(ind_opacity_choices) <- names
  ind_opacity_input <- if (length(ind_opacity_choices) > 1) {
    selectInput("ind_opacity_var", 
                gettext("Points opacity :"),
                choices = ind_opacity_choices,
                selected = "Fixed")
  } else NULL
  return(ind_opacity_input)
}

## Auto labels input
explor_multi_auto_labels_input <- function(data, type) {
    if (sum(data$Axis == 1) < 200) {
        checkboxInput(paste0(type, "_auto_labels"),
                      gettext("Automatic labels position"),
                      value = FALSE)
    }
}


## Supplementary variables choice input
explor_multi_var_sup_choice_input <-function(data, settings) {
  if (settings$type == "CA") {
    vnames <- data %>% 
      filter(Type == "Supplementary variable") %>% 
      select(.data$Level) %>% 
      distinct() %>% 
      pull(.data$Level)
  } else {
    vnames <- data %>% 
      filter(Type == "Supplementary") %>% 
      select(.data$Variable) %>% 
      distinct() %>% 
      pull(.data$Variable)
  }
  checkboxGroupInput(
    "var_sup_choice",
    gettext("Supplementary variables to display"),
    choices = vnames,
    selected = vnames
  )
}


## Min contrib to show labels input
explor_multi_min_contrib_input <- function(data, settings, type) {
    if (!settings$has_contrib) return(NULL)
    cmax <- if (type == "bi") NA else ceiling(2 * max(data$Contrib, na.rm = TRUE))
    numericInput(
        paste0(type, "_lab_min_contrib"),
        gettext("Minimum contribution to show label"),
        min = 0,
        max = cmax,
        value = 0)
}

## Biplot symbol input
explor_multi_bi_symbol_input <- function(settings) {
    if (settings$has_sup_vars || settings$has_sup_ind) {
        choices <- c("None", "Variable", "Type", "Nature")
        selected <- "Nature"
    }
    if (!(settings$has_sup_vars || settings$has_sup_ind)) {
        choices <- c("None", "Variable", "Nature")
        selected <- "Nature"
    }
    names(choices)[choices == "None"] <- gettext("None")
    names(choices)[choices == "Variable"] <- gettext("Variable name")
    names(choices)[choices == "Type"] <- gettext("Active / Supplementary")
    names(choices)[choices == "Nature"] <- gettext("Variable level / Individual")

    selectInput("bi_symbol", gettext("Points symbol :"),
                choices = choices, selected = selected)   
}

## Biplot color input
explor_multi_bi_col_input <- function(settings) {
    if (settings$has_sup_vars || settings$has_sup_ind) {
        choices <- c("None", "Variable", "Type", "Nature")
        selected <- "Variable"
    }
    if (!(settings$has_sup_vars || settings$has_sup_ind)) {
        choices <- c("None", "Variable", "Nature")
        selected <- "Variable"
    }
    names(choices)[choices == "None"] <- gettext("None")
    names(choices)[choices == "Variable"] <- gettext("Variable name")
    names(choices)[choices == "Type"] <- gettext("Active / Supplementary")
    names(choices)[choices == "Nature"] <- gettext("Variable level / Individual")
    
    selectInput("bi_col", gettext("Points color :"),
                choices = choices,  selected = selected)
}

## Biplot individuals opacity input
explor_multi_bi_ind_opacity_input <- function(settings) {
  bi_opacity_choices <- "Fixed"
  names <- gettext("Fixed")
  if (settings$has_contrib) {
    bi_opacity_choices <- append(bi_opacity_choices, "Contrib")
    names <- append(names, gettext("Contribution"))
  }
  if (settings$has_cos2) {
    bi_opacity_choices <- append(bi_opacity_choices, "Cos2")
    names <- append(names, gettext("Squared cosinus"))
  }
  names(bi_opacity_choices) <- names
  bi_opacity_input <- if (length(bi_opacity_choices) > 1) {
    selectInput("bi_opacity_var", 
                gettext("Points opacity :"),
                choices = bi_opacity_choices,
                selected = "Fixed")
  } else NULL
  return(bi_opacity_input)
}


## VARIABLE DATA SHINY MODULE ---------------------------------------------------------

## Hide input choices for CA results
explor_multi_hide_choices <- function() {
    choices <- c("None", "Row", "Column")
    names(choices) <- c(gettext("None"),
                        gettext("Rows"),
                        gettext("Columns"))
    choices
}


## UI for variable data panel
explor_multi_var_dataUI <- function(id, settings, axes) {
    ns <- NS(id)
    fluidRow(
        column(2,
               wellPanel(
                   selectInput(ns("vardim"), 
                               gettext("Dimension"),
                               choices = axes, selected = "1"),
                   if (settings$type == "CA") {
                       selectInput(ns("var_tab_hide"), 
                                   gettext("Hide :"),
                                   choices = explor_multi_hide_choices(),
                                   selected = "None")
                   }
               )),
        column(10,
               h4(if(settings$type == "CA") gettext("Active levels")                   
                  else gettext("Active variables")),
               DT::DTOutput(ns("vartable")),
               if (settings$has_sup_vars || (settings$type == "CA" && settings$has_sup_levels)) {
                   list(h4(
                     if(settings$type == "CA") {
                       gettext("Supplementary elements")
                     } else {
                       gettext("Supplementary variables")
                     }),
                     DT::DTOutput(ns("vartablesup")))
               },
               if (settings$has_var_eta2) {
                   list(h4(withMathJax(gettext("Variables \\(\\eta^2\\)"))),
                        DT::DTOutput(ns("vartableeta2")))
               },
               if (settings$has_sup_vars && settings$has_varsup_eta2) {
                   list(h4(gettext("Supplementary variables \\(\\eta^2\\)")),
                        DT::DTOutput(ns("vartablesupeta2")))
               },
               if (settings$type == "PCA" && settings$has_quali_sup_vars) {
                   list(h4(gettext("Qualitative supplementary variables")),
                        DT::DTOutput(ns("vartablequalisup")))
               }
               )
    )
}


## Server for variable data panel
explor_multi_var_data <- function(input, output, session, res, settings) {

    table_options <- list(lengthMenu = c(10,20,50,100),
                          pageLength = 10, orderClasses = TRUE,
                          autoWidth = FALSE, searching = TRUE)
    ## Active variables
    varTable <- reactive({
        tmp <- res()$vars %>% 
                   filter(Type == "Active", Axis == input$vardim) %>%
                   select(all_of(settings()$var_columns))
        ## CA data hide option
        if (settings()$type == "CA" && input$var_tab_hide != "None") {
            tmp <- tmp %>% filter(Position != input$var_tab_hide)
        }
        data.frame(tmp)
    })
    vartable_sort <- reactive({
        if(settings()$has_contrib) "Contrib" else "Coord"
    })
    output$vartable <- DT::renderDT(
                               explor_multi_table(varTable(), table_options, vartable_sort()))

    ## Supplementary variables
    varTableSup <- reactive({
        tmp <- res()$vars %>% 
                   filter(grepl("Supplementary", Type), Axis == input$vardim) %>%
                   mutate(Level = ifelse(Class == "Quantitative", "-", Level))
        ## CA data hide option
        if (settings()$type == "CA" && input$var_tab_hide != "None") {
            tmp <- tmp %>% filter(Position != input$var_tab_hide)
        }
        ## PCA with qualitative supplementary
        if (settings()$type == "PCA" && settings()$has_quali_sup_vars) {
            tmp <- tmp %>% filter(Class == "Quantitative")
        }
        tmp <- tmp %>% select(all_of(settings()$varsup_columns))
        data.frame(tmp)
    })
    
    output$vartablesup <- DT::renderDT(
                                  explor_multi_table(varTableSup(), table_options, "Coord"))

    ## PCA qualitative supplementary variable
    varTableQualiSup <- reactive({
        if (settings()$type == "PCA" && settings()$has_quali_sup_vars) {
            tmp <- res()$vars %>% 
                       filter(Type == "Supplementary", Class == "Qualitative",
                              Axis == input$vardim) %>%
                       select(all_of(settings()$varsup_quali_columns))
            data.frame(tmp)
        }
    })
    output$vartablequalisup <- DT::renderDT(
                                       explor_multi_table(varTableQualiSup(), table_options, "Coord"))

    ## Variables eta2
    varTableEta2 <- reactive({
        if (settings()$has_var_eta2) {
            res()$vareta2 %>% filter(Type == "Active", Axis == input$vardim) %>%
                    select(all_of(settings()$vareta2_columns)) %>%
                    arrange(eta2) %>%
                    mutate(eta2 = format(eta2, scientific = FALSE, nsmall = 3, digits = 1))
        }
    })
    output$vartableeta2 <- DT::renderDT(
                                   explor_multi_table(varTableEta2(), table_options, "eta2"))

    ## Supplementary variables eta2
    varTableSupEta2 <- reactive({
        if (settings()$has_varsup_eta2) {
            res()$vareta2 %>% filter(Type == "Supplementary",
                                     Class == "Qualitative",
                                     Axis == input$vardim) %>%
                    select(all_of(settings()$varsupeta2_columns)) %>%
                    arrange(eta2) %>%
                    mutate(eta2 = format(eta2, scientific = FALSE, nsmall = 3, digits = 1))
        }
    })
    output$vartablesupeta2 <- DT::renderDT(
                                  explor_multi_table(varTableSupEta2(), table_options, "eta2"))

}


## EIGENPLOT MODULE ------------------------------------------------------------------


explor_multi_eigenUI <- function(id, eig) {
    ns <- NS(id)
    fluidRow(
        column(2,
               wellPanel(numericInput(ns("eig_nb"), 
                                      gettext("Dimensions to plot"), 
                                      min = 2, max = max(eig$dim), value = max(eig$dim), 
                                      step = 1))),
        column(5,
               h4(gettext("Eigenvalues histogram")),
               plotOutput(ns("eigplot"), height = "500px")),
        column(3, offset = 1,
               h4(gettext("Eigenvalues table")),
               DT::DTOutput(ns("eigtab"))))
}


explor_multi_eigen <- function(input, output, session, eig) {
    nb <- reactive({ifelse(is.null(input$eig_nb), nrow(eig()), input$eig_nb)})
    ## Histogram
    output$eigplot <- renderPlot({
        tmp <- eig()[1:nb(),]
        tmp$dim <- factor(tmp$dim)
        ggplot(data = tmp) +
          geom_bar(aes_string(x = "dim", y = "percent"), stat = "identity") +
          scale_x_discrete(gettext("Axis")) +
          scale_y_continuous(gettext("Percentage of inertia"))
    })
    ## Table
    output$eigtab <- DT::renderDT({
        tmp <- eig()[1:nb(),]
        tmp$cumpercent <- cumsum(tmp$percent)
        names(tmp) <- c(gettext("Axis"), "%", "Cum. %")
        dt <- DT::datatable(tmp, rownames = FALSE,
                            options = list(dom = 't', pageLength = nb()))
        dt %>% DT::formatRound(c("%", "Cum. %"), digits = 1)
    })
}


