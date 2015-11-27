##' @rdname explor
##' @aliases explor.PCA
##' @import shiny
##' @import dplyr
##' @import scatterD3
##' @import ggplot2
##' @export

explor.PCA <- function(obj) {
  
  if (!inherits(obj, "PCA")) stop("obj must be of class PCA")
  
  ## results preparation
  res <- prepare_results(obj)
  
  ## Precompute inputs 
  has_sup_vars <- "Supplementary" %in% res$vars$Type 
  if (has_sup_vars) {
      choices <- c("None", "Type")
      names(choices) <- c(gettext("None", domain = "R-explor"),
                          gettext("Variable type", domain = "R-explor"))
  } else {
    choices <- c("None", "Variable")
    names(choices) <- c(gettext("None", domain = "R-explor"))
  }
  ## Variable color input
  var_col_input <- selectInput("var_col", gettext("Points color :", domain = "R-explor"),
                               choices = choices,  selected = "Type")

  ## Individual color input
  ind_col_choices <- c("None", "Type")
  names(ind_col_choices) <- c(gettext("None", domain = "R-explor"),
                              gettext("Individual type", domain = "R-explor"))
  ind_col_input <- selectInput("ind_col", 
                               gettext("Points color :", domain = "R-explor"),
                               choices = ind_col_choices,
                               selected = "Type")
  
  
  
  has_sup_ind <- "Supplementary" %in% res$ind$Type

  ## Custom CSS
  css_string <- "
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
  "
  
  shiny::shinyApp(
    ui = navbarPage(gettext("PCA", domain = "R-explor"),
                  header = tags$head(
                  tags$style(HTML(css_string))),
                  tabPanel(gettext("Eigenvalues", domain = "R-explor"),
                           fluidRow(
                             column(2,
                                    wellPanel(numericInput("eig_nb", 
                                                           gettext("Dimensions to plot", domain = "R-explor"), 
                                                 min = 2, max = max(res$eig$dim), value = max(res$eig$dim), 
                                                 step = 1))),
                            column(10,
                                    plotOutput("eigplot", height = "600px"))
                             
                             )),
                  
                  tabPanel(gettext("Variables plot", domain = "R-explor"),
                           fluidRow(
                             column(2,
                                    wellPanel(
                                      selectInput("var_x", 
                                                  gettext("X axis", domain = "R-explor"), 
                                                  choices = res$axes, selected = "1"),
                                      selectInput("var_y", 
                                                  gettext("Y axis", domain = "R-explor"), 
                                                  choices = res$axes, selected = "2"),
                                      sliderInput("var_lab_size", 
                                                  gettext("Labels size", domain = "R-explor"),
                                                  4, 20, 10),
                                      var_col_input,
                                      if(has_sup_vars)
                                        checkboxInput("var_sup", 
                                                      HTML(gettext("Supplementary variables", domain = "R-explor")), 
                                                      value = TRUE),
                                      checkboxInput("var_transitions", 
                                                    HTML(gettext("Animations", domain = "R-explor")),
                                                    value = TRUE),
                                      tags$p(actionButton("imca-var-reset-zoom", 
                                                          title = gettext("Reset zoom", domain = "R-explor"),
                                                          HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span>")),
                                             tags$a(id = "imca-var-svg-export", href = "#",
                                                    class = "btn btn-default", 
                                                    title = gettext("Export as SVG", domain = "R-explor"),
                                                    HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span>"))))),
                             column(10,
                                    scatterD3Output("varplot", height = "auto"))
                  )),
                  
                  tabPanel(gettext("Variables data", domain = "R-explor"),
                           fluidRow(
                             column(2,
                                    wellPanel(
                                    selectInput("vardim", 
                                                gettext("Dimension", domain = "R-explor"),
                                                choices = res$axes, selected = "1"))),
                             column(10,
                                    h4(gettext("Active variables", domain = "R-explor")),                   
                                    DT::dataTableOutput("vartable"),
                                     if (has_sup_vars) {
                                       list(h4(gettext("Supplementary variables", domain = "R-explor")),
                                            DT::dataTableOutput("vartablesup"))
                                     }
                             ))),
                  
                  tabPanel(gettext("Individuals plot", domain = "R-explor"),
                           fluidRow(
                             column(2,
                                    wellPanel(
                                      selectInput("ind_x", 
                                                  gettext("X axis", domain = "R-explor"),
                                                  choices = res$axes, selected = "1"),
                                      selectInput("ind_y", 
                                                  gettext("Y axis", domain = "R-explor"),
                                                  choices = res$axes, selected = "2"),
                                      sliderInput("ind_point_size", 
                                                  gettext("Points size", domain = "R-explor"),
                                                  8, 128, 64),
                                      sliderInput("ind_opacity", 
                                                  gettext("Points opacity", domain = "R-explor"),
                                                  0, 1, 0.5),
                                      checkboxInput("ind_labels_show", 
                                                    HTML(gettext("Show labels", domain = "R-explor")),
                                                    value = FALSE),
                                      conditionalPanel(
                                        condition = 'input.ind_labels_show == true',
                                        sliderInput("ind_labels_size", 
                                                    gettext("Labels size", domain = "R-explor"),
                                                            5, 20, 9)
                                      ),
                                      if (has_sup_ind) ind_col_input,
                                      if (has_sup_ind)
                                        checkboxInput("ind_sup", 
                                                      HTML(gettext("Supplementary individuals", domain = "R-explor")),
                                                      value = TRUE),
                                      checkboxInput("ind_transitions", 
                                                    HTML(gettext("Animations", domain = "R-explor")),
                                                    value = TRUE),          
                                      tags$p(actionButton("imca-ind-reset-zoom", 
                                                          title = gettext("Reset zoom", domain = "R-explor"),
                                                          HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span>")),
                                             tags$a(id = "imca-ind-svg-export", href = "#",
                                                    class = "btn btn-default", 
                                                    title = gettext("Export as SVG", domain = "R-explor"),
                                                    HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span>"))))),                                 column(10,
                                    scatterD3Output("indplot")))),
                  
                  tabPanel(gettext("Individuals data", domain = "R-explor"),
                           fluidRow(
                             column(2,
                                    wellPanel(
                                    selectInput("inddim", 
                                                gettext("Dimension", domain = "R-explor"),
                                                choices = res$axes, selected = "Axis 1"))),
                             column(10,
                                    h4(gettext("Active individuals", domain = "R-explor")),
                                    DT::dataTableOutput("indtable"),
                                    if (has_sup_ind) {
                                      list(h4(gettext("Supplementary individuals", domain = "R-explor")),
                                      DT::dataTableOutput("indtablesup"))
                                    }
                             )))
                  
    ),
    
    server = function(input, output) {
      
      output$eigplot <- renderPlot({
        tmp <- res$eig[1:input$eig_nb,]
        tmp$dim <- factor(tmp$dim)
        ggplot(data = tmp) +
          geom_bar(aes_string(x = "dim", y = "percent"), stat = "identity") +
          scale_x_discrete(gettext("Axis", domain = "R-explor")) +
          scale_y_discrete(gettext("Percentage of inertia", domain = "R-explor"))
      })

      ## Variables plot reactive data
      var_data <- reactive({
        tmp_x <- res$vars %>% 
          filter(Axis == input$var_x) %>%
          select_("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2", "Cor")
        if (is.null(input$var_sup) || !input$var_sup)
          tmp_x <- tmp_x %>% filter(Type == 'Active')
        tmp_y <- res$vars %>% 
          filter(Axis == input$var_y) %>%
          select_("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2", "Cor")
        if (is.null(input$var_sup) || !input$var_sup)
          tmp_y <- tmp_y %>% filter(Type == 'Active')
        tmp <- tmp_x %>%
          left_join(tmp_y, by = c("Variable", "Level", "Type", "Class")) %>%
          mutate(Contrib = Contrib.x + Contrib.y,
                 Cos2 = Cos2.x + Cos2.y,
                 Cor = Cor.x + Cor.y,
                 tooltip = paste(paste0("<strong>", Level, "</strong>"),
                                 paste0("<strong>",
                                        gettext("Variable", domain = "R-explor"),
                                        ":</strong> ", Variable),
                                  paste0("<strong>x:</strong> ", Coord.x),
                                  paste0("<strong>y:</strong> ", Coord.y),
                                  paste0("<strong>",
                                         gettext("Cos2", domain = "R-explor"),
                                         ":</strong> ", Cos2),
                                  paste0("<strong>",
                                         gettext("Contribution:", domain = "R-explor"),
                                         "</strong> ", Contrib),
                                 paste0("<strong>",
                                        gettext("Correlation:", domain = "R-explor"),
                                        "</strong> ", Cor),
                                  sep = "<br />"))
        data.frame(tmp)
      })
      
      ## Variables plot
      output$varplot <- scatterD3::renderScatterD3({
        col_var <- if (input$var_col == "None") NULL else var_data()[, input$var_col]
        type_var <- ifelse(var_data()[,"Class"] == "Quantitative", "arrow", "point")
        scatterD3::scatterD3(
          x = var_data()[, "Coord.x"],
          y = var_data()[, "Coord.y"],
          xlab = names(res$axes)[res$axes == input$var_x],
          ylab = names(res$axes)[res$axes == input$var_y],
          lab = var_data()[, "Variable"],
          labels_size = input$var_lab_size,
          point_opacity = 1,
          col_var = col_var,
          col_lab = input$var_col,
          tooltip_text = var_data()[, "tooltip"],
          type_var = type_var,
          unit_circle = obj$call$scale.unit,
          xlim = if (obj$call$scale.unit) c(-1.1, 1.1) else NULL,
          ylim = if (obj$call$scale.unit) c(-1.1, 1.1) else NULL,          
          key_var = var_data()[, "Variable"],
          fixed = TRUE,
          transitions = input$var_transitions,
          html_id = "imca_var",
          dom_id_reset_zoom = "imca-var-reset-zoom",
          dom_id_svg_export = "imca-var-svg-export"
        )
      })
      

      
      ## Individuals plot reactive data
      ind_data <- reactive({
        tmp_x <- res$ind %>% 
          filter(Axis == input$ind_x) %>%
          select(Name, Type, Coord, Contrib, Cos2)
        if (is.null(input$ind_sup) || !input$ind_sup)
          tmp_x <- tmp_x %>% filter(Type == "Active")
        tmp_y <- res$ind %>% 
          filter(Axis == input$ind_y) %>%
          select(Name, Type, Coord, Contrib, Cos2)
        if (is.null(input$ind_sup) || !input$ind_sup)
          tmp_y <- tmp_y %>% filter(Type == "Active")
        tmp <- tmp_x %>%
          left_join(tmp_y, by = c("Name", "Type")) %>%
          mutate(Contrib = Contrib.x + Contrib.y,
                 Cos2 = Cos2.x + Cos2.y,
                 tooltip = paste(paste0("<strong>", Name, "</strong>"),
                                 paste0("<strong>x:</strong> ", Coord.x),
                                 paste0("<strong>y:</strong> ", Coord.y),
                                 paste0("<strong>",
                                        gettext("Squared cosinus", domain = "R-explor"),
                                        ":</strong> ", Cos2),
                                 paste0("<strong>",
                                        gettext("Contribution:", domain = "R-explor"),
                                        "</strong> ", Contrib),
                                 sep = "<br />"))
        data.frame(tmp)
      })
      
      ## Individuals plot
      output$indplot <- scatterD3::renderScatterD3({
        col_var <- if (is.null(input$ind_col) || input$ind_col == "None") NULL else ind_data()[, input$ind_col]
        lab_var <- if (input$ind_labels_show) ind_data()[, "Name"] else NULL
        scatterD3::scatterD3(
          x = ind_data()[, "Coord.x"],
          y = ind_data()[, "Coord.y"],
          xlab = names(res$axes)[res$axes == input$ind_x],
          ylab = names(res$axes)[res$axes == input$ind_y],
          point_size = input$ind_point_size,
          point_opacity = input$ind_opacity,
          lab = lab_var,
          labels_size = input$ind_labels_size,
          col_var = col_var,
          col_lab = input$ind_col,
          tooltip_text = ind_data()[, "tooltip"],
          key_var = ind_data()[, "Name"],
          fixed = TRUE,
          transitions = input$ind_transitions,
          html_id = "imca_ind",
          dom_id_reset_zoom = "imca-ind-reset-zoom",
          dom_id_svg_export = "imca-ind-svg-export"
        )
      })
      
      tableOptions_var <- list(lengthMenu =  c(10,20,50,100), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = FALSE)
      tableOptions_ind <- list(lengthMenu = c(10,20,50,100), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = TRUE)
      tableOptions_eta2 <- list(lengthMenu = c(10,20,50), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = FALSE)
      
      varTable <- reactive({
        res$vars %>% 
          filter(Type == "Active", Axis == input$vardim) %>%
          select(-Type, -Level, -Class, -Axis)
      })
      output$vartable <- DT::renderDataTable(
        DT::datatable({varTable()}, 
                      options=c(tableOptions_var,list(order=list(list(3,'desc')))),rownames=FALSE))
      
      ## Supplementary variables
      varTableSup <- reactive({
        res$vars %>% 
          filter(Type == "Supplementary", Axis == input$vardim, 
                 Class == "Quantitative") %>%
          select(-Type, -Level, -Axis, -Contrib)
      })
      output$vartablesup <- DT::renderDataTable(
        DT::datatable({varTableSup()}, 
                      options = c(tableOptions_var,list(order = list(list(2,'desc')))),rownames = FALSE))
      
      ## Active individuals
      indTable <- reactive({
        res$ind %>% 
          filter(Type == "Active", Axis == input$inddim) %>%
          select(-Type, -Axis)
      })
      output$indtable = DT::renderDataTable(
        DT::datatable({indTable()}, 
                      options=c(tableOptions_ind,list(order=list(list(2,'desc')))),rownames=FALSE))

      
      ## Supplementary individuals
      indTableSup <- reactive({
        res$ind %>% 
          filter(Type == "Supplementary", Axis == input$inddim) %>%
          select(-Type, -Axis, -Contrib)
      })
      output$indtablesup = DT::renderDataTable(
        DT::datatable({indTableSup()}, 
                      options = c(tableOptions_ind, list(order = list(list(1,'asc')))), rownames = FALSE))
      
          
    }
  )
}
