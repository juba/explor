##' @rdname explor
##' @aliases explor.PCA
##' @export

explor.PCA <- function(obj) {
  
  if (!inherits(obj, "PCA")) stop("obj must be of class PCA")
  
  ## results preparation
  res <- prepare_results(obj)
  
  ## Settings
  settings <- list()
  settings$var_columns <- c("Variable", "Coord", "Contrib", "Cos2", "Cor")
  settings$varsup_columns <- c("Variable", "Coord", "Cos2", "Cor")
  settings$ind_columns <- c("Name", "Coord", "Contrib", "Cos2")
  settings$indsup_columns <- c("Name", "Coord", "Cos2")
  settings$scale_unit <- obj$call$scale.unit
  
  ## Launch interface
  explor_pca(res, settings)
  
}


##' @rdname explor
##' @aliases explor.pca
##' @details 
##' If you want to display supplementary individuals or variables and you're using
##' the \code{\link[ade4]{dudi.pca}} function, you can add the coordinates of 
##' \code{\link[ade4]{suprow}} and/or \code{\link[ade4]{supcol}} to as \code{supi} and/or 
##' \code{supv} elements added to your \code{\link[ade4]{dudi.pca}} result (See example).
##' @export
##' @examples
##' \dontrun{
##' 
##' library(ade4)
##' data(deug)
##' d <- deug$tab
##' sup_var <- d[-(1:10), 8:9]
##' sup_ind <- d[1:10, -(8:9)]
##' pca <- dudi.pca(d[-(1:10), -(8:9)], scale = TRUE, scannf = FALSE, nf = 5)
##' supi <- suprow(pca, sup_ind)
##' pca$supi <- supi$lisup
##' supv <- supcol(pca, dudi.pca(sup_var, scale = TRUE, scannf = FALSE)$tab)
##' pca$supv <- supv$cosup
##' explor(pca)
##' }


explor.pca <- function(obj) {
  
  if (!inherits(obj, "pca") || !inherits(obj, "dudi")) stop("obj must be of class dudi and pca")
  
  ## results preparation
  res <- prepare_results(obj)
  
  ## Settings
  settings <- list()
  settings$var_columns <- c("Variable", "Coord", "Contrib", "Cos2")
  settings$varsup_columns <- c("Variable", "Coord")
  settings$ind_columns <- c("Name", "Coord", "Contrib", "Cos2")
  settings$indsup_columns <- c("Name", "Coord")
  settings$scale_unit <- if (is.null(obj$call$scale)) TRUE else obj$call$scale
  

  ## Launch interface
  explor_pca(res, settings)
  
}



##' @import shiny
##' @import shinyBS
##' @import dplyr
##' @import scatterD3
##' @import ggplot2

explor_pca <- function(res, settings) {
  
  ## Precompute inputs 
  has_sup_vars <- "Supplementary" %in% res$vars$Type 
  ## Variable color input
  if (has_sup_vars) {
    choices <- c("None", "Type")
    names(choices) <- c(gettext("None", domain = "R-explor"),
                        gettext("Variable type", domain = "R-explor"))
    var_col_input <- selectInput("var_col", 
                                 gettext("Points color :", domain = "R-explor"),
                                 choices = choices,  selected = "Type")
  } else {
    var_col_input <- NULL
  }

  ## Individual color input
  ind_col_choices <- c("None", "Type")
  names(ind_col_choices) <- c(gettext("None", domain = "R-explor"),
                              gettext("Individual type", domain = "R-explor"))
  ind_col_input <- selectInput("ind_col", 
                               gettext("Points color :", domain = "R-explor"),
                               choices = ind_col_choices,
                               selected = "Type")
  
  
  
  has_sup_ind <- "Supplementary" %in% res$ind$Type

  shiny::shinyApp(
    ui = navbarPage(gettext("PCA", domain = "R-explor"),
                  header = tags$head(
                  tags$style(explor_css())),
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
                                      numericInput("var_lab_min_contrib",
                                                   gettext("Minimum contribution to show label", domain = "R-explor"),
                                                   min = 0, max = ceiling(2*max(res$vars$Contrib, na.rm = TRUE)), value = 0),
                                      if (has_sup_vars) var_col_input,
                                      if (has_sup_vars)
                                        checkboxInput("var_sup", 
                                                      HTML(gettext("Supplementary variables", domain = "R-explor")), 
                                                      value = TRUE),
                                      checkboxInput("var_transitions", 
                                                    HTML(gettext("Animations", domain = "R-explor")),
                                                    value = TRUE),
                                      tags$p(actionButton("explor-var-reset-zoom", 
                                                          title = gettext("Reset zoom", domain = "R-explor"),
                                                          HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span>")),
                                             actionButton("explor-var-lasso-toggle", 
                                                          title = gettext("Toggle lasso", domain = "R-explor"),
                                                          HTML("<span class='glyphicon glyphicon-screenshot' aria-hidden='true'></span>"), 
                                                          "data-toggle" = "button"),                                             
                                             tags$a(id = "explor-var-svg-export", href = "#",
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
                                      if (has_sup_ind) 
                                        ind_col_input,
                                      if (has_sup_ind) 
                                        checkboxInput("ind_ellipses", 
                                                      HTML(gettext("Ellipses", domain = "R-explor")),
                                                      value = FALSE),
                                      if (has_sup_ind)
                                        checkboxInput("ind_sup", 
                                                      HTML(gettext("Supplementary individuals", domain = "R-explor")),
                                                      value = TRUE),
                                      checkboxInput("ind_transitions", 
                                                    HTML(gettext("Animations", domain = "R-explor")),
                                                    value = TRUE),          
                                      tags$p(actionButton("explor-ind-reset-zoom", 
                                                          title = gettext("Reset zoom", domain = "R-explor"),
                                                          HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span>")),
                                             actionButton("explor-ind-lasso-toggle", 
                                                          title = gettext("Toggle lasso", domain = "R-explor"),
                                                          HTML("<span class='glyphicon glyphicon-screenshot' aria-hidden='true'></span>"), 
                                                          "data-toggle" = "button"),
                                             tags$a(id = "explor-ind-svg-export", href = "#",
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
                             ))),
                  footer = shinyBS::bsModal(id = "lasso-modal", trigger = NULL,
                                            title = gettext("Selected points", domain = "R-explor"), 
                                            tags$p(id = "lasso-mod-content"))
                  
    ),
    
    server = function(input, output) {
      
      output$eigplot <- renderPlot({
        tmp <- res$eig[1:input$eig_nb,]
        tmp$dim <- factor(tmp$dim)
        ggplot(data = tmp) +
          geom_bar(aes_string(x = "dim", y = "percent"), stat = "identity") +
          scale_x_discrete(gettext("Axis", domain = "R-explor")) +
          scale_y_continuous(gettext("Percentage of inertia", domain = "R-explor"))
      })

      ## Variables plot reactive data
      var_data <- reactive({
        tmp_x <- res$vars %>% 
          filter(Axis == input$var_x) %>%
          select_("Variable", "Type", "Class", "Coord", "Contrib", "Cos2")
        if (is.null(input$var_sup) || !input$var_sup)
          tmp_x <- tmp_x %>% filter(Type == 'Active')
        tmp_y <- res$vars %>% 
          filter(Axis == input$var_y) %>%
          select_("Variable", "Type", "Class", "Coord", "Contrib", "Cos2")
        if (is.null(input$var_sup) || !input$var_sup)
          tmp_y <- tmp_y %>% filter(Type == 'Active')
        tmp <- tmp_x %>%
          left_join(tmp_y, by = c("Variable", "Type", "Class")) %>%
          mutate(Contrib = Contrib.x + Contrib.y,
                 Cos2 = Cos2.x + Cos2.y,
                 tooltip = paste(paste0("<strong>",
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
                                  sep = "<br />"),
                 Lab = ifelse(Contrib >= as.numeric(input$var_lab_min_contrib) | 
                                  (is.na(Contrib) & as.numeric(input$var_lab_min_contrib) == 0), Variable, ""))
        data.frame(tmp)
      })
      
      ## Variables plot
      output$varplot <- scatterD3::renderScatterD3({
        col_var <- if (is.null(input$var_col) || input$var_col == "None") NULL else var_data()[, input$var_col]
        type_var <- ifelse(var_data()[,"Class"] == "Quantitative", "arrow", "point")
        scatterD3::scatterD3(
          x = var_data()[, "Coord.x"],
          y = var_data()[, "Coord.y"],
          xlab = names(res$axes)[res$axes == input$var_x],
          ylab = names(res$axes)[res$axes == input$var_y],
          lab = var_data()[, "Lab"],
          labels_size = input$var_lab_size,
          point_opacity = 1,
          col_var = col_var,
          col_lab = input$var_col,
          tooltip_text = var_data()[, "tooltip"],
          type_var = type_var,
          unit_circle = settings$scale_unit,
          xlim = if (settings$scale_unit) c(-1.1, 1.1) else NULL,
          ylim = if (settings$scale_unit) c(-1.1, 1.1) else NULL,          
          key_var = var_data()[, "Variable"],
          fixed = TRUE,
          transitions = input$var_transitions,
          html_id = "explor_var",
          dom_id_reset_zoom = "explor-var-reset-zoom",
          dom_id_svg_export = "explor-var-svg-export",
          dom_id_lasso_toggle = "explor-var-lasso-toggle",
          lasso = TRUE,
          lasso_callback = explor_lasso_callback()
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
          ellipses = if (is.null(input$ind_ellipses)) FALSE else input$ind_ellipses,
          tooltip_text = ind_data()[, "tooltip"],
          key_var = ind_data()[, "Name"],
          fixed = TRUE,
          transitions = input$ind_transitions,
          html_id = "explor_ind",
          dom_id_reset_zoom = "explor-ind-reset-zoom",
          dom_id_svg_export = "explor-ind-svg-export",
          dom_id_lasso_toggle = "explor-ind-lasso-toggle",
          lasso = TRUE,
          lasso_callback = explor_lasso_callback()
        )
      })
      
      tableOptions_var <- list(lengthMenu =  c(10,20,50,100), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = TRUE)
      tableOptions_ind <- list(lengthMenu = c(10,20,50,100), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = TRUE)
      tableOptions_eta2 <- list(lengthMenu = c(10,20,50), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = TRUE)
      
      ## Generate correct datatable order option from a column name
      order_option <- function(table, name, order="desc") {
        index <- which(names(table) == name) - 1
        list(order = list(list(index, order)))
      }
      
      varTable <- reactive({
        res$vars %>% 
          filter(Type == "Active", Axis == input$vardim) %>%
          select_(.dots = settings$var_columns)
      })
      output$vartable <- DT::renderDataTable(
        DT::datatable({varTable()}, 
                      options = c(tableOptions_var, order_option(varTable(), "Contrib")), rownames = FALSE))
      
      ## Supplementary variables
      varTableSup <- reactive({
        res$vars %>% 
          filter(Type == "Supplementary", Axis == input$vardim, 
                 Class == "Quantitative") %>%
          select_(.dots = settings$varsup_columns)
      })
      output$vartablesup <- DT::renderDataTable(
        DT::datatable({varTableSup()}, 
                      options = c(tableOptions_var, order_option(varTableSup(), "Coord")), rownames = FALSE))
      
      ## Active individuals
      indTable <- reactive({
        res$ind %>% 
          filter(Type == "Active", Axis == input$inddim) %>%
          select_(.dots = settings$ind_columns)
      })
      output$indtable = DT::renderDataTable(
        DT::datatable({indTable()}, 
                      options = c(tableOptions_ind, order_option(indTable(), "Coord")), rownames = FALSE))

      
      ## Supplementary individuals
      indTableSup <- reactive({
        res$ind %>% 
          filter(Type == "Supplementary", Axis == input$inddim) %>%
          select_(.dots = settings$indsup_columns)
      })
      output$indtablesup = DT::renderDataTable(
        DT::datatable({indTableSup()}, 
                      options = c(tableOptions_ind,  order_option(indTableSup(), "Coord")), rownames = FALSE))
      
          
    }
  )
}
