##' Interface for multiple correspondence analysis
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive visualisation and exploration of a multiple correspondence analysis.
##'
##' @param acm an object of class MCA, result of the \code{MCA()} function from the \code{FactoMineR} package.
##' @return
##' The function launches a shiny app in the system web browser.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @seealso \code{\link[FactoMineR]{MCA}}
##' @import shiny
##' @import ggvis
##' @export imca

imca <- function(acm) {
  
  if (!inherits(acm, "MCA")) stop("acm must be of class MCA")
  
  vars <- data.frame(acm$var$coord)
  varnames <- sapply(acm$call$X[,acm$call$quali], nlevels)
  vars$varnames <- rep(names(varnames),varnames)
  vars$modnames <- rownames(vars)
  
  ind <- data.frame(acm$ind$coord)
  ind$name <- rownames(ind)
  
  eig <- data.frame(dim=1:nrow(acm$eig),percent=acm$eig[,2])
  max.eig <- max(eig$dim)  
  
  comps <- grep("^Dim\\.\\d+$", names(vars), value=TRUE)
  names(comps) <- paste(comps, paste0("(",head(round(acm$eig[,2],2), length(comps)),"%)"))
  
  vardata <- list()
  vardata <- lapply(1:length(comps), function(i) {
    tmp <- data.frame(Var=vars$varnames,
                      Mod=vars$modnames,
                      Coord=signif(acm$var$coord[,i],3),
                      Contrib=signif(acm$var$contrib[,i],3),
                      Cos2=signif(acm$var$cos2[,i],2),
                      V.test=acm$var$v.test[,i])
    tmp$P.value <- ifelse(tmp$V.test>=0, 2*(1-pnorm(tmp$V.test)), 2*(pnorm(tmp$V.test)))
    tmp$V.test <- signif(tmp$V.test, 3)
    tmp$P.value <- signif(tmp$P.value, 3)
    tmp})
  names(vardata) <- comps
  
  inddata <- list()
  inddata <- lapply(1:length(comps), function(i) {
    tmp <- data.frame(Ind=rownames(ind),
                      Coord=signif(acm$ind$coord[,i],3),
                      Contrib=signif(acm$ind$contrib[,i],3),
                      Cos2=signif(acm$ind$cos2[,i],2))
    tmp})
  names(inddata) <- comps
  
  
  
  css_string <- "
  .well label, 
  .well input, 
  .well select, 
  .well option,
  .well *,
  input, label, select, option {
      font-size: 11px !important;
      padding: 2px !important; 
  }
  .table th, 
  .table td {
      font-size: 11px !important;
      padding: 2px !important; 
  }
  .dataTables_info, .dataTables_length, 
  .datatables_filter, .dataTables_paginate {
      font-size: 11px !important;
  }"
  
  shiny::shinyApp(
    ui=navbarPage("iMCA",
                  header=tags$head(
                    tags$style(HTML(css_string))),
                  tabPanel("Eigenvalues",
                           wellPanel(fluidRow(
                             column(4,
                                    numericInput("eigNb", "Dimensions to plot", min=2, max=max.eig, value=max.eig, step=1)))),
                           ggvisOutput("eigplot")),
                  
                  tabPanel("Variables plot",
                           wellPanel(fluidRow(
                             column(4,
                                    selectInput("xVar", "X axis", choices=comps, selected="Dim.1"),
                                    selectInput("yVar", "Y axis", choices=comps, selected="Dim.2")),
                             column(4,
                                    sliderInput("size", "Size", 4, 20, 10)))),
                           ggvisOutput("varplot")
                  ),
                  
                  tabPanel("Variables data",
                           wellPanel(fluidRow(
                             column(4, 
                                    selectInput("vardim", "Dimension", choices=comps, selected="Dim.1")),
                             column(4, 
                                    textInput("varpvalue", "Max p-value", 1)))),
                           h3("Positive coordinates"),
                           dataTableOutput("vartablepos"),
                           h3("Negative coordinates"),                   
                           dataTableOutput("vartableneg")
                  ),
                  
                  tabPanel("Individuals plot",
                           wellPanel(fluidRow(
                             column(4,
                                    selectInput("xInd", "X axis", choices=comps, selected="Dim.1"),
                                    selectInput("yInd", "Y axis", choices=comps, selected="Dim.2")),
                             column(4,
                                    sliderInput("indOpacity", "Opacity", 0, 1, 0.8)))),
                           ggvisOutput("indplot")),
                  
                  tabPanel("Individuals data",
                           wellPanel(fluidRow(
                             column(4, 
                                    selectInput("inddim", "Dimension", choices=comps, selected="Dim.1")))),
                           h3("Positive coordinates"),
                           dataTableOutput("indtablepos"),
                           h3("Negative coordinates"),                   
                           dataTableOutput("indtableneg")
                  )
                  
                  
    ),
    
    server=function(input, output) {
      
      
      ## Eig plot tooltip function
      show_nameeig <- function(x) {
        if(is.null(x)) return(NULL)
        name <- paste0("Dim: ",x$x_)
        percent <- paste0("Var: ",round(x$stack_upr_,2), "%")
        out <- paste0(c(name, percent), collapse="<br />\n")  
        out
      }
      
      ## Eig plot
      reactive({
        eig[1:input$eigNb,] %>%
          ggvis(~factor(dim), ~percent, fill:="#999") %>%
          layer_bars() %>%
          add_tooltip(show_nameeig, "hover")
      }) %>% bind_shiny("eigplot")
      
      
      ## Var plot tooltip function
      show_namev <- function(x) {
        if(is.null(x)) return(NULL)
        name <- paste0("id: ",x$modnames)
        comps <- grep("^Dim\\.\\d+$", names(x), value=TRUE)
        tmp <- character()
        for (comp in comps) tmp <- append(tmp, paste0(comp, ": ", round(x[,comp],2)))
        out <- paste0(c(name, tmp), collapse="<br />\n")  
        out
      }
      
      ## Var plot
      reactive({ 
        vars %>%
          ggvis(x=as.name(input$xVar),y=as.name(input$yVar)) %>%
            add_axis("x") %>%
            add_axis("x", values=c(0,0), properties = axis_props(
              axis=NULL, ticks=NULL, grid = list(stroke = "#999"))) %>%
            add_axis("y") %>%
            add_axis("y", values=c(0,0), properties = axis_props(
              axis=NULL, ticks=NULL, grid = list(stroke = "#999"))) %>%
            layer_points(fill= ~factor(varnames)) %>%
            add_tooltip(show_namev, "hover") %>%
            layer_text(text:=~modnames, fill= ~factor(varnames), 
                       align:="center", baseline:="bottom", dy:=-5,
                       fontSize:=input$size) %>%
            add_legend("fill", title="Variable")                   
      }) %>%  bind_shiny("varplot")
      
      
      ## Ind plot tooltip function
      show_namei <- function(x) {
        if(is.null(x)) return(NULL)
        name <- paste0("id: ",x$name)
        comps <- grep("^Dim\\.\\d+$", names(x), value=TRUE)
        tmp <- character()
        for (comp in comps) tmp <- append(tmp, paste0(comp, ": ", round(x[,comp],2)))
        out <- paste0(c(name, tmp), collapse="<br />\n")  
        out
      }
      
      ## Ind plot
      reactive({ 
        ind %>%
          ggvis(x=as.name(input$xInd),y=as.name(input$yInd)) %>%
            add_axis("x") %>%
            add_axis("x", values=c(0,0), properties = axis_props(
              axis=NULL, ticks=NULL, grid = list(stroke = "#999"))) %>%
            add_axis("y") %>%
            add_axis("y", values=c(0,0), properties = axis_props(
              axis=NULL, ticks=NULL, grid = list(stroke = "#999"))) %>%
            layer_points(fill:="#0000AA", opacity:=input$indOpacity,
                         stroke:=~name, strokeWidth:=0, strokeOpacity:=0) %>%
            #layer_text(text:=~name, fontSize:=0) %>%
            add_tooltip(show_namei, "hover")
      }) %>%  bind_shiny("indplot")      
      
      ## Var table
      varTablePos <- reactive({
        vardata[[input$vardim]] %>% filter(Coord>=0 & P.value<=as.numeric(input$varpvalue))
      })
      varTableNeg <- reactive({
        vardata[[input$vardim]] %>% filter(Coord<0 & P.value<=as.numeric(input$varpvalue))
      })
      tableOptions <- list(lengthMenu=c(10,20,50,100), pageLength=10, orderClasses=TRUE, autoWidth=TRUE, searching=FALSE)
      output$vartablepos = renderDataTable({varTablePos()}, options=c(tableOptions,list(order=list(list(2,'desc')))))
      output$vartableneg = renderDataTable({varTableNeg()}, options=c(tableOptions,list(order=list(list(2,'asc')))))

      ## Ind table
      indTablePos <- reactive({
        inddata[[input$inddim]] %>% filter(Coord>=0)
      })
      indTableNeg <- reactive({
        inddata[[input$inddim]] %>% filter(Coord<0)
      })
      tableOptions <- list(lengthMenu=c(10,20,50,100), pageLength=10, orderClasses=TRUE, autoWidth=TRUE, searching=FALSE)
      output$indtablepos = renderDataTable({indTablePos()}, options=c(tableOptions,list(order=list(list(1,'desc')))))
      output$indtableneg = renderDataTable({indTableNeg()}, options=c(tableOptions,list(order=list(list(1,'asc')))))
    
    }
  )
}
