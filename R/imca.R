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
##' @import dplyr
##' @import ggvis
##' @export imca

imca <- function(acm) {
  
  if (!inherits(acm, "MCA")) stop("acm must be of class MCA")
  
  vars <- data.frame(acm$var$coord)
  varnames <- sapply(acm$call$X[,acm$call$quali,drop=FALSE], nlevels)
  vars$varnames <- rep(names(varnames),varnames)
  vars$modnames <- rownames(vars)

  vars.quali.sup <- data.frame(acm$quali.sup$coord)
  varnames <- sapply(acm$call$X[,acm$call$quali.sup,drop=FALSE], nlevels)
  vars.quali.sup$varnames <- rep(names(varnames),varnames)
  vars.quali.sup$modnames <- rownames(vars.quali.sup)

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

  vareta2 <- data.frame(acm$var$eta2)
  vareta2 <- format(vareta2,scientific=FALSE, nsmall=3, digits=0)
  vareta2$Var <- rownames(vareta2)
  vareta2 <- vareta2 %>% select(Var, starts_with("Dim"))
  
  if (!is.null(acm$quali.sup)) {
    varsupdata <- list()
    varsupdata <- lapply(1:length(comps), function(i) {
      tmp <- data.frame(
        Var = vars.quali.sup$varnames,
        Mod = vars.quali.sup$modnames,
        Coord = signif(acm$quali.sup$coord[,i],3),
        Cos2 = signif(acm$quali.sup$cos2[,i],2),
        V.test = acm$quali.sup$v.test[,i]
      )
      tmp$P.value <-
        ifelse(tmp$V.test >= 0, 2 * (1 - pnorm(tmp$V.test)), 2 * (pnorm(tmp$V.test)))
      tmp$V.test <- signif(tmp$V.test, 3)
      tmp$P.value <- signif(tmp$P.value, 3)
      tmp
    })
    names(varsupdata) <- comps
    
    varsupeta2 <- data.frame(acm$quali.sup$eta2)
    varsupeta2 <-
      format(
        varsupeta2,scientific = FALSE, nsmall = 3, digits = 0
      )
    varsupeta2$Var <- rownames(varsupeta2)
    varsupeta2 <- varsupeta2 %>% select(Var, starts_with("Dim"))
  }
  
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
  input, label, select, option, .selectize-input {
      font-size: 11px !important;
      padding: 3px 5px !important; 
      height: auto !important;
  }
  .well .checkbox { margin-left: 20px !important; }
  .row {margin-top: 15px !important;}
  .well {padding: 5px !important;}
  .table th, 
  .table td {
      font-size: 11px !important;
      padding: 2px !important; 
  }
  .dataTables_info, .dataTables_length, 
  .datatables_filter, .dataTables_paginate {
      font-size: 11px !important;
  }
  .dataTables_wrapper "
  
  shiny::shinyApp(
    ui=navbarPage("iMCA",
                  header=tags$head(
                  tags$style(HTML(css_string))),
                  tabPanel("Eigenvalues",
                           fluidRow(
                             column(2,
                                    wellPanel(numericInput("eigNb", "Dimensions to plot", 
                                                 min=2, max=max.eig, value=max.eig, 
                                                 step=1))),
                            column(10,
                                    ggvisOutput("eigplot"))
                             
                             )),
                  
                  tabPanel("Variables plot",
                           fluidRow(
                             column(2,
                                    wellPanel(
                                      selectInput("xVar", "X axis", choices=comps, selected="Dim.1"),
                                      selectInput("yVar", "Y axis", choices=comps, selected="Dim.2"),
                                      sliderInput("size", "Size", 4, 20, 10),
                                      checkboxInput("supvar", HTML("Supplementary variables"), value=TRUE))),
                             column(10,
                                    ggvisOutput("varplot"))
                  )),
                  
                  tabPanel("Variables data",
                           fluidRow(
                             column(2,
                                    wellPanel(
                                    selectInput("vardim", "Dimension", choices=comps, selected="Dim.1"),
                                    textInput("varpvalue", "Max p-value", 1))),
                             column(10,
                                    h3("Positive coordinates"),
                                    dataTableOutput("vartablepos"),
                                    h3("Negative coordinates"),                   
                                    dataTableOutput("vartableneg"),
                                    h3("Variables eta2"),                   
                                    dataTableOutput("vartableeta2"),
                                    h3("Supplementary variables"),                   
                                    dataTableOutput("vartablesup"),
                                    h3("Supplementary variables eta2"),                   
                                    dataTableOutput("vartablesupeta2")
                                    ))),
                  
                  tabPanel("Individuals plot",
                           fluidRow(
                             column(2,
                                    wellPanel(
                                    selectInput("xInd", "X axis", choices=comps, selected="Dim.1"),
                                    selectInput("yInd", "Y axis", choices=comps, selected="Dim.2"),
                                    sliderInput("indOpacity", "Opacity", 0, 1, 0.5))),
                             column(10,
                                    ggvisOutput("indplot")))),
                  
                  tabPanel("Individuals data",
                           fluidRow(
                             column(2,
                                    wellPanel(
                                    selectInput("inddim", "Dimension", choices=comps, selected="Dim.1"))),
                             column(10,
                                    h3("Positive coordinates"),
                                    dataTableOutput("indtablepos"),
                                    h3("Negative coordinates"),                   
                                    dataTableOutput("indtableneg"))))
                  
                  
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
        g <- vars %>%
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
        ## Supplementary variables
        if(input$supvar) 
          g <- g %>% 
            layer_points(data=vars.quali.sup,
                         shape:="cross", fill= ~factor(varnames),
                         x=as.name(input$xVar),y=as.name(input$yVar)) %>%
            layer_text(data=vars.quali.sup, text:=~modnames, fill= ~factor(varnames), 
                       align:="center", baseline:="bottom", dy:=-5,
                       fontSize:=input$size) %>%
            add_tooltip(show_namev, "hover")
        g
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
      varTableEta2 <- reactive({
        tmp <- vareta2
        dimcols <- grep("^Dim", names(tmp))
        for(col in dimcols) {
          tmp[tmp[,col]>=as.numeric(input$varpvalue),col] <- "-"
        }
        tmp
      })
      tableOptions <- list(lengthMenu=c(10,20,50,100), pageLength=10, orderClasses=TRUE, autoWidth=TRUE, searching=FALSE)
      output$vartablepos = renderDataTable({varTablePos()}, options=c(tableOptions,list(order=list(list(2,'desc')))))
      output$vartableneg = renderDataTable({varTableNeg()}, options=c(tableOptions,list(order=list(list(2,'asc')))))
      output$vartableeta2 = renderDataTable({varTableEta2()}, options=list(lengthMenu=c(10,20,50,100), pageLength=100, orderClasses=TRUE, autoWidth=TRUE, searching=FALSE))
      
      ## Supplementary var table
      varTableSup <- reactive({
        varsupdata[[input$vardim]] %>% filter(P.value<=as.numeric(input$varpvalue))
      })
      varTableSupEta2 <- reactive({
        tmp <- varsupeta2
        dimcols <- grep("^Dim", names(tmp))
        for(col in dimcols) {
          tmp[tmp[,col]>=as.numeric(input$varpvalue),col] <- "-"
        }
        tmp
      })
      tableOptions <- list(lengthMenu=c(10,20,50,100), pageLength=10, orderClasses=TRUE, autoWidth=TRUE, searching=FALSE)
      output$vartablesup = renderDataTable({varTableSup()}, options=c(tableOptions,list(order=list(list(2,'desc')))))
      output$vartablesupeta2 = renderDataTable({varTableSupEta2()}, options=list(lengthMenu=c(10,20,50,100), pageLength=100, orderClasses=TRUE, autoWidth=TRUE, searching=FALSE))
      
      
      
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
