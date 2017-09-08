if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("group", "nb_docs", "nb_terms", "nom", "prop_docs", "term"))

##' @rdname explor
##' @param obj_brut optional raw documents corpus
##' @aliases explor.Corpus
##' @export

explor.Corpus <- function(obj, obj_brut = NULL, ...) {
    
    if (!inherits(obj, "Corpus")) stop("obj must be of class Corpus")
    
    ## corpus preparation
    corpus <- prepare_results(obj)
    if (!is.null(obj_brut)) {
      if (!inherits(obj_brut, "Corpus")) stop("obj must be of class Corpus")
      obj_brut <- prepare_results(obj_brut)
    }
    
    ## Settings
    settings <- list()

    ## Launch interface
    explor_corpus(corpus, settings, obj_brut)
    
}

##' @rdname explor
##' @aliases explor.corpus
##' @export

explor.corpus <- function(obj, obj_brut = NULL, ...) {
    
    if (!inherits(obj, "corpus")) stop("obj must be of class corpus")

    ## corpus preparation
    corpus <- prepare_results(obj)
    if (!is.null(obj_brut)) {
      if (!inherits(obj_brut, "corpus")) stop("obj must be of class corpus")
      obj_brut <- prepare_results(obj_brut)
    }
    
    ## Settings
    settings <- list()

    ## Launch interface
    explor_corpus(corpus, settings, obj_brut)
    
}



## Custom CSS
explor_corpus_css <- function() {
  shiny::HTML("
              body {margin: 0 15px;}
              
              #sidebar {padding: 5px 10px;}

              #sidebar label, 
              #sidebar input, 
              #sidebar select, 
              #sidebar option,
              #sidebar button,
              #sidebar a,
              div.option,
              input, label, select, option, .selectize-input {
                  font-size: 12px !important;
                  height: auto !important;
              }

              #sidebar .btn { padding: 6px 10px; }

              #sidebar .shiny-input-checkboxgroup .shiny-options-group {
                  max-height: 200px; 
                  overflow-y: scroll; 
              }

              #sidebar .shiny-input-checkboxgroup .shiny-options-group,
              #filters .shiny-input-container > .irs,
              #filters .shiny-input-container > .input-daterange {
                  margin: 8px 5px;
              }

              #filters .shiny-input-container {
                  border-bottom: 1px solid #BBB;
                  margin: 0px;
                  padding: 5px 4px;
                  background-color: #FAFAFA;
              }

              #filters .shiny-input-container:first-child {
                  border-top: 1px solid #BBB;
                  margin-top: 30px;
              }

             #sidebar > .shiny-input-container:last-child {
                  margin-top: 30px;
              }

              #filters .shiny-input-container > label {
                  cursor: pointer;
                  font-size: 14px !important;
                  font-weight: normal;
                  width: 100%;
              }

              #filters .shiny-input-container label  i {
                  min-width: 1.3em;
              }

               .checkbox { margin-bottom: 3px;}
              
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
              
              .document-content {
                  font-size: 12px !important;
                  background-color: #EEE;
                  border-radius: 8px;
                  padding: 10px;
                  margin: 20px 5px;
              }
              .highlight {background-color: yellow;}
              
              .inline-small * {
                  display: inline;    
                  font-size: 80% !important;
              }
              .inline-small .btn {
                  padding: 3px 5px;
              }
              ")
    }

# Custom JavaScript
explor_corpus_js <- function() {
  shiny::HTML("
(function($) {

var triggered = 0;

$('#filters').on('shiny:visualchange', function(event) {

  triggered += 1;

  if (triggered == 3) {
    $('#filters .shiny-options-group').hide();
    $('#filters .shiny-input-container > .irs').hide();
    $('#filters .shiny-input-container > .input-daterange').hide();

    $('#filters .shiny-input-container > label').prepend('<i class=\"fa fa-chevron-right\"></i> ');

    $('#filters .shiny-input-container > label').click(function() {
      $(this).next().toggle('fast');
      $(this).find('i').toggleClass('fa-chevron-right');
      $(this).find('i').toggleClass('fa-chevron-down');
    });
  };

});
    

})(jQuery);
  ")
}


##' @import stringi
explor_corpus_highlight <- function(x, str) {
  stringi::stri_replace_all_fixed(x, pattern = str,  replacement = paste0("<span class='highlight'>",str,"</span>"),
                         vectorize_all = FALSE,
                         opts_fixed = stri_opts_fixed(case_insensitive = TRUE))
}


##' @import shiny
##' @import quanteda
##' @importFrom highr hi_html

explor_corpus <- function(qco, settings, qco_brut = NULL) { 
    

    ## Document level variables
    vars <- lapply(docvars(qco), unique)
    nvalues <- lapply(vars, length)
    vars <- vars[nvalues > 1 & nvalues < 100]

    ## Document level variables filters
    filter_inputs <- lapply(names(vars), function(name) {
      v <- vars[[name]]
      input_name <- paste0("meta_", name)
      if (class(v) == "numeric") {
        return(sliderInput(input_name, name, min(v), max(v), value = c(min(v), max(v))))
      }
      if (class(v) == "factor") {
        v <- as.character(v)
      }
      if (class(v) == "character") {
        levels <- unique(v)
        if (length(levels) == 1) return(NULL)
        levels[is.na(levels)] <- "NA"
        return(checkboxGroupInput(input_name, name, choices = levels))
      }
      if (class(v) == "Date") {
        return(dateRangeInput(input_name, name, start = min(v), end = max(v))) 
      }
    })
    
    ## n-grams
    m_ngrams <- 1:5
    names(m_ngrams) <- paste0(1:5, "-gram")
    
    shiny::shinyApp(
      ui = navbarPage(gettext("Corpus", domain = "R-explor"),
                      header = tags$head(
                        tags$style(explor_corpus_css())),
                      tabPanel("Termes",
                         sidebarLayout(
                           sidebarPanel(id = "sidebar",
                                        h4("Sélection du corpus"),
                                        p("Si rien n'est coché, tout est sélectionné."),
                                        uiOutput("filters"),
                                        numericInput("term_min_occurrences", "Occurrence minimale des termes retenus", 0, 0, 1000, 1)
                           ),
                           mainPanel(
                             tabsetPanel(
                               
                               ## "Frequent terms" tab
                               tabPanel("Termes fréquents",
                                        h3("Termes les plus fréquents"),
                                        checkboxGroupInput("ngrams",
                                                           "Ngrams",
                                                           m_ngrams,
                                                           selected = 1),
                                        p(HTML("<strong>Nombre de documents&nbsp;:</strong>"), textOutput("nbdocs", inline = TRUE)),
                                        DT::dataTableOutput("freqtable")),
                               
                               ## "Documents" tab                      
                               tabPanel("Recherche de termes",
                                        h3("Choix des termes"),
                                        HTML("<p>Saisir un ou plusieurs termes. Vous pouvez utiliser les connecteurs logiques <code>&</code> (\"et\"), <code>|</code> (\"ou\") <code>!</code> (\"not\") ainsi que des parenthèses :</p>"),
                                        fluidRow(
                                          column(8, textInput("terms", "Termes", width = "100%")),
                                          column(4, selectInput("term_group",
                                                                "Grouper par",
                                                                choices = names(vars)))
                                        ),
                                        tags$p(actionButton("launch_search", "Rechercher")),
                                        uiOutput("termsAlert"),
                                        uiOutput("evalAlert"),
                                        h3("Fréquence des termes choisis"),
                                        htmlOutput("freqterm_query"),
                                        htmlOutput("freqterm_total"),
                                        DT::dataTableOutput("freqtermtable"),
                                        h3("Documents comportant les termes"),
                                        div(style = "display: none;",
                                            numericInput("start_documents", "À partir de", value = 1)),
                                        div(class = "inline-small form-inline",
                                            actionButton("prev_documents", "Précédent", icon("arrow-left")),
                                            textOutput("documents_pagination"),
                                            #numericInput("nb_documents", "Nombre", 10, min = 1, width = "4em"),
                                            actionButton("next_documents", "Suivant", icon("arrow-right")),
                                            numericInput("nb_documents_display", "Afficher : ", 
                                                         value = 10, min = 1, max = 100, step = 1, width = "auto")),  
                                        htmlOutput("documenttable")
                               ),
                               
                               ## "Similarities" tab
                               tabPanel("Similarités",
                                        h3("Choix du terme"),
                                        HTML("<p>Saisir un terme dans le champ ci-dessous :</p>"),
                                        fluidRow(
                                          column(6, textInput("termsim", "Terme")),
                                          column(6,  selectInput("simmethod", "Type de similarité",
                                                                 choices = c("correlation", "cosine", "phi", "Jaccard"),
                                                                 selected = "correlation"))),
                                        uiOutput("termsimAlert"),
                                        h3("Termes associés"),
                                        DT::dataTableOutput("simtermtable")
                               ),
                               
                               ## "Help" tab
                               tabPanel("Aide",
                                        h2("Aide"),
                                        
                                        h3("Termes les plus fréquents"),
                                        p(HTML("Lecture du tableau :")),
                                        tags$ul(
                                          tags$li(HTML("<code>nb_terms</code> : nombre de fois où le terme apparaît dans le corpus sélectionné")),
                                          tags$li(HTML("<code>nb_docs</code> : nombre de documents du corpus sélectionné dans lesquels le terme apparaît au moins une fois")),
                                          tags$li(HTML("<code>prop_docs</code> : pourcentages de documents du corpus sélectionné dans lesquels le terme apparaît au moins une fois"))
                                        ),
                                        
                                        h3("Recherche de termes"),
                                        p(HTML("Permet de rechercher la fréquence de certains termes ou combinaisons de termes dans le corpus sélectionné, et de visualiser les documents correspondant. À noter que la recherche de termes se fait sur le corpus \"nettoyé\", après lemmatisation, suppression des majuscules, etc. À noter aussi que le surlignement des termes de recherche dans les documents affichés en intégralité est imparfait du fait de cette lemmatisation préalable : par exemple, si la recherche est <code>i</code>, tous les \"i\" des documents seront surlignés, mais la recherche aura bien été effectuée sur le mot \"i\".")),
                                        p("Exemples de requêtes :"),
                                        tags$ul(
                                          tags$li(HTML("<code>i</code> : recherche des documents comportant le mot \"i\" (ou \"I\")")),
                                          tags$li(HTML("<code>!i</code> : recherche des documents ne comportant pas le mot \"i\"")),
                                          tags$li(HTML("<code>i | me | we</code> : recherche des documents comportant \"i\", \"me\" ou \"we\" (ou plusieurs d'entre eux)")),
                                          tags$li(HTML("<code>i & we</code> : recherche des documents comportant à la fois \"i\" et \"we\"")),
                                          tags$li(HTML("<code>sky & (sea | ocean)</code> : recherche des documents comportant à la fois \"sky\" et les termes \"sea\" ou \"ocean\" (ou les deux)"))
                                        ),
                                        
                                        h3("Similarités"),
                                        p("Permet de rechercher les termes les plus associés à un mot donné, selon différentes statistiques :"),
                                        tags$ul(
                                          tags$li(HTML("<code>correlation</code> : Corrélation entre les deux vecteurs d'occurrences par document.")),
                                          tags$li(HTML("<code>cosine</code> : Cosinus entre les deux vecteurs d'occurrences par document. Le cosinus sur les vecteurs centrés équivaut )à la corrélation.")),
                                          tags$li(HTML("<code>phi</code> : Mesure d'association entre deux variables binaires. Utilise les vecteurs de présence/absence, pas les vecteurs de nombre d'occurrences. Racine carrée du φ² du tableau croisé des vecteurs de présence/absence.")),
                                          tags$li(HTML("<code>Jaccard</code> : Mesure d'association entre deux variables binaires. Utilise les vecteurs de présence/absence, pas les vecteurs de nombre d'occurrences. On divise le nombre d'occurrences communes par le nombre total d'occurrences, en excluant les couples absence/absence."))
                                        ),
                                        tags$p(HTML("Avec certaines statistiques (notamment <code>correlation</code> et <code>cosine</code>), il peut être intéressant de limiter les termes retenus selon une occurrence minimale, faut de quoi des termes n'apparaissant qu'une fois, par exemple, peuvent apparaître fortement associés."))
                               )
                               
                             )
                           )
                         ),
                         tags$script(explor_corpus_js()))
      ),

               server = function(input, output, session) {

                 
                 ## Variable inputs
                 output$filters <- renderUI({
                   filter_inputs  
                 })
                 
                 # Create logical vector from selected filter elements
                 selected_elements <- function(corpus, meta) {
                   varname <- gsub("^meta_", "", meta)
                   var <- docvars(corpus)[[varname]]
                   selected_values <- input[[meta]]
                   ## Date variables (date range)
                   if (inherits(var, "Date")) {
                     return(var >= selected_values[1] &
                              var <= selected_values[2])
                   }
                   ## Character variables (checkboxes)
                   if (is.character(selected_values)) {
                     selected_values[selected_values == "NA"] <- NA
                     return(var %in% selected_values)
                   }
                   ## Numeric variables (range)
                   if (is.numeric(selected_values)) {
                     return(var >= selected_values[1] &
                              var <= selected_values[2])
                   }
                 }
                 
                 ## Clean corpus filtered
                 co <- reactive({
                   tmp <- qco
                   metas <- grep("^meta_", names(input), value = TRUE)
                   for (meta in metas) {
                     if (is.null(input[[meta]])) next()
                     tmp <- corpus_subset(tmp, selected_elements(tmp, meta))
                   }
                   tmp
                 })
                 
                 ## Brut corpus filtered
                 co_brut <- reactive({
                   if (is.null(qco_brut)) return(co())
                   tmp <- qco_brut
                   metas <- grep("^meta_", names(input), value = TRUE)
                   for (meta in metas) {
                     if (is.null(input[[meta]])) next()
                     tmp <- corpus_subset(tmp, selected_elements(tmp, meta))
                   }
                   tmp
                 })
                 
                 ## Clean corpus DTM filtered by n-grams
                 dtm <- reactive({
                   if (length(input$ngrams) == 0 || is.null(co()) || ndoc(co()) == 0) { 
                     return(NULL)
                   }
                   dtm <- dfm(co(), what = "fastestword", 
                              verbose = FALSE, tolower = FALSE, remove_punct = FALSE,
                              ngrams = input$ngrams, groups = NULL)
                   dtm <- dfm_trim(dtm, min_count = input$term_min_occurrences)
                   dtm
                 })  
                 
                 ## Terms input
                 terms <- reactive({
                   tmp <- unlist(stri_extract_all_words(input$terms))
                   if (length(tmp) == 1 && is.na(tmp)) return(NULL)
                   tmp <- tolower(tmp[tmp != ""])
                 })
                 
                 
                 ## Run the query on the document term matrix as environment,
                 ## and returns the result
                 terms_query <- reactive({
                   error <- NULL
                   if (length(terms()) == 0) return(list(res = NULL, error = NULL))
                   dtm_terms <- dtm() %>% 
                     dfm_select(features = terms(), selection = "keep") %>% 
                     as.data.frame()
                   ## Convert count to presence / absence
                   if (ncol(dtm_terms) > 0) {
                     dtm_terms[dtm_terms > 0] <- 1
                   }
                   res <- try(
                     eval(parse(text = input$terms), envir = dtm_terms) %>% 
                       data.frame()
                     , silent = TRUE)
                   if (inherits(res, "try-error")) {
                     res <- NULL
                     error <- geterrmessage()
                   }
                   list(res = res, error = error)
                 })
                 
                 ## Invalid terms in terms input
                 invalid_terms <- reactive({
                   tmp_terms <- terms()
                   tmp_terms[!(tmp_terms %in% colnames(dtm()))]    
                 })
                 ## Alert if term is missing from corpus
                 output$termsAlert <- renderUI({
                   if (length(invalid_terms() > 0) && invalid_terms() != "") {
                     tmp_terms <- paste(invalid_terms(), collapse = ", ")
                     div(class = "alert alert-warning",
                         HTML(paste("<strong>Attention :</strong> les termes suivants sont absents du corpus : <i>", tmp_terms, "</i>")))
                   }
                 })
                 
                 ## Alert if error in search expression
                 output$evalAlert <- renderUI({
                   input$launch_search
                   e <- terms_query()$error
                   if (!is.null(e)) {
                     div(class = "alert alert-danger",
                         HTML(paste("<strong>Attention :</strong> Erreur dans l'expression : <i>", e, "</i>")))
                   }
                 })
                 
                 ## Number of documents
                 output$nbdocs <- renderText({
                   ndoc(co())
                 })
                 
                 ## Global table options
                 tableOptions <- list(lengthMenu =  c(10,20,50,100), 
                                      pageLength = 20, orderClasses = TRUE, 
                                      autoWidth = TRUE, searching = TRUE)
                 ## Generate correct datatable order option from a column name
                 order_option <- function(table, name, order="desc") {
                   index <- which(names(table) == name) - 1
                   list(order = list(list(index, order)))
                 }
                 
                 ## Most frequent terms table
                 output$freqtable <- DT::renderDataTable({
                   if (is.null(dtm())) return(NULL)
                   frq <- data.frame(topfeatures(dtm(), n = 10000))
                   names(frq) <- "nb_terms"
                   frq$term <- rownames(frq)
                   docf <- data.frame(docfreq(dtm(), scheme = "count"))
                   names(docf) <- "nb_docs"
                   docf$term <- rownames(docf)
                   docf$prop_docs <- (round(docf$nb_docs / ndoc(dtm()) * 100, 2))
                   tab <- frq %>% left_join(docf, by = "term") %>% select(term, nb_terms, nb_docs, prop_docs)
                   DT::datatable(tab, 
                                 options = c(tableOptions, order_option(tab, "nb_terms")), rownames = FALSE)
                 })
                 
                 ## Search term frequency table
                 tab_term <- reactive({
                   tmp_dtm <- terms_query()$res
                   if (is.null(tmp_dtm)) return(NULL)
                   updateNumericInput(session, "start_documents", value = 1)
                   tmp_dtm <- docvars(co()) %>% select_(input$term_group) %>% bind_cols(tmp_dtm)
                   names(tmp_dtm) <- c("group", "n")
                   res <- tmp_dtm %>% group_by(group) %>% summarise(nb_docs = sum(n), prop_docs = round(nb_docs / n() * 100, 1))
                   res
                 })
                 
                 
                 ## Search term total frequency
                 tab_term_tot <- reactive({
                   tmp_dtm <- terms_query()$res
                   if (is.null(tmp_dtm)) return(NULL)
                   names(tmp_dtm) <- "n"
                   res <- tmp_dtm %>% 
                     summarise(nb_docs = sum(n), prop_docs = round(nb_docs / n() * 100, 1)) %>%
                     mutate(nom = "Ensemble") %>% select(nom, nb_docs, prop_docs)
                   res
                 })
                 
                 nb_docs_term <- reactive({
                   if (is.null(tab_term())) return(0)
                   as.numeric(tab_term() %>% summarise(n = sum(nb_docs)))
                 })
                 
                 ## Searched terms query text
                 output$freqterm_query <- renderText({
                   ## Dependency on search button
                   input$launch_search
                   isolate({
                     if (input$terms == "") {
                       return("")
                     }
                     res <- paste0("<p><strong>Requête :</strong> <code>", input$terms, "</code>.</p>")
                     return(HTML(res))
                   })
                 })
                 
                 ## Total searched terms frequency text
                 output$freqterm_total <- renderText({
                   ## Dependency on search button
                   input$launch_search
                   isolate({
                     if (is.null(tab_term_tot())) {
                       return("")
                     }
                     tab <- tab_term_tot()
                     res <- paste0("<p><strong>Ensemble du corpus :</strong> ", tab$nb_docs, " documents, soit ", tab$prop_docs, "%.</p>")
                     return(HTML(res))
                   })
                 })
                 
                 ## Searched terms frequency table
                 output$freqtermtable <- DT::renderDataTable({
                   ## Dependency on search button
                   input$launch_search
                   isolate({
                     if (is.null(tab_term())) {
                       return(DT::datatable(data.frame(table = character())))
                     }
                     tab <- DT::datatable(tab_term(), 
                                   options = c(tableOptions, order_option(tab_term(), "prop_docs")), rownames = FALSE)
                   })
                   tab
                 })

               
                 
                 ## Observers on pagination events
                 observeEvent(input$prev_documents, {
                   updateNumericInput(session, "start_documents", value = max(1, isolate(input$start_documents) - isolate(input$nb_documents_display)), min = 1)
                 })
                 observeEvent(input$next_documents, {
                   val <- isolate(input$start_documents) + isolate(input$nb_documents_display)
                   if (val <= nb_docs_term()) {
                     print("obs")
                     updateNumericInput(session, "start_documents", value = val)
                   }
                 })
                 
                 ## Documents list
                 output$documenttable <- renderText({
                   ## Acquire dependencies
                   input$launch_search
                   start <- input$start_documents
                   nb_documents <- input$nb_documents_display
                   isolate({
                     if (is.null(terms_query()$res)) return("<div class='document-content'>Aucun document trouvé.</p>")
                     indexes <- which(as.vector(terms_query()$res) > 0)
                     end <- min(start + nb_documents - 1, nb_docs_term())
                     indexes <- indexes[start:end]
                     out <- ""
                     for (i in indexes) {
                       out <- paste(out, "<div class='document-content'>")
                       out <- paste(out, "<p><strong>", docvars(co())$orientation[i] ,"</strong><br />")
                       out <- paste(out, explor_corpus_highlight(co_brut()[i], terms()))
                       out <- paste(out, "</p></div>")
                     }
                     HTML(out)
                   })
                 })
                 
                 ## documents list pagination text
                 output$documents_pagination <- renderText({
                   input$launch_search
                   start <- input$start_documents
                   nb_documents <- input$nb_documents_display
                   isolate({
                     end <- min(start + nb_documents - 1, nb_docs_term())
                     if (end == 0) start <- 0
                     paste(start, "à", end, "sur", nb_docs_term())
                   })
                 })
                 
                 ## Similarities
                 invalid_sim_term <- reactive({
                   !(input$termsim %in% colnames(dtm()))
                 })
                 sim_term <- reactive({
                   if (is.null(input$termsim) || input$termsim == "" || invalid_sim_term()) return(NULL)
                   sim <- textstat_simil(dtm(), selection = input$termsim, margin = "features", method = input$simmethod)
                   sim <- sim[[input$termsim]]
                   res <- data.frame(term = names(sim), similarity = round(sim,4))
                   for (i in 1:nrow(res)) {
                     res$nb_docs_commun[i] <- sum(dtm()[, res$term[i]] & dtm()[, input$termsim])
                   }
                   res
                 })
                 
                 ## Alert if term in similarity term is missing from corpus
                 output$termsimAlert <- renderUI({
                   if (input$termsim != "" && invalid_sim_term()) {
                     div(class = "alert alert-warning",
                         HTML(paste("<strong>Attention :</strong> le terme est absent du corpus : <i>", input$termsim, "</i>")))
                   }
                 })
                 
                 ## Similarities table
                 output$simtermtable <- DT::renderDataTable({
                   if (is.null(sim_term())) {
                     return(DT::datatable(data.frame(table = character())))
                   }
                   DT::datatable(sim_term(), 
                                 options = c(tableOptions, order_option(sim_term(), "similarity")), rownames = FALSE)
                 })
                 
               })
}