if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("group", "nb_docs", "nb_terms", "nom", "prop_docs", "term"))

##' @rdname explor
##' @param obj_brut optional raw documents corpus
##' @param stopwords stopwords character vector
##' @param thesaurus quanteda thesaurus list
##' @aliases explor.Corpus
##' @export

explor.Corpus <- function(obj, obj_brut = NULL, stopwords = NULL, thesaurus = NULL, ...) {
    
    if (!inherits(obj, "Corpus")) stop(gettext("obj must be of class Corpus", domain = "R-explor"))
    
    ## corpus preparation
    corpus <- prepare_results(obj)
    if (!is.null(obj_brut)) {
      if (!inherits(obj_brut, "Corpus")) stop(gettext("obj must be of class Corpus", domain = "R-explor"))
      obj_brut <- prepare_results(obj_brut)
    }
    
    ## Settings
    settings <- list(obj_brut = obj_brut,
                     stopwords = stopwords,
                     thesaurus = thesaurus)

    ## Launch interface
    explor_corpus(corpus, settings)
    
}

##' @rdname explor
##' @aliases explor.corpus
##' @export

explor.corpus <- function(obj, obj_brut = NULL, stopwords = NULL, thesaurus = NULL, ...) {
    
    if (!inherits(obj, "corpus")) stop(gettext("obj must be of class corpus", domain = "R-explor"))

    ## corpus preparation
    corpus <- prepare_results(obj)
    if (!is.null(obj_brut)) {
      if (!inherits(obj_brut, "corpus")) stop(gettext("obj must be of class corpus", domain = "R-explor"))
      obj_brut <- prepare_results(obj_brut)
    }
    
    ## Settings
    settings <- list(obj_brut = obj_brut,
                     stopwords = stopwords,
                     thesaurus = thesaurus)

    ## Launch interface
    explor_corpus(corpus, settings)
    
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

explor_corpus <- function(qco, settings) { 
    

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
                      tabPanel(gettext("Terms", domain = "R-explor"),
                         sidebarLayout(
                           sidebarPanel(id = "sidebar",
                                        h4(gettext("Corpus treatment", domain = "R-explor")),
                                        checkboxInput("treat_tolower", gettext("Convert to lowercase", domain = "R-explor"), value = TRUE),
                                        checkboxInput("treat_removepunct", gettext("Remove punctuation", domain = "R-explor"), value = TRUE),
                                        checkboxInput("treat_rmnum", gettext("Remove numbers", domain = "R-explor"), value = TRUE),
                                        if (!is.null(settings$stopwords)) 
                                          checkboxInput("treat_stopwords", gettext("Remove stopwords", domain = "R-explor"), value = TRUE),
                                        if (!is.null(settings$thesaurus)) 
                                          checkboxInput("treat_thesaurus", gettext("Apply thesaurus", domain = "R-explor"), value = TRUE),
                                        checkboxInput("treat_stem", gettext("Stem words", domain = "R-explor"), value = FALSE),                                        
                                        h4(gettext("Corpus filtering", domain = "R-explor")),
                                        p(gettext("If nothing is selected, no filter is applied.", domain = "R-explor")),
                                        uiOutput("filters"),
                                        numericInput("term_min_occurrences", gettext("Filter terms on minimal frequency", domain = "R-explor"), 0, 0, 1000, 1)
                           ),
                           mainPanel(
                             tabsetPanel(
                               
                               ## "Frequent terms" tab
                               tabPanel(gettext("Frequent terms", domain = "R-explor"),
                                        h3(gettext("Most frequent terms", domain = "R-explor")),
                                        checkboxGroupInput("ngrams",
                                                           gettext("Ngrams", domain = "R-explor"),
                                                           m_ngrams,
                                                           selected = 1),
                                        p(HTML("<strong>", gettext("Number of documents", domain = "R-explor"), "&nbsp;:</strong>"), textOutput("nbdocs", inline = TRUE)),
                                        DT::dataTableOutput("freqtable")),
                               
                               ## "Documents" tab                      
                               tabPanel(gettext("Terms search", domain = "R-explor"),
                                        h3(gettext("Terms selection", domain = "R-explor")),
                                        HTML("<p>", gettext('Enter one or more terms. You can use logical operators like <code>&</code> ("and"), <code>|</code> ("or"), <code>!</code> ("not") and parentheses :', domain = "R-explor"), "</p>"),
                                        fluidRow(
                                          column(8, textInput("terms", gettext("Terms", domain = "R-explor"), width = "100%")),
                                          column(4, selectInput("term_group",
                                                                gettext("Group by", domain = "R-explor"),
                                                                choices = names(vars)))
                                        ),
                                        tags$p(actionButton("launch_search", gettext("Search", domain = "R-explor"))),
                                        uiOutput("termsAlert"),
                                        uiOutput("evalAlert"),
                                        h3(gettext("Selected terms frequency", domain = "R-explor")),
                                        htmlOutput("freqterm_query"),
                                        htmlOutput("freqterm_total"),
                                        DT::dataTableOutput("freqtermtable"),
                                        h3(gettext("Corresponding documents", domain = "R-explor")),
                                        div(style = "display: none;",
                                            numericInput("start_documents", gettext("From", domain = "R-explor"), value = 1)),
                                        div(class = "inline-small form-inline",
                                            actionButton("prev_documents", gettext("Previous", domain = "R-explor"), icon("arrow-left")),
                                            textOutput("documents_pagination"),
                                            #numericInput("nb_documents", "Nombre", 10, min = 1, width = "4em"),
                                            actionButton("next_documents", gettext("Next", domain = "R-explor"), icon("arrow-right")),
                                            numericInput("nb_documents_display", gettext("Display : ", domain = "R-explor"), 
                                                         value = 10, min = 1, max = 100, step = 1, width = "auto")),  
                                        htmlOutput("documenttable")
                               ),
                               
                               ## "Similarities" tab
                               tabPanel(gettext("Similarities", domain = "R-explor"),
                                        h3(gettext("Term", domain = "R-explor")),
                                        HTML("<p>", gettext("Enter a term :</p>", domain = "R-explor"), "</p>"),
                                        fluidRow(
                                          column(6, textInput("termsim", gettext("Term", domain = "R-explor"))),
                                          column(6,  selectInput("simmethod", gettext("Similarity", domain = "R-explor"),
                                                                 choices = c("correlation", "cosine", "phi", "Jaccard"),
                                                                 selected = "correlation"))),
                                        uiOutput("termsimAlert"),
                                        h3(gettext("Associated terms", domain = "R-explor")),
                                        DT::dataTableOutput("simtermtable")
                               ),
                               
                               ## "Help" tab
                               tabPanel(gettext("Help", domain = "R-explor"),
                                        h2(gettext("Help", domain = "R-explor")),
                                        
                                        h3(gettext("Most frequent terms", domain = "R-explor")),
                                        p(HTML(gettext("How to read the table :", domain = "R-explor"))),
                                        tags$ul(
                                          tags$li(HTML(gettext("<code>Term frequency</code> : number of times this term is found in the selected corpus", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>Documents frequency</code> : number of documents in the selected corpus in which this term appears at least once", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>Documents proportion</code> : percentage of documents in the selected corpus in which this term appears at least once", domain = "R-explor")))
                                        ),
                                        
                                        h3(gettext("Tearms search", domain = "R-explor")),
                                        p(HTML(gettext("Allows to search for terms or terms combinations in the selected corpus, and to display both frequencies and the corresponding documents. Note that the search is made on the cleaned corpus (after filtering, stemming, removing of stopwords, etc.). Also note that highlighting is not perfect : for example, if searching for <code>I</code>, every \"i\" in the documents will be highlighted, but the search has been made only on the word \"I\".", domain = "R-explor"))),
                                        p("Query examples :"),
                                        tags$ul(
                                          tags$li(HTML(gettext("<code>I</code> : search for documents with the term \"I\" (or \"i\")", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>!i</code> : search for documents without the term \"I\"", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>i | me | we</code> : search for documents with \"i\", \"me\" or \"we\" (or any combination)", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>i & we</code> : search for documents with both \"i\" and \"we\"", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>sky & (sea | ocean)</code> : search for documents with \"sky\" and the terms \"sea\" or \"ocean\" (or both)", domain = "R-explor")))
                                        ),
                                        
                                        h3(gettext("Similarities", domain = "R-explor")),
                                        p(gettext("Allow to search for terms associated to a given word. Several similarities measures can be used :", domain = "R-explor")),
                                        tags$ul(
                                          tags$li(HTML(gettext("<code>correlation</code> : correlation between the two vectors of frequencies by document.", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>cosine</code> : cosinus between the two vectors of frequencies by document. Cosinus on the centred vectors is equal to the correlation.", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>phi</code> : Association measure between two binary vectors. Use binary presence/absence vectors instead of frequencies. Phi is the squared root of the φ² of the cross tabulation of the two binary vectors.", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>Jaccard</code> : Association measure between two binary vectors. Use binary presence/absence vectors instead of frequencies. Total number of occurences divided by the number of common occurrences, excluding absence/absence couples.", domain = "R-explor")))
                                        ),
                                        tags$p(HTML(gettext("With some similarities (notably <code>correlation</code> and <code>cosine</code>), it can be interesting to filter terms on a minimal frequency, otherwise terms that would only appear once could appear as highly associated.", domain = "R-explor")))
                               )
                             )
                           )
                         )
                      ),
                      tags$script(explor_corpus_js())),
      

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
                   if (!is.null(input$treat_stopwords) && input$treat_stopwords) {
                     remove <- settings$stopwords
                   } else {
                     remove <- NULL
                   }
                   if (!is.null(input$treat_thesaurus) && input$treat_thesaurus) {
                     thesaurus <- settings$thesaurus
                   } else {
                     thesaurus <- NULL
                   }
                   dtm <- dfm(co(), what = "fastestword", 
                              verbose = FALSE, groups = NULL,
                              tolower = input$treat_tolower, 
                              remove_punct = input$treat_removepunct,
                              remove_numbers = input$treat_rmnum,
                              stem = input$treat_stem,
                              remove = remove,
                              thesaurus = thesaurus,
                              ngrams = input$ngrams)
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
                         HTML(paste(gettext("<strong>Warning :</strong> the following terms are missing from the corpus : <i>", domain = "R-explor"), tmp_terms, "</i>")))
                   }
                 })
                 
                 ## Alert if error in search expression
                 output$evalAlert <- renderUI({
                   input$launch_search
                   e <- terms_query()$error
                   if (!is.null(e)) {
                     div(class = "alert alert-danger",
                         HTML(paste(gettext("<strong>Warning :</strong> Query error : <i>", domain = "R-explor"), e, "</i>")))
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
                   # names(docf) <- c(gettext("Term frequency", domain = "R-explor"),
                   #                  gettext("Documents frequency", domain = "R-explor"),
                   #                  gettext("Documents proportion", domain = "R-explor"))
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
                   names(tmp_dtm) <- c("Group", "n")
                   res <- tmp_dtm %>% group_by(Group) %>% summarise(nb_docs = sum(n), prop_docs = round(nb_docs / n() * 100, 1))
                   res
                 })
                 
                 
                 ## Search term total frequency
                 tab_term_tot <- reactive({
                   tmp_dtm <- terms_query()$res
                   if (is.null(tmp_dtm)) return(NULL)
                   names(tmp_dtm) <- "n"
                   res <- tmp_dtm %>% 
                     summarise(nb_docs = sum(n), prop_docs = round(nb_docs / n() * 100, 1)) %>%
                     mutate(nom = gettext("Total", domain = "R-explor")) %>% select(nom, nb_docs, prop_docs)
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
                     res <- paste0(gettext("<p><strong>Query :</strong> <code>", domain = "R-explor"), input$terms, "</code>.</p>")
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
                     res <- paste0(gettext("<p><strong>Whole corpus :</strong> ", domain = "R-explor"), tab$nb_docs, gettext(" documents (", domain = "R-explor"), tab$prop_docs, "%).</p>")
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
                     if (is.null(terms_query()$res)) return(gettext("<div class='document-content'>No document found.</p>", domain = "R-explor"))
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
                     paste(start, gettext("à", domain = "R-explor"), end, gettext("sur", domain = "R-explor"), nb_docs_term())
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
                         HTML(paste(gettext("<strong>Warning :</strong> term not found in the corpus : <i>", domain = "R-explor"), input$termsim, "</i>")))
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