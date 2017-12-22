##' @rdname explor
##' @param raw_corpus optional raw documents corpus
##' @param stopwords stopwords character vector
##' @param thesaurus quanteda thesaurus list
##' @aliases explor.Corpus
##' @export

explor.Corpus <- function(obj, raw_corpus = NULL, stopwords = NULL, thesaurus = NULL, ...) {
    
    if (!inherits(obj, "Corpus")) stop(gettext("obj must be of class Corpus", domain = "R-explor"))
    
    ## corpus preparation
    corpus <- prepare_results(obj)
    if (!is.null(raw_corpus)) {
      if (!inherits(raw_corpus, "Corpus")) stop(gettext("raw_corpus must be of class Corpus", domain = "R-explor"))
      raw_corpus <- prepare_results(raw_corpus)
    }
    
    ## Settings
    settings <- list(raw_corpus = raw_corpus,
                     stopwords = stopwords,
                     thesaurus = thesaurus,
                     corpus_name = deparse(substitute(obj)),
                     raw_corpus_name = deparse(substitute(raw_corpus)),
                     stopwords_name = deparse(substitute(stopwords)),
                     thesaurus_name = deparse(substitute(thesaurus)))

    ## Launch interface
    explor_corpus(corpus, settings)
    
}

##' @rdname explor
##' @aliases explor.corpus
##' @export

explor.corpus <- function(obj, raw_corpus = NULL, stopwords = NULL, thesaurus = NULL, ...) {
    
    if (!inherits(obj, "corpus")) stop(gettext("obj must be of class corpus", domain = "R-explor"))

    ## corpus preparation
    corpus <- prepare_results(obj)
    if (!is.null(raw_corpus)) {
      if (!inherits(raw_corpus, "corpus")) stop(gettext("raw_corpus must be of class corpus", domain = "R-explor"))
      raw_corpus <- prepare_results(raw_corpus)
    }
    
    ## Settings
    settings <- list(raw_corpus = raw_corpus,
                     stopwords = stopwords,
                     thesaurus = thesaurus,
                     corpus_name = deparse(substitute(obj)),
                     raw_corpus_name = deparse(substitute(raw_corpus)),
                     stopwords_name = deparse(substitute(stopwords)),
                     thesaurus_name = deparse(substitute(thesaurus)))

    ## Launch interface
    explor_corpus(corpus, settings)
    
}



## Custom CSS
explor_corpus_css <- function() {
  shiny::HTML("
              #filters .shiny-input-checkboxgroup .shiny-options-group,
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

              #filters {
                  margin-bottom: 30px;
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
              ")
    }

# Custom JavaScript
explor_corpus_js <- function() {
  shiny::HTML("
(function($) {

var triggered = 0;

$('#filters').on('shiny:visualchange', function(event) {

  triggered += 1;

  if (triggered == 5) {
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
explor_corpus_highlight <- function(x, str, ngrams) {
  if (!identical(ngrams, 1)) {
    str <- stri_replace_all_fixed(str, pattern = "_", replacement = " ")
  }
  stringi::stri_replace_all_fixed(x, pattern = str,  replacement = paste0("<span class='highlight'>",str,"</span>"),
                         vectorize_all = FALSE,
                         opts_fixed = stri_opts_fixed(case_insensitive = TRUE))
}


##' @import shiny
##' @import quanteda
##' @importFrom highr hi_html
##' @importFrom SnowballC getStemLanguages
##' @importFrom formatR tidy_source

explor_corpus <- function(qco, settings) { 
    

    ## Document level variables
    vars <- lapply(docvars(qco), unique)
    nvalues <- lapply(vars, length)
    vars <- vars[nvalues > 1 & nvalues < 100]

    ## n-grams
    m_ngrams <- 1:5
    names(m_ngrams) <- paste0(1:5, "-gram")
    
    ## Document corpus choices
    doc_corpus_choices <- "clean"
    tmp_names <- gettext("Clean corpus", domain = "R-explor")
    if (!is.null(settings$raw_corpus)) {
      doc_corpus_choices <- c(doc_corpus_choices, "raw")
      tmp_names <- c(tmp_names, gettext("Raw corpus", domain = "R-explor"))
      names(doc_corpus_choices) <- tmp_names
    }

    ## Location terms type choices
    loc_type_choices <- c("words", "sentence")
    names(loc_type_choices) <- c(gettext("Words", domain = "R-explor"),
                                 gettext("Sentence", domain = "R-explor"))

    shiny::shinyApp(
      
      ui = navbarPage(gettext("Corpus", domain = "R-explor"),
                      header = tags$head(                        
                        tags$style(explor_text_css()),
                        tags$style(explor_corpus_css())),
                      
                      ## "DTM EXPLORATION" tab --------------------------------------------
                      
                      tabPanel(gettext("DTM exploration", domain = "R-explor"),
                         sidebarLayout(
                           
                           ## Sidebar ---------------------------------------------------------------
                           
                           sidebarPanel(id = "sidebar",
                                        h4(gettext("Corpus treatment", domain = "R-explor")),
                                        div(id = "treatment",
                                            checkboxInput("treat_tolower", gettext("Convert to lowercase", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_numbers", gettext("Remove numbers", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_punct", gettext("Remove punctuation", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_symbols", gettext("Remove symbols", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_twitter", gettext("Remove Twitter", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_hyphens", gettext("Remove hyphens", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_url", gettext("Remove URLs", domain = "R-explor"), value = TRUE),
                                            if (!is.null(settings$stopwords)) 
                                              checkboxInput("treat_stopwords", gettext("Remove stopwords", domain = "R-explor"), value = TRUE),
                                            if (!is.null(settings$thesaurus)) 
                                              checkboxInput("treat_thesaurus", gettext("Apply thesaurus", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_stem", gettext("Stem words", domain = "R-explor"), value = FALSE),
                                            conditionalPanel("input.treat_stem",
                                                             selectInput("treat_stem_lang", gettext("Stemming language", domain = "R-explor"),
                                                                         choices = SnowballC::getStemLanguages(), selected = "english"), width = "50%")
                                        ),
                                        checkboxGroupInput("ngrams",
                                                           gettext("Ngrams", domain = "R-explor"),
                                                           m_ngrams,
                                                           selected = 1),
                                        h4(gettext("Corpus filtering", domain = "R-explor")),
                                        p(gettext("If nothing is selected, no filter is applied.", domain = "R-explor")),
                                        uiOutput("filters"),
                                        numericInput("term_min_occurrences", gettext("Filter terms on minimal frequency", domain = "R-explor"), 0, 0, 1000, 1),
                                        tags$p(actionButton("get_r_code",
                                                            class = "btn-success",
                                                            icon = icon("code"),
                                                            label = gettext("Get R code", domain = "R-explor"))),
                                        tags$script(explor_corpus_js())
                           ),
                           mainPanel(
                             tabsetPanel(
                               
                               ## "Frequent terms" tab -----------------------------------------------
                               
                               tabPanel(gettext("Frequent terms", domain = "R-explor"),
                                        h3(gettext("Most frequent terms", domain = "R-explor")),
                                        p(HTML("<strong>", gettext("Number of documents", domain = "R-explor"), "&nbsp;:</strong>"), textOutput("nbdocs", inline = TRUE)),
                                        DT::dataTableOutput("freqtable")),
                               
                               ## "Terms search" tab --------------------------------------------------   
                               
                               tabPanel(gettext("Terms search", domain = "R-explor"),
                                        h3(gettext("Terms search", domain = "R-explor")),
                                        HTML("<p>", gettext('Enter one or more terms. You can use logical operators like <code>&</code> ("and"), <code>|</code> ("or"), <code>!</code> ("not") and parentheses :', domain = "R-explor"), "</p>"),
                                        fluidRow(
                                          column(8, textInput("terms", gettext("Terms", domain = "R-explor"), width = "100%")),
                                          column(4, selectInput("term_group",
                                                                gettext("Group by", domain = "R-explor"),
                                                                choices = c("none", names(vars))))),
                                          uiOutput("termsAlert"),
                                          uiOutput("evalAlert"),
                                          h3(gettext("Selected terms frequency", domain = "R-explor")),
                                          htmlOutput("freqterm_query"),
                                          htmlOutput("freqterm_total"),
                                          conditionalPanel("input.term_group != 'none'",
                                                           tabsetPanel(type = "pills",
                                                                       tabPanel(gettext("Table", domain = "R-explor"),
                                                                                DT::dataTableOutput("freqtermtable")
                                                                       ),
                                                                       tabPanel(gettext("Plot", domain = "R-explor"),
                                                                                tags$p(htmlOutput("freqtermplottext")),
                                                                                plotOutput("freqtermplot")
                                                                       )
                                                           )
                                          ),
                                          conditionalPanel("input.term_group == 'none'",
                                                           div(style = "display: none;",
                                                               numericInput("start_documents", gettext("From", domain = "R-explor"), value = 1)),
                                                           fluidRow(
                                                             if (!is.null(settings$raw_corpus)) {
                                                               column(4,
                                                                      selectInput("doc_corpus", 
                                                                                  gettext("Display documents from", domain = "R-explor"), 
                                                                                  choices = doc_corpus_choices))
                                                             },
                                                             column(4,
                                                                    selectInput("doc_display", 
                                                                                gettext("Display", domain = "R-explor"),
                                                                                choices = c("Documents", "Kwics")))
                                                           ),
                                                           fluidRow(
                                                             column(4,
                                                                    checkboxInput("doc_metadata", gettext("Display metadata", domain = "R-explor"), value = TRUE))
                                                           ),
                                                           div(class = "inline-small form-inline",
                                                               actionButton("prev_documents", gettext("Previous", domain = "R-explor"), icon("arrow-left")),
                                                               textOutput("documents_pagination"),
                                                               actionButton("next_documents", gettext("Next", domain = "R-explor"), icon("arrow-right")),
                                                               numericInput("nb_documents_display", gettext(" Number : ", domain = "R-explor"), 
                                                                            value = 10, min = 1, max = 100, step = 1, width = "auto")),
                                                           htmlOutput("documenttable")
                                          )
                               ),
                             
                               
                       

                                                              
                               ## "Help" tab -----------------------------------------------
                               
                               tabPanel(gettext("Help", domain = "R-explor"),
                                        h2(gettext("Help", domain = "R-explor")),
                                        
                                        h3(gettext("Most frequent terms", domain = "R-explor")),
                                        p(HTML(gettext("How to read the table :", domain = "R-explor"))),
                                        tags$ul(
                                          tags$li(HTML(gettext("<code>Term frequency</code> : number of times this term is found in the selected corpus", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>Number of documents</code> : number of documents in the selected corpus in which this term appears at least once", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>Percentage of documents</code> : percentage of documents in the selected corpus in which this term appears at least once", domain = "R-explor")))
                                        ),
                                        
                                        h3(gettext("Terms search", domain = "R-explor")),
                                        p(HTML(gettext("Allows to search for terms or terms combinations in the selected corpus, and to display both frequencies and the corresponding documents. Note that the search is made on the cleaned corpus (after filtering, stemming, removing of stopwords, etc.). Also note that highlighting is not perfect : for example, if searching for <code>I</code>, every \"i\" in the documents will be highlighted, but the search has been made only on the word \"I\".", domain = "R-explor"))),
                                        p("Query examples :"),
                                        tags$ul(
                                          tags$li(HTML(gettext("<code>I</code> : search for documents with the term \"I\" (or \"i\")", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>!i</code> : search for documents without the term \"I\"", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>i | me | we</code> : search for documents with \"i\", \"me\" or \"we\" (or any combination)", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>i & we</code> : search for documents with both \"i\" and \"we\"", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>sky & (sea | ocean)</code> : search for documents with \"sky\" and the terms \"sea\" or \"ocean\" (or both)", domain = "R-explor")))
                                        )
                                        
                               )
                             )
                           )
                         )
                      ),
                      
                      ## "TERMS LOCATION" tab --------------------------------------------
                      
                      tabPanel(gettext("Terms location", domain = "R-explor"),
                               h3(gettext("Terms location", domain = "R-explor")),
                               HTML("<p>", gettext('Enter one or more terms :', domain = "R-explor"), "</p>"),
                               fluidRow(
                                 column(8, textInput("location_terms", gettext("Terms", domain = "R-explor"), width = "100%")),
                                 column(4, radioButtons("location_terms_type", label = NULL,
                                                        choices = loc_type_choices))),
                               uiOutput("loctermsAlert"),
                               tabsetPanel(type = "pills",
                                           tabPanel(gettext("Kwics", domain = "R-explor"),
                                                    DT::dataTableOutput("loctermtable")
                                                    
                                           ),
                                           tabPanel(gettext("Position plot", domain = "R-explor"),
                                                    tags$p(htmlOutput("loctermplottext")),
                                                    plotOutput("loctermplot")
                                           )            
                               )
                      )
      ),
                            

      server = function(input, output, session) {
        
        
        ## Variable inputs
        output$filters <- renderUI({
          
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
          return(filter_inputs)
        })
        
        
        
        ## Corpus filtering R Code
        filtering_code <- reactive({
          code <- ""
          metas <- grep("^meta_", names(input), value = TRUE)
          for (meta in metas) {
            if (is.null(input[[meta]])) next()
            varname <- gsub("^meta_", "", meta)
            var <- docvars(qco)[[varname]]
            selected_values <- input[[meta]]
            test <- ""
            ## Date variables (date range)
            if (inherits(var, "Date")) {
              if (selected_values[1] != min(var) || selected_values[2] != max(var)) {
                test <- sprintf("%s >= %s & %s <= %s", varname, selected_values[1], varname, selected_values[2])
              }
            }
            ## Character variables (checkboxes)
            if (is.character(selected_values)) {
              selected_values[selected_values == "NA"] <- NA
              selected_values <- utils::capture.output(dput(selected_values))
              test <- paste0(varname, " %in% ", selected_values)
            }
            ## Numeric variables (range)
            if (is.numeric(selected_values)) {
              if (selected_values[1] != min(var) || selected_values[2] != max(var)) {
                test <- sprintf("%s >= %s & %s <= %s", varname, selected_values[1], varname, selected_values[2])
              }
            } 
            if (test != "") {
              code <- paste0(code, sprintf("tmp_corpus <- corpus_subset(tmp_corpus, %s)\n", test))
            }
          }
          code
        })
        corpus_filtering_code <- function(corpus_name) {
          code <- filtering_code()
          if (code != "") {
            code <- paste0("tmp_corpus <- ", corpus_name, "\n", code)
          }
          return(code)
        }   
        
        ## Create logical vector from selected filter elements
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
          code <- corpus_filtering_code("qco")
          if (code != "") {
            eval(parse(text = code))
            return(tmp_corpus)
          } else {
            return(qco)
          }
        })
        
        ## Raw corpus filtered
        raw_co <- reactive({
          if (is.null(settings$raw_corpus)) return(co())
          code <- corpus_filtering_code(settings$raw_corpus)
          if (code != "") {
            eval(parse(text = code))
            return(tmp_corpus)
          } else {
            return(settings$raw_corpus)
          }
        })
        
        ## Tokenization code
        tokens_code <- reactive({
          code <- paste0("corpus_tokens <- tokens(%s, what='word'",
                         ", remove_punct = ", input$treat_remove_punct, 
                         ", remove_symbols = ", input$treat_remove_symbols, 
                         ", remove_twitter = ", input$treat_remove_twitter, 
                         ", remove_hyphens = ", input$treat_remove_hyphens, 
                         ", remove_url = ", input$treat_remove_url, 
                         ", remove_numbers = ", input$treat_remove_numbers, ")")
          if (input$treat_tolower) {
            code <- paste0(code, "\n", 
                           "corpus_tokens <- tokens_tolower(corpus_tokens)")
          }
          if (!is.null(input$treat_thesaurus) && input$treat_thesaurus) {
            code <- paste0(code, "\n", 
                           "corpus_tokens <- tokens_lookup(corpus_tokens, dictionary(%s), exclusive = FALSE)")
          }
          if (!is.null(input$treat_stopwords) && input$treat_stopwords) {
            code <- paste0(code, "\n", 
                           "corpus_tokens <- tokens_remove(corpus_tokens, %s)")
          }
          ngrams <- utils::capture.output(dput(as.numeric(input$ngrams)))
          if (ngrams != 1) {
            code <- paste0(code, "\n", 
                           "corpus_tokens <- tokens_ngrams(corpus_tokens, n = ", ngrams, ")")
          }
          code
        })
        ## dfm computation code
        dfm_code <- reactive({
          ngrams <- utils::capture.output(dput(as.numeric(input$ngrams)))
          code <- paste0("dfm <- dfm(corpus_tokens, tolower = FALSE)\n")
          if (input$treat_stem) {
            code <- paste0(code, "dfm <- dfm_wordstem(dfm, language = '", input$treat_stem_lang, "')\n")
          }
          if (!is.na(input$term_min_occurrences) && input$term_min_occurrences > 0) {
            code <- paste0(code, "dfm <- dfm_trim(dfm, min_count = ", input$term_min_occurrences, ")")
          }
          code
        })
        corpus_dfm_code <- function(corpus_name, stopwords_name, thesaurus_name) {
          out <- sprintf(tokens_code(), corpus_name, thesaurus_name, stopwords_name)
          out <- paste(out, dfm_code(), sep = "\n")
          out
        }
        
        ## Clean corpus dfm
        dtm <- reactive({
          if (length(input$ngrams) == 0 || is.null(co()) || ndoc(co()) == 0) { 
            return(NULL)
          }
          code <- corpus_dfm_code("co()", "settings$stopwords", "settings$thesaurus")
          withProgress(message = gettext("Recomputing dfm", domain = "R-explor"), value = 0.3, {
            eval(parse(text = code))
            incProgress(0.7)
          })
          dfm
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
        
        ### FREQUENT TERMS -----------------------------------------------------------
        
        ## Number of documents
        output$nbdocs <- renderText({
          ndoc(co())
        })
        
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
          names(tab) <- c(gettext("Term", domain = "R-explor"),
                          gettext("Term frequency", domain = "R-explor"),
                          gettext("Number of documents", domain = "R-explor"),
                          gettext("Percentage of documents", domain = "R-explor"))
          DT::datatable(tab, 
                        options = c(tableOptions, order_option(tab, gettext("Term frequency", domain = "R-explor"))), rownames = FALSE)
        })
        
        ### TERMS SEARCH ---------------------------------------------------------------------
        
        ## Terms input
        terms <- reactive({
          tmp <- unlist(stri_extract_all_words(input$terms))
          if (length(tmp) == 1 && is.na(tmp)) return(NULL)
          tmp <- tmp[tmp != ""]
        })
        ## Invalid terms in terms input
        invalid_terms <- reactive({
          tmp_terms <- terms()
          tmp_terms[!(tmp_terms %in% colnames(dtm()))]    
        })
        
        ## Run the query on the document-feature matrix as environment,
        ## and returns the result
        terms_query <- reactive({
          
          ## Progress
          query_progress <- shiny::Progress$new()
          on.exit(query_progress$close())
          query_progress$set(message = gettext("Running query", domain = "R-explor"), value = 0)
          
          error <- NULL
          if (length(terms()) == 0) return(list(res = NULL, error = NULL))
          dfm_terms <- dtm() %>% 
            dfm_select(pattern = terms(), valuetype = "fixed", selection = "keep") %>% 
            as.data.frame()
          query_progress$inc(0.3)
          ## Convert count to presence / absence
          if (ncol(dfm_terms) > 0) {
            dfm_terms[dfm_terms > 0] <- 1
          }
          query_progress$inc(0.1)
          res <- try(
            eval(parse(text = input$terms), envir = dfm_terms) %>% 
              data.frame()
            , silent = TRUE)
          query_progress$inc(0.6)
          if (inherits(res, "try-error")) {
            res <- NULL
            error <- geterrmessage()
          }
          list(res = res, error = error)
        })
        
        ## Alert if term is missing from corpus
        output$termsAlert <- renderUI({
          if (length(invalid_terms() > 0) && invalid_terms() != "") {
            tmp_terms <- paste(invalid_terms(), collapse = ", ")
            div(class = "alert alert-danger",
                HTML(paste(gettext("<strong>Warning :</strong> the following terms are missing from the corpus : <i>", domain = "R-explor"), tmp_terms, "</i>")))
          }
        })
        
        ## Alert if error in search expression
        output$evalAlert <- renderUI({
          if (length(invalid_terms() > 0) && invalid_terms() != "") { return(NULL) }
          e <- terms_query()$error
          if (!is.null(e)) {
            div(class = "alert alert-danger",
                HTML(paste(gettext("<strong>Warning :</strong> Query error : <i>", domain = "R-explor"), e, "</i>")))
          }
        })
        
        ## Search term frequency table
        tab_term <- reactive({
          if (input$term_group == "none") return(NULL)
          tmp_dfm <- terms_query()$res
          if (is.null(tmp_dfm)) return(NULL)
          updateNumericInput(session, "start_documents", value = 1)
          tmp_dfm <- docvars(co()) %>% select_(input$term_group) %>% bind_cols(tmp_dfm)
          names(tmp_dfm) <- c("group", "n")
          res <- tmp_dfm %>% 
            group_by(group) %>% 
            summarise(nb_docs = sum(n), prop_docs = round(nb_docs / n() * 100, 1)) 
          res
        })
        
        
        ## Search term total frequency
        tab_term_tot <- reactive({
          tmp_dfm <- terms_query()$res
          if (is.null(tmp_dfm)) return(NULL)
          names(tmp_dfm) <- "n"
          res <- tmp_dfm %>% 
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
          if (input$terms == "") {
            return("")
          }
          res <- paste0(gettext("<p><strong>Query :</strong> <code>", domain = "R-explor"), input$terms, "</code>.</p>")
          return(HTML(res))
        })
        
        ## Total searched terms frequency text
        output$freqterm_total <- renderText({
          if (is.null(tab_term_tot())) {
            return("")
          }
          tab <- tab_term_tot()
          res <- paste0(gettext("<p><strong>Frequency in corpus :</strong> ", domain = "R-explor"), tab$nb_docs, gettext(" documents (", domain = "R-explor"), tab$prop_docs, "%).</p>")
          return(HTML(res))
        })
        
        ## Searched terms frequency table
        output$freqtermtable <- DT::renderDataTable({
          if (is.null(tab_term()) || nb_docs_term() == 0) {
            return(DT::datatable(data.frame(table = character())))
          }
          tab <- tab_term()
          names(tab) <- c(input$term_group,
                          gettext("Number of documents", domain = "R-explor"),
                          gettext("Percentage of documents", domain = "R-explor"))
          tab <- DT::datatable(tab, 
                               options = c(tableOptions, order_option(tab, gettext("Percentage of documents", domain = "R-explor"))), rownames = FALSE)
          tab
        })
        
        output$freqtermplottext <- renderText({
          if (is.null(tab_term()) || nb_docs_term() == 0) {
            return(gettext("No document found", domain = "R-explor"))
          } else {
            text <- paste0(nb_docs_term(), gettext(" documents found. ", domain = "R-explor"))
          }
          return(HTML(text))
        })
        
        ## Searched terms frequency plot
        output$freqtermplot <- renderPlot({
          if (is.null(tab_term()) || nb_docs_term() == 0) {
            return()
          }
          tab <- tab_term()
          group <- quo(input$term_group)
          var <- docvars(co()) %>% pull(!!group)
          g <- NULL
          if (is.character(var) || is.factor(var)) {
            tab <- tab %>% 
              filter(prop_docs > 0) %>%
              mutate(group = stats::reorder(group, prop_docs))
            g <- ggplot(tab) + 
              geom_bar(aes(x = stats::reorder(group, prop_docs), y = prop_docs), stat = "identity") +
              xlab(input$term_group) +
              ylab(gettext("Percentage of documents", domain = "R-explor")) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              expand_limits(y = 0)
          } 
          if (is.numeric(var) || inherits(var, "Date")) {
            g <- ggplot(tab, aes(x = group, y = prop_docs)) + 
              geom_line() +
              geom_smooth() +
              xlab(input$term_group) +
              ylab(gettext("Percentage of documents", domain = "R-explor")) +
              expand_limits(y = 0)
          }
          g
        })
        
        
        ## Observers on pagination events
        observeEvent(input$prev_documents, {
          updateNumericInput(session, "start_documents", value = max(1, isolate(input$start_documents) - isolate(input$nb_documents_display)), min = 1)
        })
        observeEvent(input$next_documents, {
          val <- isolate(input$start_documents) + isolate(input$nb_documents_display)
          if (val <= nrow(terms_query()$res)) {
            updateNumericInput(session, "start_documents", value = val)
          }
        })
        
        ## Documents list
        output$documenttable <- renderText({
          ## Acquire dependencies
          start <- input$start_documents
          nb_documents <- input$nb_documents_display
          if (is.null(terms_query()$res)) return(gettext("<div class='document-content'>No document found.</p>", domain = "R-explor"))
          indexes <- which(as.vector(terms_query()$res) > 0)
          end <- min(start + nb_documents - 1, nrow(terms_query()$res))
          indexes <- indexes[start:end]
          out <- ""
          for (i in indexes) {
            out <- paste(out, "<div class='document-content'>")
            out <- paste(out, "<p><strong>", rownames(docvars(co()))[i] ,"</strong></p>")
            if (is.null(input$doc_corpus) || req(input$doc_corpus) == "clean") {
              tmp_corp <- co()
            } else { 
              tmp_corp <- raw_co()
            }
            tmp_terms <- terms()
            if (!identical(ngrams, 1)) {
              tmp_terms <- stri_replace_all_fixed(tmp_terms, pattern = "_", replacement = " ")
            }
            if (input$doc_display == "Documents") {
              out <- paste(out, explor_corpus_highlight(tmp_corp[i], tmp_terms, input$ngrams))                       
            }
            if (input$doc_display == "Kwics") {
              tmp_terms <- phrase(tmp_terms)
              kwics <- kwic(tmp_corp[i], pattern = tmp_terms, window = 7, valuetype = "fixed")
              kwics$text <- paste0("...", kwics$pre, " <strong>", kwics$keyword, "</strong> ", kwics$post, "...")
              kwics <- paste(kwics$text, collapse = "<br />")
              out <- paste(out, kwics)
            }
            if (input$doc_metadata) {
              meta <- docvars(co())[i,]
              metadata <- character(0)
              for (m in colnames(meta)) {
                metadata <- append(metadata, paste0("<span>", m, "</span>: <code>", meta[, m], "</code>"))
              }
              metadata <- paste(metadata, collapse = "<br />")
              out <- paste(out, HTML("<p class='metadata'>", metadata, "</p>"))
            }
            out <- paste(out, "</div>")
          }
          HTML(out)
        })
        
        ## documents list pagination text
        output$documents_pagination <- renderText({
          start <- input$start_documents
          nb_documents <- input$nb_documents_display
          end <- min(start + nb_documents - 1, nrow(terms_query()$res))
          if (end == 0) start <- 0
          sprintf(gettext("%s to %s of %s", domain = "R-explor"), start, end, nrow(terms_query()$res))
        })
        
        
        ### TERMS LOCATION ---------------------------------------------------------------------
        
        ## Location terms
        loc_terms <- reactive({
          tmp <- unlist(stri_extract_all_words(input$location_terms))
          if (length(tmp) == 1 && is.na(tmp)) return(NULL)
          tmp <- tmp[tmp != ""]
        })
        
        ## Location terms kwic object
        kwic_loc_terms <- reactive({
          withProgress(message = gettext("Computing Kwics", domain = "R-explor"), value = 0.3, {
            if (is.null(loc_terms())) {
              return(NULL)
            }
            if (input$location_terms_type == "sentence") {
              terms <- phrase(input$location_terms)
            } else {
              terms <- loc_terms()
            }
            kw <- kwic(co(), terms, window = 7)
            incProgress(0.7)
            return(kw)
          })
        })
        
        ## Terms location table
        output$loctermtable <- DT::renderDataTable({
          if (is.null(kwic_loc_terms()) || nrow(kwic_loc_terms()) == 0) {
            return(DT::datatable(data.frame(table = character())))
          }
          tab <- data.frame(kwic_loc_terms())
          tab$text <- paste0(tab$pre, " <strong>", tab$keyword, "</strong> ", tab$post)
          tab <- tab %>% select("docname", "from", "to", "text") 
          tab <- DT::datatable(tab, options = c(tableOptions), rownames = FALSE, escape = FALSE)
          tab
        })
        
        ## Maximum number of documents to display location plot
        kwic_loc_nb_max <- 80
        ## Terms location plot text
        output$loctermplottext <- renderText({
          text <- ""
          kw <- kwic_loc_terms()
          if (is.null(kw) || nrow(kw) == 0) {
            return(gettext("No document found", domain = "R-explor"))
          } 
          nb_kw_docs <- length(unique(kw$docname))
          text <- paste0(nb_kw_docs, gettext(" documents found. ", domain = "R-explor"))
          if (nb_kw_docs > kwic_loc_nb_max) {
            text <- paste0(text,
                           sprintf(gettext("<strong>Only the first %s ones are displayed.</strong>", domain = "R-explor"), kwic_loc_nb_max))
          }
          return(HTML(text))
        })
        ## Terms location plot
        output$loctermplot <- renderPlot({
          kw <- kwic_loc_terms()
          if (is.null(kw) || nrow(kw) == 0) return(NULL)
          nb_kw_docs <- length(unique(kw$docname))
          if (nb_kw_docs > kwic_loc_nb_max) kw <- head(kw, kwic_loc_nb_max)
          withProgress(message = gettext("Creating x-ray plot", domain = "R-explor"), value = 0.3, {
            g <- textplot_xray(kw)
            incProgress(0.7)
            return(g)
          })
        }, height = 650)
        
        
        ### CODE EXPORT ---------------------------------------------------------------------
        
        ## Code export modal dialog
        observeEvent(input$get_r_code, {
          code <- ""
          corpus_name <- settings$corpus_name
          filter_code <- corpus_filtering_code(corpus_name)
          if (filter_code != "") {
            code <- paste0("## ", gettext("Corpus filtering", domain = "R-explor"), "\n")
            code <- paste0(code, filter_code, "\n")
            corpus_name <- "tmp_corpus"
          }
          code <- paste0(code, "## ", gettext("Document-feature matrix computation", domain = "R-explor"), "\n")
          code <- paste0(code, corpus_dfm_code(corpus_name, settings$stopwords_name, settings$thesaurus_name))
          code <- formatR::tidy_source(text = code, 
                                       width.cutoff = 75, 
                                       output = FALSE)$text.tidy
          showModal(modalDialog(
            title = gettext("Export R code", domain = "R-explor"), size = "l", 
            HTML(paste0(gettext("Copy, paste and run the following code in your script to compute the corresponding document-feature matrix (dfm) :", domain = "R-explor"),
                        "<pre><code>",
                        paste(highr::hi_html(code), collapse = "\n"),
                        "</code></pre>")),
            easyClose = TRUE))
        })
        
      })
}