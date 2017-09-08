
##' @rdname prepare_results
##' @aliases prepare_results.Corpus
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @seealso \code{\link[tm]{Corpus}}
##' @import quanteda
##' @export

prepare_results.Corpus <- function(obj) {
  
  if (!inherits(obj, "Corpus")) stop("obj must be of class Corpus")
  
  ## convert to quanteda corpus
  obj <- quanteda::corpus(obj)
  
  return(prepare_results(obj))
}


##' @rdname prepare_results
##' @aliases prepare_results.corpus
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @seealso \code{\link[quanteda]{corpus}}
##' @import quanteda
##' @export

prepare_results.corpus <- function(obj) {
    
    if (!inherits(obj, "corpus")) stop("obj must be of class corpus")

    return(obj)
}


