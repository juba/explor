library(tm)
library(quanteda)

## From tm
data("acq")
class(acq)
res_brut <- acq
res <- tm_map(acq, removeWords, stopwords('english'))
explor(res, res_brut)




## From quanteda
data("data_corpus_inaugural")
class(data_corpus_inaugural)
res <- data_corpus_inaugural
explor(res)
