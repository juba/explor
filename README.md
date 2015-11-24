# explor

Interactive interfaces for multivariate analysis results exploration in R (early pre-alpha beginning work in progress).


## Installation

Obviously not on CRAN yet :

```r
devtools::install_github("juba/explor")
```
    
## Usage

For the moment there is little you can do, but you can test the interactive interface for visualizing Multiple Correspondance Analysis from the `FactoMineR` package :

Small example :

```r
library(FactoMineR)
library(explor)
library(MASS)
mca <- MCA(farms, quali.sup=4, graph=FALSE)
explor(mca)
```
