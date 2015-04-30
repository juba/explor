# imva
Interactive interfaces for Multivariate Analysis in R (early pre-alpha beginning work in progress).


## Installation

Not on CRAN yet :

    devtools::install_github("juba/imva")
    
## Usage

For the moment there is little you can do, but you can test the interactive interface for visualizing Multiple Correspondance Analysis from the `FactoMineR` package by using the `imca())` function.

Small example :

    library(FactoMineR)
    library(imva)
    library(MASS)
    res.mca <- MCA(farms,quali.sup=4,graph=FALSE)
    imca(res.mca)