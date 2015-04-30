# imva
Interactive interfaces for Multivariate Analysis in R (early pre-alpha beginning work in progress).


## Installation

Obviously not on CRAN yet :

```r
devtools::install_github("juba/imva")
```
    
## Usage

For the moment there is little you can do, but you can test the interactive interface for visualizing Multiple Correspondance Analysis from the `FactoMineR` package by using the `imca())` function.

Small example :

```r
library(FactoMineR)
library(imva)
library(MASS)
mca <- MCA(farms, quali.sup=4, graph=FALSE)
imca(mca)
```
