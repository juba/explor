# explor

[![Travis-CI Build Status](https://travis-ci.org/juba/explor.svg?branch=master)](https://travis-ci.org/juba/explor)

`explor` is an R package to allow interactive exploration of multivariate analysis results.

For the moment, it is usable on two types of analyses :

- principal component analysis computed by the `PCA` function of the [FactoMineR package](http://factominer.free.fr/)
- multiple correspondence analysis computed by the `MCA` function of the [FactoMineR package](http://factominer.free.fr/)


## Features

For each type of analysis, `explor` launches a `shiny` interactive Web interface which is launched inside RStudio or in your system Web browser. This interface provides both numerical results as dynamic tables (sortable and searchable thanks to the `DT` package) and interactive graphics thanks to the [scatterD3](https://github.com/juba/scatterD3) package. You can zoom, drag labels, hover points to display tooltips, hover legend items to highlights points, and the graphics are fully updatable with animations which can give some visual clues. You can also export the current plot as an SVG file.

Here is a preview of what you will get :

![example](https://raw.github.com/juba/explor/master/resources/screencast_0.1.gif) 


## Installation

The package is not on CRAN yet, so you'll have to install it from GitHub :

```r
install.packages("devtools")
devtools::install_github("juba/explor")
```
    
## Usage

Usage is very simple : you just apply the `explor` function to the result of one of the supported analysis functions.

Example with a principal correspondence analysis :

```r
library(FactoMineR)
library(explor)

data(decathlon)
pca <- PCA(decathlon[,1:12], quanti.sup = 11:12, graph = FALSE)
explor(pca)
```

Example with a multiple correspondence analysis :

```r
library(FactoMineR)
library(explor)

data(hobbies)
mca <- MCA(hobbies[1:1000,c(1:8,21:23)],quali.sup = 9:10, quanti.sup = 11, ind.sup = 1:100)
explor(mca)
```
