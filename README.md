# explor

[![Travis-CI Build Status](https://travis-ci.org/juba/explor.svg?branch=master)](https://travis-ci.org/juba/explor)

`explor` is an R package to allow interactive exploration of multivariate analysis results.

For now on, it is usable the following types of analyses :

Analysis | Function  | Package | Notes
------------- | ------------- | ---------- | --------
Principal component analysis  | PCA  | [FactoMineR](http://factominer.free.fr/) | Qualitative supplementary variables are ignored
Correspondance analysis  | CA  | [FactoMineR](http://factominer.free.fr/) | -
Multiple correspondence analysis  | MCA  | [FactoMineR](http://factominer.free.fr/) | -
Principal component analysis  | dudi.pca  | [ade4](https://cran.r-project.org/web/packages/ade4/) | Qualitative supplementary variables are ignored
Correspondance analysis  | dudi.coa  | [ade4](https://cran.r-project.org/web/packages/ade4/)  | -
Multiple correspondence analysis  | dudi.acm  | [ade4](https://cran.r-project.org/web/packages/ade4/) | Quantitative supplementary variables are ignored


## Features

For each type of analysis, `explor` launches a `shiny` interactive Web interface which is displayed inside RStudio or in your system Web browser. This interface provides both numerical results as dynamic tables (sortable and searchable thanks to the `DT` package) and interactive graphics thanks to the [scatterD3](https://github.com/juba/scatterD3) package. You can zoom, drag labels, hover points to display tooltips, hover legend items to highlights points, and the graphics are fully updatable with animations which can give some visual clues. You can also export the current plot as an SVG file.

Here is a preview of what you will get (note that real colors are much better than those in this GIF) :

![example](https://raw.github.com/juba/explor/master/resources/screencast_0.1.gif) 


## Installation

The package is not on CRAN yet, so you'll have to install it from GitHub :

```r
install.packages("devtools")
devtools::install_github("juba/scatterD3")
devtools::install_github("juba/explor")
```
    
## Usage

Usage is very simple : you just apply the `explor` function to the result of one of the supported analysis functions.

Example with a principal correspondence analysis from `FactoMineR::PCA` :

```r
library(FactoMineR)
library(explor)

data(decathlon)
pca <- PCA(decathlon[,1:12], quanti.sup = 11:12, graph = FALSE)
explor(pca)
```

Example with a multiple correspondence analysis from `FactoMineR::MCA`:

```r
data(hobbies)
mca <- MCA(hobbies[1:1000,c(1:8,21:23)],quali.sup = 9:10, quanti.sup = 11, ind.sup = 1:100)
explor(mca)
```

## Documentation and localization

Two vignettes are provided for more detailed documentation :

- [English introduction vignette](https://github.com/juba/explor/blob/master/vignettes/introduction_en.Rmd)
- [French introduction vignette](https://github.com/juba/explor/blob/master/vignettes/introduction_fr.Rmd)

Depending on your system locale settings, the interface is displayed in english or french (other languages can be easily added).
