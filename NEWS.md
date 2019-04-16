# explor 0.3.5

* Upgrade to scatterD3 0.9
* Add automatic labels positioning
* Add biplot to MCA interface


# explor 0.3.4

* Fix improper computation in levels number in FactoMineR::PCA (thanks @Bhavanight)
* Fox compatibility with ade4 1.7-13


# explor 0.3.3

* Fix CRAN tests


# explor 0.3.2

* Compatibility with dplyr 0.7
* Add eigenvalues table beside barplot
* Change points opacity according to contrib or cos2 in PCA/MCA individual plots (suggestion by @ginolhac)
* Add ability to hide individuals points labels based on contribution value (suggestion by @ginolhac)


# explor 0.3.1

* Compatibility with ade4 1.7-5


# explor 0.3.0

* Add a "Get R code" button which allows to get the R code to reproduce the displayed plot (minus custom labels positions)
* Add support for `princomp` and `prcomp`
* Add support for `MASS::mca`
* Add support for `GDAtools::speMCA`
* Qualitative supplementary variables are now displayed with `FactoMineR::PCA` results
* `explor.MCA` now works if MCA has been called with an `excl` argument
* Code refactoring


# explor 0.2.1

* Bugfix : core dump in explor.MCA when only one supplementary qualitative variable
* Bugfix : No variable plot when missing `scale` argument in `dudi.pca`
* Fix test failing with next `testthat` version


# explor 0.2

* Add ability to select points with lasso
* Add ellipses to color mapping variables
* Add ability to color individual points according to one of the qualitative variables in MCA
* Add control to hide Rows or Columns in CA plot and tables
* Add the ability to change point size and sizes range in variables plots for CA and MCA.


# explor 0.1

* First version
