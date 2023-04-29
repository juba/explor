# explor 0.3.10

- Fix varsup and indsup functions renamed to supvar and supind in GDAtools
- Fix duplicated level names in supplementary variables in speMCA
- Fix warnings in Font Awesome icon names (#39, thanks @jl5000)
- Fix supplementary variables not displayed in individual plot for speMCA (thanks @419kfj)

# explor 0.3.9

- Compatibility with GDAtools 1.7

# explor 0.3.8

- Add `speMCA_varsup` function and support for supplementary variables in `GDAtools::speMCA` results
- Fix supplementary variables not showing in MCA biplot

# explor 0.3.7

- Fix percentage of variance computation for prcomp() and princomp() (thanks @zenn1989)
- Fix conditional use of suggested packages

# explor 0.3.6

- Add support for textmodel_ca
- Fix supplementary elements in dudi.coa
- Change supplementary variables / individuals handling in dudi.* functions. You now have to supply the entire suprow() ou supcol() result instead if sub-elements `$cosup` and `$lisup`.
- Fix individual plot point coloration by supplementary variable in dudi.mca
- Add ability to select which supplementary variable to display
- Add support for qualitative supplementary variables in FactoMineR::CA
- Fix error when color on "None" in MCA biplot
- Add ability to prepend variable name to labels in MCA variable plot (thanks @larmarange)

# explor 0.3.5

- Upgrade to scatterD3 0.9
- Add automatic labels positioning
- Add biplot to MCA interface

# explor 0.3.4

- Fix improper computation in levels number in FactoMineR::PCA (thanks @Bhavanight)
- Fox compatibility with ade4 1.7-13

# explor 0.3.3

- Fix CRAN tests

# explor 0.3.2

- Compatibility with dplyr 0.7
- Add eigenvalues table beside barplot
- Change points opacity according to contrib or cos2 in PCA/MCA individual plots (suggestion by @ginolhac)
- Add ability to hide individuals points labels based on contribution value (suggestion by @ginolhac)

# explor 0.3.1

- Compatibility with ade4 1.7-5

# explor 0.3.0

- Add a "Get R code" button which allows to get the R code to reproduce the displayed plot (minus custom labels positions)
- Add support for `princomp` and `prcomp`
- Add support for `MASS::mca`
- Add support for `GDAtools::speMCA`
- Qualitative supplementary variables are now displayed with `FactoMineR::PCA` results
- `explor.MCA` now works if MCA has been called with an `excl` argument
- Code refactoring

# explor 0.2.1

- Bugfix : core dump in explor.MCA when only one supplementary qualitative variable
- Bugfix : No variable plot when missing `scale` argument in `dudi.pca`
- Fix test failing with next `testthat` version

# explor 0.2

- Add ability to select points with lasso
- Add ellipses to color mapping variables
- Add ability to color individual points according to one of the qualitative variables in MCA
- Add control to hide Rows or Columns in CA plot and tables
- Add the ability to change point size and sizes range in variables plots for CA and MCA.

# explor 0.1

- First version
