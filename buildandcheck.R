library(roxygen2)

roxygen2::roxygenize('ggmotif')

system('R CMD build ggmotif --binary')

system('R CMD check ggmotif_0.2.0.tar.gz --as-cran')

devtools::install_local('ggmotif_0.2.0.tar.gz')

