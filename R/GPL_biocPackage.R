#'  To know what bioconductor package is releted data from GEO database
#'
#' You can easily input GPL aceession and return the annotation package names.
#'
#' @param GPL  character, one GPL aceession
#' @return  a character or NA

#' @examples
#' GPL_biocPackage('GPL20')
#' GPL_biocPackage('GPL33')


GPL_biocPackage = function(GPL){
  #load("data/GPL_bioc_package.RData")
  pkg = GPL_pkg[which(GPL_pkg$gpl == GPL), ]$bioc_package
  rm(GPL_pkg)
  pkg
}

