#' @title Download pathway from kegg
#' @author xiaowei
#' @description Download pathway from kegg with input the organism. Retrieves all entries from the KEGG database for a set of KEGG identifers.
#' @details Because KEGGREST package only return 10 results at once. So I made it again to get results
#' ignored the numbers of query. So this functions is extended the function KEGGREST::keggGet()
#' @param pathway.list a character vector, details see: KEGGREST::keggList()
#'
#'
#'
#' @return A list wrapping a KEGG flat file.
#'
#'
#'
#' @examples
#' library(XWRpackage)
#' library(KEGGREST)
#' pathway.list <- keggList("pathway", organism = "hsa")
#' kegg.path <- kegg.download(pathway.list = pathway.list[1:11])
#'
#'
#'
#'
#'
#'
###############################################################################
# install packages
###############################################################################

kegg.download <- function(pathway.list){
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", dependencies = TRUE)
  requiredPackages <- c("KEGGREST", "GSEABase")
  newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  if(length(newPackages)) BiocManager::install(newPackages, ask = TRUE)
  suppressPackageStartupMessages(library(KEGGREST))
  suppressPackageStartupMessages(library(GSEABase))
  #因为keggGet()一次最大查询10条，所以这里先将pathway.list分成10*n个，每10个放在pathway.x中
  #确定pathway.x长度
  if (length(pathway.list)%%10 == 0){
    pathway.x.len <- length(pathway.list)%/%10
  }else{
    pathway.x.len <- length(pathway.list)%/%10 + 1
  }
  #pathway.x,每个中包含10个pathway
  pathway.x <- vector(mode = "list", length = pathway.x.len)
  for (i in 1:length(pathway.x)){
    min.x <- 10*(i -1)+1
    max.x <- 10*i
    pathway.x[[i]] <- names(pathway.list)[min.x:max.x]
    rm(min.x,max.x)
  }
  #下载n次，每次下载10个pathway
  pathway.kegg <- list()
  for (i in 1:pathway.x.len){
    pathway.kegg.x <- keggGet(pathway.x[[i]])
    pathway.kegg <- append(pathway.kegg, pathway.kegg.x)
    rm(pathway.kegg.x)
  }

  names(pathway.kegg) <- unlist(lapply(names(pathway.list), function(x){trimws(strsplit(x, ':', fixed = TRUE)[[1]][2])} ))

  return(pathway.kegg)

}
