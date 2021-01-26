#' @title  download pathway of one organism from KEGG and make it as GeneSetCollection
#' @author xiaowei
#'
#' @description download pathway of one organism from KEGG and make it as GeneSetCollection
#' @details We always use GeneSet/GeneSetCollection in GeneSet Enrichment Analysis, so it is important to make those pathway as GeneSet.
#' @param organism A KEGG organism code from https://www.kegg.jp/kegg/catalog/org_list.html . for example, human is hsa, Mus musclus(mouse) is mmu.
#' @param outputfile strings for GMT format file. Default as NULL. If it is not NULL, it will save GMT file in your working directory.
#'
#'
#'
#' @return A GeneSetCollection object, GeneSetCollection class see GSEABase package. And GMT file.
#'
#'
#'
#' @examples
#' library(XWRpackage)
#' pathway.list <- keggList("pathway", organism = "hsa")
#' kegg.path <- kegg.download(pathway.list = pathway.list[1:11])
#' kegg.geneSet <- path.to.geneSet(kegg.path[[1]])
#'
#' hsa.geneSet <- kegg.geneSet("hsa") # Homo sapiens (human)
#' ptr.geneSet <- kegg.geneSet("ptr", outputfile = "ptr.gmt") # Pan troglodytes (chimpanzee)
#' ggo.geneSet <- kegg.geneSet("ggo") # Gorilla gorilla gorilla (western lowland gorilla)
#' pon.geneSet <- kegg.geneSet("pon") # Nomascus leucogenys (northern white-cheeked gibbon)
#'
#'
#'
###############################################################################
# install packages
###############################################################################
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager", dependencies = TRUE)

requiredPackages <- c("KEGGREST", "GSEABase")
newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) BiocManager::install(newPackages, ask = TRUE)



suppressPackageStartupMessages(library(KEGGREST))
suppressPackageStartupMessages(library(GSEABase))
###############################################################################
# Function -- kegg.geneSet
# description: download pathway of one organism from KEGG and make it as GeneSetCollection
# input:
# organism: a KEGG organism code (list via keggList("organism"))
# outputfile: name of GMT format file
# output: A GeneSetCollection object or/and GMT file, gene ID is Entrez
###############################################################################

kegg.geneSet <- function(organism = "hsa", outputfile = NULL){
  pathway.list <- keggList("pathway", organism) #获取所有pathway的名称和kegg标识符

  kegg.path <- kegg.download(pathway.list = pathway.list) #download pathway

  kegg.geneSetCollection <- mapply(path.to.geneSet, kegg.path) #pathway to GeneSet

  null.index <- unlist(lapply(kegg.geneSetCollection, is.null)) #remove NULL
  kegg.geneSetCollection <- GeneSetCollection(kegg.geneSetCollection[!null.index])

  #导出为gmt文件
  if (!is.null(outputfile)){toGmt(x=kegg.geneSetCollection, con = outputfile)}

  return(kegg.geneSetCollection)
}
