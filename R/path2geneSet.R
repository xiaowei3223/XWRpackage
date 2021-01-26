#' @title make the result of kegg.download to GeneSet
#' @author xiaowei
#'
#' @description make the result of kegg.download to GeneSet
#' @details We always use GeneSet in GeneSet Enrichment Analysis, so it is important to make those pathway as GeneSet.
#' @param kegg.path   the result of kegg.download()
#'
#'
#'
#' @return A GeneSet object, GeneSet class see GSEABase package.
#'
#'
#'
#' @examples
#' library(XWRpackage)
#' library(KEGGREST)
#' pathway.list <- keggList("pathway", organism = "hsa")
#' kegg.path <- kegg.download(pathway.list = pathway.list[1:11])
#' kegg.geneSet <- path.to.geneSet(kegg.path[[1]])
#'

###############################################################################
# Function -- path.to.GeneSet
# description: make the result of kegg.download to GeneSet
# input: the result of kegg.download
# Output: A GeneSet object
###############################################################################
path2geneSet <- function(kegg.path){
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", dependencies = TRUE)

  requiredPackages <- c("KEGGREST", "GSEABase")
  newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  if(length(newPackages)) BiocManager::install(newPackages, ask = TRUE)
  suppressPackageStartupMessages(library(KEGGREST))
  suppressPackageStartupMessages(library(GSEABase))

  genes <- kegg.path$GENE

  if(!is.null(genes)){
    genelist_entrez <- genes[1:length(genes)%%2 ==1]  #entrez

    gs <- GeneSet(geneIds = as.character(genelist_entrez),
                  geneIdType = EntrezIdentifier(),
                  #organism = 'hsa',
                  collectionType = KEGGCollection(),
                  #longDescription = kegg.path$DESCRIPTION,
                  #shortDescription = names(kegg.path$PATHWAY_MAP),
                  setName = kegg.path$PATHWAY_MAP,
    )
  }else{gs = NULL;print("Not had genes.")}

  return(gs)
}
