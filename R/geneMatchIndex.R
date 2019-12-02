#' @title find out nodes of target genes/TF/LncRNA
#'
#' @param gene a vector, what genes you want to match.
#' @param allGeneNode allGeneNode is data you chose which one as your match. It must is the results from getAllGeneName function.
#' @param FullMatch  Default is 1. please set argument FullMatch = 0 ,if you want use fuzzy match for those gene names contain strings of your input.
#' @param by  Default is "geneName". one type of "geneName", "NodeID", "NodeLabel" and "NodeNames", it should be as your genes types.
#'
#' @return
#' A list contains `TargetNodes` and `NoGeneIndex`.
#' `TargetNodes` is a frame data of those matched in allGeneNode. `NoGeneIndex` is a vector included those didnot find out in allGeneNode.


geneMatchIndex = function(gene, allGeneNode, FullMatch = 1, by = "geneName"){
  geneIndex = c()
  NoGeneIndex =  c()
  Ag = allGeneNode[,by]
  if (FullMatch == 1){
    #----------which-----------------
    for (i in 1:length(gene) ) {
      geneIndex1 = which(Ag == gene[i] )
      if (length(geneIndex1) == 0){NoGeneIndex = c(gene[i], NoGeneIndex)}
      else{geneIndex = c(geneIndex,geneIndex1)}
    }
  }

  else{
    #----------grep---------------
    for (i in 1:length(gene) ) {
      geneIndex1 = grep(gene[i],Ag, fixed = TRUE)
      if (length(geneIndex1) == 0){NoGeneIndex = c(gene[i], NoGeneIndex)}
      else{geneIndex = c(geneIndex,geneIndex1)}
    }
  }

  TargetNodes <- allGeneNode[geneIndex, ]
  geneInGREG = list(TargetNodes, NoGeneIndex)
  names(geneInGREG) = c("TargetNodes", "NoGeneIndex")
  #返回匹配上的TargetNodes和没有找到基因NoGeneIndex
  return (geneInGREG )
}
