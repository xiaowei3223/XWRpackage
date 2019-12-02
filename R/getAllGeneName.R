#' @title  get all TF, LncRNA, genes in chromosome in GREG
#' @param node_labels  one of "genesInchr", "TF", "LncRNA", "all", "chr1", "chr2", "chr3" ... "chr22", "chrX" and "chrY"
#'
#' @return a data.frame which included geneName, NodeID, NodeLabel and NodeNames
#' @author Xiaowei
#'
#'
#' @examples
#' library(RNeo4j)
#' graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="xiaowei")
#' AllgeneName_TF_LncRNA <- getAllGeneName('all')  #get all nodes that contains TF, lncRNA and gene names
#' TFName <- getAllGeneName('TF')                  #get all nodes of TF names
#' LncRNAName <- getAllGeneName('LncRNA')          #get all nodes of LncRNA names
#' AllgeneName <- getAllGeneName('genesInChr')     #get all nodes of gene names in chromosome
#' chr1 <- getAllGeneName('chr1')                  #get one chromosome nodes of genes names




getAllGeneName = function(node_labels){

  #get all nodes of gene names in chromosome
  if (node_labels == 'genesInChr'){
    query = "
      MATCH (n)
      where exists(n.Details)
      with split(n.Details, ';') AS Details,id(n) AS NodeID, labels(n) AS NodeLabel, n.Name AS NodeNames
      UNWIND Details AS details
      with details, NodeID, NodeLabel, NodeNames
      where details contains 'gene_name'
      with details as genes, NodeID, NodeLabel, NodeNames
      return substring(genes,11) as geneName, NodeID, NodeLabel, NodeNames
  "
    allGenes <- RNeo4j::cypher(graph, query)
  }

  #get all nodes of TF names
  else if (node_labels == 'TF'){
    query = "MATCH (n:TF) RETURN n.Name AS geneName, id(n) AS NodeID, labels(n) AS NodeLabel, n.Name AS NodeNames"
    allGenes <- RNeo4j::cypher(graph, query) }

  #get all nodes of LncRNA names
  else if (node_labels == 'LncRNA'){
    query = "MATCH (n:LncRNA)  RETURN n.Name AS geneName, id(n) AS NodeID, labels(n) AS NodeLabel, n.Name AS NodeNames"
    allGenes <- RNeo4j::cypher(graph, query) }

  #get all nodes that contains TF, lncRNA and gene names
  else if (node_labels == 'all'){
    query = "
      MATCH (n)
      where exists(n.Details)
      with split(n.Details, ';') AS Details,id(n) AS NodeID, labels(n) AS NodeLabel, n.Name AS NodeNames
      UNWIND Details AS details
      with details, NodeID, NodeLabel, NodeNames
      where details contains 'gene_name'
      with details as genes, NodeID, NodeLabel, NodeNames
      return substring(genes,11) as geneName, NodeID, NodeLabel, NodeNames
  "
    GenesInchr <- RNeo4j::cypher(graph, query)

    query = "MATCH (n:TF) RETURN n.Name AS geneName, id(n) AS NodeID, labels(n) AS NodeLabel, n.Name AS NodeNames"
    TF <- RNeo4j::cypher(graph, query)

    query = "MATCH (n:LncRNA)  RETURN n.Name AS geneName, id(n) AS NodeID, labels(n) AS NodeLabel, n.Name AS NodeNames"
    LncRNA <- RNeo4j::cypher(graph, query)

    allGenes <- rbind( TF, LncRNA, GenesInchr)
  }

  # get one chromosome nodes of genes names
  else{
    query = paste0("MATCH (n:",
                   node_labels,
                   ")
      where exists(n.Details)
      with split(n.Details, ';') AS Details,id(n) AS NodeID, labels(n) AS NodeLabel, n.Name AS NodeNames
      UNWIND Details AS details
      with details, NodeID, NodeLabel, NodeNames
      where details contains 'gene_name'
      with details as genes, NodeID, NodeLabel, NodeNames
      return substring(genes,11) as geneName, NodeID, NodeLabel, NodeNames
  ")
    allGenes <- RNeo4j::cypher(graph, query ,chr_label = node_labels)
  }
  allGenes = unique( allGenes )

  #return results
  return(allGenes)
}
