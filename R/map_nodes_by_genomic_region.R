#' @title input genomic region to get the node
#'
#' @description  This function help you get the node about your input genomic region
#input:
#' @param g1  data.frame  likes g1 object which included nodellabel, start_number and end_number columns
#' @return  A data.frame  included all nodes' information about your input data
#'
#' @author Xiaowei
#' @examples
#' library(RNeo4j)
#' graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="xiaowei")
#' data("g123")
#' g1_nodes <- map_nodes_by_genomic_region(g1)
#' g2_nodes <- map_nodes_by_genomic_region(g2)
#' g3_nodes <- map_nodes_by_genomic_region(g3)


map_nodes_by_genomic_region <- function(g1){

  nodelist <- list()
  for (i in 1:nrow(g1)){
    label = g1$nodelabel[i]
    start_number = g1$start_number[i]
    end_number = g1$end_number[i]
    query = paste0("MATCH(A:",label, ")
                  where toInt(A.Start) <= ",end_number, " and toInt(A.End) >= ", start_number,
                   "
                 RETURN id(A) AS NodeID,
                      labels(A) AS NodeLabel,
                      properties(A) AS properties
    ")
    chr_node <- cypher(graph, query)
    library(neo4r)
    nodesdetails <- neo4r::unnest_nodes(chr_node, what = "properties")

    nodesdetails$gene_name <- unlist(lapply(nodesdetails$Details, FUN = gene_from_node_Detials))

    nodelist[[i]] <- nodesdetails
  }

  names(nodelist) <- g1$genename

  return(nodelist)

}
