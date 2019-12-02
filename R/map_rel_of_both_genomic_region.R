#' @title  get the relationship between genomic regions in the same input table.
#'
#' @details   The relationship is (A)-[r1:Inclusion]->(B)-[r2:Interaction]->(C)<-[r3:Inclusion]-(D)
#' So A and D nodes are chromosome nodes In GREG, B and C nodes are chromosome Range nodes.
#'
#' @param g1   A data.frame  likes g1 object which included nodellabel, start_number and end_number columns
#' @param celltype  You can choose celltype one of "'MCF7'", "'K562'", "'IMR90'", "'A549'", "'HELA'", "'H1ESC'", "'IPS6.9'" and "'IPS19.11'"
#' @param rel  Only suppost "Interaction"
#' @author Xiaowei
#'
#' @return
#' A data.frame  which included all relationships information above-mentioned. colnames are A_label,A_Name, B_label, B_Start, B_End, C_label,
#'             C_Start, C_End, D_label and D_Name
#' @examples
#' library(RNeo4j)
#' graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="xiaowei")
#' data("g123")
#' g1_rel <- map_rel_of_both_genomic_region(g1)

map_rel_of_both_genomic_region = function(g1, rel = "Interaction",  celltype = "'IMR90'"){

  g1_nodes <- map_nodes_by_genomic_region(g1)
  nodelist <- data.frame()
  for (i in 1:length(g1_nodes)){nodelist <- rbind(nodelist, g1_nodes[[i]])}

  query = paste0(" MATCH (A)-[r1:Inclusion]->(B)-[r2:", rel, "]->(C)<-[r3:Inclusion]-(D)
                    where id(A) in {nodelist} and id(D) in {nodelist} and r2.CellType = ",celltype,
                 "
                    RETURN labels(A) AS A_label,
                           A.Name AS A_Name,

                           labels(B) AS B_label,
                           B.Start AS B_Start,
                           B.End AS B_End,

                           labels(C) AS C_label,
                           C.Start AS C_Start,
                           C.End AS C_End,

                           labels(D) AS D_label,
                           D.Name AS D_Name,

                           A.Details AS A_Details,
                           D.Details AS D_Details

  ")

  ADRelationship <- cypher(graph, query, nodelist = unlist(nodelist$NodeID))

  return(ADRelationship)
}
