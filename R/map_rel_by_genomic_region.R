#' @title get all bind relationships with a same node via define a genomic regions, relationship and celltype
#' @description  In this function, we can get interaction or bind relationships with a same nodes in different cell.
#' When you choose Interation relationship, the path is (A)-[r1:Inclusion]->(B)-[r2:Interaction]-(C)<-[r3:Inclusion]-(D)
#' In this path relationship, A and D nodes are chromosome nodes In GREG, B and C nodes are chromosome Range nodes.
#' When you choose Bind relationship, the path is (A)-[r:Bind]-(B)
#' In this path relationship, A nodes are chromosome nodes In GREG,but B nodes are TF or LncRNA nodes.
#'       B_label, B_Start, B_End, C_label,C_Start, C_End, D_label and D_Name
#' @author Xiaowei
#'
#' @param g1  A data.frame  likes g1 object which included nodellabel, start_number and end_number columns
#' @param celltype You can choose celltype one of "'MCF7'", "'K562'", "'IMR90'", "'A549'", "'HELA'", "'H1ESC'", "'IPS6.9'" and "'IPS19.11'"
#' @param rel default as "Interaction", you can choose "Interaction" or "Bind"
#' @return results is a list about your input data, and each genename in your input data as a inside list which has two data.frame.
#'                   one is ADRelationship object,
#'                   the other is ADRelationship_summary object, which sum of ADRelationship object.
#'
#' @examples
#' library(RNeo4j)
#' graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="xiaowei")
#' data("g123")
#' g2_rel_dna_dna <- map_rel_by_genomic_region(g2)
#' g3_rel_dna_TF.LncRNA <- map_rel_by_genomic_region(g3, rel = "Bind")




map_rel_by_genomic_region = function(g1, rel = "Interaction", celltype = "'IMR90'"){

  dd_inter <- list()
  for (i in 1:nrow(g1)){
    label = g1$nodelabel[i]
    start_number = g1$start_number[i]
    end_number = g1$end_number[i]
    #----------------------------------------------------------------------------------------------------------------------------------------
    # When relationship is Interaction
    #----------------------------------------------------------------------------------------------------------------------------------------
    if (rel == "Interaction"){

      query = paste0(" MATCH(A:", label,")-[r1:Inclusion]->(B)-[r2:", rel, "]->(C)<-[r3:Inclusion]-(D)
                       where toInt(A.Start) <= ",end_number, " and toInt(A.End) >= ", start_number, " and r2.CellType =",celltype,
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


      ADRelationship <- cypher(graph, query)

      if ( is.null(ADRelationship) ){
        dd_inter[[i]] = paste0("No_", rel,"_relationship")
      }else{

        ADRelationship$A_gene_name <- unlist(lapply(ADRelationship$A_Details, FUN = gene_from_node_Detials))
        ADRelationship$D_gene_name <- unlist(lapply(ADRelationship$D_Details, FUN = gene_from_node_Detials))
        ADRelationship_summary <- ADRelationship
        ADRelationship_summary$A_node <- paste0(ADRelationship$A_label,': ', ADRelationship$A_Name)
        ADRelationship_summary$B_node <- paste0(ADRelationship$B_label,': { Start: ', ADRelationship$B_Start, ' End: ', ADRelationship$B_End, '}')
        ADRelationship_summary$C_node <- paste0(ADRelationship$C_label,': { Start: ', ADRelationship$C_Start, ' End: ', ADRelationship$C_End, '}')
        ADRelationship_summary$D_node <- paste0(ADRelationship$D_label,': ', ADRelationship$D_Name)
        ADRelationship_summary <- ADRelationship_summary[, c("A_node", "B_node", "C_node", "D_node", "A_gene_name", "D_gene_name")]

        dd_inter[[i]] = list(ADRelationship = ADRelationship, ADRelationship_summary = ADRelationship_summary)
      }
    }
    #----------------------------------------------------------------------------------------------------------------------------------------
    # When relationship is bind
    #----------------------------------------------------------------------------------------------------------------------------------------
    if (rel == 'Bind'){
      query = paste0("MATCH(A:", label,")-[r:",  rel,  "]-(B)
          where toInt(A.Start) <= ",end_number, " and toInt(A.End) >= ", start_number, " and r.CellType =",celltype,
                     "
           RETURN labels(A) AS A_label,
                               A.Name AS A_Name,

                               labels(B) AS B_label,
                               B.Name AS B_Name,

                               A.Details AS A_Details
  ")
      ABRelationship <- cypher(graph, query)
      if ( is.null(ABRelationship) ){
        dd_inter[[i]] = paste0("No_", rel,"_relationship")
      }else{

        ABRelationship$A_gene_name <- unlist(lapply(ABRelationship$A_Details, FUN = gene_from_node_Detials))
        ABRelationship_summary <- ABRelationship
        ABRelationship_summary$A_node <- paste0(ABRelationship$A_label,': ', ABRelationship$A_Name)
        ABRelationship_summary$B_node <- paste0(ABRelationship$B_label,': ', ABRelationship$B_Name)
        ABRelationship_summary <- ABRelationship_summary[, c("A_node", "B_node","A_gene_name")]

        dd_inter[[i]] = list(ABRelationship = ABRelationship, ABRelationship_summary = ABRelationship_summary)
      }



    }
  }


  names(dd_inter) <- g1$genename

  return(dd_inter)

}


