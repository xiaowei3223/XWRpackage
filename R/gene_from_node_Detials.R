#' @title get gene names from Details property of chromosome
#' @description Because Details property in chromosome have many information about genes. Then I make a function to get gene names from Details property of chromosome
#
#' @param details  the property in chromosome node
#' @param re   default as 'string', means the result is a string, 'vector' means the result is a vector included all gene names.
#' @author Xiaowei
#' @return  A string of genenames or a vector of genenames

gene_from_node_Detials <- function(details, re = "string"){

  details2 = strsplit(details, fixed = TRUE,';')[[1]]
  details3 = details2[grep("gene_name", details2, fixed = TRUE)]
  details4 = unique( gsub("( )gene_name( )", "", details3, fixed = F) )
  details5 = paste0(details4,  collapse = "; " )

  if (re == 'string'){  return (details5)}
  if (re == 'vector'){ return (details4)}
  }
