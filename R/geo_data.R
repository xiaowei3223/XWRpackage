#' @title  input GEO accession to get expression, phenotype and annotation data
#' @author xiaowei
#' @description input geo accession and gene id which you want, then this function will help you get all expression data which included gene id you want had converted.
#' @details  if annotation not has your input gene id, then will be only return raw expression data. Also, I should remind you, after mapped gene id, I do not remove those
#' data not successful map the gene id, which is NA I do not remove.
#' @param geo   a character, one geo accession
#' @param geneID  a character, default as "SYMBOL". So you can choose one of "SYMBOL", "ENTREZ" and "RefSeq". If you don't want convert the gene id, this argument set as "NO".
#'
#'
#' @return a list which included expression data, phenotype data, annotation data and GPL number
#'
#'
#'
#' @examples
#' Y = geo_data("GSE2553")
#' names(Y$expression_data)
#' names(Table(Y$GPL_ann))
#' expression_data <- Y$expression_data
#' GPL_ann <- Y$GPL_ann
#' pheno_Data <- Y$pheno_Data
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
geo_data <- function(geo,geneID = 'SYMBOL'){

  if (!requireNamespace("GEOquery", quietly = TRUE)){
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")

    BiocManager::install("GEOquery") }


  library(GEOquery)
  #获取expressionSet
  GEO_data <- getGEO(geo,GSEMatrix=TRUE)

  #show(GEO_data)
  GEO_dataa = GEO_data[[1]]
  #获取表达矩阵
  exp = as.data.frame(exprs(GEO_dataa))

  #获取表型数据
  pd = pData(GEO_dataa)
  #head(pd)

  #获取GPL平台号
  GPL = annotation(GEO_dataa)
  #获取GPL平台号注释信息
  GPL_ann = getGEO(GPL, destdir=".")

  #查看平台号注释信息有哪些
  cn_GPL_ann <- colnames(Table(GPL_ann))
  if(geneID == 'NO'){expr_data <- exp }
  else{
    if (geneID == 'SYMBOL'){geneID = "(S|s)(Y|y)(M|m)(B|b)(O|o)(L|l)"}
    if (geneID == 'ENTREZ'){geneID = "(E|e)(N|n)(T|t)(R|r)(E|e)(Z|z)"}
    if (geneID == 'RefSeq'){geneID = "(R|r)(e|E)(f|F)(S|s)(e|E)(q|Q)"}
    #找到含有目标geneID的那一列的名称
    geneID2 <- grep(geneID, cn_GPL_ann, value = T, ignore.case = FALSE, fixed = FALSE)

    if (length(geneID2) != 0 ){
      #取对应的想要的平台号信息，比如：SYMBOL
      iddata <- Table(GPL_ann)[,c('ID', geneID2)]
      #将探针ID从行名变成一列
      exp$ID <- rownames(exp)
      #转换ID
      expr_data <- merge(x= exp, y=iddata, by = "ID", all.x = T, all.y = F)
      expr_data1 <- expr_data[,c("ID", geneID2)]
      expr_data2 <- expr_data[!( names(expr_data) %in% c("ID", geneID2) )]
      expr_data <- cbind(expr_data1,expr_data2)
      rm(expr_data1, expr_data2, exp)
    }else{
      expr_data <- exp}
  }

  #返回结果
  geo_exprs_pdata <- list(expr_data, pd, GPL, GPL_ann)
  names(geo_exprs_pdata) <- c("expression_data", "pheno_Data", "GPL_number", "GPL_ann")
  return (geo_exprs_pdata)
}


