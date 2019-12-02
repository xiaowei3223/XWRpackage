# XWRpackage
Myself R package


## install this package:
```
devtools::install_github('xiaowei3223/XWRpackage')
```

## 查找GPL对应的bioconductor注释包,例如
```
GPL_biocPackage(GPL76)
```
如果有的话，会返回包名，如果没有的话，那就真的没有。

## 获取GEO的矩阵表达数据和表型数据以及对应的GPL平台的注释信息
其中矩阵表达数据包含有已经转换好的数据,例子：
```
Y = geo_data("GSE2553")
names(Y$expression_data)
names(Table(Y$GPL_ann))

```
# GREG 函数
## 从GREG中获取基因名/TF/LncRNA
这里试着来获取基因名称、TF、LncRNA的节点，方便可以快速地根据基因名得到对应的节点id号、label
```
library(RNeo4j)
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="xiaowei")
AllgeneName_TF_LncRNA <- getAllGeneName('all')  #get all nodes that contains TF, lncRNA and gene names
TFName <- getAllGeneName('TF')                  #get all nodes of TF names
LncRNAName <- getAllGeneName('LncRNA')          #get all nodes of LncRNA names
AllgeneName <- getAllGeneName('genesInChr')     #get all nodes of gene names in chromosome
chr1 <- getAllGeneName('chr1')                  #get one chromosome nodes of genes names
```

## 根据基因名从上面的结果中找到对应的节点id号等
```
gene <- c('NANOG', 'CTCF', 'EP300', 'POLR2A', 'YY1', 'RAD21', 'SMC3', 'STAG1', 'MED1', 'MED12')
geneNodes = geneMatchIndex(gene,TFName)
```
## 根据染色体上的位置（范围）获取对应的节点和基因名称
```
library(RNeo4j)
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="xiaowei")
data("g123")
g1_nodes <- map_nodes_by_genomic_region(g1)
g2_nodes <- map_nodes_by_genomic_region(g2)
g3_nodes <- map_nodes_by_genomic_region(g3)
```
## 查看两染色体上的两位置是否有基因间相互作用
```
library(RNeo4j)
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="xiaowei")
data("g123")
g1_rel <- map_rel_of_both_genomic_region(g1)
```

## 获取某个染色体上基因范围内所有有关系的基因 （Interaction, Bind)
```
library(RNeo4j)
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="xiaowei")
data("g123")
g2_rel_dna_dna <- map_rel_by_genomic_region(g2)
g3_rel_dna_TF.LncRNA <- map_rel_by_genomic_region(g3, rel = "Bind")
```

## 从details中获取节点中的所有基因名称
```
query = "match (B:chrY{Name:'Bin28428'})
        return  B.Details AS B_Details "
details <- cypher(graph, query)
gene_from_node_Detials(details[1,])
```