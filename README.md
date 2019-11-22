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
