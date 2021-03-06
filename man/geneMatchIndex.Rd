% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geneMatchIndex.R
\name{geneMatchIndex}
\alias{geneMatchIndex}
\title{find out nodes of target genes/TF/LncRNA}
\usage{
geneMatchIndex(gene, allGeneNode, FullMatch = 1, by = "geneName")
}
\arguments{
\item{gene}{a vector, what genes you want to match.}

\item{allGeneNode}{allGeneNode is data you chose which one as your match. It must is the results from getAllGeneName function.}

\item{FullMatch}{Default is 1. please set argument FullMatch = 0 ,if you want use fuzzy match for those gene names contain strings of your input.}

\item{by}{Default is "geneName". one type of "geneName", "NodeID", "NodeLabel" and "NodeNames", it should be as your genes types.}
}
\value{
A list contains `TargetNodes` and `NoGeneIndex`.
`TargetNodes` is a frame data of those matched in allGeneNode. `NoGeneIndex` is a vector included those didnot find out in allGeneNode.
}
\description{
find out nodes of target genes/TF/LncRNA
}
