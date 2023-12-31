% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MakeUMAP.R
\name{MakeUMAP}
\alias{MakeUMAP}
\title{Calculate a UMAP}
\usage{
MakeUMAP(mysco, nPC = 10)
}
\arguments{
\item{mysco}{The mysco object}

\item{nPC}{The number of PCs to use}
}
\value{
a mysco object with UMAP coordinates in the umap slot.
}
\description{
Calculates a UMP using the first N principal components.
}
