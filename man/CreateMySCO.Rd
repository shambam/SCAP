% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateMySCO.R
\name{CreateMySCO}
\alias{CreateMySCO}
\title{A helper function to make a mysco dataset with minimal meta}
\usage{
CreateMySCO(mat)
}
\arguments{
\item{A}{matrix of expression values. Genes in the rows, cells across columns.}
}
\value{
A class of mysco with single-cell data
}
\description{
Creates a mysco class which has basic QC metrics in the metadata
}
\examples{
pbmc <- CreateMySCO(mat)
}
