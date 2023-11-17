#' A helper function to make a mysco dataset with minimal meta
#' @name CreateMySCO
#' @description Creates a mysco class which has basic QC metrics in the metadata
#' @param A matrix of expression values. Genes in the rows, cells across columns.
#' @return A class of mysco with single-cell data
#' @export CreateMySCO
#' @examples
#' pbmc <- CreateMySCO(mat)

CreateMySCO <- function(mat){

  features <- apply(mat >0, 2, sum)
  nCount <- apply(mat,2,sum)

  df <- data.frame(features=features,nCount=nCount)

  msc <- new("mysco",meta=df,counts=mat)
  msc
}
