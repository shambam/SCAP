#' Single cells from Peripheral Blood Mononuclear Cells (PBMC)
#'
#' 2,700 single cells that were sequenced on the Illumina NextSeq 500, freely available from 10X Genomics
#'
#' @docType data
#'
#' @usage data(pbmc.data)
#'
#' @format An matrix of PBMC data, genes in rows, cells in columns
#'
#' @keywords datasets
#'
#' @source \href{https://satijalab.org/seurat/articles/pbmc3k_tutorial}{Seurat tutorial}
#'
#' @examples
#' data(pbmc.data)
#' pbmc <- CreateMySCO(as.matrix(pbmc.data))
"pbmc.data"
