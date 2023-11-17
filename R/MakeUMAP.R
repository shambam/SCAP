#' Calculate a UMAP
#' @name MakeUMAP
#' @description Calculates a UMP using the first N principal components.
#' @param mysco The mysco object
#' @param nPC The number of PCs to use
#' @return a mysco object with UMAP coordinates in the umap slot.
#' @export MakeUMAP

MakeUMAP <- function(mysco,nPC=10){
  ump <- umap::umap(mysco@pcs[,1:nPC])
  mysco@umap <- ump$layout
  mysco
}
