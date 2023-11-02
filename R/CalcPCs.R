#' Calculate the principal components of the cells
#' @name CalcPCs
#' @description Calculates the first 50 principal components of the cells and stores them.
#' @param mysco The mysco object
#' @return a mysco object with 50 PCs in the pcs slot.
#' @export CalcPCs

CalcPCs <- function(mysco){
  req.data <- mysco@data.scale[mysco@hvgs,]
  pcs <- prcomp(t(req.data),center=FALSE)$x[,1:50]

  rownames(pcs) <- colnames(mysco@data.scale)

  mysco@pcs <- pcs
  mysco
}
