#' Calculate a Leiden clustering of the cells
#' @name ClusterCells
#' @description Calculates a Leiden clustering of the cells after construction of an SNN
#' @param mysco The mysco object
#' @param nPC The number of PCs to use
#' @param nK The number of neigbours
#' @param res The resolution of the clustering. Default is 1
#' @return a mysco object with clusters in the cluster slot.
#' @export ClusterCells

ClusterCells <- function(mysco,nPC=10,nK,res=1){

  snn <- RANN::nn2(mysco@pcs[,1:nPC], k=nK)$nn.idx # calculate the graph using 30 max nearest neighnours using he first 10 principal components
  adjacency_matrix <- matrix(0L, nrow(mysco@pcs), nrow(mysco@pcs)) # make an empty cell x cells matrix

  rownames(adjacency_matrix) <- colnames(adjacency_matrix) <- colnames(mysco@data.scale) # names the rows and the columns

  #populated the adjacency matrix saying which cells are connected to which other cells using the calculated weights.
  for(ii in 1:nrow(mysco@pcs)) {
    adjacency_matrix[ii,rownames(mysco@pcs)[snn[ii,]]] <- 1L
  }
  #check that rows add to k
  #sum(adjacency_matrix[1,]) == 30
  #table(apply(adjacency_matrix, 1, sum))

  clus <- igraph::cluster_louvain(igraph::graph_from_adjacency_matrix(adjacency_matrix,mode ="undirected"),resolution = res)$membership
  mysco@clusters <- clus
  mysco
}





