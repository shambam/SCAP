#' Find highly variable genes for further analysis.
#' @name FindHVGs
#' @description Calculate and define HVGs
#' @param mysco The mysco object
#' @param nhvgs The number of hvgs required.
#' @return A mysco object with variance stats and a list of HVGs.
#' @export FindHVGs
#'
FindHVGs <- function(mysco,nhvgs=2000){

  vrs <- log10(apply(pbmc@counts,1,var))
  mns <- log10(apply(pbmc@counts,1,mean))

  vrs[is.infinite(vrs)] <-0
  mns[is.infinite(mns)] <-0

  los <- loess(vrs~mns,span=0.3)

  pred.vars <- 10^los$fitted

  zs <- matrix(0,nrow=nrow(pbmc@counts),ncol=ncol(pbmc@counts))

  for(i in 1:nrow(pbmc@counts)){
    g <- (pbmc@counts[i,]-mean(pbmc@counts[i,])) / sqrt(pred.vars[i])
    zs[i,] <-g
  }

  zs[which(zs>sqrt(ncol(pbmc@counts)))] <- sqrt(ncol(pbmc@counts))
  #zs[which(zs< -sqrt(ncol(pbmc@counts)))] <- -1*sqrt(ncol(pbmc@counts))

  st.var <- apply(zs,1,var)
  var.df <- data.frame(gene.mean=mns,gene.var=st.var)
  rownames(var.df) <- rownames(pbmc@counts)
  pbmc@meta.genes <- var.df
  pbmc@hvgs <- rownames(var.df[rev(order(var.df$gene.var)),])[1:nhvgs]
  pbmc
}


#' Plot the HVGs
#' @name PlotHVGs
#' @description Plot the means vs the standardised variances
#' @param mysco The mysco object

PlotHVGs <- function(mysco){
  plot(mysco@meta.genes$gene.mean,mysco@meta.genes$gene.var,pch=".",cex=2)
  points(mysco@meta.genes[pbmc@hvgs,]$gene.mean,mysco@meta.genes[pbmc@hvgs,]$gene.var,pch=".",cex=2,col="red")
}
