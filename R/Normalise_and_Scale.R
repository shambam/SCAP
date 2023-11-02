#' Normalise the data according to total read count per cell
#' @name NormaliseData
#' @description Normalise the data
#' @param mysco The mysco object
#' @param scale.factor The scale factor which the normalised counts are multipled are.
#' @return a mysco object with normalised data in the data.norm slot.
#' @export NormaliseData

NormaliseData <- function(mysco,scale.factor){
  nd <- t(t(mysco@counts)/apply(mysco@counts,2,sum))*scale.factor
  mysco@data.norm <- log1p(nd)
  mysco
}

#' Scale the data
#' @name ScaleData
#' @description Centralise and standardise the means and variances.
#' @param mysco The mysco object
#' @return a mysco object with normalised data in the data.norm slot.
#' @export ScaleData

ScaleData <- function(mysco){

  zscore <- function(v){

    v <- (v-mean(v))/sd(v)
    v
  }

  cent.sc <- t(apply(mysco@data.norm,1,zscore))
  rownames(cent.sc) <- rownames(mysco@counts)

  cent.sc[which(is.nan(cent.sc)==T)] <- 0

  cent.sc[which(cent.sc > 10)] <- 10

  mysco@data.scale <- cent.sc
  mysco
}
