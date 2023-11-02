#' A header line for your function
#' @name mysco
#' @description A class of mycso
#' @slot meta A data.frame
#' @slot meta,genes A data.frame
#' @slot counts a matrix of raw counts as class matrix
#' @slot data.norm a matrix of normalised and log transformed data
#' @slot data.scale a matrix of scaled and centralised data
#' @slot hvgs a vector of gene names
#' @slot pcs a matrix of the first 50 components
#' @slot clusters a vector of cluster assignments
#'
#' @exportClass mysco

setClass("mysco",slots= c(meta="data.frame",meta.genes="data.frame",counts="matrix",data.norm="matrix",data.scale="matrix",hvgs="vector",pcs="matrix",clusters="vector",umap="matrix"))

setValidity("mysco", function(object) {
  if (ncol(object@counts) != nrow(object@meta)) {
    "@counts and @meta must be the same length"
  } else {
    TRUE
  }
})

