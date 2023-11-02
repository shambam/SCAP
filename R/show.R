#' Detail the contents of a mysco object
#' @name show
#' @description  show the contents of a mysco object
#' @param object the cellexalvr object
#' @return nothing
#' @title show the object contents
#' @export show

setMethod('show', signature = c ('mysco'),
          definition = function (object) {
            cat (paste("An object of class", class(object)),"\n" )
            cat (paste( 'with',nrow(object@counts),'genes and', ncol(object@counts),' cells.'),"\n")
          }
)
