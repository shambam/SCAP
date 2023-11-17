#' Calculate the mitochondrial percentage
#' @name CalcMitoPct
#' @description Calculate the mitochondrial percentages
#' @param mysco the mysco object
#' @param prefix the pattern specifying mitochondrial genes
#' @return a mysco object with mito percents in the metadata
#' @export CalcMitoPct

CalcMitoPct <- function(mysco,prefix){

  mtpc <- 100*apply(mysco@counts[grep(prefix,rownames(mysco@counts)),],2,sum)/apply(mysco@counts,2,sum)
  mysco@meta$perc.mt <- mtpc
  mysco
}

#' Make QC plots needed for filtering
#' @name MakeQCPlots
#' @description make QC plots
#' @param mysco the mysco object
#' @return nothing
#' @export MakeQCPlots

MakeQCPlots <- function(mysco){

  qc.df <- mysco@meta

  fill_color <- "lightcoral"

  plots <- lapply(names(qc.df), function(variable) {
    ggplot(qc.df, aes(x = 1, y = qc.df[[variable]], fill = fill_color)) +
      geom_violin() +
      labs(x = NULL, y = variable) +
      scale_fill_manual(values = fill_color, name = variable) +  # Set the fill color
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      geom_jitter(shape=".", position=position_jitter(0.2))
  })
  #grid.arrange(grobs = plots, ncol = length(plots))

  plots[[4]] <- ggplot(qc.df, aes(x=nCount, y=perc.mt)) + geom_point(color = fill_color)
  plots[[5]] <- ggplot(qc.df, aes(x=nCount, y=features)) + geom_point(color = fill_color)

  grid.arrange(grobs = plots, ncol = 3)

}

#' Filter single-cell data
#' @name FilterData
#' @description make QC plots
#' @param mysco the mysco object
#' @param sub The expression of what should be filtred and how.
#' @param min.cells Minimum number of cells a gene should be expressed in for it to be kept.
#' @return A filtered mysco object.
#' @examples
#' pbmc <- FilterData(pbmc,sub=features > 200 & features < 2500 & perc.mt < 5,min.cells=3)
#' @export FilterData

FilterData <- function(mysco,sub,min.cells){

  min.cls <- apply(mysco@counts >0, 1, sum)

  d.filt <- subset(mysco@meta, eval(parse(text=sub)))
  mysco@meta <- d.filt
  mysco@counts <- mysco@counts[which(min.cls>=min.cells),rownames(d.filt)]
  mysco
}

#FilterData <- function(mysco,sub,min.cells){

  #min.cls <- apply(mysco@counts >0, 1, sum)

  #d.filt <- subset(mysco@meta, eval(parse(text=sub)))
  #mysco@meta <- d.filt
  #tcount <- mysco@counts[,rownames(d.filt)]
  #min.cls <- apply(tcount >0, 1, sum)
  #mysco@counts <- tcount[which(min.cls>=min.cells),]
  #mysco
#}




