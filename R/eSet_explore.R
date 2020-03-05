es_explore_plot_samples <- function(es,X=NULL,Y='not0f',label.top=5L,label.btm=5L,label.col=NULL){
  if (Y %!in% varLabels(es)) stop(Y,' not found in eSet!')
  aes.str.Y <- Y
  
  dtP <- as.data.table(pData(es), keep.rownames = T)

  if (!is.null(X)){ # if X is defined, use it
    aes.str.X <- X
  } else {          # otherwise order by rank
    aes.str.X <- 'tmp.rank'
    dtP[, tmp.rank:=frank(get(Y))]
  }
  
  dtP[, tmp.label:='']
  if (is.null(label.col)) label.col <- 'rn';
  if (label.top>0) dtP[frank(-get(Y))<=label.top, tmp.label:=get(label.col)]
  if (label.btm>0) dtP[frank( get(Y))<=label.btm, tmp.label:=get(label.col)]
  
  
  p1 <- 
    ggplot(dtP, aes_string(x=aes.str.X,y=aes.str.Y)) + 
    geom_point(size=3, alpha=0.5, color='blue')
  
  if (sum(dtP$tmp.label!='')>0)
    p1 <- p1 + geom_text_repel(aes(label=tmp.label))
  
 p1  
}