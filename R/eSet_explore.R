es_explore_plot_samples <- function(es,X=NULL,Y='not0f',
                                    lim.top=5L,lim.btm=5L,
                                    label.col=NULL,
                                    label.X=NULL,label.Y=NULL,label.title=NULL
                                    ){
  if (Y %!in% varLabels(es)) stop(Y,' not found in eSet!')
  aes.str.Y <- Y

  dtP <- as.data.table(pData(es), keep.rownames = T)

  if (!is.null(X)){ # if X is defined, use it
    aes.str.X <- X
  } else {          # otherwise order by rank
    aes.str.X <- 'tmp.rank'
    dtP[, tmp.rank:=frank(get(Y))]
    if (is.null(label.X)) label.X <- 'Rank, #';
  }

  if (Y=='not0f' & is.null(label.Y)) label.Y <- 'Detection rate';

  dtP[, tmp.label:='']
  if (is.null(label.col)) label.col <- 'rn';
  if (lim.top>0) dtP[frank(-get(Y))<=lim.top, tmp.label:=get(label.col)]
  if (lim.btm>0) dtP[frank( get(Y))<=lim.btm, tmp.label:=get(label.col)]


  p1 <-
    ggplot(dtP, aes_string(x=aes.str.X,y=aes.str.Y)) +
    geom_point(size=3, alpha=0.5, color='blue')

  if (!is.null(label.X)) p1 <- p1 + xlab(label.X);
  if (!is.null(label.Y)) p1 <- p1 + ylab(label.Y);
  if (!is.null(label.title)) p1 <- p1 + ggtitle(label.title);

  if (sum(dtP$tmp.label!='')>0)
    p1 <- p1 + geom_text_repel(aes(label=tmp.label))

 p1
}
