getsurvtrends <- function(surv.input, survFormS, cols){
  cols_not_found <- cols %-% names(surv.input);
  if (length(cols_not_found)>0) {
    warning('Fields not found! ', paste(cols_not_found, collapse = ' '));
    cols <- cols %&% names(surv.input);
  }

  #if ('formula' %!in% class(survForm)) survForm <- as.formula(survForm);

  summarize.cox.gene <- function(survFormBase, inp.data, gene){
    survFormX <- as.formula(paste(survFormBase, gene, sep = ' + '))
    summarize.cox(inp.cox = coxph(survFormX, data = inp.data),filtS = gene)
  }

  #summarize.cox.gene(survFormS, dt.expr, 'SUSD3')

  rez <- lapply(X = cols,
                FUN = function(x) summarize.cox.gene(survFormBase=survFormS, inp.data=surv.input, gene = x))

  do.call(rbind, rez)

}



summarize.cox <- function(inp.cox, filtS){
  # browser()
  inp.cox.summary <- summary(inp.cox)
  coefs    <- as.data.frame(inp.cox.summary$coefficients)
  if ('matrix' %!in% class(inp.cox.summary$coefficients)) coefs <- data.frame(coef=inp.cox.summary$coefficients)

  confInts <- as.data.frame(inp.cox.summary$conf.int)
  if (nrow(coefs)>1) {
    coefs <- coefs[filtS,]
    confInts <- confInts[filtS,]
  }


  if (ncol(confInts)==0){
    confInts <- data.frame(col1=NA_real_,col2=NA_real_,low95=inp.cox.summary$ci.lower,hi95=inp.cox.summary$ci.upper)
  }

  if ('Pr(>|z|)'  %!in% names(coefs)) coefs$`Pr(>|z|)` <- inp.cox.summary$prob
  if ('exp(coef)' %!in% names(coefs)) coefs$`exp(coef)` <- exp(coefs$coef)

  coefs$CIl <- confInts[,3];
  coefs$CIh <- confInts[,4];
  setnames(coefs, 'Pr(>|z|)', 'p')
  #setnames(rez_tab0, cs('exp(coef) coef se(coef)'), cs('HR lnHR se.lnHR'));
  return(coefs);
}

fitsum <- function(inpFit, cox.fun=coxph){
  # browser()
  cox.obj <- cox.fun(as.formula(inpFit$call$formula), data = eval(inpFit$call$data))

  cox.obj.sum <- summary(cox.obj)
  # cox.obj.sum.coefs <- as.data.frame(cox.obj.sum$coefficients)

  sumcox <- summarize.cox(cox.obj)

  # if (identical(cox.fun,coxph)){
  #   sumcox <- cox.obj.sum
  # }


  lab.s <- sprintf('HR=%.1f, 95%% CI: %.1f-%.1f, p=%.2e', sumcox$`exp(coef)`, sumcox$CIl, sumcox$CIh, sumcox$p)
  sumcox$label <- lab.s
  return(sumcox)
}


surv.formulaS <- function(inp.time, inp.event, inp.factors=NULL){
  f.str <- paste0('Surv(', inp.time, ', ', inp.event, ') ~ ')
  if (!is.null(inp.factors)) f.str <- paste0(f.str, paste(inp.factors, collapse = ' + '))
  return(f.str)
}

surv.formula <- function(inp.time, inp.event, inp.factors=NULL){
  f.str <- paste0('Surv(', inp.time, ', ', inp.event, ') ~ ')
  if (!is.null(inp.factors)) f.str <- paste0(f.str, paste(inp.factors, collapse = ' + '))
  return(as.formula(f.str))
}


# getsurvforthisq ####
#
getsurvforthisq <- function(inp.dataset, surv.form, this.field, this.q){
  if (this.field %!in% names(inp.dataset)) {warning(this.field, ' not found!'); return(NULL);}
  if ('this.isHigh' %in% names(inp.dataset)) {stop(' this.isHigh in table fields!'); return(NULL);}

  cat(' ',this.q,': ');

  if (!is.character(surv.form)) surv.form <- deparse(surv.form)
  thisG.qvals <- inp.dataset[[this.field]]
  inp.dataset <- inp.dataset[!is.na(thisG.qvals),] ; # NEW!!!
  thisG.qvals <- inp.dataset[[this.field]]
  this.isHigh <- (as.numeric(as.character(thisG.qvals)) > this.q)
  cat(' ',sum(this.isHigh, na.rm = T),'; ');
  this.cox <- coxph(as.formula(surv.form %+% '+ this.isHigh'), data=inp.dataset);
  coefs <- summarize.cox(this.cox,"this.isHighTRUE");
  coefs$Gene <- this.field
  coefs$q <- this.q
  coefs$nhi <- sum(this.isHigh);
  coefs$nlo <- sum(!this.isHigh);
  #cat(coefs$p,'!; ')
  #dt.rez0 <- ifelse1(is.null(dt.rez0), coefs, rbind(dt.rez0,coefs));
  return(coefs)
}

myggsurvplot <- function(inp.survfit, legend.title = '', ...){
  ggsurvplot(inp.survfit,
             break.time.by = 5, xlab = 'Time (years)',
             risk.table = T, risk.table.pos = 'in', risk.table.title = 'Numbers at risk',
             tables.col = 'strata', legend.title = legend.title,
             ...)}



# build_surv_plot() - previously was in [build_surv_plot function.R] file
#inp.field = 'SUSD3_q10'
#threshold=3L
build_surv_plot <- function(surv.input,inp.field,threshold,form.time,form.evnt,form.cofactors){
  sform1  <- surv.formula(form.time,form.evnt,form.cofactors)
  sform2  <- surv.formula(form.time,form.evnt,'this.isHigh')
  sform2s <- surv.formulaS(form.time,form.evnt,'this.isHigh')

  #this.isHigh <- surv.input[[inp.field]] > threshold;
  vecHigh <- surv.input[[inp.field]] > threshold;


  coefs <- getsurvforthisq(surv.input, surv.form = deparse(sform1), this.field = inp.field, this.q=threshold)

  surv.input[, this.isHigh:=vecHigh]


  lab.s <- sprintf('HR = %.1f [%.1f - %.1f]', coefs[,2], coefs$CIl, coefs$CIh)
  lab.s <- lab.s %+% ifelse(coefs$p<1e-3, sprintf('\np = %.2e',coefs$p), sprintf('\np = %.3f',coefs$p))
  lab.n <- sprintf('cutoff = %1.0f0%%', coefs$q)

  sf <- survfit(sform2, data=surv.input)
  #  sf$call$data <- (deparse(substitute(surv.input))); # freaking magic

  sf1 <- survfit(as.formula('Surv(OS_YRS, isBRCA) ~ this.isHigh'), data=surv.input)
  sf2 <- survfit(as.formula(sform2s), data=surv.input)

  p <-   ggsurvplot(sf1, data =  surv.input,
                    palette = cs('black red'),
                    break.time.by = 5, xlab = 'Time (years)',
                    tables.col = 'strata', risk.table = T, risk.table.pos = 'in', legend = 'none')


  surv.input[, this.isHigh:=NULL]

  #p$plot <- p$plot + annotate('text', x=mean(p$plot$coordinates$limits$x), y=1, label=this.g, size=10)

  p$plot <- p$plot + annotate('text', x=0, y=0.4, label=lab.s, hjust=0, size=6)
  p$plot <- p$plot + annotate('text', x=0, y=0.3, label=lab.n, hjust=0, size=5)

  #p$plot <- p$plot + annotate('text', x=0, y=0.03, label='n.hi = ' %+% sum(this.isHigh), color='red', hjust=0)
  #p$plot <- p$plot + annotate('text', x=0, y=0.00, label='n.lo = ' %+% sum(this.isHigh), hjust=0)
  return(p)
}



