BiocManager::install("waldronlab/MicrobiomeWorkshop", build_vignettes=TRUE, 
                     dependencies=TRUE)

remove.packages("BiocParallel", lib="~/R/win-library/3.6")
remove.packages("BiocParallel")
detach("package:BiocParallel", unload = TRUE)


catch1 <- function(e){
  print(e)
}

rez1 <- tryCatch(detach("package:BiocParallel", unload = TRUE), error=function(e){catch1(e)}, warning=function(w){catch1(w)})
