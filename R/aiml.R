# usage:
# head(explore_wordvec(wordvec_train_vectors, 'folfox'), 20)
explore_wordvec <- function(vectors, word){
  if (word %!in% rownames(vectors)) return(NULL);
  mtx.arr <- vectors[word, , drop = F] # tmp.cancer <- wordvec_train_vectors["cancer", , drop = F]
  cos_sim <- sim2(x=vectors, y=mtx.arr, method = "cosine", norm = "l2")  # cos_sim_1 = sim2(x = wordvec_train_vectors, y = tmp.cancer, method = "cosine", norm = "l2")
  rez <- sort(cos_sim[,1], decreasing = T)
  return(rez)
}
