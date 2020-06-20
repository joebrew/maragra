examples.predict.felm = function() {
  T = 12
  x <- rnorm(T)
  fe1 = sample(c("yes","no"), T, replace=TRUE)
  fe2 = sample(c("a","b","c"), T, replace=TRUE)
  y <- x1  + (fe1=="yes") + 2*(fe2=="a") + (fe2=="b")+ rnorm(T)
  
  new <- data.frame(
    x = seq(-3, 3, length=T), 
    fe1 = sample(c("yes","no"), T, replace=TRUE),
    fe2 = sample(c("a","b","c"), T, replace=TRUE)    
  )
  p.lm = predict(lm(y ~ x+fe1+fe2), new)

  library(lfe)  
  p.felm = predict(felm(y~x|fe1+fe2),new)
  cbind(p.lm, p.felm)
}

#' An implementaion of predict for felm
predict.felm = function(object, newdata, use.fe = TRUE,...) {
  restore.point("predict.felm")
  co = coef(object)
  
  # too make code compatible with tibbles
  newdata = as.data.frame(newdata)
  
  rownames(newdata) = seq_along(newdata[,1])
  # model matrix without intercept
  mm = model.matrix(object = object,data = newdata)
  
  if (NROW(mm)<NROW(newdata)) {
    warning("Observations dropped from newdata due to NA.")
  }
  
  # remove intercept
  if (NCOL(mm)==length(co)+1) {
    mm = mm[,-1,drop=FALSE]
  }
  y.pred = mm %*% co
  
  fe.vars = names(object$fe)
  if (use.fe & length(fe.vars)>0) {
    rows = as.integer(rownames(mm))
    nd = newdata[rows,]
    all.fe = getfe(object)
    fe.var = fe.vars[1]
    for (fe.var in fe.vars) {
      df = all.fe[all.fe$fe == fe.var,]
      frows = match(nd[[fe.var]],df$idx)    
      myfe = df$effect[frows]
      myfe[is.na(myfe)] = 0
      
      y.pred = y.pred + myfe
    }
  }
  as.vector(y.pred)
}