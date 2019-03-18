#####
# define function to winsorize
wz <- function(x, probs = c(.01, .99), lb = NULL, ub = NULL, do_scale = FALSE, 
               scale = NULL, do_center = FALSE, mu = NULL){
  if(is.null(lb))
    lb <- unname(quantile(x, probs[1]))
  if(is.null(ub))
    ub <- unname(quantile(x, probs[2]))
  
  x <- pmin(pmax(x, lb), ub)
  if(is.null(scale))
    scale <- sd(x)
  if(is.null(mu))
    mu <- mean(x)
  if(do_center)
    x <- x - mu
  if(do_scale)
    x <- x / scale
  
  structure(x, lb = lb, ub = ub, class = "wz", scale = scale, 
            do_scale = do_scale, do_center = do_center, mu = mu)
}

makepredictcall.wz <- function(var, call){
  if(as.character(call)[1L] != "wz") 
    return(call)
  at <- attributes(var)[c("lb", "ub", "scale", "do_scale", "do_center", 
                          "mu")]
  xxx <- call[1L:2L]
  xxx[names(at)] <- at
  xxx
}

local({
  set.seed(48350025)
  df <- data.frame(y = rnorm(100), x = rnorm(100, 1, sd = 2))
  f <- lm(y ~ wz(x), df)
  mm <- model.matrix(f)
  stopifnot(min(mm[, 2]) == quantile(df$x, .01), 
            max(mm[, 2]) == quantile(df$x, .99))
  stopifnot(
    predict(f, newdata = df)[1:10] == predict(f, newdata = df[1:10, ]))
  
  f <- lm(y ~ wz(x, do_scale = TRUE), df)
  mm <- model.matrix(f)
  stopifnot(
    isTRUE(all.equal(sd(mm[, 2]), 1)),
    predict(f, newdata = df)[1:10] == predict(f, newdata = df[1:10, ]))
  
  f <- lm(y ~ wz(x, do_scale = TRUE, do_center = TRUE), df)
  mm <- model.matrix(f)
  stopifnot(
    isTRUE(all.equal(sd(mm[, 2]), 1)),
    isTRUE(all.equal(mean(mm[, 2]), 0)),
    predict(f, newdata = df)[1:10] == predict(f, newdata = df[1:10, ]))
})

#####
# function to winsorize and then make B-spline basis matrix for a 3. order
# polynomial spline with a sum-to-zero constraint and which is orthogonal to 
# 1. order term. The latter is to make it easy to test the significance w/
# `drop1`
sp_w_c <- function(
  x, df = NULL, lb = NULL, ub = NULL, probs = c(.01, .99), 
  Boundary.knots = NULL, knots = NULL, Z = NULL, do_excl_slope = TRUE, 
  do_center = FALSE, mu = NULL){
  # first winsorize
  x <- wz(x, lb = lb, ub = ub, do_center =  do_center, mu = mu, 
          probs = probs, do_scale = FALSE)
  lb <- attr(x, "lb")
  ub <- attr(x, "ub")
  do_center <- attr(x, "do_center")
  mu <- attr(x, "mu")
  
  if(is.null(knots) | is.null(Boundary.knots)){
    # from 
    #   Regression Modeling Strategies with Applications to Linear Models, Logistic and Ordinal Regression and Survival Analysis
    qs <- switch(
      df - 3L, 
       `4` = .5,
       `5` = c(.1, .9), 
       `6` = c(.1, .5, .9), 
       `7` = c(.05, .35, .65, .95), 
       `8` = c(.05, .275, .5, .725, .95), 
       `9` = c(.05, .23, .41, .59, .77, .95), 
      `10` = c(.025, .1833, .3417, .5, .6583, .8167, .975), 
      seq(0, 1, length.out = df - 1L)[-c(1L, df - 1L)])
    
    if(length(qs) == 0 | (!qs[1] == 0 && !tail(qs, 1) == 1))
      qs <- c(0, qs, 1)
    
    knots_use <- quantile(x, qs)
    if(is.null(Boundary.knots))
      Boundary.knots <- c(knots_use[1], knots_use[length(knots_use)])
    if(is.null(knots))
      knots <- knots_use[-c(1, length(knots_use))]
  }
  
  # apply a sum-to-zero constraint
  require(splines)
  X <- bs(x, df = df + 1L, intercept = TRUE, knots = knots, 
          Boundary.knots = Boundary.knots)
  knots <- attr(X, "knots")
  
  if(is.null(Z)){
    if(do_excl_slope){
      C <- crossprod(cbind(rep(1, nrow(X)), x), X)
      qrc <- qr(t(C))
      Z <- qr.Q(qrc, complete = TRUE)[, (nrow(C) + 1):ncol(C)]
      
    } else {
      C <- rep(1, nrow(X)) %*% X
      qrc <- qr(t(C))
      Z <- qr.Q(qrc, complete = TRUE)[, (nrow(C) + 1):ncol(C)]
      
    }
  }
  
  structure(
    X %*% Z, lb = lb, ub = ub, knots = knots, Boundary.knots = Boundary.knots,
    do_excl_slope = do_excl_slope, do_center = do_center, mu = mu, 
    Z = Z, class = "sp_w_c")
}

makepredictcall.sp_w_c <- function(var, call){
  if(as.character(call)[1L] != "sp_w_c") 
    return(call)
  at <- attributes(var)[c("lb", "ub", "knots", "Boundary.knots", "Z", 
                          "do_excl_slope", "do_center", "mu")]
  xxx <- call[1L:2L]
  xxx[names(at)] <- at
  xxx
}

local({
  set.seed(48350025)
  df <- data.frame(y = rnorm(100), x = rnorm(100, 1, sd = 2))
  f <- lm(y ~ wz(x) + sp_w_c(x, 4L), df)
  mm <- model.matrix(f)
  stopifnot(
    ncol(mm) == 5L,
    sum(attr(mm, "assign") == 2) == 3,
    predict(f, newdata = df)[1:10] == predict(f, newdata = df[1:10, ]))
  
  # we get the same if we call `bs` and omit the first order term
  f2 <- lm(y ~ bs(wz(x), 4L), df) 
  stopifnot(isTRUE(all.equal(predict(f), predict(f2))))
  
  f3 <- lm(y ~ sp_w_c(x, 4L, do_excl_slope = FALSE), df)
  stopifnot(
    isTRUE(all.equal(predict(f), predict(f3))), 
    sum(attr(model.matrix(f3), "assign") == 1) == 4)
  
  f4 <- lm(y ~ wz(x) + sp_w_c(x, 4L, do_center = TRUE), df)
  stopifnot(isTRUE(all.equal(predict(f), predict(f4))))
})

#####
# function to plot spline
plot_sp_w_c <- function(fit, term, ylab = "Linear predictor term"){
  tt <- terms(fit)
  w_tr <- which(grepl(paste0("^sp_w_c\\(", term), attr(tt, "term.labels")))
  stopifnot(length(w_tr) == 1L)
  m_tr <- which(
    grepl(paste0("^wz\\(", term), attr(tt, "term.labels")) &
      attr(tt, "order") == 1L)
  stopifnot(length(m_tr) <= 1L)
  
  x <- eval(parse(text = term), fit$data)
  df <- list()
  df[[term]] <- seq(quantile(x, .01), quantile(x, .99), length.out = 1000)
  
  k <- c(m_tr, w_tr)
  tt <- terms(fit)
  n_terms <- length(attr(tt, "term.labels"))
  tt <- drop.terms(tt, setdiff(1:n_terms, k))
  attr(tt, "intercept") <- 0
  M <- model.matrix(tt, df)
  
  cl <- colnames(M)
  co <- coef(fit)[cl]
  cv <- vcov(fit)[cl, cl]
  
  # extremly stupid way to do this...
  se <- sqrt(diag(M %*% tcrossprod(cv, M)))
  
  y <- drop(M %*% co)
  # 95% confidence bounds
  lb <- y - 1.96 * se
  ub <- y + 1.96 * se
  
  function(){
    plot(y ~ df[[1]], type = "l", xlab = get_label(term), 
         ylab = ylab, 
         ylim = range(lb, ub))
    lines(df[[1]], lb, lty = 2)
    lines(df[[1]], ub, lty = 2)
    add_hist(wz(x, do_center = FALSE), breaks = 50L)
  }
}
