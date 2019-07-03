residuals. <- function(object, type = "pearson", ...){
  if(type != "martingale"){
    cl <- match.call()
    cl[[1L]] <- quote(stats::residuals)
    return(eval(cl, parent.frame()))
  }
  
  # we make the assumption that `glm` has been used with `poisson` family 
  # with `log` link and log-time offsets. Then we compute the martingale
  # residuals as suggested in 
  #   Therneau, T. M., Grambsch, P. M., & Fleming, T. R. (1990). 
  #   Martingale-based residuals for survival models. Biometrika, 77(1), 
  #   147-160.
  stopifnot(inherits(object, "glm"), object$family$family == "poisson", 
            object$family$link == "log", all(object$y %in% 0:1))
  
  # the code is slightly redundant as we subtract and then add the 
  # offsets...
  offs <- object$offset
  etas <- object$linear.predictors - offs
  ys   <- object$y
  
  ys - exp(etas + offs)
}

#####
# function to add histogram in the background
add_hist <- function(x, breaks = 100){
  h <- hist(x, plot = FALSE, breaks = breaks)
  nB <- length(h$breaks)
  y <- h$counts
  y_min <- par("usr")[3]
  y_max <- par("usr")[4]
  y_max <- y_max - .04 * (y_max - y_min)
  # substract w/ zero to make it clear that the lowest value is zero
  y <- (y - 0) / (max(y) - 0) * (y_max - y_min) / 5 + y_min 
  rect(h$breaks[-nB], y_min, h$breaks[-1L], y, border =  par("fg"), 
       angle = 45, col = rgb(0, 0, 0, .1))
}

#####
# function to plot Pearson residuals versus the linear predictor
pear_vs_res <- function(fit, bw = 2, ylim = c(-.05, .05), type = "pearson"){
  require(KernSmooth)
  if(any(weights(fit) != 1))
    warning("Some weights are not one. Plot may be missleading")
  res <- residuals.(fit, type = type)
  eta <- predict(fit, type = "link")
  rg <- range(eta)
  rg <- c(rg[1L] - 1e-4 * diff(rg), rg[2L] + 1e-4 * diff(rg))
  ks <- locpoly(eta, res, bandwidth = bw, range.x = rg)
  par_old <- par(no.readonly = TRUE)
  on.exit(par(par_old))
  par(mar = c(5, 4, 2, 2))
  plot(ks, type = "l", xlab = expression(eta), ylab = type, 
       ylim = ylim)
  abline(h = 0, lty = 2)
  
  # add histogram 
  add_hist(eta)
}

#####
# function to plot Pearson residuals versus the covariate
resid_vs_covar <- function(fit, expr, bw = 1, ylim = NULL, keep = NULL,
                          alter_par = TRUE, xlab_extra = NULL, 
                          silent = FALSE, type = "pearson"){
  require(KernSmooth)
  if(any(weights(fit) != 1))
    warning("Some weights are not one. Plot may be missleading")
  
  res <- residuals.(fit, type = type)
  if(is.matrix(res) && type == "partial"){
    sexpr <- deparse(substitute(expr))
    if(!sexpr %in% colnames(res)){
      res <- with(
        fit, 
        (y - family$linkinv(linear.predictors)) / 
          family$mu.eta(linear.predictors))
      xvar <- eval(substitute(expr), fit$data)
      xtrm <- rep(0, length(xvar))
      
    } else {
      res <- res[, sexpr]
      xvar <- model.matrix(terms(fit), fit$data)[, sexpr]
      xtrm <- coef(fit)[sexpr] * xvar
    }
    
    # # comment back to set that we get the same as in 
    # #   Cook, R. Dennis, and Rodney Croos-Dabrera. "Partial residual plots in generalized linear models." Journal of the American Statistical Association 93, no. 442 (1998): 730-739.
    # # As shown in the article though, these residuals may not be a good idea
    # # when 
    # #  - there is a non-linear dependence to other covariates
    # #  - e.g., in binary model estimated probabilities are _NOT_ close to 
    # #    the boundary
    # tmp <- with(fit, {
    #   (y - family$linkinv(linear.predictors)) / 
    #     fit$family$mu.eta(linear.predictors) + 
    #     predict(fit, type = "terms")[, sexpr]
    # })
    # stopifnot(isTRUE(all.equal(res, tmp)))
      
  }
  qs <- quantile(res, c(.001, .999))
  resids <- pmin(pmax(res, qs[1]), qs[2])
  if(!silent){
    cat("Residuals quantiles\n")
    print(qs)
  }
  tr <- eval(substitute(expr), fit$data)
  
  if(!is.null(keep)){
    res <- res[keep] 
    tr  <- tr [keep]
  }
  if(missing(bw))
    bw <- signif(.2 * unname(diff(quantile(tr, c(.05, .95)))), 3)
  rg <- range(tr)
  rg <- c(rg[1L] - 1e-4 * diff(rg), rg[2L] + 1e-4 * diff(rg))
  ks <- locpoly(tr, res, bandwidth = bw, range.x = rg)
  
  if(is.null(ylim))
    ylim <- if(type != "partial") range(ks$y) else range(xtrm, ks$y)
  if(alter_par){
    par_old <- par(no.readonly = TRUE)
    on.exit(par(par_old))
    par(mar = c(5, 4, 2, 2))
  }
  xlab <- paste0(deparse(substitute(expr)), collapse = " ")
  if(!is.null(xlab_extra))
    xlab <- paste(xlab, xlab_extra)
  plot(ks, type = "l", 
       xlab = xlab, ylab = type, ylim = ylim)
  if(type != "partial"){
    abline(h = 0, lty = 2)
  } else {
    oid <- order(xvar)
    lines(xvar[oid], xtrm[oid], lty = 2)
  }
  
  # add histogram 
  add_hist(tr)
  
  legend("topright", bty = "n", legend = paste0("bw is ", bw), 
         cex = par()$cex * .8)
  
  rug(tr[fit$y == 1], col = "DarkBlue")
}

#####
# assign function to look for interaction effects
resid_vs_covar_inter <- function(
  fit, by, expr, bw = 1, ylim = NULL, n_grp = 4L, type = "pearson"){
  cl <- match.call()
  by_org <- substitute(by)
  by <- eval(substitute(by), fit$data)
  if(is.numeric(by)){
    brs <- seq(min(by), max(by), length.out = n_grp + 2L)
    brs <- cbind(brs[seq_len(n_grp)], brs[seq_len(n_grp) + 2L] + 1e-4)
    grps <- matrix(NA, length(by), nrow(brs))
    for(i in 1:nrow(brs))
      grps[, i] <- brs[i, 1] <= by & by < brs[i, 2]
    
    colnames(grps) <- paste0(
      "[", sprintf("%.3f", brs[, 1]), ",", sprintf("%.3f", brs[, 2]), ")")
  } else if(is.factor(by) || is.logical(by)){
    grps <- model.matrix(~ by - 1)
    grps <- apply(grps, 2, `>`, y = 0)
    
  } else
    stop("not implemented")
  
  cat("Groups are given below with number of observations for", 
      sQuote(deparse(cl$by)), "\n")
  print(colSums(grps))
  
  cl[[1]] <- quote(resid_vs_covar)
  cl <- cl[!names(cl) %in% c("n_grp", "by")]
  cl$alter_par <- FALSE
  cl$type <- type
  
  par_old <- par(no.readonly = TRUE)
  on.exit(par(par_old))
  if(ncol(grps) <= 2){
    par(mfcol = c(1, 2))
  } else if(ncol(grps) <= 4){
    par(mfcol = c(2, 2))
  } else if(ncol(grps) <= 6){
    par(mfcol = c(2, 3))
  } else
    par(mfcol = c(3, 3))
  par(mar = c(5.5, 4.5, 0, 0))
  
  x <- eval(substitute(expr), fit$data)
  if(missing(bw))
    bw <- signif(.2 * unname(diff(quantile(x, c(.05, .95)))), 3)
  cl$bw <- bw
  cl$silent = TRUE
  for(i in 1:ncol(grps)){
    cl$keep <- quote(grps[, i])
    cl$xlab_extra <- paste0("\n", deparse(match.call()$by), " ", 
                            colnames(grps)[i])
    try(eval(cl))
    if(inherits(try, "try-error"))
      plot.new() else {
        rug_keep <- which(grps[, i] & fit$y == 1)
        rug(x[rug_keep], col = "DarkBlue")
      }
  }
  
  if(is.factor(by) || is.logical(by))
    return(invisible())
  res <- residuals.(fit, type = type)
  if(is.matrix(res) && type == "partial"){
    res <- with(
      fit, 
      (y - family$linkinv(linear.predictors)) / 
        family$mu.eta(linear.predictors))
    trs <- predict(fit, type = "terms")
    trs <- trs[, match(c(deparse(by_org), deparse(substitute(expr))), 
                       colnames(trs), 0L), drop = FALSE]
    res <- res + rowSums(trs)
  }
  qs <- quantile(res, c(.001, .999))
  resids <- pmin(pmax(res, qs[1]), qs[2])
  
  v1 <- all.vars(substitute(expr))
  v2 <- all.vars(by_org)
  stopifnot(length(v1) == 1L, length(v2) == 1L)
  assign(v1, eval(substitute(expr), fit$data))
  assign(v2, eval(by_org, fit$data))
  
  library(mgcv)
  bam_fit <- eval(substitute(bam(
    resids ~ te(v1, v2, k = c(10, 10), bs = "cr"), family = gaussian()), 
    list(v1 = as.name(v1), v2 = as.name(v2))))
  print(summary(bam_fit))
  par(mar = c(5, 4, .5, .5), mfcol = c(2, 2))
  for(x in 0:3)
    vis.gam(bam_fit, se = -1, theta = -35 + 90 * x, ticktype = "detailed")
}