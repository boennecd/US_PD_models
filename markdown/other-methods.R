#####
# set directory
local({
  wd <- getwd()
  if(!grepl("US_PD_models$", wd)){
    cat("Changing directory...\n")
    reg_e <- "(.+)(US_PD_models)(.+)"
    stopifnot(grepl(reg_e, wd))
    setwd(gsub(reg_e, "\\1\\2", wd))
  }
})

#####
# setup plot device and assign function to save output (after asking)
pdf(file.path("markdown", "fig", "other-methods-%03d.pdf"), onefile = FALSE)
library(tcltk)
saveRDS_ask <- function(object, file){
  save <- !file.exists(file) || 
    tclvalue(tkmessageBox(
      title = "Save?", message = "Want to save", type = "yesno")) == "yes"
  
  if(save){
    cat("Saving...\n")
    saveRDS(object, file)
    return(invisible())
    
  }
  
  cat("Not saving...\n")
}

##### 
# source script to setup data
source(knitr::purl(file.path("markdown", "setup.Rmd"), output = tempfile()))

#####
# formulas that we use later
f0 <- y ~ wz(r_wcapq_atq) + wz(r_oiadpq_atq) + wz(r_mv_ltq) + 
  wz(r_niq_atq) + wz(r_ltq_atq) + wz(r_actq_lctq) + wz(sigma) + 
  wz(excess_ret) + wz(rel_size) + wz(dtd) + log_market_ret + r1y

i0 <- update(
  f0, . ~ . + sp_w_c(r_niq_atq, 3) + sp_w_c(sigma, 3) - wz(r_mv_ltq) + 
    + wz(r_mv_ltq_log) + sp_w_c(r_mv_ltq_log, 3) + wz(r_actq_lctq):wz(sigma))

#####
# glmnet
library(glmnet)
func <- function(frm, data, do_cv, lambda = NULL, family = "binomial"){
  mf <- model.frame(frm, data)
  X <- model.matrix(terms(mf), mf)
  X <- X[, colnames(X) != "(Intercept)"]
  y <- model.response(mf)
  if(do_cv)
   return(cv.glmnet(X, y, type.measure = "deviance", nfolds = 10, alpha = 0, 
                    family = family))
  
  glmnet(X, y, alpha = 0, lambda = lambda, family = family)
}

set.seed(24203767)
g1 <- func(i0, dat, do_cv = TRUE)
saveRDS_ask(g1, file.path("markdown", "cache", "glmnet-cv.RDS"))
plot(g1)

g2 <- func(i0, dat, do_cv = FALSE)
dim(coef(g2))
min(log(g2$lambda))
glm_fit <- glm(i0, binomial(), dat)
cbind(coef(g2)[, 95:100], i0 = coef(glm_fit))


#####
# boosted trees
library(xgboost)
X <- model.matrix(f0, dat)
X <- X[, colnames(X) != "(Intercept)"]
y <- model.response(model.frame(f0, dat))
xg_dat <- xgb.DMatrix(data = X, label = y)

set.seed(24203767)
depths <- 4:1
names(depths) <- 4:1
eta_use <- .02
xgb_cv_out <- lapply(depths, function(depth){
  cb_func <- function (period = 100L) {
    callback <- function(env = parent.frame()) {
      if (length(env$bst_evaluation) == 0 || period == 0)
        return()
      i <- env$iteration
      if ((i - 1)%%period == 0 || i == env$begin_iteration || i == env$end_iteration)
        cat(sprintf(
          "depth: %1d it: %4d train: %7.5f test: %7.5f\n", 
          depth, i, env$bst_evaluation[1], env$bst_evaluation[2]))
    }
    attr(callback, 'call') <- match.call()
    attr(callback, 'name') <- 'cb-func'
    callback
  }
  xgb.cv(
    params = list(objective = "binary:logistic", max_depth = depth, 
                  eta = eta_use, nthread = 6L), 
    xg_dat, nfold = 5L, metrics = "logloss", nrounds = 5000L, verbose = F,
    callbacks = list(cb_func()), early_stopping_rounds = 100L)
})
saveRDS_ask(xgb_cv_out, file.path("markdown", "cache", "xgb-cv.RDS"))

logs <- lapply(xgb_cv_out, "[[", "evaluation_log")
logs <- lapply(logs, "[[", "test_logloss_mean")
# get deviance insetad
logs <- lapply(logs, '*', 2)

# cut away the first indices
start_at <- 300L
max_length <- max(sapply(logs, length))
stopifnot(all(max_length > start_at), start_at >= 2L)
logs <- lapply(logs, function(x) x[-(1:(start_at - 1L))])

# plot
par(mar = c(5, 4, .5, .5))
plot(1, type = "n", xlim = c(start_at, max_length), ylim = 
     range(logs), ylab = "deviance")
for(i in 1:4)
  lines(seq_along(logs[[i]]) - 1L + start_at, logs[[i]], lty = i)

legend("topright", legend = names(logs), lty = seq_along(logs), bty = "n")

# minimum and standard deviations
sapply(logs, min)
sapply(logs, which.min) + start_at - 1L
local({
  lgs <- lapply(xgb_cv_out, function(x) x$evaluation_log)  
  mins <- sapply(lgs, function(x) which.min(x$test_logloss_mean))
  mapply(`[`, lapply(lgs, `[[`, "test_logloss_std"), mins)
})

# boost stumps
xgb_fit <- xgb.train(
  params = list(objective = "binary:logistic", max_depth = 1L, 
                eta = eta_use, nthread = 6L),
  data = xg_dat, nrounds = 1500L)
(imp <- xgb.importance(model = xgb_fit))

# look at partial effects
local({
  set.seed(32997487)
  Xt <- X[sample.int(nrow(X), min(nrow(X), 10000L)), ]
  plot_info <- lapply(colnames(X), function(s){
    z <- X[, s]
    z_grid <- seq(min(z), max(z), length.out = 201)
    y_hat <- sapply(z_grid, function(x){
      Xt[, s] <- x
      y_hat <- predict(xgb_fit, newdata = Xt, outputmargin = TRUE)
      mean(y_hat)
    })
    y_hat <- y_hat - mean(y_hat)
    
    list(y_hat = y_hat, z_grid = z_grid)
  })
  names(plot_info) <- colnames(X)

  par(mar = c(5, 4, 1, 1), mfcol = c(2, 2))
  y_lim <- range(lapply(plot_info, "[[", "y_hat"))
  for(i in seq_along(plot_info))
    plot(plot_info[[i]]$z_grid, plot_info[[i]]$y_hat, ylim = y_lim,
         type = "l", xlab = names(plot_info)[i], ylab = "log-odds")
})

# boost deepth two tree
xgb_fit_two <- xgb.train(
  params = list(objective = "binary:logistic", max_depth = 2L, 
                eta = eta_use, nthread = 6L),
  data = xg_dat, nrounds = 1000L)
(imp <- xgb.importance(model = xgb_fit_two))

library(data.table)
local({
  tree_dat <- xgb.model.dt.tree(model = xgb_fit_two)  
  tree_dat <- tree_dat[Feature != "Leaf"]
  func <- function(.SD){
    root <- .SD$Feature[1]
    lvl_2 <- .SD[2:3, ]
    lvls_2 <- lvl_2[, Feature := ifelse(
      Feature < root, 
      paste0(Feature, ":", root),
      paste0(root, ":", Feature))]
    rbind(.SD[1, ], lvls_2)
  }
  tree_dat <- tree_dat[, func(.SD), by = Tree]
  tree_dat <- tree_dat[
    , .(Quality = sum(Quality), Frequency = .N), by = Feature]
  tree_dat[, `:=`(Quality = Quality / sum(Quality), 
                  Frequency = Frequency / sum(Frequency))]
  print(head(tree_dat[order(Quality, decreasing = TRUE)], 50))
})

# make sure to shut down the plot device
dev.off()