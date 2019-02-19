concatenate <- function(
  formula, tstart, tstop, id, by, max_T, data, start_time){
  # find the variables to keep and check that they are in `data`
  keep <- c(
    s_tstart <- deparse(substitute(tstart)), 
    s_tstop <- deparse(substitute(tstop)), s_id <- deparse(substitute(id)))
  keep <- c(all.vars(formula), keep)
  stopifnot(all(keep %in% colnames(data)))
  
  # find the response variable and chech that it is binary
  ev <- model.response(model.frame(update(formula, . ~ 1), data))
  stopifnot(is.logical(ev))
  
  # find other variables and check that assumptions abot them are true
  tstart <- eval(substitute(tstart), data)
  tstop <- eval(substitute(tstop), data)
  id <- eval(substitute(id), data)
  if(missing(start_time)){
    start_time <- min(tstart)
    start_time <- as.integer(start_time + sign(start_time) * 1e-2)
  }
  stopifnot(
    is.integer(tstart), is.integer(tstop), is.integer(id), 
    is.integer(by), is.integer(start_time), is.integer(max_T), 
    !any(tapply(tstart, id, is.unsorted)), 
    !any(tapply(tstop , id, is.unsorted)), 
    # at most one event per id
    max(tapply(ev, id, max)) < 2, 
    # the event happens on the largest date
    all(tapply(tstop[ev], id[ev], max) == 
          tapply(tstop[id %in% id[ev]], id[id %in% id[ev]], max)))
  
  # find crossing points
  library(survival)
  borders <- seq.int(start_time, max_T, by = by)
  b_tstart <- findInterval(tstart, borders, left.open = TRUE)
  b_tstop  <- findInterval(tstop , borders, left.open = TRUE)

  idx_keep <- 
    # after `start_time`
    0L < b_tstop & 
    # before `max_T`
    b_tstop <= length(borders) - 1L & 
    # start and stop time cross an interval
    b_tstart < b_tstop & 
    # assume that we have carried forward covariates. Thus, if there 
    # is no information on a border then the observations gets excluded. 
    # this makes the previous check redundant
    tstart %in% borders
  
  # find new stop time and whether there is an event
  library(data.table)
  dt <- data.table(id, idx_keep, tstart, tstop, ev, b_tstart, b_tstop)
  borders <- c(borders[1L] - 1L, borders, borders[length(borders)] + 1L)
  dt <- dt[
    , max_tstop := max(tstop), by = id][
      , `:=`(
        ev_final = any(ev), 
        tstop_final = if(all(tstop < max_tstop))
          # Is alive in the future. Set to border value.
          borders[b_tstop + 2L] else max(tstop)), 
      by = .(id, b_tstop)][
      idx_keep  == TRUE, .(id, ev_final, tstart, tstop_final)]
  
  # return output
  out <- data[idx_keep, keep]
  s_stop_org <- paste0(s_tstop, "_org")
  s_ev_org <- paste0(s_ev <- all.vars(update(formula, . ~ 1)), "_org")
  stopifnot(!c(s_stop_org, s_ev_org) %in% colnames(out), 
            length(s_ev_org) == 1L)
  out[[s_stop_org]] <- out[[s_tstop]]
  out[[s_ev_org]] <- ev[idx_keep]
  
  out[[s_tstop]] <- dt$tstop_final
  out[[s_ev]] <- dt$ev_final
  out
}

######
# test the function
local({
  con_dat <- function(n, final, id){
    stopifnot(is.integer(n), is.logical(final), is.integer(id))
    tstart <- 0:(n - 1L)
    tstop  <- tstart + 1L
    y <- logical(n)
    if(final)
      y[n] <- TRUE
    
    data.frame(y, x = round(runif(n), 3), tstart, tstop, id)
  }
  
  t_func <- function(data, tstop, y){
    if(!missing(tstop))
      data$tstop = tstop
    if(!missing(y))
      data$y <- y
    data
  }
  
  #####
  # one individual with an event and one without any events
  df <- rbind(d1 <- con_dat(22L, FALSE, 1L), d2 <- con_dat(22L, TRUE, 2L))
  o <- concatenate(
    y ~ x, tstart = tstart, tstop = tstop, id = id, max_T = 24L, data = df, 
    start_time = 0L, by = 12L)
  
  is_ae <- function(e1, e2)
    eval(bquote(isTRUE(all.equal(
      .(substitute(e1)), .(substitute(e2)), check.attributes = FALSE))))
  stopifnot(
    is_ae(
      subset(o, id == 1L)[, 1:5],
      t_func(d1[c(1L, 13L), 1:5], c(12L, 22L))), 
    is_ae(
      subset(o, id == 2L)[, 1:5],
      t_func(d2[c(1L, 13L), 1:5], c(12L, 22L), c(FALSE, TRUE))))

  #####
  # same indiviudals but now we use a different starting point
  o <- concatenate(
    y ~ x, tstart = tstart, tstop = tstop, id = id, max_T = 18L, data = df, 
    start_time = 6L, by = 12L)  
  stopifnot(
    is_ae(
      subset(o, id == 1L)[, 1:5],
      t_func(d1[7L, 1:5], 18L)), 
    is_ae(
      subset(o, id == 2L)[, 1:5],
      t_func(d2[7L, 1:5], 18L)))
  
  #####
  # individuals inside an interval are not included
  df <- rbind(con_dat(12L, FALSE, 1L), con_dat(4L, TRUE, 2L))
  while({
    df <- within(df, {
      tstart[id == 2L] <- tstart[id == 2L] + 1L
      tstop [id == 2L] <- tstop [id == 2L] + 1L
    }); 
    max(df[df$id == 2L, "tstart"]) < 12L
  }){
    o <- concatenate(
      y ~ x, tstart = tstart, tstop = tstop, id = id, max_T = 24L, data = df, 
      start_time = 0L, by = 12L)
    stopifnot(sum(o$id == 2L) == 0L, 
              is_ae(
                subset(o, id == 1L)[, 1:5],
                t_func(df[1L, 1:5], 12L)))
  }
  
  #####
  # Observation with "hole"
  df <- con_dat(24L, TRUE, 1L)
  df <- subset(df, !tstart %in% 7:11)
  o <- concatenate(
    y ~ x, tstart = tstart, tstop = tstop, id = id, max_T = 24L, data = df, 
    start_time = 0L, by = 12L)
  stopifnot(is_ae(
    o[, 1:5], t_func(df[c(1L, 8L), ], c(12L, 24L), c(FALSE, TRUE))))
  
  df <- con_dat(24L, TRUE, 1L)
  df <- subset(df, !tstart %in% c(1L, 5L, 11L))
  o <- concatenate(
    y ~ x, tstart = tstart, tstop = tstop, id = id, max_T = 24L, data = df, 
    start_time = 0L, by = 12L)
  stopifnot(is_ae(
    o[, 1:5], t_func(df[c(1L, 10L), ], c(12L, 24L), c(FALSE, TRUE))))
  
  df <- con_dat(10L, TRUE, 1L)
  df <- subset(df, !tstart %in% c(1L, 5L, 7L))
  o <- concatenate(
    y ~ x, tstart = tstart, tstop = tstop, id = id, max_T = 24L, data = df, 
    start_time = 0L, by = 12L)
  stopifnot(is_ae(
    o[, 1:5], t_func(df[1L, ], 10L, TRUE)))
})

