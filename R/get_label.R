get_label <- function(x){
  x <- as.character(x)
  if(grepl(":", x)){
    z <- strsplit(x, ":")
    return(paste0(sapply(z[[1L]], get_label), collapse = " * "))
    
  }
  if(grepl("\\*", x)){
    z <- strsplit(x, "\\*")
    return(paste0(sapply(z[[1L]], get_label), collapse = " * "))
    
  }
  stopifnot(length(x) == 1)
  switch(
    x, 
    r_wcapq_atq = "Working capital / total assets", 
    r_req_atq = "Retained Earnings / total assets", 
    r_oiadpq_atq = "Operating income / total assets", 
    r_saleq_atq = "Sales / total assets", 
    r_niq_atq = "Net income / total assets", 
    r_ltq_atq = "Total liabilities / total assets", 
    r_mv_ltq = "Market value / total liabilities",
    r_mv_ltq_log = "log Market value / total liabilities",
    r_mv_atq = "Market value / total assets",
    
    r_wcapq_nn = "Working capital / size", 
    r_req_nn = "Retained Earnings / size", 
    r_oiadpq_nn = "Operating income / size", 
    r_saleq_nn = "Sales / size", 
    r_niq_nn = "Net income / size", 
    r_ltq_nn = "Total liabilities / size", 
    
    r_actq_lctq = "Current ratio", 
    dtd = "Distance-to-default",
    sigma = "Idiosyncratic volatility", 
    excess_ret = "Log excess return",
    log_market_ret = "Log market return", 
    r1y = "T-bill rate", 
    rel_size = "Relative log market size", 
    actq_defl_log = "log current assets", 
    atq_defl_log = "log total assets", 
    
    stop("Not implemented for ", sQuote(x)))
}
