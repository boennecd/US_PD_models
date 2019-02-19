palette(c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
  "#CC79A7"))

get_plot_device <-  function(
  expr, file_name_wo_extension, onefile = TRUE, set_par = TRUE)
  {
    if(!onefile)
      file_name_wo_extension <- paste0(file_name_wo_extension, "_%03d")
    pdf. <- file.path(
      "markdown", "fig", paste0(file_name_wo_extension, ".pdf"))
    pdf_half <- file.path(
      "markdown", "fig", paste0(file_name_wo_extension, "-half.pdf"))
    jpeg. <- file.path(
      "markdown", "fig", paste0(file_name_wo_extension, ".jpg"))
    
    # make the plot 
    if(set_par){
      par_old <- par(no.readonly = TRUE)
      on.exit(par(par_old))
      expr <- bquote({
        par(mar = c(5, 4, 1, 1))
        .(substitute(expr))
      })
    } else
      expr <- substitute(expr)
    eval(expr, parent.frame())
    
    jpeg(file = jpeg., width = 1900, height = 1200, res = 300)
    on.exit(dev.off()) # at-least one `dev.off()` will be called
    eval(expr, parent.frame())
    dev.off()
    
    pdf(file = pdf., width = 6, height = 3.75, onefile = onefile)
    eval(expr, parent.frame())
    dev.off()
    
    pdf(file = pdf_half, width = 4, height = 3.75, onefile = onefile)
    eval(expr, parent.frame())
    
    invisible()
  }