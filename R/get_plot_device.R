palette(c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
  "#CC79A7"))

.fig_dir <- file.path("markdown", "fig")

.get_plot_device_ggplot <- function(expr, file_name_wo_extension){
  x <- eval(expr, parent.frame(2L))
  print(x)
  
  pref <- file.path(.fig_dir, file_name_wo_extension)
  ggsave(paste0(pref, ".pdf"), width = 6, height = 3.75)
  ggsave(paste0(pref, "-half.pdf"), width = 4, height = 3.75)
  ggsave(paste0(pref, ".png"), width = 6, height = 3.75)
  
  invisible(NULL)
}

get_plot_device <-  function(
  expr, file_name_wo_extension, onefile = TRUE, set_par = TRUE, 
  ggplot = FALSE)
  {
    if(ggplot)
      return(.get_plot_device_ggplot(
        substitute(expr), file_name_wo_extension))
  
    if(!onefile)
      file_name_wo_extension <- paste0(file_name_wo_extension, "_%03d")
    pdf. <- file.path(
      .fig_dir, paste0(file_name_wo_extension, ".pdf"))
    pdf_half <- file.path(
      .fig_dir, paste0(file_name_wo_extension, "-half.pdf"))
    jpeg. <- file.path(
      .fig_dir, paste0(file_name_wo_extension, ".jpg"))
    
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