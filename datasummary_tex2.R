datasummary_tex2 <- function(x, 
                             var_title = "Variable",
                             placement = "h!",
                             caption,
                             label,
                             size = "normalsize",
                             tablewidth = 0.98,
                             note,
                             file) {
  if(class(x)[1] != "tinytable"){
    cli::cli_abort("{.arg x} must be of class 'tinytable'.")
  }
  
  tab <- x@table_dataframe
  
  ncol <- ncol(tab)-1
  colnames(tab)[1] <- var_title
  
  head <- c(
    "% HEAD BEGIN",
    paste0("\\begin{table}", "[", placement, "]"),
    if (!missing(caption)) paste0("\\caption", "{", caption, "}") else "",
    if (!missing(label)) paste0("\\label", "{", label, "}") else "",
    paste0("\\centering"),
    paste0("\\", size),
    "",
    paste0("\\begin{tabularx}",
           "{", tablewidth, "\\textwidth}",
           "{l", paste0(rep("Y",ncol), collapse = ""), "}"),
    "",
    paste0(paste0(names(tab), collapse = " & "), " \\\\"),
    "\\midrule",
    "% HEAD END",
    ""
  )
  
  
  main <- c(
    "% MAIN BEGIN", 
    apply(tab, 1, FUN = function(x) paste0(paste0(x, collapse = " & "), " \\\\")),
    "% MAIN END",
    ""
  )
  
  foot <- c(
    "% FOOT BEGIN",
    "\\end{tabularx}",
    if (!missing(note)) paste0("\\floatnote{", note, "}") else "",
    "\\end{table}",
    "% FOOT END"
  )
  
  out <- c(head, main, foot)
  
  if (missing(file)) {
    cat(out, sep = "\n")
  } else {
    cat(out, sep = "\n", file = file)
  }
  
}























