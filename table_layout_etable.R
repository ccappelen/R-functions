##################################################################
##
##              TABLE LAYOUT FOR ETABLE (FIXEST)
##
##################################################################

pacman::p_load("stringr")

# Set table layout --------------------------------------------------------

set_table_layout <- function(x, size = "small", note, clustervar, subsample, 
                             tabularx = FALSE, tablewidth=0.98, robust = F, conley = F, conley_cut, tabularY = FALSE,
                             sidewaystable = FALSE, linespacing = 1, replace.name){
  
  if(tabularx & tabularY) stop("ERROR: tabularx and tabularY can't both be TRUE")
  if(sidewaystable){
    x <- str_replace(x, "\\\\begin\\{table\\}", "\\\\begin\\{sidewaystable\\}")
    x <- str_replace(x, "\\\\end\\{table\\}", "\\\\end\\{sidewaystable\\}")
  }
  
  if(tabularx){
    x <- str_replace_all(x, "\\\\begin\\{tabular\\}\\{l", paste0("\\\\begin\\{tabularx\\}\\{",tablewidth,"\\\\textwidth\\}\\{X"))
    x <- str_replace_all(x, "\\\\end\\{tabular\\}", "\\\\end\\{tabularx\\}")
  }
  
  if(tabularY){
    modno <- str_extract(x[str_detect(x, "\\\\begin\\{tabular\\}\\{l")], "(?<=\\\\begin\\{tabular\\}\\{l)(.+)(?=\\})") %>% 
      str_length()
    x <- str_replace_all(x, "\\\\begin\\{tabular\\}\\{l(.*?)\\}", paste0("\\\\begin\\{tabularx\\}\\{",tablewidth,"\\\\textwidth\\}\\{l", rep("Y", modno) %>% paste(collapse = ""), "\\}"))
    x <- str_replace_all(x, "\\\\end\\{tabular\\}", "\\\\end\\{tabularx\\}")
  }
  
  x <- str_replace(x, "\\n\\\\centering", paste0("\n\\\\centering", "\n\\\\captionsetup{width=", tablewidth, "\\\\textwidth} \n \\\\centering"))
  
  
  htex2add = ""
  if(!missing(size)){
    htex2add = paste0("\\", size, "\n")
  }
  
  if(nchar(htex2add) > 0){
    x[x == "%start:tab\n"] = htex2add
  }
  
  if(!missing(replace.name)){
    x <- str_replace_all(x, replace.name[1], replace.name[2])
  }
  
  ftex2add = paste0("\\floatnote{$^{*} p < .1$, $^{**} p < .05$, $^{***} p < .01$. ")
  if(!missing(clustervar)){
    ftex2add = paste0(ftex2add, "Standard errors clustered by ", clustervar, " in parentheses.")
  }else if(conley){
    ftex2add = paste0(ftex2add, "Conley standard errors in parentheses (", conley_cut, " km cutoff)")
  }else if(robust){
    ftex2add = paste0(ftex2add, "Robust standard errors in parentheses.")
  }else{
    ftex2add = paste0(ftex2add, "Standard errors in parentheses.")
  }
  if(!missing(note)){
    ftex2add = paste0(ftex2add, "\\\\ ",
                      note, "}", "\n")
  }else{
    ftex2add = paste0(ftex2add, "}", "\n")
  }
  if(nchar(ftex2add) > 0){
    x[x=="%end:tab\n"] = ftex2add
  }
  
  tex_subsample = ""
  if(!missing(subsample)){
    tex_subsample <- paste0(" & ", paste0(subsample, collapse = " & "), "\\\\", " \n")
    x[4] <- x[5]
    x[5] <- tex_subsample
  }
  
  x <- str_replace_all(x , "\n(.*?) \\$\\=\\$ ", "\n")
  x <- str_replace_all(x , "(?<=\\$\\\\times \\$).*(?<=\\$\\=\\$)", "")
  
  if(!missing(linespacing) & linespacing > 0){
    main_table <- min(which(str_detect(x, "\\\\midrule"))) + 1
    main_table_split <- x[main_table] %>% 
      str_split("\n") %>% 
      unlist()
    for(i in 1:length(main_table_split)){
      if(i %% 2 == 0){
        main_table_split[i] <- paste0(main_table_split[i], "[",linespacing,"em]")
      }
    }
    main_table_split <- main_table_split %>% 
      paste(collapse = "\n")
    x[main_table] <- main_table_split
  }
  
  x
}

fitstat_register("y_mean", function(x) 
  if(length(x$obs_selection) == 0){
  round(mean(eval(x$call$data)[[paste0(x$fml[[2]])]], na.rm = T), 3)
}else{
  round(mean(eval(x$call$data)[x$obs_selection$obsRemoved,][[paste0(x$fml[[2]])]], na.rm = T), 3)
}, alias = "Mean Y")

fitstat_register("panel_fe", function(x) x$fixef_sizes[[1]], alias = "Territories")
fitstat_register("year_fe", function(x) x$fixef_sizes[[2]], alias = "Years")
fitstat_register("f1", function(x) fitstat(x, "ivf1.stat"), alias = "F-test (1st stage)")
fitstat_register("w1", function(x) fitstat(x, "ivwald1.stat"), alias = "Wald (1st stage)")

table_style <- style.tex(main = "aer", 
                         stats.title = "\\midrule",
                         depvar.title = "Dep. var.",
                         fixef.where = "var", tablefoot = F, tablefoot.value = "default",
                         # tablefoot.title = "\midrule",
                         # yesNo = c("$\\checkmark$", ""),
                         yesNo = c("Yes", "No"),
                         line.bottom = "",
                         line.top = "", fixef.suffix = " FE", model.format = "(1)")





# Descriptive summary table -----------------------------------------------

options("modelsummary_format_numeric_latex" = "plain")

datasummary_tex <- function(formula = f, data_new = data_new, output = "latex", linespace = T,
                            sideways = F, coltitle, var_column = "", note, append = "none", header,
                            filename = "print", title = title, tabular = "tabularx", stats, escape = T,
                            fontsize = "small", placement = "t", label = "", tablewidth = 0.98){
  tex_output <- datasummary(formula = formula, data = data_new, output = "latex", title = title, escape = escape) %>% 
    as.character() %>% 
    str_split("\n") %>% 
    unlist()
  if(sideways){
    tex_output[str_detect(tex_output, "begin\\{table\\}")] <- paste0("\\begin{sidewaystable}", "\\", fontsize)
    tex_output[str_detect(tex_output, "end\\{table\\}")] <- paste0("\\end{sidewaystable}")
  }else{
    tex_output[str_detect(tex_output, "begin\\{table\\}")] <- paste0("\\begin{table}[", placement, "] ", "\\", fontsize)
    
  }
  tex_output[str_detect(tex_output, "begin\\{tabular\\}")] <- paste("\\begin{tabularx}{",tablewidth,"\\textwidth}{l*{",stats,"}{S}r}", sep = "")
  tex_output[str_detect(tex_output, "end\\{tabular\\}")] <- paste("\\end{tabularx}", sep = "")
  if(label != ""){
    tex_output[str_detect(tex_output, "caption")] <- paste0("\\caption{", title, "} ", "\\label{", label, "}")
  }
  
  tex_output <- tex_output %>%
    str_replace_all(".times", "$\\\\times$")
  
  tex_output[which(str_detect(tex_output, "toprule"))+1] <- paste0(var_column, "  &  ", paste0(coltitle, collapse = " & "), "\\\\")
  tex_output[str_detect(tex_output, "toprule|bottomrule")] <- ""
  if(linespace){
    main_table <- which(str_detect(tex_output, "\\\\\\\\"))
    for(i in main_table[-1]){
      tex_output[i] <- paste0(tex_output[i], "[.5em]")
    }
  }
  
  if(!missing(note)){
    end_no <- which(str_detect(tex_output, "end\\{sidewaystable\\}|end\\{table\\}"))
    tex_output[end_no + 1] <- tex_output[which(str_detect(tex_output, "end\\{sidewaystable\\}|end\\{table\\}"))]
    tex_output[end_no] <- paste0("\\floatnote{", note, "}")
  }
  
  if(append == "first"){
    midr_no <- which(str_detect(tex_output, "\\\\midrule"))
    tex_output[midr_no-2] <- tex_output[midr_no-1]
    tex_output[midr_no-1] <- tex_output[midr_no]
    tex_output[midr_no] <- paste0(" & \\multicolumn{", stats, "}{c}{\\emph{", header, "}} \\\\ \\cmidrule(lr){2-", stats+2, "}")
    end_no <- which(str_detect(tex_output, "\\\\end\\{table"))
    tex_output[end_no-2] <- "\\\\"
    tex_output[end_no-1] <- ""
    tex_output <- tex_output[-c(end_no)]
  }
  
  if(append == "mid"){
    midr_no <- which(str_detect(tex_output, "\\\\midrule"))
    tex_output <- tex_output[-c(1:midr_no-1)]
    tex_output[1] <- paste0(" & \\multicolumn{", stats, "}{c}{\\emph{", header, "}} \\\\ \\cmidrule(lr){2-", stats+2, "}")
    end_no <- which(str_detect(tex_output, "\\\\end\\{table"))
    tex_output[end_no-2] <- "\\\\"
    tex_output[end_no-1] <- ""
    tex_output <- tex_output[-c(end_no)]
  }
  
  if(append == "last"){
    midr_no <- which(str_detect(tex_output, "\\\\midrule"))
    tex_output <- tex_output[-c(1:midr_no-1)]
    tex_output[1] <- paste0(" & \\multicolumn{", stats, "}{c}{\\emph{", header, "}} \\\\ \\cmidrule(lr){2-", stats+2, "}")
  }
  
  if(filename == "print"){
    print(tex_output)
  }else{
    if(append == "none" | append == "first"){
      cat(tex_output, file = filename, sep = "\n")
    }else if(append == "mid" | append == "last"){
      cat(tex_output, file = filename, append = T, sep = "\n")
    }else{
      stop("'Append' option not valid")
    }
    
  }
}
