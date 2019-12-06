#jasp
# variables = options$variables
# dataset =data = exercise_data
add_polynomials = function(variables, data, degree=2){
  cat = sapply(data[,variables, drop=FALSE], check.non.number)
  f = function(x) paste0("I(", x, "^", degree, ")")
  sapply(variables[!cat], f)
}

theme_it = function(plot, theme){
  if (theme == "JASP"){
    plot = themeJasp(plot)
  } else {
    themes = list("black and white"="theme_bw()+ theme(text=element_text(size=18))",
                 "minimal" = "theme_minimal()+ theme(text=element_text(size=18))",
                 "classic" = "theme_classic()+ theme(text=element_text(size=18))",
                 "dark" = "theme_dark() + theme(text=element_text(size=18))")
    plot = plot + eval(parse(text=themes[[theme]]))
  }
  return(plot)
}