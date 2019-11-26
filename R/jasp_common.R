#jasp
# variables = options$variables
# dataset =data = exercise_data
add_polynomials = function(variables, data, degree=2){
  cat = sapply(data[,variables, drop=FALSE], check.non.number)
  f = function(x) paste0("I(", x, "^", degree, ")")
  sapply(variables[!cat], f)
}