#### this function converts a binary variable to a 1/0 for logistic regression
factor.to.logistic = function(data, outcome, method=NULL, labels=F){

  levels_dv = length(unique(data[,outcome]))
  # return if it's not logistic
  if (levels_dv != 2) return(data)
  if (labels) return(unique(data[,outcome]))
  if (is.numeric(data[,outcome])) return(data)
  if (method != "logistic") return(data)
  if (is.factor(data[[outcome]])) data[[outcome]] = droplevels(data[[outcome]])
  # at this point it's categorical, has two levels, but doesn't necessarily have "logistic" as a method  
  
  # if it's an ordered factor, make the second level the referent level
  if (class(data[,outcome])[1] == "ordered") {
    data[,outcome] = factor(data[,outcome], levels=levels(data[,outcome]), labels=c(0,1)) %>% 
      as.character() %>%
      as.numeric() 
    return(data)
  }
  
  # if they have yes and no, convention is to put yes at the top of the y axis
  #(glm will predict the *second* level, so we need to reverse it)
  #the rest are just regular factors
  # Get stable, sorted levels (works for factor or character)
  if (is.factor(data[[outcome]])) {
    lvls = levels(data[[outcome]])
  } else {
    lvls = sort(unique(as.character(data[[outcome]])))  # for character vars
  }

  data[,outcome] = factor(data[,outcome], levels=lvls, labels=(c(0,1))) %>%
    as.character() %>%
    as.numeric()
  
  return(data)
}


# reverse_yes_and_no_in_logistic = function(data, outcome) {
#   current_levels = levels(data[,outcome])
#   
#   if (all(c("yes", "no") %in% current_levels) | 
#       all(c("Yes", "No") %in% current_levels) |
#       all(c("YES", "NO") %in% current_levels) |
#       all(c("Y", "N") %in% current_levels)) {
#     
#     new_dv = factor(data[,outcome], levels=rev(levels(data[,outcome])), labels=c(0,1)) 
#     return(new_dv)
#   } else {
#     data[,outcome] = factor(data[,outcome], levels=unique(data[,outcome]), labels=rev(c(0,1))) %>%
#       as.character() %>%
#       as.numeric() 
#     return(data[,outcome])
#   }
#   
# }

return_labels_for_logistic_regression = function(data, outcome, method) {
  if (method != "logistic")                  return(NULL)
  if (length(unique(data[,outcome]))!=2)     return(NULL)
  if (class(data[,outcome])[1] == "ordered") return(levels(data[,outcome]))
  if (is.numeric(data[,outcome]))            return(sort(unique(data[,outcome])))
  # R predicts the second of the factors
  return(sort(unique(data[,outcome]), decreasing = F))
}  

flexplot_alpha_default = function(data, axis, alpha){
  if (axis[1] == "1") return(alpha)
  if (!is.numeric(data[,axis[1]]) & alpha == .99977) return(.4)
  if ( is.numeric(data[,axis[1]]) & alpha == .99977) return(.5)
  return(alpha)
}

is_binary = function(x) {
  x = stats::na.omit(x)
  
  if (is.factor(x) || is.character(x)) {
    return(length(unique(x)) == 2)
  }
  
  if (is.numeric(x)) {
    return(length(unique(x)) == 2 && all(x %in% c(0, 1)))
  }
  
  return(FALSE)
}

is_categorical = function(x) {
  is.factor(x) || is.character(x) ||
    (is.numeric(x) &&
       length(unique(stats::na.omit(x))) <= 5)
}


make_levels_same_for_prediction_dataset = function(data, prediction, axis) {
  axis_1_is_categorical = !is.numeric(data[[axis[1]]])
  axis_1_not_1          = axis[1] != "1"
  if (axis_1_is_categorical & axis_1_not_1){
    prediction[[axis[1]]] = factor(prediction[[axis[1]]], levels=levels(data[[axis[1]]]))
  }
  return(prediction)
}

check_se = function(se=NULL, axis) {
  if (!is.null(se)) return (se)
  if (length(axis)>1) return(F)
  return(T)
}


flexplot_high_cardinality_message = function() {
  message("It looks like you're trying to plot more than 6 groups. I gotta give it to you...you're ambitious! Alas, things get a little crowded with that many colors, lines, and symbols. So I'm keeping the colors, ditching the fancy symbols and line types, and hiding the legend. Don't worry, all your data are still there. I hope we can still be friends.")
}


make_prediction_dataset_same_type_on_x1 = function(data, prediction, axis1) {
  if (is.null(prediction)) return(NULL)
  if (is.numeric(data[,axis1])  & ! is.numeric(prediction[,axis1])) prediction[,axis1] = as.numeric(as.character(prediction[,axis1]))
  if (!is.numeric(data[,axis1]) &   is.numeric(prediction[,axis1])) prediction[,axis1] = (as.character(prediction[,axis1]))
  return(prediction)
}
