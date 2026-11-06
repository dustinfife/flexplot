#' @importFrom rlang ":="
add_bin_to_new_dataset = function(plot, d, terms, term.re, outcomevar) {
  
  # variable isn't binned/summarized!
  are_any_binned = grep("_binned", names(plot$data))
  if (length(are_any_binned)==0) return(d)
  
  # figure out which is binned
  binned_var = names(plot$data)[are_any_binned]
  variable_to_be_binned = gsub("_binned", "", binned_var)
  gg_dataset = plot$data
  
  
  extract_numbers_from_binned_var(x_bin=gg_dataset[,binned_var[1]], 
                                  x_original = d[,variable_to_be_binned[1]])
  # extract breakpoints from plot data, then break the new one
    d_to_binned = 1:length(variable_to_be_binned) %>% 
    purrr::map(function(x) extract_numbers_from_binned_var(gg_dataset[,binned_var[x]], d[,variable_to_be_binned[x]])) %>%
    data.frame 
  names(d_to_binned) = binned_var
  
  if (nrow(d_to_binned)==0) return(d)
  d[,binned_var] = d_to_binned[,binned_var]
  # create a new string of terms that needs to be summarized by
  nt = c(terms[-which(terms %in% variable_to_be_binned)], binned_var)
  
  # remove extra REs because we don't need any of them,e xcept ggplot requires a column name for it
  if ("model" %in% names(d)) d[[term.re]] = factor(d[[term.re]]) else d[[term.re]] = d[[term.re]][1] %>% factor(d[[term.re]][1]) 
  d = d %>% 
    group_by_at(nt) %>% 
    summarize(!!rlang::sym(outcomevar) := mean(!!(rlang::sym(outcomevar))))
  return(d)
}

extract_numbers_from_binned_var = function(x_bin, x_original) {
  
  # regex to extract the breaks
  regex_cmd_one = gsub("([(]?-?[0-9]*.?[0-9]*[e]?-?[0-9]*[)]?)-([(]?-?[0-9]*.?[0-9]*[e]?-?[0-9]*[)]?)", "\\1", x_bin)
  regex_cmd_two = gsub("([(]?-?[0-9]*.?[0-9]*[e]?-?[0-9]*[)]?)-([(]?-?[0-9]*.?[0-9]*[e]?-?[0-9]*[)]?)", "\\2", x_bin)
  regex_cmd = c(regex_cmd_one, regex_cmd_two)
  regex_cmd = gsub("[)]", "", gsub("[(]", "", regex_cmd))
  break_vals = sort(as.numeric(unique(regex_cmd)))
  
  which_too_high = x_original>max(break_vals)
  x_original[which_too_high] = max(break_vals)-.0001
  which_too_low = x_original<min(break_vals)
  x_original[which_too_low] = min(break_vals)+.0001  
  x_original = data.frame(x_original)
  names(x_original)[1] = "x_original"
  # replace observations > max with the max
  x_original = bin.me("x_original", x_original, breaks=break_vals)
  return(x_original)
}

'%!in%' <- function(x,y)!('%in%'(x,y))

## function to generate predicted differences, standardized
standardized_differences = function(model1, model2, sigma=TRUE){
  
  ### check for nested functions
  nested = check_nested(model1, model2)
  
  ### handle missing data from one model to the next
  new_models = check_model_rows(model1, model2, nested)
  model1 = new_models[[1]]; model2 = new_models[[2]]  
  
  # make sure the two models have the same DV
  dv1 = extract_data_from_fitted_object(model1)[,1]
  dv2 = extract_data_from_fitted_object(model2)[,1]

  if (all(dv1 != dv2)) {
    stop("It looks like you're trying to compare two models with different outcome variables.")
  }
  
  pred1 = predict(model1, type="response")
  pred2 = predict(model2, type="response")
  differences = round(
    (quantile(abs(predict(model1, type="response") - 
                    predict(model2, type="response")))), digits=3)
  differences
}

#### make a custom labeller that removes "_binned"
custom.labeler = function(x){
  lapply(names(x),function(y){
    paste0(gsub("_binned", "", y),":\n", x[[y]])
  })
}

#### make sure all variables are in data
check_all_variables_exist_in_data = function(variables, data) {
  
  if (is.null(variables)) return(NULL)
  
  missing.preds = variables[which(!(variables %in% names(data)))]
  if (length(missing.preds)>0){
    stop(paste0("One or more of your predictor variables: ", paste0(missing.preds, collapse=","), " are missing. Did you specify the right dataset and spell the variables correctly?"))
  }
  return(NULL)
}  

# In flexplot/R/compare.fits_methods.R (or wherever this lives)
extract_data_from_fitted_object = function(object) {
  
  # Helper: safest env to evaluate the model's data
  get_eval_env = function(obj) {
    f = tryCatch(stats::formula(obj), error = function(e) NULL)
    if (!is.null(f) && !is.null(environment(f))) return(environment(f))
    parent.frame()
  }
  
  # 1) Prefer model.frame() whenever available (keeps NA handling, contrasts, etc.)
  if (inherits(object, c("lm", "glm", "rlm",
                         "lmerMod", "glmerMod", "lmerModLmerTest"))) {
    return(stats::model.frame(object))
  }
  
  # 2) party::RandomForest (S4) — keep your current handling
  if (inherits(object, "RandomForest")) {
    outcome = object@responses@variables
    predictors = object@data@get("input")
    data = cbind(outcome, predictors)
    return(data)
  }
  
  # 3) randomForest::randomForest fitted with a formula
  #    (class often "randomForest", sometimes shows as "randomForest.formula")
  if (inherits(object, c("randomForest", "randomForest.formula"))) {
    fml = tryCatch(stats::formula(object), error = function(e) NULL)
    if (!is.null(fml)) {
      vars = all.vars(fml)
      cl = tryCatch(stats::getCall(object), error = function(e) NULL)
      if (!is.null(cl) && !is.null(cl$data)) {
        env = get_eval_env(object)
        data = eval(cl$data, envir = env)
        return(data[, vars, drop = FALSE])
      }
    }
    # Fall through to the generic path if we couldn't resolve it here
  }
  
  # 4) Generic fallback — evaluate call$data in the model/formula environment
  cl = tryCatch(stats::getCall(object), error = function(e) NULL)
  if (!is.null(cl) && !is.null(cl$data)) {
    env = get_eval_env(object)
    return(eval(cl$data, envir = env))
  }
  
  stop("extract_data_from_fitted_object: could not recover the original data from this model.")
}


check_nested = function(model1, model2) {
  #### collect terms
  mod1 = get_predictors(model1)
  mod2 = get_predictors(model2)
  
  #### check for nested models
  if (all(length(mod1)> length(mod2) & (mod2 %in% mod1) & (class(model1)[1] == class(model2)[1]))) return(T)
  if (all(length(mod2)>=length(mod1) & (mod1 %in% mod2) & (class(model1)[1] == class(model2)[1]))) return(T)
  return(F)
}

get_predictors = function(model) {
  
  # try to get the terms and see if it fails
  mod1 = try({attr(terms(model), "term.labels")}, silent=TRUE)
  
  # no failure = return
  if (class(mod1)!="try-error") return(unlist(mod1))
  
  # now try another way (this will get randomForest)
  mod = try({getCall(model)$formula}, silent=TRUE)
  
  # no failure = return
  if (class(mod)!="try-error") return(all.vars(mod)[-1])
  
  # now deal with cases where there is a failure
  if (class(model)[1]=="RandomForest") {
    return(all.vars(model@data@formula$input))
  }
}

## function that does nested model comparisons on a single fitted model
nested_model_comparisons = function(object){
  
  ### extract necessary terms
  terms = attr(terms(object), "term.labels")
  
  ### temporary function that updates model
  removed.one.at.a.time = function(i, terms, object){
    new.f = as.formula(paste0(". ~ . -", terms[i]))
    new.object = update(object, new.f)
    list(
      rsq = summary(object)$r.squared - summary(new.object)$r.squared,
      bayes.factor = bf.bic(object, new.object, invert=FALSE)
    )
  }
  
  mc = t(sapply(1:length(terms), removed.one.at.a.time, terms=terms, object=object))
  mc = data.frame(cbind(terms,mc), stringsAsFactors = FALSE)
  mc
}

check.non.number = function(x){
  return.bool = ifelse(is.character(x) | is.factor(x), TRUE, FALSE)
  return.bool
}

variable_types = function(variables, data, return.names=F){
  if (length(variables)>0){
    characters = sapply(data[,variables, drop=F], check.non.number) 
    numbers = !characters
    if (return.names){
      list(characters=names(characters)[which(characters)], numbers=names(characters)[which(numbers)])  
    } else {
      list(characters=(characters), numbers=(numbers))  
    }
    
  }
}

#### if both numeric and factor, put numeric on x axis and factor as color/line
# predictors = c("Grad.School", "Years", "GPA", "Profession")
# data = graduate_income
# outcome = "Income"
make_flexplot_formula = function(predictors, outcome, data){
  
  # omit those variables not in the dataset
  nothere = which (!(predictors %in% names(data)))
  if (length(nothere)>0) predictors = predictors[-nothere]
  
  # if they don't have predictors (i.e., fitting a means model)
  if (length(predictors) == 0) {
    return(make.formula(outcome, "1"))
  }
  # if there's only one variable, make it
  if (length(predictors)==1){
    return(make.formula(outcome, predictors))
  } 
  
  
  # algorithm that puts numeric in first slot, categorical in second slot
  favored.slots = c(1,4,3,2)
  vtypes = variable_types(predictors, data)
  numb = vtypes$numbers
  cat = vtypes$characters
  levs = sapply(data[,predictors], function(x) length(levels(x)))
  custom.sort = numb*1000 + cat*levs
  custom.sort = sort(custom.sort, decreasing=T)
  slots = names(custom.sort)[favored.slots]
  
  
  #### now create formula
  x = c(outcome, "~",slots[1], slots[2], "|", slots[3], slots[4])
  if (any(is.na(x)))  x = x[-which(is.na(x))]
  x = paste0(x, collapse="+")
  x = gsub("+|+", "|", x, fixed=T);x = gsub("+~+", "~", x, fixed=T)
  x = gsub("+|", "", x, fixed=T)
  f = as.formula(x)	
  return(f)
  
}

remove_nonlinear_terms = function(terms) {
  return(grep("[/^:]", terms, value=T, invert=T))
}
# tested
# tested
test_same_class = function(model1, model2) {
  # if neither are lme4
  if (class(model1)[1] != "lmerMod" & class(model2)[1] != "lmerMod") return(NULL)
  
  # if one, but not both are lme4
  if (class(model1)[1] != class(model2)[1]) stop("It looks like you're trying to compare two models that are not both lme4 models. I can't do that! Sorry! \n\n Maybe you should go binge Netflix.")
  
  # if they have different random terms
  re_one = extract_random_term(model1)
  re_two = extract_random_term(model2)
  if (re_one != re_two) stop("Whoa there, tiger. You can't have different random effects for the two models.")
}


	### create custom function to sample data
sample.subset = function(sample, data){
	if (sample!=Inf){
		m = data[sample(1:nrow(data), size=min(sample, nrow(data))),]
		return(m)
	} 
	return(data)
}

	### if they don't want raw data, just make alpha = 0
raw.alph.func = function(raw.data,alpha=1){
	if (raw.data){
		alpha.raw = alpha
	} else {
		alpha.raw = 0
	}	
  alpha.raw
}


#match_jitter_categorical(.1)
match_jitter_categorical = function(x){
  
  if (is.null(x)) return(c(.2, 0))
  if (x[1]==TRUE) return(c(.2, 0))
  
  if (length(x)==2 & is.numeric(x))
    return(x)
  else if (is.numeric(x) & length(x)==1)
    return(c(x, 0))  
  else if (!x[1])
    return(c(0,0))
  else
    stop("something's wrong with how you specified jittering.")
}

#match_jitter_categorical(.2)
#match_jitter_categorical(T)
#match_jitter_categorical(c(.2, .1))
#match_jitter_categorical(F)
#match_jitter_categorical(c(F,T))
# find in file. Thanks to https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files
fif <- function(what, where=".", in_files="\\.[Rr]$", recursive = TRUE,
                ignore.case = TRUE) {

  fils <- list.files(path = where, pattern = in_files, recursive = recursive, full.names = TRUE)
  found <- FALSE
  
  file_cmd <- Sys.which("file")
  
  for (fil in fils) {
    
    if (nchar(file_cmd) > 0) {
      ftype <- system2("file",fil, TRUE)
      if (!grepl("text", ftype)[1]) next
    }
    
    contents <- readLines(fil, warn=FALSE)
    
    res <- grepl(what, contents, ignore.case = ignore.case)
    res <- which(res)
    
    if (length(res) > 0) {
      
      found <-  TRUE
      
      cat(sprintf("%s\n", fil), sep="")
      cat(sprintf(" % 4s: %s\n", res, contents[res]), sep="")
      
    }
    
  }
  
  if (!found) message("(No results found)")
  
}
