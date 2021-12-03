#' @importFrom rlang ":="
add_bin_to_new_dataset = function(plot, d, terms, term.re, outcomevar) {
  
  # variable isn't binned/summarized!
  are_any_binned = grep("_binned", names(plot$data))
  if (length(are_any_binned)==0) return(d)
  
  # figure out which is binned
  binned_var = names(plot$data)[are_any_binned]
  variable_to_be_binned = gsub("_binned", "", binned_var)
  gg_dataset = plot$data
  
  # extract breakpoints from plot data, then break the new one
  regex_cmd_one = gsub("([(]?-?[0-9]*.?[0-9]*[)]?)-([(]?-?[0-9]*.?[0-9]*[)]?)", "\\1", gg_dataset[[binned_var]])
  regex_cmd_two = gsub("([(]?-?[0-9]*.?[0-9]*[)]?)-([(]?-?[0-9]*.?[0-9]*[)]?)", "\\2", gg_dataset[[binned_var]])
  regex_cmd = c(regex_cmd_one, regex_cmd_two)
  regex_cmd = gsub("[)]", "", gsub("[(]", "", regex_cmd))
  break_vals = sort(as.numeric(unique(regex_cmd)))
  rep = gg_dataset[,variable_to_be_binned]>max(break_vals)
  gg_dataset[rep,variable_to_be_binned] = max(break_vals)-.0001
  rep = gg_dataset[,variable_to_be_binned]<min(break_vals)
  
  gg_dataset[rep,variable_to_be_binned] = min(break_vals)+.0001  
  breaks = prep.breaks(variable_to_be_binned, gg_dataset, breaks=break_vals)
  
  # replace observations > max with the max
  rep = d[,variable_to_be_binned]>max(break_vals)
  d[rep,variable_to_be_binned] = max(break_vals)-.0001
  rep = d[,variable_to_be_binned]<min(break_vals)
  d[rep,variable_to_be_binned] = min(break_vals)+.0001  
  d[[binned_var]] = bin.me(variable_to_be_binned, d, breaks=breaks)
  
  # create a new string of terms that needs to be summarized by
  nt = c(terms[-which(terms %in% variable_to_be_binned)], binned_var)
  
  # remove extra REs because we don't need any of them,e xcept ggplot requires a column name for it
  if ("model" %in% names(d)) d[[term.re]] = factor(d[[term.re]]) else d[[term.re]] = d[[term.re]][1] %>% factor(d[[term.re]][1]) 
  d = d %>% 
    group_by_at(nt) %>% 
    summarize(!!rlang::sym(outcomevar) := mean(!!(rlang::sym(outcomevar))))
  return(d)
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

extract_data_from_fitted_object = function(object) {
  if (class(object)[1]=="lm" | class(object)[1]=="glm" | class(object)[1] == "rlm") return(object$model)
  if (class(object)[1]=="RandomForest") {
    outcome = object@responses@variables
    predictors = object@data@get("input")
    data = cbind(outcome, predictors)
    return(data)
  }  
  if (class(object)[1] == "randomForest.formula") {
    vars = all.vars(formula(object))
    data = eval(getCall(object)$data)
    return(data[,vars])
  }
  
  if (class(object)[1] == "lmerMod") {
    data = model.frame(object)
    return(data)
  }
  # this should work for the rest?? But it won't be in the right order!
  return(eval(getCall(object)$data))
}

check_nested = function(model1, model2) {
  #### collect terms
  mod1 = get_predictors(model1)
  mod2 = get_predictors(model2)
  
  #### check for nested models
  if (all(length(mod1)> length(mod2) & (mod2 %in% mod1) & (class(model1) == class(model2)))) return(T)
  if (all(length(mod2)>=length(mod1) & (mod1 %in% mod2) & (class(model1) == class(model2)))) return(T)
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
  if (is.null(x)){
    return(c(.2, 0))
  }else if (length(x)==2 & is.numeric(x))
    return(x)
  else if (is.numeric(x) & length(x)==1)
    return(c(x, 0))  
  else if (is.null(x) | x==TRUE)
    return(c(.2, 0))
  else if (!x)
    return(c(0,0))
  else
    stop("something's wrong with how you specified jittering.")
}

#match_jitter_categorical(.2)
#match_jitter_categorical(T)
#match_jitter_categorical(c(.2, .1))
#match_jitter_categorical(F)
#match_jitter_categorical(c(F,T))
#jitter = c(.2, .4); data=exercise_data; axis.var=c("therapy.type", "gender")
	#### points = the datapoints
points.func = function(axis.var, data, jitter){

  if (is.null(jitter) & !check.non.number(data[,axis.var[1]])){
    jitter = c(0,0)
  } else {
    jitter = match_jitter_categorical(jitter)
  }
  
  ### if they have two axis variables that are categorical
  if (length(axis.var)>1 & all(sapply(data[,axis.var, drop=F], check.non.number))){
    jit = paste0("geom_point(data=sample.subset(sample, ", deparse(substitute(data)), 
                 "), alpha=raw.alph.func(raw.data, alpha=alpha), position=position_jitterdodged(jitter.width=", jitter[1], ", jitter.height=", jitter[2], ", dodge.width=.5))")				
  
  ### if they have one categorical axis
  } else if (length(axis.var)==1 & check.non.number(data[,axis.var])){
    jit = paste0("geom_jitterd(data=sample.subset(sample, ", deparse(substitute(data)), "), alpha=raw.alph.func(raw.data, alpha=alpha), width=", jitter[1], ", height=", jitter[2], ")")				
  } else {  
    jit = paste0("geom_jitterd(data=sample.subset(sample, ", deparse(substitute(data)), "), alpha=raw.alph.func(raw.data, alpha=alpha), width=", jitter[1], ", height=", jitter[2], ")")				
  }
  
	#### return the jittered string
	return(jit)		
}

# points.func(axis.var="therapy.type", data=exercise_data, jitter=NULL)
# points.func("therapy.type", exercise_data, T)
# points.func("therapy.type", exercise_data, F)
# points.func("motivation", exercise_data, NULL)
# points.func(axis.var=c("motivation", "therapy.type"), data=exercise_data, jitter=NULL)
# points.func(c("gender", "therapy.type"), exercise_data, NULL)
# points.func(c("gender", "therapy.type"), exercise_data, c(.2, .1))


	#### this function converts a binary variable to a 1/0 for logistic regression
factor.to.logistic = function(data, outcome, method=NULL, labels=F){
  
  levels_dv = length(unique(data[,outcome]))
  
  # return if it's not logistic
  if (levels_dv != 2) return(data)
  if (labels) return(unique(data[,outcome]))
  if (is.numeric(data[,outcome])) return(data)
  if (method != "logistic") return(data)
  # at this point it's categorical, has two levels, but doesn't necessarily have "logistic" as a method  
  ### now do the conversion
  data[,outcome] = as.numeric(as.character(factor(data[,outcome], levels=unique(data[,outcome]), labels=c(0,1))))
  return(data)

}

##' @importFrom MASS rlm	
#### identify the correct "fit"
fit.function = function(outcome, predictors, data, suppress_smooth=FALSE, method="loess", spread="sterr", mean.line=F, categorical=FALSE){
	
	if (is.numeric(data[,predictors]) & !categorical){
		if (suppress_smooth){
			fit.string = "xxxx"
		} else if (method=="logistic") {
	
			#### make sure there's only two levels
			if (length(unique(data[,outcome]))!=2){
				stop("To fit a logistic curve, you must have only two levels of your outcome variable.")
			}

			fit.string = 'geom_smooth(method = "glm", method.args = list(family = "binomial"), se = se, formula = y~x)'			
		} else if (method=="rlm"){
			fit.string = 'geom_smooth(method = "rlm", se = se, formula = y~x)'
		}else if (method=="poisson" | method=="Gamma") {
			#### specify the curve
			fit.string = 'geom_smooth(method = "glm", method.args = list(family = method), se = se, formula = y~x)'
		} else if (method=="polynomial" | method == "quadratic"){
			fit.string = 'stat_smooth(method="lm", se=se, formula=y ~ poly(x, 2, raw=TRUE))'
		} else if (method=="cubic"){
			fit.string = 'stat_smooth(method="lm", se=se, formula=y ~ poly(x, 3, raw=TRUE))'
		} else if (method=="lm"){
			fit.string = 'stat_smooth(method="lm", se=se, formula = y~x)'
		} else {
			fit.string = 'geom_smooth(method="loess", se=se, formula = y~x)'
		}
		

		
	} else {
		
		if (suppress_smooth){
			summary1="xxxx"
			summary2="xxxx"
			sum.line="xxxx"						
		} else if (spread=="stdev"){
			summary1 = "stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')" 
			summary2 = "stat_summary(geom='errorbar', fun.ymin = function(z){mean(z)-sd(z)}, fun.ymax = function(z) {mean(z)+sd(z)}, fun.y=median, size = 1.25, width=.2, position=position_dodge(width=.5), color = '#bf0303')"
			if (mean.line){
				sum.line = 'stat_summary(aes_string(group= axis[2]), geom="line", fun.y="mean", position=position_dodge(width=.5), color = "#bf0303")'
			} else {
				sum.line='xxxx'
			}
		} else if (spread=="sterr"){	
			summary1 = "stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')"
			summary2 = "stat_summary(geom='errorbar', fun.ymin = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, fun.ymax = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, width=.2, size = 1.25, position=position_dodge(width=.2), color = '#bf0303')"
			if (mean.line){
				sum.line = 'stat_summary(aes_string(group= axis[2]), geom="line", fun.y="mean", position=position_dodge(width=.2), color = "#bf0303")'
			} else {
				sum.line='xxxx'
			}

		} else if (spread == "quartiles"){	
			summary1 = "stat_summary(fun.y='median', geom='point', size=3, position=position_dodge(width=.4), color = '#bf0303')" 
			summary2 = "stat_summary(geom='errorbar', fun.ymin = function(z){quantile(z, .25)},size = 1.25,  fun.ymax = function(z) {quantile(z, .75)}, fun.y=median, width=.2, position=position_dodge(width=.4), color = '#bf0303')"
			if (mean.line){
				sum.line = 'stat_summary(aes_string(group=axis[2]), geom="line", fun.y="median", position=position_dodge(width=.4), color = "#bf0303")'
			} else {
				sum.line='xxxx'
			}

		}
		
		fit.string = paste0(summary1, "+",summary2, "+", sum.line)		
		### check package version of ggplot2
		if (packageVersion("ggplot2")>"3.2.1"){
		  fit.string = gsub("fun.ymin", "fun.min", fit.string, fixed=T)
		  fit.string = gsub("fun.ymax", "fun.max", fit.string, fixed=T)
		  fit.string = gsub("fun.y", "fun", fit.string, fixed=T)
		} else {
		  fit.string
		}
		
	}

	return(fit.string)
	
}


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