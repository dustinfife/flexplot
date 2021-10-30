### return dataset containing factorized random effects (tested)
subset_random_model = function(object, formula, d, samp.size = 3) {
  
  if (class(object)[1] == "lmerMod") {
    
    ## get random term
    term.re = extract_random_term(object)
    samp = stratified_sample_re(formula, data=d, re=term.re, samp.size)
    
    #### randomly sample the re terms and convert to numeric
    # unique.terms = unique(d[[term.re]])
    # samp = sample(unique.terms, size=min(samp.size, length(unique.terms)))
    k = d[d[[term.re]]%in%samp,]; k[[term.re]] = as.factor(k[[term.re]])
    return(k)
  }
  
  return(d)
}

extract_random_term = function(object) {
  if (class(object)[1] != "lmerMod") return(NULL)
  
  #### extract formula
  form = as.character(formula(object))[3]
  
  #### identify random effects
  term.re = trimws(gsub("(.*\\|)(.*)[)]", "\\2", form))
  return(term.re)
}

#expect_equal(gsub_data_first(c("a", "b", "c"), "b", "c"), c("a", "c", "c"))
gsub_data_first = function(data, old, new, ...) {
  return(gsub(old, new, data, ...))
}

# expect_equal(remove_term_from_formula(y~a + b | c + d, "b", F), "y~a|c+d")
# expect_equal(remove_term_from_formula(y~a + b | c + d, "c", F), "y~a+b|d")
# expect_equal(remove_term_from_formula(y~a + b | c + d, "d", F), "y~a+b|c")
# expect_equal(remove_term_from_formula(y~a + b | c + d, "d", T), y~a+b|c)
remove_term_from_formula = function(formula, term, as_formula = T) {
  
  formula_no_ws = gsub(" ", "",  format(formula)) %>%
    gsub_data_first(paste0(term, "+"), "", fixed=T) %>%
    gsub_data_first(paste0("+",term), "", fixed=T) %>%
    gsub_data_first(paste0("|",term), "|", fixed=T) 
  
  # check for hanging pipe at the end
  last_char = nchar(formula_no_ws)
  if (substr(formula_no_ws, last_char, last_char) == "|") formula_no_ws = gsub("|", "", formula_no_ws, fixed=T)
  
  # return as either a formula or as text
  if (as_formula) return(formula(formula_no_ws))
  return(formula_no_ws)
}

#p = flexplot(speed~agility | superpower + ptsd, data=avengers)
#expect_equal(get_row_col(p), 6)
# expect_equal(get_row_col(flexplot(speed~agility | superpower, data=avengers)), 2)
# expect_equal(get_row_col(flexplot(speed~superpower | agility + ptsd, data=avengers)), 9)
# expect_equal(get_row_col(flexplot(speed~superpower | agility + ptsd, data=avengers, bins=4)), 16)
get_row_col <- function(p) {
  elements = p %>%
    ggplot2::ggplot_build() %>%
    purrr::pluck('layout') %>% 
    purrr::pluck('layout') %>%
    suppressWarnings
  return(nrow(elements))
}

#a = stratified_sample_re(weight.loss~health | motivation + therapy.type, data=exercise_data, re="satisfaction", samp.size=6)
#expect_true(length(a)==6)
#expect_true(all(!duplicated(a)))
#b = stratified_sample_re(MathAch~Sex | SES + School, data=math, re="School", samp.size =8)
#expect_true(length(b)==8)
#expect_true(all(!duplicated(b)))
#c = stratified_sample_re(MathAch~Sex | SES + School, data=math, re="School", samp.size =200)
#expect_true(length(c)!=200)
#expect_true(all(!duplicated(c)))
#stratified_sample_re(MathAch~ SES + School| Sex, data=math, re="School", 11)
stratified_sample_re = function(formula, data, re, samp.size=6) {

  paneled = find_paneled_variables(formula)
  
  # identify the panels from the formula
  rhs = labels(terms(formula))
  predictors = strsplit(rhs, "\\|") %>% unlist %>% strsplit("\\+") %>% unlist %>% trimws
  
  if (is.null(paneled)) return(sample(data[,re], min(samp.size, nrow(data))))
  
  # remove id from formula
  formula_sans_re = remove_term_from_formula(formula, re)
  
  # make preliminary flexplot (without ID)
  flexplot_data = flexplot(formula_sans_re, data=data) 
  
  # identify/remove those variables binned
  minus = !(paste0(predictors, "_binned") %in% names(flexplot_data$data))
  binned_data = flexplot_data$data %>% dplyr::select(c(contains("_binned"),predictors[minus], re))
  binned_or_not = c(paneled, paste0(paneled, "_binned")) 
  grouped_variables = binned_or_not[binned_or_not %in% names(binned_data)]
  toss_re = which(grouped_variables == re)
  if (length(toss_re)>0) { grouped_variables = grouped_variables[-toss_re] } 
  
  # figure out how many panels there are
  number_panels = get_row_col(flexplot_data)%>%prod
  
  # figure out how many to sample within a panel
  sample_size_within_group = round(samp.size/number_panels)

  # this will ensure all panels are sampled. 
  # then we'll have to add to that to meet our minimum sample size
  selected_IDs = binned_data %>% 
    group_by(across(all_of(c(grouped_variables)))) %>%
    sample_n(max(sample_size_within_group, 1)) %>%
    ungroup %>%
    select(re) %>%
    purrr::pluck(re) %>%
    unique

  if (length(selected_IDs) >= samp.size) return(selected_IDs[1:samp.size])
  additional_n = samp.size - length(selected_IDs)
  remaining_IDs = data[,re][data[,re] %!in% selected_IDs] %>% 
    unique() 
  IDs = c(selected_IDs, sample(remaining_IDs, min(additional_n, length(remaining_IDs))))
  return(IDs)
}


find_paneled_variables = function(formula) {
  
  # see if there's a vertical pipe in there
  if (length(grep("|", format(formula), fixed=T))==0) return(NULL)
      
  # identify the panels from the formula
  rhs = labels(terms(formula))
  predictors = strsplit(rhs, "\\|") %>% unlist %>% strsplit("\\+") %>% unlist %>% trimws
  
  # group_by variables are those that were panelled. identify using the formula
  paneled = strsplit(rhs, "\\|") %>% unlist %>% purrr::pluck(2) %>% strsplit("\\+") %>% unlist %>% trimws
  
  return(paneled)
}

# randomly_sample_clusters = function(d, term.re, sample){
#   #### randomly sample the re terms and convert to numeric
#   unique.terms = unique(d[,term.re])
#   samp = sample(unique.terms, size=min(sample, length(unique.terms)))
#   k = d[d[,term.re]%in%samp,]; k[,term.re] = as.factor(k[,term.re])
#   return(k)
# }

# this will return a flexplot formula for mixed models
make_formula_mixed = function(preds, term.re, outcome, formula=NULL) {
  if (!is.null(formula)) return(formula)
  
  ### come up with formula
  slots = c(1,3,4)
  form.slots = rep(NA, times=4)
  for (i in 1:min(4,length(preds))){
    if (preds[i]!=term.re){
      form.slots[slots[i]] = preds[i]
    }
  }
  
  ### for random effects models, just put school in first slot
  if (length(preds)>1) form.slots[2] = term.re else form.slots[1] = term.re
  
  symbol.slots = c("~","+", "|", "+")
  formula = paste0(symbol.slots, form.slots, collapse="")
  formula = gsub("\\|NA", "", formula);formula = gsub("\\+NA", "", formula);
  formula = paste0(outcome, formula, collapse="")
  
  formula = formula(formula)
  return(formula)
}

# this function identifies whether RE are going to be plotted
are_re_plotted = function (formula, term.re) {
  
  ### figure out where random component is
  f.char = as.character(formula)[3]
  criteria = paste0("\\+.*", term.re)
  
  ### if random component is in slot 2, modify the formula
  if (length(grep(criteria, f.char))>0) return(T) else return(F)
}
