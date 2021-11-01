bin.me = function(variable, data, bins=NULL, labels=NULL, breaks=NULL, check.breaks=TRUE, return.breaks=FALSE){

  ### if they come as a list, unlist them
  # no need for if because unlist will return vectors if passed a vector
  breaks = unlist(breaks)
  labels = unlist(labels) 
  
  bins = choose_bins(labels, breaks)
  
  #### if they supply breaks, make sure there's a good min/max value	
  if (!is.null(breaks) & check.breaks) breaks = prep.breaks(variable, data, breaks)
  
  ### if we don't have breaks at this point, make some
  if (is.null(breaks)) breaks = quantile(as.numeric(data[[variable]]), seq(from=0, to=1, length.out=bins+1), na.rm=T)
  
  ### if they don't provide labels, make them easier to read (than R's native bin labels)
  labels = label_bins(labels, breaks)
  
  if (return.breaks) return(breaks)
  
  binned.variable = cut(as.numeric(data[[variable]]), breaks, labels= labels, include.lowest=T, include.highest=T)
  return(binned.variable)
}

choose_bins = function(labels, breaks) {
  #### if they provide labels or breaks, choose the number of bins
  if (!is.null(labels)) return(length(labels))
  if (!is.null(breaks)) return(length(breaks)+1)
  return(3)
}

prep.breaks = function(variable, data, breaks=NULL, bins=3){
  
  breaks = unlist(breaks)	
  if (is.null(bins)) bins=3
  
  # return quantiles if they don't give breaks
  if (is.null(breaks)){
    quants = quantile(data[[variable]], seq(from=0, to=1, length.out=bins+1), na.rm=T)
    breaks = quants[!duplicated(quants)]
    return(breaks)
  }
  
  #### give min as breaks, if the user doesn't
  if (min(breaks)>min(data[[variable]], na.rm=T)){
    breaks = c(min(data[[variable]], na.rm=T), breaks)
  }
  if (max(breaks,na.rm=T)<max(data[[variable]], na.rm=T)){
    breaks = c(breaks, max(data[[variable]], na.rm=T))
  }	
  
  return(breaks)
  
}

# this function bins a specific variable
bin_variables_loop = function(i=1, data, break.me, bins, labels, breaks) {

  # indexing fails if i > the number of slots in the list
  if (length(labels)>= i) labs = labels[[i]] else labs = NULL
  
  b = bin.me(break.me[i], data, bins[i], labs, breaks[[i]])
  
  #### if there's only one category after we've binned things, fix that succa!
  if (length(levels(b))==1 & length(unique(data[[break.me[i]]]))>1) b = factor(data[,break.me[i]])
  return(b)
}

## this function loops through all variables that need binning, then renames the columns
bin_variables = function(data, bins, labels, break.me, breaks) {
  
  if (length(break.me)==0) return(data)
  
  new_cols = lapply(1:length(break.me), bin_variables_loop, data, break.me, bins, labels, breaks)
  data[,paste0(break.me, "_binned")] = new_cols
  return(data)
}

# expect_equal(label_bins_loop(1, c(-3.684, -1, 0, 1)), "(-3.7)-(-1)")
# expect_equal(label_bins_loop(2, c(-3.684, -1, 0, 1)), "(-1)-0")
# this receives the breakpoints, labels them all pretty, and returns the labels for a particular i
label_bins_loop = function(i, breaks) {
  digs1 = round_digits(breaks[i])
  digs2 = round_digits(breaks[i+1])
  
  # put parenthases around the negative numbers
  first  = label_negatives(breaks[i], digs1)
  second = label_negatives(breaks[i+1], digs2) 
  
  return(paste0(first, "-", second))
}


# breaks = seq(from = -10.3, to = 3, length.out = 3)
# expect_equal(label_bins(NULL, breaks)[2], "(-3.7)-3" )
# expect_equal(label_bins(c("a", "b"), breaks)[2], "b")
label_bins = function(labels, breaks) {
  
  # if they give labels, return them
  if (!is.null(labels)) return(labels)
  
  # otherwise, loop through all the breaks and create labels
  labels = 1:(length(breaks)-1)		
  return(labels %>% purrr::map_chr(label_bins_loop, breaks))
  
}

# expect_equal(label_negatives(-3.4, 1), "(-3.4)")
# expect_equal(label_negatives(3.4, 1), "3.4")
label_negatives = function(breaks, digits) {
  if (breaks<0) return(paste0("(", round(breaks, digits=digits), ")"))
  return(paste0(round(breaks, digits=digits)))
}


round_digits = function(breaks) {
  if (abs(breaks)<.0001) return(6)
  if (abs(breaks)<.001) return(5)
  if (abs(breaks)<.01) return(4)
  if (abs(breaks)<.1) return(3)
  if (abs(breaks)<1) return(2)
  return(1)
}