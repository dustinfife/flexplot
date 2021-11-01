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

choose_bins = function(labels, breaks) {
  #### if they provide labels or breaks, choose the number of bins
  if (!is.null(labels)) return(length(labels))
  if (!is.null(breaks)) return(length(breaks)+1)
  return(3)
}

bin.me = function(variable, data, bins=NULL, labels=NULL, breaks=NULL, check.breaks=TRUE, return.breaks=FALSE){
  
  
  ### if they come as a list, unlist them
  if (is.list(breaks)) breaks = unlist(breaks)
  if (is.list(labels)) labels = unlist(labels)

  bins = choose_bins(labels, breaks)

  #### if they supply breaks, make sure there's a good min/max value	
  if (!is.null(breaks) & check.breaks) breaks = prep.breaks(variable, data, breaks)
  
  ### if we don't have breaks at this point, make some
  if (is.null(breaks)) breaks = quantile(as.numeric(data[[variable]]), seq(from=0, to=1, length.out=bins+1), na.rm=T)
  
  ### if they don't provide labels, make them easier to read (than R's native bin labels)\
  
  if (is.null(labels)){
    labels = 1:(length(breaks)-1)		
    for (i in 1:(length(breaks)-1)){
      digs1 = round_digits(breaks[i])
      digs2 = round_digits(breaks[i+1])
      # put parenthases around the negative numbers
      if (breaks[i]<0) {
        first = paste0("(", round(breaks[i], digits=digs1), ")") 
      } else {
        first = round(breaks[i], digits=digs1)
      }
      if (breaks[i+1]<0) {
        second = paste0("(", round(breaks[i+1], digits=digs2), ")") 
      } else {
        second = round(breaks[i+1], digits=digs2)
      }
      labels[i] = paste0(first, "-", second)
    }
  }
  
  
  
  if (return.breaks){
    return(breaks)
  } else {
    binned.variable = cut(as.numeric(data[[variable]]), breaks, labels= labels, include.lowest=T, include.highest=T)
    binned.variable
  }
  
}