	### taken from gtools::permutations
permute.vector = function (n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE) 
{
    if (mode(n) != "numeric" || length(n) != 1 || n < 1 || (n%%1) != 
        0) 
        stop("bad value of n")
    if (mode(r) != "numeric" || length(r) != 1 || r < 1 || (r%%1) != 
        0) 
        stop("bad value of r")
    if (!is.atomic(v) || length(v) < n) 
        stop("v is either non-atomic or too short")
    if ((r > n) & repeats.allowed == FALSE) 
        stop("r > n and repeats.allowed=FALSE")
    if (set) {
        v <- unique(sort(v))
        if (length(v) < n) 
            stop("too few different elements")
    }
    v0 <- vector(mode(v), 0)
    if (repeats.allowed) 
        sub <- function(n, r, v) {
            if (r == 1) 
                matrix(v, n, 1)
            else if (n == 1) 
                matrix(v, 1, r)
            else {
                inner <- Recall(n, r - 1, v)
                cbind(rep(v, rep(nrow(inner), n)), matrix(t(inner), 
                  ncol = ncol(inner), nrow = nrow(inner) * n, 
                  byrow = TRUE))
            }
        }
    else sub <- function(n, r, v) {
        if (r == 1) 
            matrix(v, n, 1)
        else if (n == 1) 
            matrix(v, 1, r)
        else {
            X <- NULL
            for (i in 1:n) X <- rbind(X, cbind(v[i], Recall(n - 
                1, r - 1, v[-i])))
            X
        }
    }
    sub(n, r, v[1:n])
}




rotate.view = function(formula, fixed.positions, which.perms = NULL, return.perms=F){

	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	given = unlist(subsetString(as.character(formula)[3], sep=" | ", position=2, flexible=F))
	given = gsub(" ", "", given)		
	given = unlist(strsplit(given, "+", fixed=T))	
	axis = unlist(subsetString(as.character(formula)[3], sep=" | ", position=1, flexible=F))
	axis = gsub(" ", "", axis)			
	axis = unlist(strsplit(axis, "+", fixed=T))
	
	if (is.null(fixed.positions)){
		fixed.positions = rep(T, times=4)
	}	
	
	if (length(predictors)>1){
		
		### if their list is longer than the number of variables, delete the last ones
		if (length(fixed.positions)>4){
			message("You provided more slots in fixed.positions than possible. I'm deleting the latter ones.\n")
			fixed.positions = fixed.positions[1:4]
		
		### if their list is shorter than the number of variables, assume the rest are free to vary		
		} else if (length(fixed.positions)<4){
			message("fixed.positions expects four slots (two axes, two panels). You provided less, but I'll assume you want to vary the locations of the other slots as well.\n")
			a = rep(T, times=4)
			a[1:length(fixed.positions)] = fixed.positions
			fixed.positions = a
		}
		
		### make a vector of all slots
		all.vars = rep("", times=4)
		all.vars[1:length(axis)] = axis
		if (!is.na(given[1])){
			all.vars[3:(2+length(given))] = given			
		}
		all.nums = 1:4; if (sum(fixed.positions)!=4) all.nums = all.nums[-which(!fixed.positions)]
		
		#### specify which slots are allowed to vary and which are fixed
		fixed = all.vars[!fixed.positions]
		free = all.vars[which(!(all.vars %in% fixed))]
	
		#### figure out numbers of slots allowed to vary
		slots.to.randomize = which(!(all.vars %in% fixed))
	
		#### find all possible permutations
		if (length(variables)==2){
			perms = matrix(c(
				1,2,3,4,
				2,1,3,4,
				1,3,2,4,
				2,3,1,4), ncol=4)
		} else {
			perms = permute.vector(length(slots.to.randomize), length(slots.to.randomize), slots.to.randomize)
		}
		
		#### remove perms rows that are the same ordering
		# same.order = which(apply(perms, 1, paste0, collapse=",")==paste0(all.nums, collapse=","))
		# if (length(same.order)>0){
			# perms = perms[-same.order,]
		# }
		
		### make sure axis[1] is not random
		if (fixed.positions[1]){
			### find those where ""
			mis = which(free=="")
			
			#### remove those situations where mis (missing value) is given a 1 in perm
			good.bye=which(perms[,mis] == 1)
			if (length(good.bye)>0) perms = perms[-good.bye,]
		}

		### get rid of those ones that leave the first axis blank
		blanks = which(all.vars == "")
		if (length(blanks)>0){
			perms = perms[-which(perms[,1] %in% blanks),]
		}				

		#### randomly decide permutation
		if (is.null(which.perms)){
			rand.row = sample(1:nrow(perms), size=1)			
		} else {
			rand.row = which.perms
		}

		#### create new formula
		new.allvars = all.vars
		new.allvars[perms[rand.row,]] = free
		
		new.allvars[new.allvars==""] = "xxx"
		stay = which(new.allvars!="xxx")
		symbols = c("~", "+", "|", "+")
		f = paste0(symbols, new.allvars, collapse="")
		f = gsub("xxx+", "", f, fixed=T)
		f = gsub("+xxx", "", f, fixed=T)		
		f = gsub("|xxx", "", f, fixed=T)				
		f = gsub("xxx|", "", f, fixed=T)						
		formula = formula(paste0(outcome, f, collapse=""))
		
	} 
	
	if(return.perms){
		perms
	} else {
		formula
	}
}

prep.breaks = function(variable, data, breaks=NULL, bins=3){

		breaks = unlist(breaks)	
		if (is.null(bins)){bins=3}

		if (is.null(breaks)){
			quants = quantile(data[[variable]], seq(from=0, to=1, length.out=bins+1), na.rm=T)
			breaks = quants[!duplicated(quants)]
		} else {			
			#### give min as breaks, if the user doesn't
			if (min(breaks)>min(data[[variable]])){
				breaks = c(min(data[[variable]]), breaks)
			}
			if (max(breaks,na.rm=T)<max(data[[variable]])){
				breaks = c(breaks, max(data[[variable]]))
			}	
		}
		
		return(breaks)
		
}


bin.me = function(variable, data, bins=NULL, labels=NULL, breaks=NULL, check.breaks=TRUE, return.breaks=FALSE){


	### if they come as a list, unlist them
	if (is.list(breaks)){
		breaks = unlist(breaks)
	}
	if (is.list(labels)){
		labels = unlist(labels)
	}
	
	
	### error if their labels are not the same length as the breaks + 1
	# if (!is.null(labels) & !is.null(breaks) & length(labels) != (length(breaks))){
		# stop(paste0("The label vectors (", paste0(unlist(labels), collapse=", "), ") needs to be equal to the number of breaks (", paste0(breaks, collapse="-"), ") + 1. Either change the breaks or the number of labels"))
	# }
	
	#### if they provide labels or breaks, choose the number of bins
	if (!is.null(labels)){
		bins = length(labels)
	} else if (!is.null(breaks)){
		bins = length(breaks)+1
	#### otherwise, set bins to 3
	} else {
		bins = 3
	}

	
	

	### check length of binned variables. If <= breaks, treat as categorical
	# if (length(unique(data[[variable]]))<=bins){
		# data[[variable]] = factor(data[[variable]], ordered=T)
	# }
	


	#### if they supply breaks, make sure there's a good min/max value	
	if (!is.null(breaks) & check.breaks){
		breaks = prep.breaks(variable, data, breaks)
	} 
	
	### if they don't provide labels, make them easier to read (than R's native bin labels)\
	if (is.null(labels)){
		labels = 1:(length(breaks)-1)		
		for (i in 1:(length(breaks)-1)){
			labels[i] = paste0(round(breaks[i], digits=1), "-", round(breaks[i+1], digits=1))
		}
	}
	
	### if we don't have breaks at this point, make some
	if (is.null(breaks)){
		breaks = quantile(as.numeric(data[[variable]]), seq(from=0, to=1, length.out=bins+1))
	}

	if (return.breaks){
		breaks
	} else {
		binned.variable = cut(as.numeric(data[[variable]]), breaks, labels= labels, include.lowest=T, include.highest=T)
		binned.variable
	}
	
}


	### create custom function to sample data
sample.subset = function(sample, data){
	if (sample!=Inf){
		m = data[sample(1:nrow(data), size=sample),]
		
	} else {
		m = data
	}
	return(m)
}

	### if they don't want raw data, just make alpha = 0
raw.alph.func = function(raw.data,alpha=1){
	if (raw.data){
		alpha.raw = alpha
	} else {
		alpha.raw = 0
	}	
}

	#### points = the datapoints
points.func = function(axis.var, data, jitter){
	
	### jitter if it's numeric BUT only a few levels

	
	### if they specified something for jitter
	if (!is.null(jitter)){
	

		#### if they don't specify both vectors for jitter, warn them
		if (is.numeric(jitter[1]) & length(jitter)<2){
			message("You're supposed to supply TWO values for jitter (one for x axis, one for y axis). You only supplied one. I'm going to assume the value you gave is for the x axis, then I'll set the y axis to 0.\n")
			jitter[2] = 0
		}

		#### if they supply only one value
		if (length(jitter) == 1){

			#### if they said jitter=T
			if (jitter[1]==T){
			
				#### I'm putting the command as a string to avoid environment problems
				jit = paste0("geom_jitterd(data=sample.subset(sample,", deparse(substitute(data)), "), alpha=raw.alph.func(raw.data, alpha=alpha), width=.2, height=0)")
				
			#### if they said jitter=F	
			} else if (jitter[1] == F){
				jit = paste0("geom_point(data=sample.subset(sample, ", deparse(substitute(data)), "), alpha=raw.alph.func(raw.data, alpha=alpha))")
			}
			
		} else {
			jit = paste0("geom_jitterd(data=sample.subset(sample, ", deparse(substitute(data)), "), alpha=raw.alph.func(raw.data, alpha=alpha), width=", jitter[1], ", height=", jitter[2], ")")				
		}
		

	#### if they left jitter at the default	
	} else {
	
		### if x axis is categorical, jitter it by .2
		if (!is.numeric(data[[axis.var[1]]])){
			jit = paste0("geom_jitterd(data=sample.subset(sample, ", deparse(substitute(data)), "), alpha=raw.alph.func(raw.data, alpha=alpha), width=.2)")				
			
		### if x axis is numeric, don't jitter it	
		} else {
			jit = paste0("geom_point(data=sample.subset(sample, ", deparse(substitute(data)), "), alpha=raw.alph.func(raw.data, alpha=alpha))")
		}
	}

	
	#### return the jittered string
	return(jit)		
}

	#### this function converts a binary variable to a 1/0 for logistic regression
factor.to.logistic = function(data, outcome, labels=F){
	
	#### check if they have 2 unique values
	if (length(unique(data[,outcome]))!=2){
		stop("To fit a logistic curve, you must have only two levels of your outcome variable.")
	}	
	
	### now do the converstion
	if (labels){
		levels(data[,outcome])
	} else {
		
		data[,outcome] = as.numeric(as.character(factor(data[,outcome], levels=levels(data[,outcome]), labels=c(0,1))))
		#data %>% dplyr::mutate(!!outcome := as.numeric(as.character(factor(!!as.name(outcome), levels=levels(!!as.name(outcome)), labels=c(0,1))))) 
		return(data)
	}
	
}


##' @importFrom MASS rlm	
#### identify the correct "fit"
fit.function = function(outcome, predictors, data, suppress_smooth, method, spread, mean.line=F, categorical=FALSE){
	
	if (is.numeric(data[,predictors]) & !categorical){
		
		
		if (suppress_smooth){
			fit.string = "xxxx"
		} else if (method=="logistic") {
	
			#### make sure there's only two levels
			if (length(unique(data[,outcome]))!=2){
				stop("To fit a logistic curve, you must have only two levels of your outcome variable.")
			}

			fit.string = 'geom_smooth(method = "glm", method.args = list(family = "binomial"), se = se)'			
		} else if (method=="rlm"){
			fit.string = 'geom_smooth(method = "rlm", se = se)'
		}else if (method=="poisson" | method=="Gamma") {
			#### specify the curve
			fit.string = 'geom_smooth(method = "glm", method.args = list(family = method), se = se)'
		} else if (method=="polynomial"){
			fit.string = 'stat_smooth(method="lm", se=se, formula=y ~ poly(x, 2, raw=TRUE))'
		} else if (method=="cubic"){
			fit.string = 'stat_smooth(method="lm", se=se, formula=y ~ poly(x, 3, raw=TRUE))'
		} else if (method=="lm"){
			fit.string = 'stat_smooth(method="lm", se=se)'
		} else {
			fit.string = 'geom_smooth(method="loess", se=se)'
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
				sum.line = 'stat_summary(aes_string(group= axis[2]), geom="line", fun.y="mean", position=position_dodge(width=.5), color = "#3366FF")'
			} else {
				sum.line='xxxx'
			}
		} else if (spread=="sterr"){	
			summary1 = "stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')"
			summary2 = "stat_summary(geom='errorbar', fun.min = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, fun.ymax = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, width=.2, size = 1.25, position=position_dodge(width=.2), color = '#bf0303')"
			if (mean.line){
				sum.line = 'stat_summary(aes_string(group= axis[2]), geom="line", fun.y="mean", position=position_dodge(width=.2), color = "#bf0303")'
			} else {
				sum.line='xxxx'
			}

		} else if (spread == "quartiles"){	
			summary1 = "stat_summary(fun.y='median', geom='point', size=3, position=position_dodge(width=.4), color = '#bf0303')" 
			summary2 = "stat_summary(geom='errorbar', fun.min = function(z){quantile(z, .25)},size = 1.25,  fun.ymax = function(z) {quantile(z, .75)}, fun.y=median, width=.2, position=position_dodge(width=.4), color = '#bf0303')"
			if (mean.line){
				sum.line = 'stat_summary(aes_string(group=axis[2]), geom="line", fun.y="median", position=position_dodge(width=.4), color = "#bf0303")'
			} else {
				sum.line='xxxx'
			}

		}
		
		fit.string = paste0(summary1, "+",summary2, "+", sum.line)			
		
	}
	
	return(fit.string)
	
}
