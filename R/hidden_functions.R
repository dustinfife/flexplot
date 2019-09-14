permutations <- function(n){
    if(n==1){
        return(matrix(1))
    } else {
        sp <- permutations(n-1)
        p <- nrow(sp)
        A <- matrix(nrow=n*p,ncol=n)
        for(i in 1:n){
            A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
        }
        return(A)
    }
}

rotate.view = function(formula, third.eye){

	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	given = unlist(subsetString(as.character(formula)[3], sep=" | ", position=2, flexible=F))
	given = gsub(" ", "", given)		
	given = unlist(strsplit(given, "+", fixed=T))	
	axis = unlist(subsetString(as.character(formula)[3], sep=" | ", position=1, flexible=F))
	axis = gsub(" ", "", axis)			
	axis = unlist(strsplit(axis, "+", fixed=T))	
	
	if (!is.null(third.eye) & length(predictors)>1){
		
		### if their list is longer than the number of variables, delete the last ones
		if (length(third.eye)>4){
			cat("You provided more slots in third.eye than possible. I'm deleting the latter ones.\n")
			third.eye = third.eye[1:4]
		
		### if their list is shorter than the number of variables, assume the rest are free to vary		
		} else if (length(third.eye)<4){
			cat("Third.eye expects four slots (two axes, two panels). You provided less, but I'll assume you want to vary the locations of the other slots as well.\n")
			a = rep(T, times=4)
			a[1:length(third.eye)] = third.eye
			third.eye = a
		}
		
		### make a vector of all slots
		all.vars = rep("", times=4)
		all.vars[1:length(axis)] = axis
		if (!is.na(given[1])){
			all.vars[3:(2+length(given))] = given			
		}
		all.nums = 1:4; if (sum(third.eye)!=4) all.nums = all.nums[-which(!third.eye)]
		
		#### specify which slots are allowed to vary and which are fixed
		fixed = all.vars[!third.eye]
		free = all.vars[which(!(all.vars %in% fixed))]
	
		#### figure out numbers of slots allowed to vary
		slots.to.randomize = which(!(all.vars %in% fixed))
	
		#### find all possible permutations
		perms = matrix(slots.to.randomize[permutations(length(slots.to.randomize))], ncol=length(slots.to.randomize))
		
		#### remove perms rows that are the same ordering
		same.order = which(paste0(all.nums, collapse=",") == apply(perms, 1, paste0, collapse=","))
		perms = perms[-same.order,]
		
		### make sure axis[1] is not random
		if (third.eye[1]){
			### find those where ""
			mis = which(free=="")
			
			#### remove those situations where mis (missing value) is given a 1 in perm
			good.bye=which(perms[,mis] == 1)
			if (length(good.bye)>0) perms = perms[-good.bye,]
		}
		
		#### randomly decide permutation
		rand.row = sample(1:nrow(perms), size=1)
	
		#### create new formula
		new.allvars = all.vars
		new.allvars[perms[rand.row,]] = free
		
		new.allvars[new.allvars==""] = "xxx"
		stay = which(new.allvars!="")
		symbols = c("~", "+", "|", "+")
		f = paste0(symbols, new.allvars, collapse="")
		f = gsub("xxx+", "", f, fixed=T)
		f = gsub("+xxx", "", f, fixed=T)		
		formula = formula(paste0(outcome, f, collapse=""))
		return(formula)
	} else {
		return(formula)
	}
}

prep.breaks = function(variable, data, breaks=NULL, bins=4){

		breaks = unlist(breaks)	

		if (is.null(breaks)){
			quants = quantile(data[[variable]], seq(from=0, to=1, length.out=bins+1), na.rm=T)
			breaks = quants[!duplicated(quants)]
		} else {			
			#### give min as breaks, if the user doesn't
			if (min(breaks)>min(data[[variable]])){
				breaks = c(-Inf, breaks)
			}
			if (max(breaks,na.rm=T)<max(data[[variable]])){
				breaks = c(breaks, Inf)
			}	
		}
		
		return(breaks)
		
}
bin.me = function(variable, data, bins=NULL, labels=NULL, breaks=NULL, check.breaks=TRUE){

	bins = ifelse(is.null(bins), 4, bins)
	
	### if they come as a list, unlist them
	if (is.list(breaks)){
		breaks = unlist(breaks)
	}
	if (is.list(labels)){
		labels = unlist(labels)
	}	

	### check length of binned variables. If <= breaks, treat as categorical
	# if (length(unique(data[[variable]]))<=bins){
		# data[[variable]] = factor(data[[variable]], ordered=T)
	# }
	
	### error if their labels are not the same length as the bin length
	if (!is.null(labels) & length(labels) != bins){
		stop(paste0("The label vectors (", paste0(unlist(labels), collapse=", "), ") is not the same length as the bin length (", bins, ")", sep=""))
	}

	#### if they supply breaks, make sure there's a good min/max value	
	if (!is.null(breaks) & check.breaks){
		breaks = prep.breaks(variable, data, breaks)
	} 

	binned.variable = cut(as.numeric(data[[variable]]), breaks, labels= labels, include.lowest=T, include.highest=T)
	binned.variable
	
}


	### create custom function to sample data
sample.subset = function(sample, data){
	if (sample!=Inf){
		m = data[sample(1:nrow(data), size=sample),]
	} else {
		m = data
	}
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
		if (is.numeric(jitter)[1] & length(jitter)<2){
			cat("You're supposed to supply TWO values for jitter (one for x axis, one for y axis). You only supplied one. I'm going to assume the value you gave is for the x axis, then I'll set the y axis to 0.\n")
			jitter[2] = 0
		}
	
	
		#### if they said jitter=T
		if (jitter[1]==T & !is.numeric(jitter)[1]){
			#### I'm putting the command as a string to avoid environment problems
			jit = paste0("geom_jitterd(data=sample.subset(sample,", deparse(substitute(data)), "), alpha=raw.alph.func(raw.data, alpha=alpha), width=.2, height=.2)")
			
		#### if they said jitter=F	
		} else if (jitter[1] == F & !is.numeric(jitter)[1]){
			jit = paste0("geom_point(data=sample.subset(sample, ", deparse(substitute(data)), "), alpha=raw.alph.func(raw.data, alpha=alpha))")
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
			
			#### convert outcome to numeric (if necessary)		#### MUST DEAL WITH THIS LATER!!!
			if (!is.numeric(data[,outcome])){
				fit.string = paste0(deparse(substitute(data)), "[,outcome] = as.numeric(", deparse(substitute(data)), "[,outcome])-1")
			}
			
			#### specify the curve
			fit.string = 'geom_smooth(method = "glm", method.args = list(family = "binomial"), se = se)'
		} else if (method=="rlm"){
			fit.string = 'geom_smooth(method = "rlm", se = se)'
		}else if (method=="poisson" | method=="Gamma") {
			#### specify the curve
			fit.string = 'geom_smooth(method = "glm", method.args = list(family = method), se = se)'
		} else {
			fit.string = 'geom_smooth(method=method, se=se)'
		}
		

		
	} else {
		
		if (suppress_smooth){
			summary1="xxxx"
			summary2="xxxx"
			sum.line="xxxx"						
		} else if (spread=="stdev"){
			summary1 = "stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.2))" 
			summary2 = "stat_summary(geom='errorbar', fun.ymin = function(z){mean(z)-sd(z)}, fun.ymax = function(z) {mean(z)+sd(z)}, fun.y=median, size = 1.25, width=.2, position=position_dodge(width=.2))"
			if (mean.line){
				sum.line = 'stat_summary(aes_string(group= axis[2]), geom="line", fun.y="mean", position=position_dodge(width=.2))'
			} else {
				sum.line='xxxx'
			}
		} else if (spread=="sterr"){	
			summary1 = "stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.2))"
			summary2 = "stat_summary(geom='errorbar', fun.ymin = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, fun.ymax = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, width=.2, size = 1.25, position=position_dodge(width=.2))"
			if (mean.line){
				sum.line = 'stat_summary(aes_string(group= axis[2]), geom="line", fun.y="mean", position=position_dodge(width=.2))'
			} else {
				sum.line='xxxx'
			}

		} else if (spread == "quartiles"){	
			summary1 = "stat_summary(fun.y='median', geom='point', size=3, position=position_dodge(width=.2))" 
			summary2 = "stat_summary(geom='errorbar', fun.ymin = function(z){quantile(z, .25)},size = 1.25,  fun.ymax = function(z) {quantile(z, .75)}, fun.y=median, width=.2, position=position_dodge(width=.2))"
			if (mean.line){
				sum.line = 'stat_summary(aes_string(group=axis[2]), geom="line", fun.y="median", position=position_dodge(width=.2))'
			} else {
				sum.line='xxxx'
			}

		}
		
		fit.string = paste0(summary1, "+",summary2, "+", sum.line)			
		
	}
	
	return(fit.string)
	
}
