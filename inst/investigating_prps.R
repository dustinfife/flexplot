model = lm(ideation~stress + 
             I(stress^2)+ 
             depression * friend_ideation + 
             health, 
           data=ideation)
formula = ideation~depression | friend_ideation
data = ideation
added_term = ~depression*friend_ideation 

# 1. compute all residuals
residuals_all = residuals(model)

# 2. add fitted terms back into residuals
residual = partial_residual(model, added_term) 
data$ideation = residual

# 3. make a flexplot and extract the data
plot_data = flexplot(formula, data=data, suppress_smooth=T) 

# 4.  compute the mean within the paneled variable(s), then merge with original data
    # identify variables with _binned in the name
    binned_vars = grep("_binned", names(plot_data$data), fixed=T, value=T)
    unbinned_name = gsub("_binned", "", binned_vars)
    # merge the means with the dataset
    plot_data$data = plot_data$data %>% 
      group_by_at(vars(binned_vars)) %>% 
      summarize(Mean = mean(!!sym(unbinned_name))) %>% 
      full_join(plot_data$data)
    
# 5. identify which components go into the model
    
    keep_columns = dimnames(keep_duplicates(model.matrix(model), model.matrix(added_term, data=data))[,-1])[[2]]
    plot_data$data %>% select_at(all_of(keep_columns))
    names(plot_data$data)
    plot_data$data$predictions = as.vector( plot_data$data[[keep_columns]] %*% coef(model)[dimnames(keep_columns)[[2]]] )
    plot_data +
      geom_line(data=plot_data$data, aes(y=predictions))
    
    
    
    
    # extract names of variables on each axis
    columns_of_interest = keep_duplicates(model.matrix(model), model.matrix(added_term, data=data)) 
    columns_of_interest = dimnames(columns_of_interest)[[2]][-1]
    # find x axis
    var_placement = flexplot_axis_given(formula)
    x_axis = var_placement$axis
    # find all instances where x axis is interacting with others
    grep(x_axis, columns_of_interest, value = T)

# 5. plot it
plot_data +
  geom_line(data=plot_data$data, aes(y=prediction))
geom_smooth(method="lm", formula = y~x*z)

+ geom_abline(aes(slope = coef(model)[var_placement$axis[1]] + coef(model)[7]*Mean, intercept = 0 + coef(model)[5]*Mean)) +
  geom_smooth(method="lm")

# maybe use geom_line, not abline, and compute the predictions of the model within each bin

?geom_abline

model = lm(weight.loss~motivation*health + therapy.type, data=exercise_data)
formula = weight.loss~motivation | health
data = exercise_data
added_term = ~motivation*health


  # 1. compute all residuals
residuals_all = residuals(model)

  # 2. add fitted terms back into residuals
residual = partial_residual(model, added_term) 
data$weight.loss = residual

  # 3. make a flexplot and extract the data
plot_data = flexplot(formula, data=data, suppress_smooth=T) 
  # compute the mean within the paneled variable (health in this case)
plot_data$data = plot_data$data %>% group_by(health_binned) %>% 
    summarize(Mean = mean(health)) %>% 
    full_join(plot_data$data)



  # find the paneled variable
plot_data + geom_abline(aes(slope = coef(model)[2] + coef(model)[6]*Mean, intercept = 0 + coef(model)[3]*Mean)) +
  geom_smooth(method="lm")

  geom_abline(aes())

model

residuals_mot = residuals_all + coef(model)[2]*exercise_data$motivation
d = data.frame(residuals_all, residuals_mot, motivation = exercise_data$motivation, health = exercise_data$health)

fitted_all$prediction = fitted_all$prediction - (coef(model)[1] + coef(model)[3]*mean(exercise_data$health))
a1 = flexplot(residuals_mot~motivation |  health, data=d, method="lm")
a2 = partial_residual_plot(weight.loss~motivation | health, 
                           data=exercise_data,
                           model=model, 
                           added_term = ~motivation*health)
require(patchwork)
a1+a2
