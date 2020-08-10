# 
# ### this function returns the output table for model comparisons
# return_tabdata = function(linmod_results) {
#   #### set first instance of all
#   tabdat$terms = return_baseline_model(formula(linmod_results$model))
#   tabdat$rsq = return_baseline_rsq(linmod_results$model)
#   tabdat$bayes = NA
#   tabdat$bayesinv = NA
#   tabdat$teststat = NA
#   tabdat$statval = NA
#   tabdat$df = return_baseline_df(linmod_results$model)
#   tabdat$p = NA
#   
#   #### create variables of models
#   formula = formula(linmod_results$model)
#   all_terms = attr(terms(formula), "term.labels")
#   reg_mod_coef = summary(linmod_results$model)$coefficients
#   estimates = flexplot::estimates(linmod_results$model)
#   anova_mod_coef = anova(linmod_results$model)
#   mc = estimates$model.comparison
#   
#   #### return tabdat if it's an intercept only model
#   if (length(all_terms) == 0) return(tabdat)
#   
#   #### return if there's only one variable
#   if (length(all_terms) == 1) {
#     
#     #### create new null model
#     null_mod = update(linmod_results$model, . ~ 1) 
#     bf = flexplot::bf.bic(linmod_results$model, null_mod)
#     
#     ### fill in terms unique to this
#     tabdat$terms[2] = all_terms
#     tabdat$rsq[2] = estimates$r.squared[1]
#     tabdat$bayes[2] = bf
#     tabdat$bayesinv[2] = 1/bf
#     
#     ## enter test statistic (all this can be in the loop)
#     teststatistics = complete_teststat_when_one_var(linmod_results$model, all_terms[i])
#     tabdat$teststat[2] = teststatistics[[1]]
#     tabdat$statval[2] = teststatistics[[2]]
#     
#     df_p = return_term_df(teststatistics[[1]], linmod_results$model, all_terms[i])
#     tabdat$df[2] = df_p[[1]]
#     tabdat$p[2] = df_p[[2]]
#     return(tabdat)
#   }
#   
#   for (i in 1:(length(all_terms))){
#     bf = mc[mc$all.terms == all_terms[i], "bayes.factor"]
#     tabdat$terms[i+1] = all_terms[i]
#     tabdat$rsq[i+1] = estimates$semi.p[all_terms[i]]
#     tabdat$bayes[i+1] = bf
#     tabdat$bayesinv[i+1] = 1/bf
#     
#     ## enter test statistic (all this can be in the loop)
#     teststatistics = complete_teststat_when_one_var(linmod_results$model, all_terms[i], first.term = FALSE)
#     tabdat$teststat[i+1] = teststatistics[[1]]
#     tabdat$statval[i+1] = teststatistics[[2]]
#     
#     df_p = return_term_df(teststatistics[[1]], linmod_results$model, all_terms[i])
#     tabdat$df[i+1] = df_p[[1]]
#     tabdat$p[i+1] = df_p[[2]]
#   }
#   
#   return(tabdat)
# }
# 
# # model = lm(weight.loss~1, data=exercise_data)
# # linmod_results = list(
# #   model = model,
# #   numbers = "x",
# #   factors = "b"
# # )
# # return_tabdata(linmod_results = linmod_results)
# # 
# # model = lm(weight.loss~motivation, data=exercise_data)
# # linmod_results = list(
# #   model = model,
# #   numbers = "motivation",
# #   factors = NA
# # )
# # return_tabdata(linmod_results = linmod_results)
# # 
# # model = lm(weight.loss~therapy.type, data=exercise_data)
# # linmod_results = list(
# #   model = model,
# #   numbers = NA,
# #   factors = "therapy.type"
# # )
# # return_tabdata(linmod_results = linmod_results)
# # 
# # model = lm(weight.loss~rewards, data=exercise_data)
# # linmod_results = list(
# #   model = model,
# #   numbers = NA, 
# #   factors = "rewards"
# # )
# # return_tabdata(linmod_results = linmod_results)
# # 
# # model = lm(weight.loss~rewards + motivation, data=exercise_data)
# # linmod_results = list(
# #   model = model,
# #   numbers = "motivation",
# #   factors = "rewards"
# #     
# # )
# # return_tabdata(linmod_results = linmod_results)
