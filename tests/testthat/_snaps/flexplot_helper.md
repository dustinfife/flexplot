# flexplot_generate_prediction_lines works

    Code
      cat(flexplot_generate_prediction_lines(preds, "a", small))
    Output
      geom_point(data=prediction, aes(y=prediction, color=model),   position=position_dodge(width=.2)) + 
                   geom_line(data=prediction, aes(y=prediction, linetype=model, group=model, color=model), position=position_dodge(width=.2))

---

    Code
      cat(flexplot_generate_prediction_lines(preds, c("x"), small))
    Output
      geom_line(data= prediction, aes(linetype=model, y=prediction, colour=model), linewidth=1) + scale_linetype_manual(values=c("solid", "dotdash"))

---

    Code
      cat(flexplot_generate_prediction_lines(preds, c("x", "a"), small))
    Output
      geom_line(data= prediction, aes_string(linetype=axis[2], y="prediction", colour=axis[2]), linewidth=1)

# flexplot_panel_variables works

    Code
      cat(flexplot_panel_variables("a", c("b", "c")))
    Output
      facet_grid(as.formula(~a),labeller = custom.labeler)

---

    Code
      cat(flexplot_panel_variables("a", c("")))
    Output
      facet_grid(as.formula(~a),labeller = custom.labeler)

---

    Code
      cat(flexplot_panel_variables("", c("")))
    Output
      facet_grid(as.formula(NA~.),labeller = custom.labeler)

# flexplot_histogram works

    Code
      cat(flexplot_histogram(small, "a"))
    Output
      ggplot(data=data, aes(!!sym(outcome))) + geom_bar() + theme_bw() + labs(x= outcome)

---

    Code
      cat(flexplot_histogram(small, "x", "qq"))
    Output
      ggplot(data=data, aes(sample = !!sym(outcome))) + stat_qq() + stat_qq_line() + theme_bw() + labs(x=outcome)

---

    Code
      cat(flexplot_histogram(small, "x", "density"))
    Output
      ggplot(data=data, aes(!!sym(outcome))) + geom_density() + theme_bw() + labs(x=outcome)

---

    Code
      cat(flexplot_histogram(small, "x"))
    Output
      ggplot(data=data, aes(!!sym(outcome)))  + geom_histogram(fill="lightgray", col="black", bins=14) + theme_bw() + labs(x=outcome)

# flexplot_related works

    Code
      cat(test_plot$p)
    Output
      ggplot(data, aes(y=Difference, x=1)) + theme_bw()+ geom_hline(yintercept=0, col='lightgray') + labs(x='Difference (-)') + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

---

    Code
      cat(test_plot$points)
    Output
      geom_jitterd(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha), width=0.05, height=0)

---

    Code
      cat(flexplot_related(k, plot.type = "boxplot")$fitted)
    Output
      geom_boxplot(alpha=.1)

---

    Code
      cat(flexplot_related(k, plot.type = "violin")$fitted)
    Output
      geom_violin(alpha=.1)

---

    Code
      cat(flexplot_related(k)$fitted)
    Output
      stat_summary(fun='median', geom='point', size=3, position=position_dodge(width=.4), color = '#bf0303')+stat_summary(geom='errorbar', fun.min = function(z){quantile(z, .25)},linewidth = 1.25,  fun.max = function(z) {quantile(z, .75)}, fun=median, width=.2, position=position_dodge(width=.4), color = '#bf0303')+xxxx + coord_cartesian(xlim=c(.75, 1.25))

# flexplot_bivariate_string works

    Code
      cat(testplot$p)
    Output
      ggplot(data=data, aes(x=!!sym(axis), y=!!sym(outcome)))

---

    Code
      cat(testplot$points)
    Output
      geom_jitterd(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha), width=0.1, height=0)

---

    Code
      cat(flexplot_bivariate_string(small, "a", "b")$p)
    Output
      ggplot(data=data, aes(x=!!sym(axis), y=!!sym('Frequency'), fill=!!sym(outcome))) + geom_bar(stat='identity', position='dodge') + theme_bw()

---

    Code
      cat(flexplot_bivariate_string(small, "y", "b", plot.type = "boxplot")$fitted)
    Output
      geom_boxplot(alpha=.1)

---

    Code
      cat(flexplot_bivariate_string(small, "y", "b", plot.type = "violin")$fitted)
    Output
      geom_violin(alpha=.1)

---

    Code
      cat(flexplot_bivariate_string(small, "y", "b", plot.type = "line")$fitted)
    Output
      geom_line()

---

    Code
      cat(flexplot_bivariate_string(small, "y", "b")$fitted)
    Output
      stat_summary(fun='median', geom='point', size=3, position=position_dodge(width=.4), color = '#bf0303')+stat_summary(geom='errorbar', fun.min = function(z){quantile(z, .25)},linewidth = 1.25,  fun.max = function(z) {quantile(z, .75)}, fun=median, width=.2, position=position_dodge(width=.4), color = '#bf0303')+xxxx

# flexplot_multivariate_aes works

    Code
      cat(flexplot_multivariate_aes(small, "y", small, "x"))
    Output
      ggplot(data=data, aes(x=!! sym(predictors[1]), y=!! sym(outcome), color=!! sym(axis[2]), shape=!! sym(axis[2]))) + labs(color= axis[2], shape= axis[2])

---

    Code
      cat(flexplot_multivariate_aes(small, "y", axis = c("a", "x")))
    Output
      ggplot(data=data, aes(x=a, y, color=x_binned, linetype = x_binned, shape=x_binned)) + labs(color= "x_binned", linetype= "x_binned", shape= "x_binned")

---

    Code
      cat(flexplot_multivariate_aes(small, "y", axis = c("x", "a")))
    Output
      ggplot(data=data, aes(x=!!sym(predictors[1]), y=!!sym(outcome), color=!!sym(axis[2]), linetype = !!sym(axis[2]), shape=!!sym(axis[2]))) + labs(color= axis[2], linetype= axis[2], shape= axis[2])

---

    Code
      cat(flexplot_multivariate_aes(small, "y", axis = c("x", "z")))
    Output
      ggplot(data=data, aes(x=x, y, color=z_binned, linetype = z_binned, shape=z_binned)) + labs(color= "z_binned", linetype= "z_binned", shape= "z_binned")

