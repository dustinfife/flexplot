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
      geom_line(data= prediction, aes(linetype=model, y=prediction, colour=model), size=1) + scale_linetype_manual(values=c("solid", "dotdash"))

---

    Code
      cat(flexplot_generate_prediction_lines(preds, c("x", "a"), small))
    Output
      geom_line(data= prediction, aes_string(linetype=axis[2], y="prediction", colour=axis[2]), size=1)

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

