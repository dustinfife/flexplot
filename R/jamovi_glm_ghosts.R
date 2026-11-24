# Compatibility aliases retain the existing internal GLM entry points.
jamovi_glm_ghost_reason = function(...) jamovi_model_ghost_reason(...)
jamovi_glm_style_ghosts = function(...) jamovi_model_style_ghosts(...)

jamovi_glm_generalized_plot = function(state, x=NULL, panels=NULL, group=NULL, style="auto",
                                       se=TRUE, scale="probability", ghostLines=FALSE, ghost.reference=NULL) {
  arguments = list(state=state,x=x,panels=panels,group=group,style=style,se=se,scale=scale)
  plot = do.call(jamovi_glm_generalized_render,arguments)
  if (!isTRUE(ghostLines)) return(plot)
  formula = jamovi_model_layout(list(state$model),state$data,x,panels,group)
  jamovi_model_reference_ghosts(plot,formula,ghostLines,ghost.reference,render_ghost=function()
    do.call(jamovi_glm_generalized_render,c(arguments,list(ghost.line="gray",ghost.reference=ghost.reference))))
}
