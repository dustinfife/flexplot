# Modeling reference-panel ghost lines

LM, GLM, and standalone Mixed Models population/cluster visualizations offer
**Show ghost lines** immediately after the visualization variable selectors.
It defaults to unchecked, stays visible when unavailable, and retains the saved
preference through unsupported layouts. Label-editor visibility has no effect.
Older LM/Mixed documents without the option retain their previous appearance;
GLM's existing `ghostLines` option and values are unchanged.

Eligible plots have a continuous numeric x, one or two panel variables, multiple
prepared panels, and finite supplied fitted-curve coordinates. Color/symbol
grouping remains unsupported for these ghosts. Native JavaScript checks column
metadata; the R prepared-data gate is authoritative, including absent reference
combinations and nonfinite curves. An unsupported saved request leaves the
ordinary plot intact and reports a diagnostic using existing result objects.

Automatic x is supported. LM/GLM resolve it to the first numeric fitted predictor
outside selected panels/grouping, matching `jamovi_model_layout`. Mixed retains
its existing first-fixed-predictor convention and automatic cluster panels.
A categorical effective x remains unavailable; no destination-panel prediction
or unsupported grouped geometry is introduced.

`R/jamovi_model_ghosts.R` shares validation, reference-curve copying, and styling.
It reuses `create_ghost_reference`, `flexplot_ghost_reference_data`,
`flexplot_ghost_prediction_data`, and `flexplot_ghost_layer`; core reference
selection and exploratory styling are unchanged. LM and Mixed copy the existing
supplied curve, preserving interactions, quadratics, intervals, cached plots and
sampled clusters. Source panels keep their ordinary curve without a duplicate
reference ghost. Mixed copies one selected cluster's conditional curve when the
cluster is a panel; it does not evaluate that curve at destination random effects.

GLM retains its existing renderer/transform path through the shared gate. Its
probability/logit transforms, marginal predictions, confidence limits and
reference selection remain unchanged. Compatibility entry points in
`R/jamovi_glm_ghosts.R` and `jamovi/js/glm-ghosts.js` forward to shared helpers.
`jamovi/js/model-ghosts.js` handles metadata eligibility and rejects stale replies.

Ordinary modeling reference ghosts use `#707070`, width 1.2, opacity 0.85 and a
dashed line, drawn above confidence bands. This styling is local to adapter-owned
reference ghosts. Comparison overlay conventions and directional CAS ghosts are
unchanged.

Ordinary CAS reference ghosts are deferred to Stage 9. CAS removes estimated
random effects from observations and draws a separate fixed-effects curve; it
does not expose the supplied prediction stream used here. Adding an equivalent
reference stream needs deliberate CAS geometry work. The checkbox is disabled
in CAS and Mixed comparison modes while retaining its preference. The separate
**Add ghost line** option for directional Model 2 CAS ghosts is unaffected.
Reference explanations and plot-note spacing remain Stage 8 work.
