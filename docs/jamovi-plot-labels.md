# Shared modeling plot-label editors

Linear Model and Generalized Linear Model expose independent **Plot Labels**
editors for ordinary model plots and model-comparison plots. Each native table
lists an actual plot element and a custom-label text cell. Blank means automatic.
Titles, subtitles, axis titles, legend titles/categories, and facet titles/levels
are supported where present. This does not change jamovi result-image headings
or edit categorical axis tick labels.

Mixed Models retains its existing editor, saved option names and keys, including
its population, cluster-specific, and composite CAS behavior. Its existing
editable element set is unchanged. CAS composition stays in its adapter.

`R/jamovi_plot_labels.R` contains shared row/key generation, catalog discovery,
retention, host-protobuf publication, and plot labeling. `R/mixedmodel-labels.R`
provides backward-compatible wrappers. The LM/GLM adapters construct the same
plots as before, publish applicable editor rows in the host response, and apply
edits before printing. This adds plot preparation for discovery, not shared fit
caching; LM/GLM retain their existing fitting lifecycle.

LM/GLM keys distinguish output, outcome, displayed y meaning, role, variable,
guide and original level/bin. Family/link and effective probability/logit display
context prevent stale axis wording. Prepared facet bins come from the rendered
plot layout. Legend edits target mapped guides rather than every aesthetic with
matching category text. Hidden stores retain temporarily irrelevant edits, even
when a plot is disabled or a model specification is invalid. Clearing an edit
restores the automatic label without recoding data or changing scale transforms.

Native controls use the same Array/Group options and ListBox/TextBox pattern as
Mixed Models. The shared publisher updates both R options and the host protobuf;
new fields can be added to requests saved before these options existed.

Regression checks cover rendered grob text, exact plot data/facet membership,
independent outputs, scale changes, fit reuse for Mixed Models, native setters,
and real protobuf restoration. Installed offscreen rendering is distinct from
live jamovi GUI verification. Planned GUI cases are LM-16 and GLM-23; an archive
or saved option alone cannot prove that custom text was visibly rendered.
