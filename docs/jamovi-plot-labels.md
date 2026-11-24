# Shared modeling plot-label editors

Linear Model and Generalized Linear Model expose independent **Model Visualization**
and **Model Comparison Visualization** sections, with model specifications kept in
**Model Comparison**. Each section has a default-off **Modify Plot Labels** checkbox
revealing its native `height: large` editor. Each native table
lists an actual plot element and a custom-label text cell. Blank means automatic.
Titles, subtitles, axis titles, legend titles/categories, and facet titles/levels
are supported where present. This does not change jamovi result-image headings
or edit categorical axis tick labels.

Mixed Models retains its existing editor, saved option names and keys, including
its population, cluster-specific, and composite CAS behavior. Its combined
**Model Visualization / Model Comparison Visualization** section controls the
single active plot, with separate editor-visibility preferences for the two modes.
Its existing editable element set is unchanged. CAS composition stays in its adapter.
The combined heading avoids unsupported dynamic CollapseBox heading updates in
jamovi 2.7.41; simultaneous outputs and separate stores remain Stage 9 work.

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

Editor visibility is independent of result visibility. Unchecking a checkbox or
collapsing a section retains applied labels and does not hide results. Revision 1
of `plotLabelEditorVersion` reveals legacy nonblank edits from current rows or
retained stores once; Mixed Models reveals both mode preferences for its shared
store. Current saved false preferences survive restoration. Existing option names,
row keys, original break values, result identifiers and table schemas are retained.

Legend descriptions use the scale's displayed labels, such as `Legend entry: Model 1`.
Committed custom titles/entries update the descriptions on recalculation. Multiple
guides include their title to distinguish overlapping levels. Native TextBox cells
commit on change (blur or Enter); no per-keystroke row rebuilding is added. Legacy
Mixed metadata discovery restores the RNG state so it cannot perturb jitter or
subsequent cluster sampling.

Stage 5 desktop checks in jamovi 2.7.41 verified separate LM/GLM controls, larger
editors, default-off checkboxes, LM typing focus and committed legend descriptions,
retained labels when hidden, and Mixed shared labels across comparison toggling.
Mixed collapse/reopen retained its checkbox state. Focused renderer/R6 checks cover
independent routing, blank defaults, probability/logit transitions, multiple guides,
real protobuf restoration and unchanged numerical/graphical results. These checks
do not complete the broader GUI campaign or JASP compatibility testing.
