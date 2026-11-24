'use strict';

// Native text cells commit on change (blur/Enter), not input. Editor toggles never
// alter label rows or results; the host may refresh descriptions after a commit.
module.exports = function(ui, context, module) {
    if (context.flags && (context.flags.updating || context.flags.loaded === false)) return;
    const options = ui.view.model.options;
    const version = options.getOption('plotLabelEditorVersion');
    if (version && version.getValue() < 1) {
        const customized = names => names.some(name => {
            const option = options.getOption(name);
            return option && (option.getValue() || []).some(row =>
                row && typeof row.label === 'string' && row.label.trim().length);
        });
        const main = customized(['plotLabels','plotLabelStore']);
        const comparison = module === 'mixedmodel' ? main : customized(['comparisonPlotLabels','comparisonPlotLabelStore']);
        options.beginEdit();
        try {
            options.setOptionValue('plotLabelEditorVersion',1);
            if (options.getOption('modifyPlotLabels').getValue() !== main) options.setOptionValue('modifyPlotLabels',main);
            if (options.getOption('modifyComparisonPlotLabels').getValue() !== comparison) options.setOptionValue('modifyComparisonPlotLabels',comparison);
        } finally { options.endEdit(); }
    }
};
