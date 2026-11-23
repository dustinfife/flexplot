'use strict';

// Complete host restoration precedes this migration, as for model-term revisions.
// Revision zero denotes the old shared plot preference, never an explicit new one.
module.exports = function(ui, context, module) {
    if (context.flags && (context.flags.updating || context.flags.loaded === false)) return;
    const options = ui.view.model.options;
    const version = options.getOption('outputOptionsVersion');
    const target = module === 'glmgeneralized' ? 2 : 1;
    if (!version || version.getValue() >= target) return;
    const revision = version.getValue();
    options.beginEdit();
    try {
        // Set the marker before synchronous changed events can re-enter.
        options.setOptionValue('outputOptionsVersion',target);
        if (revision < 1 && module !== 'glmbasic' &&
            options.getOption('comparisonGraphic').getValue() !== (ui.graphic.value() === true))
            options.setOptionValue('comparisonGraphic',ui.graphic.value() === true);
        if (module === 'glmgeneralized' && revision < 2)
            for (const [destination,source] of [['comparisonSe','se'],
                ['comparisonPlotStyle','plotStyle'],['comparisonPlotScale','plotScale']]) {
                const value = options.getOption(source).getValue();
                if (options.getOption(destination).getValue() !== value)
                    options.setOptionValue(destination,value);
            }
    } finally { options.endEdit(); }
};
