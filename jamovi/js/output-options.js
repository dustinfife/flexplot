'use strict';

// Complete host restoration precedes this migration, as for model-term revisions.
// Revision zero denotes the old shared plot preference, never an explicit new one.
module.exports = function(ui, context, module) {
    if (context.flags && (context.flags.updating || context.flags.loaded === false)) return;
    const options = ui.view.model.options;
    const version = options.getOption('outputOptionsVersion');
    if (!version || version.getValue() >= 1) return;
    options.beginEdit();
    try {
        // Set the marker before synchronous changed events can re-enter.
        options.setOptionValue('outputOptionsVersion',1);
        if (module !== 'glmbasic' &&
            options.getOption('comparisonGraphic').getValue() !== (ui.graphic.value() === true))
            options.setOptionValue('comparisonGraphic',ui.graphic.value() === true);
    } finally { options.endEdit(); }
};
