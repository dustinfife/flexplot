'use strict';

// Compile the real UI and exercise its automatic handlers without a browser DOM.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const vm = require('node:vm');
const {pathToFileURL} = require('node:url');

(async () => {
    const [root, compilerPath, output, runtime, analysis = 'glmbasic'] = process.argv.slice(2);
    const temp = fs.mkdtempSync(path.join(os.tmpdir(), 'flexplot-ui-'));
    try {
        for (const suffix of ['a.yaml', 'r.yaml', 'u.yaml'])
            fs.copyFileSync(path.join(root, 'jamovi', analysis + '.' + suffix), path.join(temp, analysis + '.' + suffix));
        fs.copyFileSync(path.join(root, 'jamovi/js/' + analysis + '.js'), path.join(temp, analysis + '.js'));
        fs.copyFileSync(path.join(root, 'jamovi/js/model-terms.js'), path.join(temp, 'model-terms.js'));
        const {default: compileR} = await import(pathToFileURL(path.join(compilerPath, 'compiler.js')));
        compileR('flexplot', path.join(temp, analysis + '.a.yaml'), path.join(temp, analysis + '.r.yaml'),
            path.join(compilerPath, 'header.template'), output + '.header.R');
        const {default: compile} = await import(pathToFileURL(path.join(compilerPath, 'uicompiler.js')));
        compile(path.join(temp, analysis + '.a.yaml'), path.join(temp, analysis + '.u.yaml'),
            path.join(root, 'jamovi/js/' + analysis + '.js'), analysis,
            path.join(compilerPath, 'src.template'), path.join(temp, analysis + '.src.js'));

        const FormatDef = {term: {name:'term'}, variable: {name:'variable'}};
        const utils = {valuesToItems: (values, format) => values.map(raw => ({value:{raw, format}}))};
        // This is the automatic-handler contract: helpers are NOT installed on
        // the View when this.handlers exists. The previous stand-in did that.
        let extend = function(spec) {
            return function() {
                if (this.handlers === undefined) Object.assign(this, utils);
                Object.assign(this, this.handlers, spec);
            };
        };
        if (runtime) {
            const source = fs.readFileSync(runtime, 'utf8');
            const marker = source.indexOf('this.handlers===void 0&&Object.assign(this,le)');
            assert.ok(marker > 0, 'Installed automatic-handler contract not found');
            const start = source.lastIndexOf('static extend(', marker) + 'static '.length;
            const brace = source.indexOf('{', start);
            let depth = 1, end = brace + 1;
            while (depth) {
                if (source[end] === '{') depth++;
                if (source[end] === '}') depth--;
                end++;
            }
            const method = source.slice(start, end).replace(/^extend/, 'function');
            extend = vm.runInNewContext('(' + method + ')', {le:utils});
        }
        const sandbox = vm.createContext({utils, FormatDef, console, module:{exports:{}}, exports:{},
            ui:{extend: value => value}, DefaultControls:new Proxy({}, {get: (_, name) => name}),
            View:{extend}});
        const browserify = require(require.resolve('browserify', {paths:[compilerPath]}));
        const bundle = await new Promise((resolve, reject) => browserify(path.join(temp, analysis + '.src.js'),
            {standalone:'module'}).bundle((error, data) => error ? reject(error) : resolve(data)));
        vm.runInContext(bundle.toString(), sandbox);
        const compiled = sandbox.module.exports;
        const view = {_baseEvents:[], workspace:{}};
        compiled.view.call(view);
        assert.equal(view.valuesToItems, undefined);
        const controls = new Set(['view']);
        const walk = control => {
            if (control.name) controls.add(control.name);
            for (const child of control.controls || []) walk(child);
        };
        walk(compiled.view.layout);
        const dual = controls.has('comparisonTerms');
        for (const name of (dual ? ['preds','modelTerms','comparisonTerms','primarySupplier','comparisonSupplier'] : ['preds','modelTerms','primarySupplier']))
            assert.ok(controls.has(name), 'Missing compiled control: ' + name);
        const fire = (name, event) => {
            assert.ok(controls.has(name));
            const handler = view.handlers[name + '_' + event];
            assert.equal(typeof handler, 'function', 'Missing automatic binding: ' + name + '_' + event);
            handler.call(view, ui);
        };
        const control = initial => {
            let value = initial;
            return {value: () => value, setValue: updated => {value = updated;}};
        };
        let depth = 0;
        const ui = {preds:control([]), modelTerms:control(null), comparisonTerms:control(null),
            primarySupplier:control([]), comparisonSupplier:control([]),
            view:{model:{options:{beginEdit(){depth++;}, endEdit(){depth--;}}}}};
        for (const name of controls) {
            if (!(name in ui)) ui[name] = control(name.endsWith('Supplier') ? [] : null);
        }
        const records = ['stage\tpreds\tprimary\tsecondary\tcompare'];
        const encode = terms => terms == null ? '__NULL__' :
            (terms.length ? terms.map(term => term.join(':')).join(';') : '__EMPTY__');
        const snapshot = (stage, compare=false) => {
            assert.equal(depth, 0, 'Option edit transaction left open');
            for (const supplier of (dual ? [ui.primarySupplier, ui.comparisonSupplier] : [ui.primarySupplier]))
                assert.equal(JSON.stringify(supplier.value().map(item => item.value.raw)),
                    JSON.stringify(ui.preds.value().map(name => [name])));
            records.push([stage, ui.preds.value().join(';'), encode(ui.modelTerms.value()),
                encode(dual ? ui.comparisonTerms.value() : null), compare && dual].join('\t'));
        };
        fire('view', 'loaded');
        ui.preds.setValue(['supp']); fire('preds', 'changed'); snapshot('supp');
        ui.preds.setValue(['supp','dose']); fire('preds', 'changed'); snapshot('add-dose');
        assert.equal(encode(ui.modelTerms.value()), 'supp;dose');
        ui.modelTerms.setValue([['supp'],['dose'],['supp','dose']]);
        ui.comparisonTerms.setValue([['supp']]);
        fire('primarySupplier', 'updated'); if (dual) fire('comparisonSupplier', 'updated');
        snapshot('interaction', true);
        assert.equal(encode(ui.modelTerms.value()), 'supp;dose;supp:dose');
        assert.equal(encode(ui.comparisonTerms.value()), 'supp');
        ui.modelTerms.setValue([['supp'],['dose']]);
        fire('view', 'updated'); snapshot('remove-interaction');
        // Display variables come from terms, not the original variable pool.
        ui.plotX.setValue('dose'); ui.plotGroup.setValue('supp');
        ui.plotPanels.setValue(['supp']);
        fire('modelTerms', 'changed');
        assert.equal(ui.plotX.value(), 'dose');
        ui.modelTerms.setValue([['supp']]);
        if (dual) {
            ui.comparisonTerms.setValue([['dose']]);
            ui.comparisonX.setValue('dose');
            ui.comparisonPanels.setValue(['supp']);
        }
        fire('modelTerms', 'changed');
        assert.equal(ui.plotX.value(), null);
        assert.equal(JSON.stringify(ui.plotSupplier.value().map(item => item.value.raw)), '["supp"]');
        if (dual) {
            assert.equal(ui.comparisonX.value(), 'dose');
            assert.equal(JSON.stringify(ui.comparisonPlotSupplier.value().map(item => item.value.raw)), '["supp","dose"]');
            ui.comparisonTerms.setValue([['supp']]);
            fire('comparisonTerms', 'changed');
            assert.equal(ui.comparisonX.value(), null);
        }
        ui.modelTerms.setValue([['supp','dose']]);
        fire('modelTerms', 'changed');
        assert.equal(ui.plotSupplier.value().length, 2);
        ui.modelTerms.setValue([]); fire('modelTerms', 'changed');
        assert.equal(ui.plotSupplier.value().length, 0);
        assert.equal(ui.plotGroup.value(), null);
        assert.equal(ui.plotPanels.value().length, 0);
        ui.modelTerms.setValue([['supp']]);
        ui.preds.setValue(['supp']); fire('preds', 'changed'); snapshot('remove-dose');
        assert.equal(encode(ui.modelTerms.value()), 'supp');
        ui.comparisonTerms.setValue([]); fire('view', 'updated');
        assert.equal(encode(ui.comparisonTerms.value()), '__EMPTY__');
        // Reusing the same view for a different analysis must reset its history.
        view.workspace = {};
        ui.preds.setValue(['dose']); ui.modelTerms.setValue(null); ui.comparisonTerms.setValue(null);
        fire('view', 'updated'); snapshot('reopen');
        assert.equal(encode(ui.modelTerms.value()), 'dose');
        fs.writeFileSync(output, records.join('\n') + '\n');
        console.log(runtime ? 'Used installed jamovi View.extend' : 'Used portable automatic-handler contract');
    } finally {
        fs.rmSync(temp, {recursive:true, force:true});
    }
})().catch(error => {console.error(error); process.exitCode = 1;});
