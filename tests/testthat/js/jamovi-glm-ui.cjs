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
            if (analysis === 'mixedmodel' && control.type === 'Supplier')
                assert.equal(typeof view.handlers[control.name + '_updated'],'function',
                    'Native Supplier requires an updated handler: ' + control.name);
            if (control.name) controls.add(control.name);
            for (const child of control.controls || []) walk(child);
        };
        walk(compiled.view.layout);
        const dual = controls.has('comparisonTerms');
        const generalized = analysis === 'glmgeneralized';
        if (generalized) assert.ok(controls.has('includeP'), 'Missing Include p-values control');
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
        if (analysis === 'mixedmodel') {
            const yaml = require(require.resolve('js-yaml',{paths:[compilerPath]}));
            if (runtime) {
                const resultsFile = fs.readdirSync(path.dirname(runtime)).find(name => /^resultsview-.*\.js$/.test(name));
                assert.ok(resultsFile,'Installed results renderer is required for citation checks');
                const source = fs.readFileSync(path.join(path.dirname(runtime),resultsFile),'utf8');
                const className = source.match(/customElements.define\("jmv-reference-numbers",([\w$]+)\)/)[1];
                const start = source.indexOf('class '+className+' extends');
                const end = source.indexOf('customElements.define("jmv-reference-numbers"',start);
                class Element {
                    attachShadow() {
                        const body = {innerHTML:'',insertAdjacentHTML(position,html){this.innerHTML+=html;}};
                        return {innerHTML:'',querySelector:()=>body};
                    }
                }
                const References = vm.runInNewContext('('+source.slice(start,end)+')',{HTMLElement:Element,Tg:''});
                const references = new References();
                references.setTable({mode:'all',table:{flexplot:3,graphicalAnalysis:4},addEventListener(){}});
                references.setRefs(['flexplot','graphicalAnalysis']);
                assert.ok(references._body.innerHTML.includes('[3]') && references._body.innerHTML.includes('[4]'));
                const results = yaml.load(fs.readFileSync(path.join(root,'jamovi/mixedmodel.r.yaml'),'utf8'));
                for (const name of ['fixed','random','correlations']) {
                    references.setRefs(results.items.find(item=>item.name===name).refs || []);
                    assert.equal(references.count(),0);
                    assert.equal(references._body.innerHTML,'');
                    assert.equal(references.hasVisibleContent(),false);
                }
                references.setRefs(results.refs);
                assert.equal(references.hasVisibleContent(),true,'Retain analysis-level research references');
            }

            const definition = yaml.load(fs.readFileSync(path.join(root,'jamovi/mixedmodel.a.yaml'),'utf8'));
            assert.equal(definition.options.find(option => option.name === "casGhost").default,false);
            assert.equal(definition.options.find(option => option.name === "casGhost").title,"Add ghost line");
            const casVisualization = compiled.view.layout.controls.find(control => control.label === "Model visualization");
            const ghost = casVisualization.controls.find(control => control.name === "casGhost");
            assert.equal(ghost.contentLink,"(compare && predictionType:cas)");
            assert.equal(ghost.useSingleCell,true);
            if (runtime) require("./jamovi-binomial-visibility.cjs")(fs.readFileSync(runtime,"utf8"),ghost,null,compilerPath);
            assert.equal(definition.options.find(option => option.name === 'interactions').title,'Include Interaction Terms');
            assert.equal(definition.options.find(option => option.name === 'polynomials').title,'Include Polynomial Terms');
            assert.equal(definition.options.find(option => option.name === 'showCorrelations').default,false);
            assert.equal(definition.options.find(option => option.name === 'compare').default,false);
            assert.equal(definition.title,'Mixed Models');
            assert.equal(definition.options.find(option => option.name === 'cluster').title,'Cluster variable');
            const variables = compiled.view.layout.controls.find(control => control.type === 'VariableSupplier');
            assert.ok(JSON.stringify(variables).includes('Cluster Variable'));
            assert.ok(!JSON.stringify(variables).includes('Grouping Variable'));

            const comparison = compiled.view.layout.controls.find(control => control.label === 'Model Comparison');
            assert.ok(JSON.stringify(comparison).includes('Model 2 — Fixed Effects'));
            assert.ok(JSON.stringify(comparison).includes('comparisonFixed'));
            assert.ok(JSON.stringify(comparison).includes('Model 2 — Random Effects'));

            assert.equal(comparison.controls.find(control => control.contentLink === '(compare)').useSingleCell,true);
            const randomControls = JSON.stringify(comparison);
            assert.ok(randomControls.includes('Random slopes (Model 2)'));
            assert.ok(!randomControls.includes('Model 2 random slopes (correlated components)'));

            assert.equal(definition.options.find(option => option.name === 'interactions').default,false);
            assert.equal(definition.options.find(option => option.name === 'polynomials').default,false);
            const visualization = compiled.view.layout.controls.find(control => control.label === 'Model visualization');
            const editor = visualization.controls.find(control => control.label === 'Plot Labels');
            assert.equal(editor.contentLink,'(graphic)');
            assert.ok(editor.controls.find(control => control.name === 'plotLabels').columns.some(column => column.name === 'label'));

            for (const [name, option] of [['primarySupplier','interactions'], ['quadraticSupplier','polynomials']]) {
                const definition = compiled.view.layout.controls.find(control => control.name === name);
                assert.ok(definition, name);
                assert.equal(definition.useSingleCell, true, 'Hide the whole builder, including labels');
                assert.equal(definition.contentLink, '('+option+')');
            }
            require('./jamovi-mixed-ui.cjs')(ui, view, fire, assert);
            if (runtime) {
                const records = require('./jamovi-mixed-native-options.cjs')(
                    fs.readFileSync(runtime,'utf8'),compilerPath,view.handlers,FormatDef,utils,assert);
                const names = Object.keys(records[0].options);
                const encodeValue = (name,value) => value === null ? '__NULL__' :
                    (Array.isArray(value) ? (value.length ? value.map(item => Array.isArray(item) ? item.join(':') : item).join(';') : '__EMPTY__') : String(value));
                const rows = records.map(record => [record.stage,record.changed.join(';'),
                    ...names.map(name => encodeValue(name,record.options[name]))].join('\t'));
                fs.writeFileSync(output, ['stage\tchanged\t'+names.join('\t'),...rows].join('\n'));
            } else fs.writeFileSync(output, '[]');
            return;
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
        assert.equal(ui.plotX.value(), generalized ? 'supp' : null);
        assert.equal(JSON.stringify(ui.plotSupplier.value().map(item => item.value.raw)), '["supp"]');
        if (dual) {
            assert.equal(ui.comparisonX.value(), 'dose');
            assert.equal(JSON.stringify(ui.comparisonPlotSupplier.value().map(item => item.value.raw)), '["supp","dose"]');
            ui.comparisonTerms.setValue([['supp']]);
            fire('comparisonTerms', 'changed');
            assert.equal(ui.comparisonX.value(), generalized ? 'supp' : null);
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
        if (generalized) {
            ui.preds.setValue(['dose','supp']); ui.modelTerms.setValue([['dose'],['supp']]);
            ui.plotX.setValue(null); ui.plotPanels.setValue([]); ui.plotGroup.setValue(null);
            fire('view', 'updated');
            assert.equal(ui.plotX.value(), 'dose');
            ui.plotPanels.setValue(['dose','supp']); ui.plotGroup.setValue('supp');
            fire('plotPanels', 'changed');
            assert.equal(JSON.stringify(ui.plotPanels.value()), '["supp"]');
            assert.equal(ui.plotGroup.value(), null);
            ui.plotX.setValue(null); ui.plotPanels.setValue(['dose']);
            fire('plotX', 'changed');
            assert.equal(ui.plotX.value(), 'supp');
            ui.quadratic.setValue(['dose']); ui.modelTerms.setValue([['supp']]);
            fire('quadratic', 'changed');
            assert.equal(encode(ui.modelTerms.value()), 'supp;dose');
            ui.modelTerms.setValue([['supp']]); fire('view', 'updated');
            assert.equal(JSON.stringify(ui.quadratic.value()), '["dose"]');
            assert.ok(ui.plotSupplier.value().some(item => item.value.raw === 'dose'));
            ui.preds.setValue(['supp']); fire('preds', 'changed');
            assert.equal(JSON.stringify(ui.quadratic.value()), '[]');
            assert.equal(ui.plotPanels.value().length, 0);
            ui.preds.setValue(['supp','dose']); fire('preds', 'changed');
            ui.comparisonTerms.setValue([['supp']]);
            ui.comparisonQuadratic.setValue(['dose']); fire('comparisonQuadratic', 'changed');
            assert.equal(encode(ui.comparisonTerms.value()), 'supp;dose');
            ui.comparisonTerms.setValue([['supp']]); fire('view', 'updated');
            assert.equal(JSON.stringify(ui.comparisonQuadratic.value()), '["dose"]');
            assert.ok(ui.comparisonPlotSupplier.value().some(item => item.value.raw === 'dose'));
            assert.equal(encode(ui.modelTerms.value()), 'supp;dose');
            ui.preds.setValue(['supp']); fire('preds', 'changed');
            assert.equal(JSON.stringify(ui.comparisonQuadratic.value()), '[]');
            assert.equal(encode(ui.comparisonTerms.value()), 'supp');
            // Native supplier removes assigned entries; all model variables remain its source.
            const supplier = compiled.view.layout.controls.flatMap(c => c.controls || []).find(c => c.name === 'plotSupplier');
            assert.ok(supplier && supplier.persistentItems === false);
            const style = compiled.view.layout.controls.flatMap(c => c.controls || []).find(c => c.name === 'plotStyle');
            assert.ok(style && style.contentLink === '(family:binomial)');
            if (runtime) {
                const installed = fs.readFileSync(runtime, 'utf8');
                const start = installed.indexOf('filterSuppliersList(e=!1){');
                assert.ok(start > 0);
                const brace = installed.indexOf('{', start);
                let end = brace + 1, depth = 1;
                while (depth) {
                    if (installed[end] === '{') depth++;
                    if (installed[end] === '}') depth--;
                    end++;
                }
                const filter = vm.runInNewContext('(function' + installed.slice(start + 'filterSuppliersList'.length, end) + ')');
                const visible = [];
                const nativeSupplier = {blockFilterProcess:false, $searchInput:{value:''}, _persistentItems:false,
                    _items:[{used:1},{used:0}], supplier:{getRow: row => [{setVisibility: value => {visible[row-1] = value;}}]}};
                filter.call(nativeSupplier);
                assert.equal(JSON.stringify(visible), '[false,true]');
                nativeSupplier._items[0].used = 0; filter.call(nativeSupplier);
                assert.equal(JSON.stringify(visible), '[true,true]');
                const se = compiled.view.layout.controls.flatMap(c => c.controls || []).find(c => c.name === 'se');
                require('./jamovi-binomial-visibility.cjs')(installed, style, se, compilerPath);

            }
        }
        fs.writeFileSync(output, records.join('\n') + '\n');
        console.log(runtime ? 'Used installed jamovi View.extend' : 'Used portable automatic-handler contract');
    } finally {
        fs.rmSync(temp, {recursive:true, force:true});
    }
})().catch(error => {console.error(error); process.exitCode = 1;});
