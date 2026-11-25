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
        fs.copyFileSync(path.join(root,'jamovi/js/output-options.js'),path.join(temp,'output-options.js'));
        fs.copyFileSync(path.join(root,'jamovi/js/plot-label-editors.js'),path.join(temp,'plot-label-editors.js'));
        if (analysis === 'glmgeneralized') fs.copyFileSync(path.join(root,'jamovi/js/glm-plot-controls.js'),path.join(temp,'glm-plot-controls.js'));
        fs.copyFileSync(path.join(root,'jamovi/js/model-ghosts.js'),path.join(temp,'model-ghosts.js'));
        if (analysis === 'glmgeneralized') fs.copyFileSync(path.join(root,'jamovi/js/glm-ghosts.js'),path.join(temp,'glm-ghosts.js'));
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
            if (control.type === 'Supplier')
                assert.equal(typeof view.handlers[control.name + '_updated'],'function',
                    'Native Supplier requires an updated handler: ' + control.name);
            if (control.name) controls.add(control.name);
            for (const child of control.controls || []) walk(child);
        };
        walk(compiled.view.layout);
        const flatten = items => items.flatMap(item=>[item,...flatten(item.controls || [])]);
        const nodes = flatten(compiled.view.layout.controls);
        const mainSection = compiled.view.layout.controls.find(item=>item.label==='Model Visualization');
        assert.ok(mainSection,'Missing main visualization section');
        assert.equal(mainSection.controls.findIndex(c=>c.name==='ghostLines'),mainSection.controls.findIndex(c=>c.name===(analysis==='mixedmodel'?'plotGroupingLayout':'plotSupplier'))+1,'Ghost checkbox follows selectors');
        const compSection = compiled.view.layout.controls.find(item=>item.label==='Model Comparison Visualization');
        assert.ok(compSection,'Missing distinct comparison visualization section');
        const modelSpec = compiled.view.layout.controls.find(item=>item.label==='Model Comparison');
        assert.ok(!flatten(modelSpec.controls).some(item=>['plotX','comparisonX','plotLabels','comparisonPlotLabels'].includes(item.name)),
            'Model specifications must not contain visualization controls');
        for (const name of ['plotLabels','comparisonPlotLabels']) {
            const editor = nodes.find(item=>item.name===name);
            assert.equal(editor.height,'large');assert.equal(editor.type,'ListBox');
            assert.ok(editor.contentLink.includes(name==='comparisonPlotLabels' ? 'modifyComparisonPlotLabels' : 'modifyPlotLabels'));
            if (runtime && analysis!=='mixedmodel') require('./jamovi-binomial-visibility.cjs')(fs.readFileSync(runtime,'utf8'),editor,null,compilerPath);
        }

        const dual = controls.has('comparisonTerms');
        const generalized = analysis === 'glmgeneralized';
        if (generalized) {
            assert.ok(controls.has('includeP'), 'Missing Include p-values control');
            assert.ok(controls.has('ghostLines'),'Missing ordinary panel-ghost control');
        }
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
            const properties={};
            return {value: () => value, setValue: updated => {value = updated;},
                setPropertyValue:(name,value)=>{properties[name]=value;},getPropertyValue:name=>properties[name]};
        };
        let depth = 0;
        const ui = {preds:control([]), modelTerms:control(null), comparisonTerms:control(null),
            primarySupplier:control([]), comparisonSupplier:control([]),
            view:{model:{options:{beginEdit(){depth++;}, endEdit(){depth--;}}}}};
        for (const name of controls) {
            if (!(name in ui)) ui[name] = control(name.endsWith('Supplier') ? [] : null);
        }
        const yaml = require(require.resolve('js-yaml',{paths:[compilerPath]}));
        if (analysis === 'glmbasic' || generalized) {
            for (const [name,value] of Object.entries({interactions:false,comparisonInteractions:false,interactionSpecVersion:0})) {
                if (!ui[name]) ui[name] = control(value); else ui[name].setValue(value);
            }
            ui.view.model.options.getOption = name => ({getValue:ui[name].value});
            ui.view.model.options.setOptionValue = (name,value) => ui[name].setValue(value);
        }

        ui.outputOptionsVersion = control(0);
        ui.plotLabelEditorVersion = control(0);
        ui.plotLabelStore = control([]);ui.comparisonPlotLabelStore = control([]);
        if (analysis === 'glmbasic') ui.comparisonMagnet = control(false);
        if (generalized || analysis === 'glmbasic') ui.mainEffects = control(null); // Legacy saved option; no visible control.
        if (generalized) { ui.family=control('auto'); ui.automaticFamily=control('gaussian'); ui.automaticFamilyOutcome=control(''); ui.polynomials.setValue(false); ui.comparisonPolynomials.setValue(false); }
        if (analysis === 'glmbasic') {
            assert.ok(controls.has('se'));
            assert.equal(controls.has('line'),false,'Generic smoothers must not appear in the LM UI');
            assert.equal(controls.has('center'),false,'Observed center/spread summaries must not appear in the LM UI');
            const definition = yaml.load(fs.readFileSync(path.join(root,'jamovi',analysis+'.a.yaml'),'utf8'));
            assert.equal(definition.options.find(option => option.name === 'se').title,'95% confidence intervals');
            for (const name of ['line','center']) assert.ok(definition.options.some(option => option.name === name),'Retain saved option: '+name);
            const main = compiled.view.layout.controls.find(c=>c.label==='Model Visualization');
            assert.ok(main.controls.some(c=>c.name==='se'));
            assert.ok(!JSON.stringify(compiled.view.layout).includes('Graphic Options'));
        }
        if (analysis === 'glmbasic' || generalized) {
            const definition = yaml.load(fs.readFileSync(path.join(root,'jamovi',analysis+'.a.yaml'),'utf8'));
            const descendants = controls => controls.flatMap(control => [control,...descendants(control.controls || [])]);
            const editors = descendants(compiled.view.layout.controls);
            for (const name of ['plotLabels','comparisonPlotLabels']) {
                const editor = editors.find(control => control.name === name);
                assert.ok(editor && editor.columns.some(column => column.name === 'label'));
                for (const optionName of [name,name.replace('Labels','LabelStore')]) {
                    const option = definition.options.find(option => option.name === optionName);
                    assert.equal(option.type,'Array');
                    assert.equal(JSON.stringify(option.default),'[]');
                }
            }

            if (generalized) {
                for (const name of ['polynomials','comparisonPolynomials']) {
                    assert.equal(definition.options.find(option => option.name === name).default,false);
                    assert.equal(definition.options.find(option => option.name === name).title,'Include Polynomial Terms');
                }
                const comparison = compiled.view.layout.controls.find(control => control.label === 'Model Comparison');
                const second = comparison.controls.find(control => control.label === (generalized ? 'Model 2 effects' : 'Model 2 fixed effects'));
                const primarySquare = compiled.view.layout.controls.find(control => control.name === 'quadraticSupplier');
                const secondSquare = second.controls.find(control => control.name === 'comparisonQuadraticSupplier');
                const comparisonVisualization = compiled.view.layout.controls.find(control => control.label === 'Model Comparison Visualization');
                assert.equal(comparisonVisualization.contentLink,'(compare)');
                const comparisonPlot = comparisonVisualization.controls.find(control => control.name === 'comparisonPlotSupplier');
                assert.equal(comparisonPlot.label,'Available predictors for comparison plot');
                assert.equal(comparisonPlot.controls[0].label,'Comparison x variable');
                assert.equal(comparisonPlot.controls[1].label,'Comparison panel variables (up to two)');
                assert.equal(primarySquare.contentLink,'(polynomials)');
                assert.equal(secondSquare.contentLink,'(comparisonPolynomials)');
                assert.equal(comparisonPlot.contentLink,'(compare)');
                if (runtime) for (const cell of [primarySquare,secondSquare,comparisonPlot])
                    require('./jamovi-binomial-visibility.cjs')(fs.readFileSync(runtime,'utf8'),cell,null,compilerPath);
            }
            for (const name of ['interactions','comparisonInteractions']) {
                assert.equal(definition.options.find(option => option.name === name).title,'Include Interactions');
                assert.equal(definition.options.find(option => option.name === name).default,false);
            }
            assert.equal(definition.options.find(option => option.name === 'interactionSpecVersion').hidden,true);
            const primary = compiled.view.layout.controls.find(control => control.name === 'primarySupplier');
            const main = compiled.view.layout.controls.find(control => control.name === 'primaryMainSupplier');
            if (generalized) {
                assert.equal(main,undefined,'No redundant Model 1 selector');
                assert.equal(controls.has('mainEffects'),false);
                assert.equal(primary.label,'Effects and interactions');
            } else {
                assert.equal(main,undefined,'No redundant main-effects supplier');
                assert.equal(definition.options.find(option => option.name === 'mainEffects').hidden,true,
                    'Retain the legacy option without automatic compiler-generated controls');
                assert.equal(controls.has('mainEffects'),false,'Saved mainEffects option has no visible control');
                const advanced = compiled.view.layout.controls.find(control => control.label === 'Advanced main effects');
                assert.equal(advanced,undefined,'No replacement advanced section');
            }
            assert.equal(primary.contentLink,'(interactions)');
            assert.equal(primary.useSingleCell,true);
            const comparison = compiled.view.layout.controls.find(control => control.label === 'Model Comparison');
            const second = comparison.controls.find(control => control.label === (generalized ? 'Model 2 effects' : 'Model 2 fixed effects'));
            assert.equal(second.contentLink,'(compare)');
            const builder = second.controls.find(control => control.name === 'comparisonSupplier');
            assert.equal(builder.contentLink,'(comparisonInteractions)');
            assert.equal(second.controls.find(control => control.name === 'comparisonMainSupplier').contentLink,undefined);
            if (runtime) for (const definition of [primary,builder,second])
                require('./jamovi-binomial-visibility.cjs')(fs.readFileSync(runtime,'utf8'),definition,null,compilerPath);
        }
        if (analysis === 'glmbasic' || analysis === 'mixedmodel') {
            const descend = items => items.flatMap(item=>[item,...descend(item.controls || [])]);
            const all = descend(compiled.view.layout.controls);
            assert.ok(all.filter(item=>item.type==='TargetLayoutBox').every(item=>item.useSingleCell!==true),
                'Native target grids require the supplier aux column; never wrap a target in a generic single cell');
            const target = all.find(item=>item.name===(analysis==='glmbasic' ? 'comparisonGroupingLayout' : 'plotGroupingLayout'));
            if (analysis === 'mixedmodel') {
                const main = all.find(item=>item.name==='graphic');
                assert.equal(main.contentLink,undefined);
            }
            assert.equal(target.type,'Supplier');
            assert.equal(target.useSingleCell,true);
            assert.equal(target.contentLink,analysis === 'glmbasic' ? '(compare)' : undefined);
            if (runtime && analysis === 'glmbasic') require('./jamovi-binomial-visibility.cjs')(fs.readFileSync(runtime,'utf8'),target,null,compilerPath);
        }
        if (analysis === 'glmbasic') {
            ui.preds.setValue(['x','z']);ui.out.setValue('binary');ui.compare.setValue(true);
            ui.modelTerms.setValue([['x'],['z']]);ui.comparisonTerms.setValue([['x']]);
            fire('view','updated');
            assert.equal(ui.comparisonGroupingLayout.getPropertyValue('contentLink'),false);
            ui.comparisonMagnet.setValue(true);fire('view','updated');
            assert.equal(ui.comparisonGroupingLayout.getPropertyValue('contentLink'),true);
            ui.compare.setValue(false);fire('compare','changed');
            assert.equal(ui.comparisonGroupingLayout.getPropertyValue('contentLink'),false);
            ui.preds.setValue([]);ui.modelTerms.setValue(null);ui.comparisonTerms.setValue(null);ui.interactionSpecVersion.setValue(0);view.workspace={};
        }
        if (analysis === 'mixedmodel' || analysis === 'glmgeneralized') {
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
                const results = yaml.load(fs.readFileSync(path.join(root,'jamovi',analysis+'.r.yaml'),'utf8'));
                for (const name of (analysis === 'mixedmodel' ? ['fixed','random','correlations'] : ['coefficients','irt','probabilities'])) {
                    references.setRefs(results.items.find(item=>item.name===name).refs || []);
                    assert.equal(references.count(),0);
                    assert.equal(references._body.innerHTML,'');
                    assert.equal(references.hasVisibleContent(),false);
                }
                references.setRefs(results.refs);
                assert.equal(references.hasVisibleContent(),true,'Retain analysis-level research references');
            }

        }
        const resultDefinition = yaml.load(fs.readFileSync(path.join(root,'jamovi',analysis+'.r.yaml'),'utf8'));
        if (analysis === 'glmbasic') {
            for (const name of ['factorMeans','diff'])
                assert.equal(resultDefinition.items.find(item => item.name === name).visible,false);
        }
        if (analysis === 'glmgeneralized') {
            assert.deepEqual(resultDefinition.refs,['flexplot','graphicalAnalysis']);
            assert.equal(resultDefinition.items.find(item => item.name === 'plotExplanation').title,'');
            for (const name of ['explanation','plotExplanation','plot','comparisonplot'])
                assert.equal(resultDefinition.items.find(item => item.name === name).visible,false);
            for (const name of ['plot','comparisonplot'])
                assert.equal(resultDefinition.items.find(item => item.name === name).height,450);
        }
        if (analysis === 'mixedmodel') {
            ui.samplePolicyVersion = control(0);
            ui.view.model.options.getOption = name => ({getValue:ui[name].value});
            ui.view.model.options.setOptionValue = (name,value) => ui[name].setValue(value);

            const definition = yaml.load(fs.readFileSync(path.join(root,'jamovi/mixedmodel.a.yaml'),'utf8'));
            assert.equal(definition.options.find(option => option.name === "casGhost").default,false);
            assert.equal(definition.options.find(option => option.name === "casGhost").title,"Add ghost line");
            const casVisualization = compiled.view.layout.controls.find(control => control.name === "comparisonVisualizationSection");
            const ghost = casVisualization.controls.find(control => control.name === "casGhost");
            assert.equal(ghost.contentLink,"(comparisonPredictionType:cas)");
            assert.equal(ghost.useSingleCell,true);
            if (runtime) require("./jamovi-binomial-visibility.cjs")(fs.readFileSync(runtime,"utf8"),ghost,null,compilerPath);
            assert.equal(definition.options.find(option => option.name === 'interactions').title,'Include Interaction Terms');
            assert.equal(definition.options.find(option => option.name === 'polynomials').title,'Include Polynomial Terms');
            assert.equal(definition.options.find(option => option.name === 'showCorrelations').default,false);
            assert.equal(definition.options.find(option => option.name === 'compare').default,false);
            const resultSchema = yaml.load(fs.readFileSync(path.join(root,'jamovi/mixedmodel.r.yaml'),'utf8'));
            for(const name of ['fixed','fixed2'])assert.equal(resultSchema.items.find(item=>item.name===name).columns.find(column=>column.name==='p').visible,false);
            assert.equal(resultSchema.items.find(item=>item.name==='comparison').columns.find(column=>column.name==='p').visible,'(includeP)');
            assert.equal(controls.has('includeCI'),false,'CI preference is hidden for compatibility');
            assert.equal(definition.options.find(option=>option.name==='includeCI').hidden,true);
            assert.equal(definition.options.find(option=>option.name==='includeCI').default,true);
            const inferenceControls = compiled.view.layout.controls.find(c=>c.label==='Output Options').controls;
            assert.equal(JSON.stringify(inferenceControls.map(c=>c.name)),JSON.stringify(['graphic','estimates','showCorrelations','comparisonGraphic','comparisonStatistics','includeP']));
            const p = inferenceControls.find(c=>c.name==='includeP');
            assert.equal(p.contentLink,'(compare)');assert.equal(p.useSingleCell,true);
            if(runtime)require('./jamovi-binomial-visibility.cjs')(fs.readFileSync(runtime,'utf8'),p,null,compilerPath);
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
            const visualization = compiled.view.layout.controls.find(control => control.name === 'modelVisualizationSection');
            const editor = visualization.controls.find(control => control.name === 'plotLabels');
            assert.equal(editor.contentLink,'(modifyPlotLabels)');
            assert.equal(editor.height,'large');
            assert.ok(editor.columns.some(column => column.name === 'label'));

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
            await require('./jamovi-model-ghosts.cjs')(ui,view,fire,assert,analysis);
            return;
        }
        if (analysis === 'glmbasic' || generalized) {
            // Saved terms can arrive before native predictor controls finish loading.
            const restored = [['supp'],['dose'],['supp','dose']];
            ui.preds.setValue([]); ui.modelTerms.setValue(restored);
            ui.comparisonTerms.setValue([['supp']]);
            fire('view','loaded'); fire('primarySupplier','updated');
            assert.equal(JSON.stringify(ui.modelTerms.value()),JSON.stringify(restored));
            assert.equal(JSON.stringify(ui.comparisonTerms.value()),'[["supp"]]');
            ui.preds.setValue(['supp','dose']); fire('view','updated');
            assert.equal(JSON.stringify(ui.modelTerms.value()),JSON.stringify(restored));
            // Explicit removal must still clear model terms.
            ui.preds.setValue([]); fire('preds','changed');
            assert.equal(JSON.stringify(ui.modelTerms.value()),'[]');
            view.workspace = {};
            ui.modelTerms.setValue(null); ui.comparisonTerms.setValue(null);
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
            ui.polynomials.setValue(true); fire('polynomials','changed');
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
            ui.comparisonPolynomials.setValue(true); fire('comparisonPolynomials','changed');
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
            assert.ok(style && style.contentLink === '(family:binomial || (family:auto && automaticFamily:binomial))');
            const scale = compiled.view.layout.controls.flatMap(c => c.controls || []).find(c => c.name === 'plotScale');
            assert.equal(scale.contentLink,'(family:binomial || (family:auto && automaticFamily:binomial))');
            const visualization = compiled.view.layout.controls.find(c => c.label === 'Model Visualization').controls;
            const descendants = items => items.flatMap(item=>[item,...descendants(item.controls || [])]);
            const comparisonSection = compiled.view.layout.controls.find(c=>c.label==='Model Comparison Visualization').controls;
            assert.equal(JSON.stringify(comparisonSection.filter(c=>c.name && ['comparisonPlotStyle','comparisonPlotScale','comparisonSe'].includes(c.name)).map(c=>c.name)),JSON.stringify(['comparisonPlotStyle','comparisonPlotScale','comparisonSe']));
            assert.ok(!descendants(comparisonSection).some(c=>['se','plotStyle','plotScale'].includes(c.name)));
            assert.ok(!descendants(visualization).some(c=>['comparisonSe','comparisonPlotStyle','comparisonPlotScale'].includes(c.name)));
            if (runtime) for (const control of comparisonSection.filter(c=>c.name==='comparisonPlotStyle' || c.name==='comparisonPlotScale'))
                require('./jamovi-binomial-visibility.cjs')(fs.readFileSync(runtime,'utf8'),control,comparisonSection.find(c=>c.name==='comparisonSe'),compilerPath);

            assert.equal(visualization.findIndex(c => c.name === 'plotScale'),visualization.findIndex(c => c.name === 'plotStyle')+1);
            const top = compiled.view.layout.controls;
            assert.equal(top[0].type,'VariableSupplier');
            assert.equal(top[1].name,'familySelection');
            assert.equal(top[2].name,'interactions');
            assert.ok(!JSON.stringify(top).includes('Automatic selects'));
            const familyOptions=yaml.load(fs.readFileSync(path.join(root,'jamovi/glmgeneralized.a.yaml'),'utf8')).options;
            const visibleFamily=familyOptions.find(o=>o.name==='familySelection');
            assert.ok(!visibleFamily.options.some(o=>o.name==='auto'));
            assert.ok(familyOptions.find(o=>o.name==='family').hidden);
            const familyDefinition=yaml.load(fs.readFileSync(path.join(root,'jamovi/glmgeneralized.a.yaml'),'utf8')).options.find(o=>o.name==='family');
            assert.equal(familyDefinition.default,'auto');
            assert.ok(familyDefinition.options.some(o=>o.name==='auto'));
            assert.ok(top.findIndex(c => c.name === 'polynomials') < top.findIndex(c => c.label === 'Model Comparison'));
            assert.ok(top.findIndex(c => c.label === 'Model Comparison') < top.findIndex(c => c.label === 'Output Options'));
            assert.ok(top.findIndex(c => c.label === 'Output Options') < top.findIndex(c => c.label === 'Model Visualization'));
            const outputControls = top.find(c => c.label === 'Output Options').controls;
            assert.equal(JSON.stringify(outputControls.map(c => c.name)),JSON.stringify(['graphic','showEstimates','showIRT','showProbabilities','comparisonGraphic','comparisonStatistics','includeP']));
            assert.equal(outputControls[0].contentLink,undefined);
            for (const cell of outputControls.slice(2,4)) {
                assert.equal(cell.contentLink,'(family:binomial || (family:auto && automaticFamily:binomial))');
                assert.equal(cell.useSingleCell,true);
            }
            const a = yaml.load(fs.readFileSync(path.join(root,'jamovi/glmgeneralized.a.yaml'),'utf8'));
            for (const name of ['showEstimates','showIRT','showProbabilities']) {
                assert.equal(a.options.find(o => o.name === name).default,true);
                ui[name].setValue(false); fire('view','updated');
                assert.equal(ui[name].value(),false,'Synchronization must not reset output preferences');
            }
            assert.equal(a.options.find(o => o.name === 'plotScale').default,'probability');
            assert.equal(a.options.find(o => o.name === 'se').title,'95% confidence intervals');
            assert.equal(a.options.find(o => o.name === 'ghostLines').title,'Show ghost lines');
            assert.equal(a.options.find(o => o.name === 'ghostLines').default,false);
            assert.equal(visualization.find(c => c.name === 'ghostLines').enable,undefined);
            assert.ok(visualization.findIndex(c => c.name === 'ghostLines') < visualization.findIndex(c => c.name === 'plotStyle'));
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
                require('./jamovi-binomial-visibility.cjs')(installed, scale, se, compilerPath);

            }
        }
        if (analysis === 'glmbasic') {
            const quadraticRecords = require('./jamovi-lm-quadratic.cjs')(ui,view,fire,assert);
            fs.writeFileSync(output+'.quadratic.json',JSON.stringify(quadraticRecords));
        }
        if ((generalized || analysis === 'glmbasic') && runtime) {
            const restored = await require('./jamovi-glm-restoration.cjs')(fs.readFileSync(runtime,'utf8'), compilerPath, view.handlers, FormatDef, utils, assert, analysis);
            if (generalized) fs.writeFileSync(output+'.polynomials.json',JSON.stringify(restored.polynomials));
            if (analysis === 'glmbasic') fs.writeFileSync(output+'.main-effects.json',JSON.stringify(restored.lmMainEffects));
            fs.writeFileSync(output+'.interactions.json',JSON.stringify(restored.interactions));
            fs.writeFileSync(output+'.restored.json',JSON.stringify(restored));
        }
        await require('./jamovi-model-ghosts.cjs')(ui,view,fire,assert,analysis);
        if(analysis==='glmbasic')await require('./jamovi-lm-manual.cjs')(ui,view,fire,assert,nodes,fs.readFileSync(runtime,'utf8'));
        fs.writeFileSync(output, records.join('\n') + '\n');
        console.log(runtime ? 'Used installed jamovi View.extend' : 'Used portable automatic-handler contract');
    } finally {
        fs.rmSync(temp, {recursive:true, force:true});
    }
})().catch(error => {console.error(error); process.exitCode = 1;});
