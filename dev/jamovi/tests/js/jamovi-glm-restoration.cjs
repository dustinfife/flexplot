'use strict';

const vm = require('node:vm');
const EventEmitter = require('node:events');

// Real installed Option setters, synchronous supplier events and View lifecycle.
// Native DOM display and persisted .omv controls are also checked in jamovi.
module.exports = function(source, compilerPath, handlers, FormatDef, utils, assert, analysis = 'glmgeneralized') {
    const acorn = require(require.resolve('acorn', {paths:[compilerPath]}));
    const extract = (marker, method = false) => {
        const start = source.indexOf(marker);
        assert.ok(start >= 0, 'Installed API changed: '+marker);
        const tokens = acorn.tokenizer(source.slice(start), {ecmaVersion:2020});
        let depth = 0, opened = false, end;
        for (;;) {
            const token = tokens.getToken();
            if (token.type.label === '{' || token.type.label === '${') { depth++; opened = true; }
            if (token.type.label === '}') depth--;
            if (opened && depth === 0) { end = start+token.end; break; }
            assert.notEqual(token.type.label, 'eof');
        }
        const value = source.slice(start,end);
        return method ? value.replace(/^[^(]+/, 'function') : value;
    };
    const FormattedValue = vm.runInNewContext('('+extract('class Z{')+')');
    const Option = vm.runInNewContext('('+extract('class he extends rt{')+')', {rt:EventEmitter,Z:FormattedValue});
    const beginEdit = vm.runInNewContext('('+extract('beginEdit(){',true)+')');
    const endEdit = vm.runInNewContext('('+extract('endEdit(){',true)+')');
    const view = {workspace:{},flags:{loaded:true,updating:false}};
    const native = {}, queued = new Set(), packets = [];
    const manager = {_beginEdit:0,beginEdit,endEdit,fireQueuedEvents() {
        if (!queued.size) return;
        packets.push([...queued]); queued.clear();
    }};
    const ui = {view:{model:{options:manager}}};
    const defaults = {polynomials:false,comparisonPolynomials:false,interactions:false,comparisonInteractions:false,mainEffects:null,comparisonMainEffects:null,interactionSpecVersion:0,out:null,preds:[],modelTerms:null,quadratic:null,compare:false,
        comparisonTerms:null,comparisonQuadratic:null,plotX:null,plotPanels:null,plotGroup:null,
        comparisonX:null,comparisonPanels:null,family:'binomial',includeP:false,graphic:true,se:true,plotStyle:'auto'};
    if (analysis === 'glmbasic') {
        delete defaults.family; delete defaults.plotStyle;
        Object.assign(defaults,{polynomials:false,comparisonPolynomials:false,graphicassump:false,estimates:true,comparisonGroup:null});
    }
    for (const [name,value] of Object.entries(defaults)) {
        const option = native[name] = new Option(value,{name});
        option.on('valuechanged',() => {
            const handler = handlers[name+'_changed'];
            if (handler) handler.call(view,ui);
        });
        ui[name] = {value:() => option.getValue(),setValue(value) {
            manager.beginEdit();
            try { if (option.setValue(value)) queued.add(name); }
            finally { manager.endEdit(); }
        }};
    }
    for (const name of ['primaryMainSupplier','comparisonMainSupplier','primarySupplier','comparisonSupplier','quadraticSupplier','comparisonQuadraticSupplier','plotSupplier','comparisonPlotSupplier']) {
        let value = [];
        ui[name] = {value:() => value,setValue(updated) {
            if (JSON.stringify(value) === JSON.stringify(updated)) return;
            value = updated;
            handlers[name+'_updated'].call(view,ui);
        }};
    }
    const snapshot = stage => {
        assert.equal(manager._beginEdit,0,'Unclosed edit transaction: '+stage);
        const options = Object.fromEntries(Object.entries(native).map(([name,option]) => [name,option.getValue()]));
        for (const name of ['modelTerms','comparisonTerms','quadratic','comparisonQuadratic'])
            assert.equal(JSON.stringify(ui[name].value()),JSON.stringify(options[name]));
        return {stage,options:JSON.parse(JSON.stringify(options))};
    };
    manager.getOption = name => native[name];
    manager.setOptionValue = (name,value) => ui[name].setValue(value);
    const records = [];
    handlers.view_loaded.call(view,ui);
    assert.equal(packets.length,0,'Fresh empty view must not write normalized defaults');
    records.push(snapshot('empty'));
    ui.out.setValue('y');ui.preds.setValue(['x','z']);
    assert.equal(JSON.stringify(ui.modelTerms.value()),'[["x"],["z"]]');
    records.push(snapshot('fresh'));
    // Installed lifecycle marks options as incomplete until view.ready.
    const marker = '_baseEvents=[{onEvent:"view.remote-data-changed"';
    const start = source.indexOf(marker), end = source.indexOf(';constructor(e)',start);
    assert.ok(start >= 0 && end > start);
    const events = vm.runInNewContext('(function(){return '+source.slice(start+'_baseEvents='.length,end)+';})').call(view);
    view.update = handlers.view_updated;
    const restore = (stage, options) => {
        view.workspace = {};
        events.find(event => event.onEvent === 'view.data-initializing').execute.call(view,ui,{id:stage});
        const count = packets.length;
        for (const [name,value] of Object.entries(defaults)) native[name].setValue(value);
        native.preds.setValue([]);
        for (const [name,value] of Object.entries(options)) native[name].setValue(value);
        handlers.view_loaded.call(view,ui);
        assert.equal(packets.length,count,'No normalization while restoring incomplete options');
        events.find(event => event.onEvent === 'view.ready').execute.call(view,ui,{id:stage});
        for (const name of ['modelTerms','comparisonTerms','quadratic','comparisonQuadratic']) {
            const expected = options[name] == null ? (name.endsWith('Terms') ? options.preds.map(name => [name]) : []) : options[name];
            assert.equal(JSON.stringify(ui[name].value()),JSON.stringify(expected),stage+': '+name);
        }
        records.push(snapshot(stage));
    };
    const prepared = {out:'y',modelTerms:[['x'],['z'],['x','z']],quadratic:['x'],
        comparisonTerms:[['z']],comparisonQuadratic:['z'],compare:true,preds:['x','z']};
    if (analysis === 'glmbasic') Object.assign(prepared,{polynomials:true,comparisonPolynomials:true});
    restore('prepared',prepared);
    // A later predictor-control update must not prune the still-loading terms.
    view.flags.updating = true;
    native.preds.setValue([]);
    handlers.primarySupplier_updated.call(view,ui);
    handlers.comparisonSupplier_updated.call(view,ui);
    assert.equal(JSON.stringify(ui.modelTerms.value()),JSON.stringify(prepared.modelTerms));
    assert.equal(JSON.stringify(ui.comparisonTerms.value()),JSON.stringify(prepared.comparisonTerms));
    native.preds.setValue(['x','z']);view.flags.updating=false;handlers.view_updated.call(view,ui);
    for (let i=0;i<2;i++) {
        ui.modelTerms.setValue([['x'],['z']]);records.push(snapshot('remove-interaction-'+i));
        ui.modelTerms.setValue(prepared.modelTerms);records.push(snapshot('add-interaction-'+i));
        ui.comparisonQuadratic.setValue([]);records.push(snapshot('remove-second-quadratic-'+i));
        ui.comparisonQuadratic.setValue(['z']);records.push(snapshot('add-second-quadratic-'+i));
        assert.equal(JSON.stringify(ui.modelTerms.value()),JSON.stringify(prepared.modelTerms));
    }
    ui.compare.setValue(false);records.push(snapshot('comparison-off'));
    ui.compare.setValue(true);records.push(snapshot('comparison-on'));
    restore('reopened',JSON.parse(JSON.stringify(snapshot('saved').options)));
    // Old analyses without explicit term/quadratic options get main effects.
    restore('legacy',{out:'y',modelTerms:null,quadratic:null,comparisonTerms:null,comparisonQuadratic:null,compare:true,preds:['x','z']});
    ui.out.setValue(null);records.push(snapshot('missing-outcome'));
    ui.out.setValue('y');ui.modelTerms.setValue([]);ui.quadratic.setValue([]);
    records.push(snapshot('empty-primary'));
    ui.modelTerms.setValue([['x']]);ui.comparisonTerms.setValue([]);records.push(snapshot('empty-secondary'));
    ui.preds.setValue([]);records.push(snapshot('remove-predictors'));
    assert.equal(JSON.stringify(ui.modelTerms.value()),'[]');
    assert.equal(JSON.stringify(ui.comparisonTerms.value()),'[]');
    console.log('PASS: GLM native restoration, independent terms/quadratics, live edits and server options.');
    const baseline = records.slice();
    baseline.interactions = require('./jamovi-interaction-controls.cjs')({ui,native,view,restore,snapshot,assert,analysis});
    if (analysis === 'glmgeneralized') baseline.polynomials = require('./jamovi-glm-polynomials.cjs')({ui,native,view,restore,snapshot,assert});
    return baseline;
};
