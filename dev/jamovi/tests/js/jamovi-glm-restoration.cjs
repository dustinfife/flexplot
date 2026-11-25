'use strict';

const vm = require('node:vm');
const EventEmitter = require('node:events');

// Real installed Option setters, synchronous supplier events and View lifecycle.
// Native DOM display and persisted .omv controls are also checked in jamovi.
module.exports = async function(source, compilerPath, handlers, FormatDef, utils, assert, analysis = 'glmgeneralized') {
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
    const defaults = {ghostLines:false,plotLabelEditorVersion:0,modifyPlotLabels:false,modifyComparisonPlotLabels:false,outputOptionsVersion:0,comparisonGraphic:true,comparisonStatistics:true,plotLabels:[],plotLabelStore:[],comparisonPlotLabels:[],comparisonPlotLabelStore:[],polynomials:false,comparisonPolynomials:false,interactions:false,comparisonInteractions:false,mainEffects:null,comparisonMainEffects:null,interactionSpecVersion:0,out:null,preds:[],modelTerms:null,quadratic:null,compare:false,
        comparisonTerms:null,comparisonQuadratic:null,plotX:null,plotPanels:null,plotGroup:null,
        comparisonX:null,comparisonPanels:null,family:'binomial',includeP:false,graphic:true,se:true,plotStyle:'auto',plotScale:'probability'};
    if (analysis === 'glmbasic') {
        delete defaults.family; delete defaults.plotStyle; delete defaults.plotScale;
        Object.assign(defaults,{polynomials:false,comparisonPolynomials:false,graphicassump:false,estimates:true,comparisonGroup:null,comparisonMagnet:false});
    }
    if (analysis === 'glmgeneralized') Object.assign(defaults,{familySelection:'gaussian',automaticFamily:'gaussian',automaticFamilyOutcome:'',ghostLines:false,comparisonSe:true,comparisonPlotStyle:'auto',comparisonPlotScale:'probability'});
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
    for (const name of ['primaryMainSupplier','comparisonMainSupplier','primarySupplier','comparisonSupplier','quadraticSupplier','comparisonQuadraticSupplier','plotSupplier','comparisonPlotSupplier','comparisonGroupingLayout']) {
        let value = [];
        ui[name] = {value:() => value,setValue(updated) {
            if (JSON.stringify(value) === JSON.stringify(updated)) return;
            value = updated;
            if (handlers[name+'_updated']) handlers[name+'_updated'].call(view,ui);
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
    assert.deepEqual(packets,[['outputOptionsVersion'],['plotLabelEditorVersion']],'Fresh view only initializes its presentation revisions');
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

    const labelRows = [{key:'saved:key',role:'title',variable:'plot',value:'',display:'Plot title',label:'Saved title'}];
    const labelOptions = Object.assign({},snapshot('before-label-edit').options,prepared,
        {interactions:true,comparisonInteractions:false,polynomials:true,comparisonPolynomials:true,
            mainEffects:['x','z'],comparisonMainEffects:['z']});
    for (const name of ['plotLabels','plotLabelStore','comparisonPlotLabels','comparisonPlotLabelStore'])
        labelOptions[name] = labelRows;
    restore('label-restoration',JSON.parse(JSON.stringify(labelOptions)));
    for (const name of ['plotLabels','plotLabelStore','comparisonPlotLabels','comparisonPlotLabelStore'])
        assert.equal(JSON.stringify(ui[name].value()),JSON.stringify(labelRows));
    ui.plotLabels.setValue([]);
    assert.equal(JSON.stringify(ui.comparisonPlotLabels.value()),JSON.stringify(labelRows));
    const baseline = records.slice();
    if (analysis === 'glmgeneralized') {
        restore('glm20-colour-role',{out:'y',preds:['x','z'],modelTerms:[['x'],['z']],
            plotX:'x',plotPanels:[],plotGroup:'z'});
        ui.modelTerms.setValue([]);ui.quadratic.setValue([]);
        assert.equal(ui.plotX.value(),null,'Empty fitted terms clear stale x');
        assert.equal(ui.plotGroup.value(),null,'Empty fitted terms clear stale colour');
        ui.modelTerms.setValue([['z']]);
        assert.equal(ui.plotX.value(),'z','A restored term supplies the new automatic x');
        assert.equal(ui.plotGroup.value(),null,'A stale colour is not resurrected');
        console.log('PASS: GLM-20 native empty-term role cleanup and recovery.');
    }
    baseline.interactions = require('./jamovi-interaction-controls.cjs')({ui,native,view,restore,snapshot,assert,analysis});
    if (analysis === 'glmgeneralized') baseline.polynomials = require('./jamovi-glm-polynomials.cjs')({ui,native,view,restore,snapshot,assert});
    if (analysis === 'glmgeneralized') await require('./jamovi-glm-visualization.cjs')({ui,view,handlers,restore,snapshot,assert,extract});
    if (analysis === 'glmbasic') {
        // Execute the installed control setter: cancellation must occur before
        // a native option changes, rather than repairing it after selection.
        const setter = vm.runInNewContext('('+extract('setSourceValue(e,t=[],s=!1)',true)+')');
        const transfer = (name,value,key=[],insert=false) => {
            let committed = false;
            const control = {
                emit(event,args) { assert.equal(event,'changing'); handlers[name+'_changing'].call(view,ui,args); },
                getFullKey:key=>key,runInEditScope:fn=>fn(),getOption:()=>({
                    runInEditScope:fn=>fn(),setValue(value,key) {
                        committed=true;
                        if(key.length) {const next=(ui[name].value() || []).slice();next[key[0]]=value;ui[name].setValue(next);}
                        else ui[name].setValue(value);
                    },insertValueAt(value,key) {
                        committed=true;const next=(ui[name].value() || []).slice();next.splice(key[0],0,value);ui[name].setValue(next);
                    }
                })
            };
            setter.call(control,value,key,insert);
            return committed;
        };
        restore('lm-roles',{out:'y',preds:['x','z'],plotPanels:['z']});
        assert.equal(transfer('plotGroup','x'),false,'Reserve a free automatic x predictor');
        assert.equal(ui.plotGroup.value(),null);
        assert.equal(transfer('plotX','x'),true);
        assert.equal(transfer('plotGroup','x'),false,'Reject conflicting explicit roles');
        assert.equal(transfer('plotPanels','x',[1],true),false,'Reject an incremental conflicting panel transfer');
        assert.equal(transfer('plotPanels',[]),true,'Always allow removing panels');
        assert.equal(transfer('plotGroup','z'),true,'Numeric grouping is not categorically disabled');
        restore('lm-invalid-saved-roles',{out:'y',preds:['x','z'],plotPanels:['z'],plotGroup:'x'});
        assert.equal(ui.plotGroup.value(),'x','Saved invalid layouts must reach server validation unchanged');
        assert.equal(ui.plotX.value(),null,'Automatic x remains supported');
        assert.equal(transfer('plotGroup',null),true,'Invalid saved layouts can be corrected');
        restore('lm-conflicting-saved-roles',{out:'y',preds:['x','z'],plotX:'x',plotGroup:'x'});
        assert.equal(ui.plotGroup.value(),'x','Do not silently reinterpret conflicting saved roles');
        assert.equal(transfer('plotGroup',null),true);
        restore('lm-empty-predictor-edit',{out:'y',preds:['x','z'],modelTerms:[],comparisonTerms:[['x']]});
        ui.preds.setValue(['x','z','group']);
        assert.equal(JSON.stringify(ui.modelTerms.value()),'[]','Predictor additions must not repopulate deliberately empty terms');
        assert.equal(JSON.stringify(ui.comparisonTerms.value()),'[["x"],["group"]]','The other model remains independent');
        ui.preds.setValue(['x','z']);
        assert.equal(JSON.stringify(ui.modelTerms.value()),'[]');
        assert.equal(JSON.stringify(ui.comparisonTerms.value()),'[["x"]]');
        restore('comparison-group-retained',{out:'y',preds:['x','z'],compare:true,
            modelTerms:[['x'],['z']],comparisonTerms:[['x']],comparisonX:'x',comparisonGroup:'z'});
        assert.equal(ui.comparisonGroup.value(),'z');
        ui.comparisonPanels.setValue(['z']);
        assert.equal(ui.comparisonGroup.value(),'z','LM-15 panel edits retain unsupported grouping preference');
        ui.comparisonPanels.setValue([]);
        assert.equal(transfer('comparisonGroup',null),true);
        assert.equal(transfer('comparisonGroup','z'),false);
        ui.comparisonMagnet.setValue(true);
        assert.equal(transfer('comparisonGroup','z'),true);
        ui.comparisonMagnet.setValue(true); // Simulate the confirmed server capability.
        handlers.view_updated.call(view,ui);assert.equal(ui.comparisonMagnet.value(),true,'Server capability must survive passive view updates');
        ui.comparisonX.setValue('z');assert.equal(ui.comparisonMagnet.value(),false,'A changed layout invalidates the old capability');
        assert.equal(ui.comparisonGroup.value(),'z','Never silently clear a saved group');
        ui.compare.setValue(false);ui.plotGroup.setValue('z');assert.equal(ui.plotGroup.value(),'z');
        console.log('PASS: installed native changing events reject invalid LM role transfers and preserve restoration.');
        baseline.lmMainEffects=require('./jamovi-lm-main-effects.cjs')({ui,restore,snapshot,assert});
    }
    for (const graphic of [false,true]) {
        restore('legacy-output',{out:'y',preds:['x','z'],graphic,outputOptionsVersion:0});
        assert.equal(ui.comparisonGraphic.value(),analysis==='glmbasic' || graphic);
        assert.equal(ui.outputOptionsVersion.value(),analysis==='glmgeneralized' ? 2 : 1);
        ui.comparisonGraphic.setValue(!graphic);ui.comparisonStatistics.setValue(false);
        ui.compare.setValue(false);ui.compare.setValue(true);handlers.view_updated.call(view,ui);
        assert.equal(ui.comparisonGraphic.value(),!graphic);assert.equal(ui.comparisonStatistics.value(),false);
        restore('current-output',{out:'y',preds:['x','z'],graphic,comparisonGraphic:!graphic,comparisonStatistics:false,outputOptionsVersion:1});
        assert.equal(ui.comparisonGraphic.value(),!graphic);assert.equal(ui.comparisonStatistics.value(),false);
    }

    const legacyLabel = [{key:'temporarily unavailable',role:'legendLevel',variable:'model',value:'model1 (lm)',display:'Old',label:'Preferred'}];
    restore('legacy-editor-labels',{out:'y',preds:['x','z'],plotLabelStore:legacyLabel,comparisonPlotLabelStore:legacyLabel});
    assert.equal(ui.modifyPlotLabels.value(),true);assert.equal(ui.modifyComparisonPlotLabels.value(),true);
    ui.modifyPlotLabels.setValue(false);handlers.view_updated.call(view,ui);
    assert.equal(ui.modifyPlotLabels.value(),false);assert.equal(ui.modifyComparisonPlotLabels.value(),true);
    assert.equal(ui.plotLabelStore.value()[0].label,'Preferred');
    restore('current-editor-hidden',{out:'y',preds:['x','z'],plotLabelStore:legacyLabel,comparisonPlotLabelStore:legacyLabel,
        plotLabelEditorVersion:1,modifyPlotLabels:false,modifyComparisonPlotLabels:false});
    assert.equal(ui.modifyPlotLabels.value(),false);assert.equal(ui.modifyComparisonPlotLabels.value(),false);
    ui.compare.setValue(true);ui.compare.setValue(false);
    assert.equal(ui.plotLabelStore.value()[0].label,'Preferred');
    console.log('PASS: installed option setters and complete restoration preserve versioned independent outputs.');
    return baseline;
};
