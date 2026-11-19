'use strict';

// Exercise installed Option setters and synchronous notifications, not controls
// that merely assign a JavaScript variable. DOM/grid rendering is covered by GUI.
const vm = require('node:vm');
const EventEmitter = require('node:events');
module.exports = function(source, compilerPath, handlers, FormatDef, utils, assert) {
    const acorn = require(require.resolve('acorn', {paths:[compilerPath]}));
    const extract = (marker, expression) => {
        const start = source.indexOf(marker);
        assert.ok(start >= 0, 'Installed API changed: '+marker);
        const tokenizer = acorn.tokenizer(source.slice(start), {ecmaVersion:2020});
        let depth = 0, opened = false, end;
        for (;;) {
            const token = tokenizer.getToken();
            if (token.start > 30000) throw Error('Extraction too long '+marker);
            if ((token.type.label === '{' || token.type.label === '${')) { depth++; opened = true; }
            if (token.type.label === '}') depth--;
            if (opened && depth === 0) { end = start+token.end; break; }
            assert.notEqual(token.type.label, 'eof');
        }
        return expression ? source.slice(start,end).replace(/^[^(]+/, 'function') : source.slice(start,end);
    };
    // Resolve the actual installed contentLink bindings on checkbox changes.
    // Whole-builder wrapper rendering is specified by the compiled UI metadata.
    const nativeMethod = signature => vm.runInNewContext('('+extract(signature,true)+')', {
        Ps:value => value !== null && typeof value === 'object'
    });
    for (const name of ['interactions','polynomials','compare']) {
        let checked = false;
        const property = {value:null, binding:'('+name+')'};
        property.get = () => property.value;
        property.set = value => {property.value = value;};
        const cell = {visible:true,setVisibility(value){this.visible=value;}};
        const control = {properties:{contentLink:property},params:{},_cell:cell,
            getPropertyValue:nativeMethod('getPropertyValue(e){'),
            setPropertyValue:nativeMethod('setPropertyValue(e,t){'),firePropertyChangedEvent(){},
            onPropertyChanged(key) {if (key === 'contentLink') cell.setVisibility(this.getPropertyValue(key));}};
        const engine = {_resources:{[name]:{value:()=>checked},builder:control},actions:[],
            addDirectAction(key,action){this.actions.push(action);}};
        for (const signature of ['_nextNonWhiteChar(e,t){','_arrayUnique(e){','_resolveBindPart(e,t){',
            '_resolveBinding(e,t){','bindActionParams(e,t,s){','bindingsToActions(){'])
            engine[signature.slice(0,signature.indexOf('('))] = nativeMethod(signature);
        engine.bindingsToActions();
        assert.equal(cell.visible,false,'New builder starts hidden: '+name);
        for (checked of [false,true,false,true]) {
            for (const action of engine.actions) if (action.onChange.includes(name)) action.execute(engine._resources);
            assert.equal(cell.visible,checked,'Immediate builder visibility: '+name);
        }
    }
    const FormattedValue = vm.runInNewContext('('+extract('class Z{',false)+')');
    const Option = vm.runInNewContext('('+extract('class he extends rt{',false)+')', {rt:EventEmitter,Z:FormattedValue});
    utils.valuesToItems = vm.runInNewContext('('+extract('valuesToItems(e,t){',true)+')', {Z:FormattedValue,G:FormatDef});
    const beginEdit = vm.runInNewContext('('+extract('beginEdit(){',true)+')');
    const endEdit = vm.runInNewContext('('+extract('endEdit(){',true)+')');
    const view = {workspace:{}};
    const defaults = {samplePolicyVersion:0,casGhost:false,compare:false,comparisonFixed:null,comparisonTerms:null,comparisonInteractions:false,comparisonPolynomials:false,comparisonQuadratic:null,comparisonRandomIntercept:true,comparisonRandomSlopes:null,comparisonEstimation:"REML",interactions:false,polynomials:false,showCorrelations:false,plotLabels:[],plotLabelStore:[],out:null,preds:[],modelTerms:null,quadratic:null,randomSlopes:null,cluster:null,
        randomIntercept:true,estimation:'REML',includeP:false,graphic:true,predictionType:'cluster',
        clusters:3,plotX:null,plotPanels:null,plotGroup:null};
    const native = {}, packets = [], queued = new Set();
    const manager = {_beginEdit:0,beginEdit,endEdit,fireQueuedEvents() {
        if (!queued.size) return;
        packets.push({changed:[...queued],options:JSON.parse(JSON.stringify(Object.fromEntries(
            Object.entries(native).map(([name,option]) => [name,option.getValue()]))))});
        queued.clear();
    }};
    const ui = {view:{model:{options:manager}}};
    const fire = name => {
        const handler = handlers[name+'_changed'];
        if (handler) handler.call(view,ui);
    };
    for (const [name,value] of Object.entries(defaults)) {
        const option = new Option(value,{name});
        native[name] = option;
        option.on('valuechanged',() => fire(name));
        ui[name] = {value:() => option.getValue(),setValue(value) {
            manager.beginEdit();
            try { if (option.setValue(value)) queued.add(name); }
            finally { manager.endEdit(); }
        }};
    }
    manager.getOption = name => native[name];
    manager.setOptionValue = (name,value) => ui[name].setValue(value);
    for (const name of ['quadraticSupplier','randomSupplier','primarySupplier','plotSupplier','comparisonFixedSupplier','comparisonSupplier','comparisonQuadraticSupplier','comparisonRandomSupplier']) {
        let value = [];
        ui[name] = {value:() => value,setValue(updated) {
            if (JSON.stringify(value) === JSON.stringify(updated)) return;
            value = updated;
            const handler = handlers[name+'_updated'];
            if (handler) handler.call(view,ui);
        }};
    }
    // Bound native changes are real synchronous events. Every batch must close.
    handlers.view_loaded.call(view,ui);
    assert.equal(packets.length,0,'Loading a view must not send empty defaults to the server');
    handlers.view_updated.call(view,ui);
    assert.equal(ui.interactions.value(),false);
    assert.equal(ui.polynomials.value(),false);
    const record = (stage,change) => {
        const count = packets.length;
        change();
        assert.equal(manager._beginEdit,0,'Unclosed native edit: '+stage);
        assert.ok(packets.length > count,'No server option packet: '+stage);
        const options = packets.at(-1).options;
        assert.equal(packets.length,count+1,'One complete server batch: '+stage);
        records.push({stage,options,changed:packets.at(-1).changed});
        return options;
    };
    const records = [{stage:'initial',options:packets.at(-1).options,changed:packets.at(-1).changed}];
    record('outcome',() => ui.out.setValue('y'));
    record('group',() => ui.cluster.setValue('id'));
    let options = record('predictors',() => ui.preds.setValue(['x','a']));
    assert.equal(JSON.stringify(options.modelTerms),'[["x"],["a"]]');
    assert.equal(JSON.stringify(ui.primarySupplier.value().map(item => item.value.raw)), '[["x"],["a"]]');
    assert.equal(JSON.stringify(ui.randomSupplier.value().map(item => item.value.raw)), '["x","a"]');
    options = record('allow-interactions',() => ui.interactions.setValue(true));
    assert.equal(JSON.stringify(options.modelTerms),'[["x"],["a"]]');
    record('interaction',() => ui.modelTerms.setValue([['x'],['a'],['x','a']]));
    record('slope',() => ui.randomSlopes.setValue(['x']));
    record('no-random-intercept',() => ui.randomIntercept.setValue(false));
    record('ML',() => ui.estimation.setValue('ML'));
    options = record('allow-polynomials',() => ui.polynomials.setValue(true));
    assert.equal((options.quadratic || []).length,0);
    record('squared',() => ui.quadratic.setValue(['x']));
    options = record('disable-interactions',() => ui.interactions.setValue(false));
    assert.equal(JSON.stringify(options.modelTerms),'[["x"],["a"]]');
    options = record('enable-interactions',() => ui.interactions.setValue(true));
    assert.equal(JSON.stringify(options.modelTerms),'[["x"],["a"]]');
    options = record('disable-polynomials',() => ui.polynomials.setValue(false));
    assert.equal(options.quadratic.length,0);
    options = record('enable-polynomials',() => ui.polynomials.setValue(true));
    assert.equal(options.quadratic.length,0);
    options = record('remove-predictor',() => ui.preds.setValue(['a']));
    assert.equal(options.quadratic.length,0);
    assert.equal(options.randomSlopes.length,0);
    assert.equal(JSON.stringify(options.modelTerms),'[["a"]]');
    record('restore-intercept',() => ui.randomIntercept.setValue(true));
    record('change-group',() => ui.cluster.setValue('school'));
    record('remove-outcome',() => ui.out.setValue(null));
    record('restore-outcome',() => ui.out.setValue('y'));
    record('remove-group',() => ui.cluster.setValue(null));
    record('restore-group',() => ui.cluster.setValue('id'));
    // Run the installed View's data-initializing/ready lifecycle. Host options
    // arrive separately; handlers must not normalize a partially restored model.
    const marker = '_baseEvents=[{onEvent:"view.remote-data-changed"';
    const start = source.indexOf(marker);
    const end = source.indexOf(';constructor(e)',start);
    assert.ok(start >= 0 && end > start);
    const events = vm.runInNewContext('(function(){return '+source.slice(start+'_baseEvents='.length,end)+';})').call(view);
    view.flags = {loaded:true,updating:false};
    view.update = handlers.view_updated;
    const count = packets.length;
    events.find(event => event.onEvent === 'view.data-initializing').execute.call(view,ui,{id:'restored'});
    for (const [name,value] of [
        ['interactions',true],['polynomials',true],['preds',[]],['modelTerms',[['a']]],['quadratic',['x']],['randomSlopes',['x']],
        ['cluster','id'],['plotX','x'],['plotGroup','id'],['plotPanels',[]],
        ['randomIntercept',false],['preds',['x','a']]]) native[name].setValue(value);
    assert.equal(packets.length,count,'No server writes while host options are being restored');
    events.find(event => event.onEvent === 'view.ready').execute.call(view,ui,{id:'restored'});
    assert.equal(view.flags.updating,false);
    assert.equal(JSON.stringify(ui.modelTerms.value()),'[["a"]]');
    assert.equal(ui.randomIntercept.value(),false);
    assert.equal(ui.interactions.value(),true);
    assert.equal(ui.polynomials.value(),true);
    assert.equal(ui.plotGroup.value(),'id');
    assert.equal(ui.plotPanels.value().length,0);
    assert.equal(manager._beginEdit,0);
    events.find(event => event.onEvent === 'view.data-initializing').execute.call(view,ui,{id:'saved-enabled'});
    native.interactions.setValue(true); native.polynomials.setValue(true);
    native.modelTerms.setValue([['x'],['a'],['x','a']]); native.quadratic.setValue(['x']);
    events.find(event => event.onEvent === 'view.ready').execute.call(view,ui,{id:'saved-enabled'});
    assert.equal(ui.interactions.value(),true);
    assert.equal(ui.polynomials.value(),true);
    assert.equal(JSON.stringify(ui.modelTerms.value()),'[["x"],["a"],["x","a"]]');
    assert.equal(JSON.stringify(ui.quadratic.value()),'["x"]');
    events.find(event => event.onEvent === 'view.data-initializing').execute.call(view,ui,{id:'disabled'});
    native.interactions.setValue(false); native.polynomials.setValue(false);
    native.modelTerms.setValue([['x'],['a'],['x','a']]); native.quadratic.setValue(['x']);
    assert.equal(JSON.stringify(ui.quadratic.value()),'["x"]');
    events.find(event => event.onEvent === 'view.ready').execute.call(view,ui,{id:'disabled'});
    assert.equal(JSON.stringify(ui.modelTerms.value()),'[["x"],["a"]]');
    assert.equal(ui.quadratic.value().length,0);
    const savedLabels = [{key:'title-x',role:'x',variable:'x',value:'',display:'X-axis label',label:'Length (cm)'}];
    ui.plotLabels.setValue(savedLabels);
    events.find(event => event.onEvent === 'view.data-initializing').execute.call(view,ui,{id:'labels'});
    native.plotLabels.setValue(savedLabels); native.plotLabelStore.setValue(savedLabels);
    events.find(event => event.onEvent === 'view.ready').execute.call(view,ui,{id:'labels'});
    assert.equal(JSON.stringify(ui.plotLabels.value()),JSON.stringify(savedLabels));
    assert.equal(JSON.stringify(ui.plotLabelStore.value()),JSON.stringify(savedLabels));
    const primaryRecords = records.length;
    record('comparison-predictors',()=>ui.preds.setValue(['x','a','z']));
    record('primary-main-effects',()=>ui.modelTerms.setValue([['x'],['a']]));
    record('initialize-second',()=>ui.compare.setValue(true));
    assert.equal(JSON.stringify(ui.comparisonFixed.value()),'["x","a"]',
        'A new comparison initializes from Model 1, not every selected candidate predictor');
    record('initialize-second-off',()=>ui.compare.setValue(false));
    record('select-second-predictors',()=>ui.comparisonFixed.setValue(['x','z']));
    assert.equal(JSON.stringify(ui.comparisonTerms.value()),'[["x"],["z"]]');
    record('comparison-on',()=>ui.compare.setValue(true));
    assert.ok(ui.plotSupplier.value().some(item=>item.value.raw==='z'));
    const firstTerms = JSON.stringify(ui.modelTerms.value());
    options = record('second-allow-interactions',()=>ui.comparisonInteractions.setValue(true));
    assert.equal(JSON.stringify(options.comparisonTerms),'[["x"],["z"]]');
    record('second-interaction',()=>ui.comparisonTerms.setValue([['x'],['z'],['x','z']]));
    options = record('second-allow-squares',()=>ui.comparisonPolynomials.setValue(true));
    assert.equal((options.comparisonQuadratic || []).length,0);
    record('second-squared',()=>ui.comparisonQuadratic.setValue(['z']));
    record('second-slopes',()=>ui.comparisonRandomSlopes.setValue(['z']));
    record('second-estimation',()=>ui.comparisonEstimation.setValue('ML'));
    assert.equal(JSON.stringify(ui.modelTerms.value()),firstTerms,'Second model edits must not change Model 1');
    record('comparison-off',()=>ui.compare.setValue(false));
    assert.ok(!ui.plotSupplier.value().some(item=>item.value.raw==='z'));
    assert.equal(JSON.stringify(ui.comparisonTerms.value()),'[["x"],["z"],["x","z"]]');
    record('comparison-back-on',()=>ui.compare.setValue(true));
    events.find(event=>event.onEvent==='view.data-initializing').execute.call(view,ui,{id:'comparison'});
    for (const [name,value] of [['preds',[]],['comparisonFixed',null],['compare',true],['comparisonInteractions',true],
        ['comparisonPolynomials',true],['comparisonTerms',[['x'],['z'],['x','z']]],
        ['comparisonQuadratic',['z']],['comparisonRandomIntercept',false],['comparisonRandomSlopes',['z']],
        ['comparisonEstimation','ML'],['preds',['x','a','z']]]) native[name].setValue(value);
    events.find(event=>event.onEvent==='view.ready').execute.call(view,ui,{id:'comparison'});
    assert.equal(ui.compare.value(),true);
    assert.equal(ui.comparisonRandomIntercept.value(),false);
    assert.equal(ui.comparisonEstimation.value(),'ML');
    assert.equal(JSON.stringify(ui.comparisonTerms.value()),'[["x"],["z"],["x","z"]]');
    assert.equal(JSON.stringify(ui.comparisonQuadratic.value()),'["z"]');
    options = record('remove-second-predictor',()=>ui.preds.setValue(['x','a']));
    assert.equal(JSON.stringify(options.comparisonTerms),'[["x"]]');
    assert.equal(options.comparisonQuadratic.length,0);
    assert.equal(options.comparisonRandomSlopes.length,0);

    // The always-visible native variable target edits Model 2, not random slopes or Model 1.
    const unchangedFirst = JSON.stringify(ui.modelTerms.value());
    ui.comparisonFixed.setValue(['x','a']);
    ui.comparisonTerms.setValue([['x'],['a']]);
    ui.comparisonRandomSlopes.setValue(['a']);
    ui.comparisonFixed.setValue(['x']);
    assert.equal(JSON.stringify(ui.comparisonFixedSupplier.value().map(item=>item.value.raw)),'["a"]');
    const beforeDisplay = JSON.stringify(ui.comparisonTerms.value());
    handlers.comparisonFixedSupplier_updated.call(view,ui);
    assert.equal(JSON.stringify(ui.comparisonTerms.value()),beforeDisplay,'Supplier display updates must not edit model terms');
    assert.equal(JSON.stringify(ui.comparisonTerms.value()),'[["x"]]');
    assert.equal(JSON.stringify(ui.comparisonRandomSlopes.value()),'["a"]');
    ui.comparisonFixed.setValue(['x','a']);
    assert.equal(JSON.stringify(ui.comparisonTerms.value()),'[["x"],["a"]]');
    assert.equal(JSON.stringify(ui.modelTerms.value()),unchangedFirst);
    ui.comparisonInteractions.setValue(true);
    ui.comparisonTerms.setValue([['x'],['a'],['x','a']]);
    ui.comparisonPolynomials.setValue(true);
    ui.comparisonQuadratic.setValue(['x']);
    assert.equal(JSON.stringify(ui.comparisonTerms.value()),'[["x"],["a"],["x","a"]]');
    assert.equal(JSON.stringify(ui.modelTerms.value()),unchangedFirst);
    // The earlier primary-model packet stream is consumed by its existing R6 test.
    records.splice(primaryRecords);
    // Native mode changes must edit the visible targets and supplier in the
    // same server batch, without touching either fitted-model specification.
    ui.preds.setValue(['x','a','z']);
    ui.modelTerms.setValue([['x'],['a'],['z']]);
    const fitNames = ['preds','modelTerms','quadratic','cluster','randomIntercept','randomSlopes','estimation',
        'comparisonFixed','comparisonTerms','comparisonQuadratic','comparisonRandomIntercept',
        'comparisonRandomSlopes','comparisonEstimation'];
    const fitSnapshot = () => JSON.stringify(fitNames.map(name=>ui[name].value()));
    for (const comparison of [false,true]) {
        ui.compare.setValue(comparison);
        ui.predictionType.setValue('cluster');
        ui.plotX.setValue('x'); ui.plotPanels.setValue(['id','z']); ui.plotGroup.setValue('a');
        const before = fitSnapshot();
        const count = packets.length;
        ui.predictionType.setValue('cas');
        assert.equal(packets.length,count+1,'CAS selection is one atomic server update');
        assert.equal(JSON.stringify(packets.at(-1).options.plotPanels),'["z"]');
        assert.equal(ui.plotX.value(),'x');
        assert.equal(ui.plotGroup.value(),'a');
        assert.ok(!ui.plotSupplier.value().some(item=>item.value.raw==='id'));
        ui.plotPanels.setValue(['id','z']);
        assert.equal(JSON.stringify(ui.plotPanels.value()),'["z"]');
        ui.plotGroup.setValue('id'); assert.equal(ui.plotGroup.value(),null);
        ui.plotX.setValue('id'); assert.notEqual(ui.plotX.value(),'id');
        assert.equal(fitSnapshot(),before,'CAS must not edit fixed or random effects');
        for (const type of ['population','cluster']) {
            ui.predictionType.setValue(type);
            assert.ok(ui.plotSupplier.value().some(item=>item.value.raw==='id'));
            ui.plotGroup.setValue('id'); assert.equal(ui.plotGroup.value(),'id');
            ui.predictionType.setValue('cas'); assert.equal(ui.plotGroup.value(),null);
        }
        events.find(event=>event.onEvent==='view.data-initializing').execute.call(view,ui,{id:'saved-cas-'+comparison});
        native.predictionType.setValue('cas'); native.plotX.setValue('id');
        native.plotPanels.setValue(['id','z']); native.plotGroup.setValue('a');
        events.find(event=>event.onEvent==='view.ready').execute.call(view,ui,{id:'saved-cas-'+comparison});
        assert.equal(ui.cluster.value(),'id');
        assert.equal(ui.plotX.value(),'x');
        assert.equal(JSON.stringify(ui.plotPanels.value()),'["z"]');
        assert.equal(ui.plotGroup.value(),'a');
        assert.equal(fitSnapshot(),before);
        assert.ok(!ui.plotSupplier.value().some(item=>item.value.raw==='id'));
        assert.equal(manager._beginEdit,0);
    }
    ui.compare.setValue(false);
    ui.samplePolicyVersion.setValue(1);
    const beforeMigration = JSON.stringify([ui.modelTerms.value(),ui.comparisonTerms.value(),
        ui.randomSlopes.value(),ui.comparisonRandomSlopes.value()]);
    const beforePackets = packets.length;
    ui.compare.setValue(true);
    assert.equal(packets.length,beforePackets+1);
    assert.equal(ui.samplePolicyVersion.value(),2);
    assert.ok(packets.at(-1).changed.includes('samplePolicyVersion'));
    assert.equal(JSON.stringify([ui.modelTerms.value(),ui.comparisonTerms.value(),
        ui.randomSlopes.value(),ui.comparisonRandomSlopes.value()]),beforeMigration);
    const afterMigration = packets.length;
    handlers.view_updated.call(view,ui);
    assert.equal(packets.length,afterMigration,'Current calculations must not trigger a repeated migration');
    console.log('PASS: legacy comparison requests one native calculation update without changing models.');
    console.log('PASS: native CAS selection filtering, mode switching, and saved-option restoration; model specifications unchanged.');
    console.log('PASS: installed native option setters, synchronous handler re-entry and server batches; fresh and restored options.');
    return records;
};
