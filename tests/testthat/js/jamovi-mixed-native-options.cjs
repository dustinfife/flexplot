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
    for (const name of ['interactions','polynomials']) {
        let checked = true;
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
    const defaults = {interactions:true,polynomials:true,showCorrelations:false,plotLabels:[],plotLabelStore:[],out:null,preds:[],modelTerms:null,quadratic:null,randomSlopes:null,cluster:null,
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
    for (const name of ['quadraticSupplier','randomSupplier','primarySupplier','plotSupplier']) {
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
    record('interaction',() => ui.modelTerms.setValue([['x'],['a'],['x','a']]));
    record('slope',() => ui.randomSlopes.setValue(['x']));
    record('no-random-intercept',() => ui.randomIntercept.setValue(false));
    record('ML',() => ui.estimation.setValue('ML'));
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
        ['preds',[]],['modelTerms',[['a']]],['quadratic',['x']],['randomSlopes',['x']],
        ['cluster','id'],['plotX','x'],['plotGroup','id'],['plotPanels',[]],
        ['randomIntercept',false],['preds',['x','a']]]) native[name].setValue(value);
    assert.equal(packets.length,count,'No server writes while host options are being restored');
    events.find(event => event.onEvent === 'view.ready').execute.call(view,ui,{id:'restored'});
    assert.equal(view.flags.updating,false);
    assert.equal(JSON.stringify(ui.modelTerms.value()),'[["a"]]');
    assert.equal(ui.randomIntercept.value(),false);
    assert.equal(ui.plotGroup.value(),'id');
    assert.equal(ui.plotPanels.value().length,0);
    assert.equal(manager._beginEdit,0);
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
    console.log('PASS: installed native option setters, synchronous handler re-entry and server batches; fresh and restored options.');
    return records;
};
