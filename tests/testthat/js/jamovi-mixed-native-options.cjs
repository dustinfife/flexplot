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
    const FormattedValue = vm.runInNewContext('('+extract('class Z{',false)+')');
    const Option = vm.runInNewContext('('+extract('class he extends rt{',false)+')', {rt:EventEmitter,Z:FormattedValue});
    utils.valuesToItems = vm.runInNewContext('('+extract('valuesToItems(e,t){',true)+')', {Z:FormattedValue,G:FormatDef});
    const beginEdit = vm.runInNewContext('('+extract('beginEdit(){',true)+')');
    const endEdit = vm.runInNewContext('('+extract('endEdit(){',true)+')');
    const view = {workspace:{}};
    const defaults = {out:null,preds:[],modelTerms:null,quadratic:null,randomSlopes:null,cluster:null,
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
    console.log('PASS: installed native option setters, synchronous handler re-entry and server batches; fresh and restored options.');
    return records;
};
