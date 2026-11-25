'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const vm = require('node:vm');
const {pathToFileURL} = require('node:url');
(async () => {
    const [root, compiler, runtime] = process.argv.slice(2);
    const work = fs.mkdtempSync(path.join(os.tmpdir(), 'flexplot-controls-'));
    try {
        for (const suffix of ['a.yaml','r.yaml','u.yaml'])
            fs.copyFileSync(path.join(root,'jamovi','flexplota.'+suffix),path.join(work,'flexplota.'+suffix));
        fs.copyFileSync(path.join(root,'jamovi/js/flexplota.events.js'),path.join(work,'flexplota.events.js'));
        const {default:compile} = await import(pathToFileURL(path.join(compiler,'uicompiler.js')));
        compile(path.join(work,'flexplota.a.yaml'),path.join(work,'flexplota.u.yaml'),
            path.join(work,'flexplota.js'),'flexplota',path.join(compiler,'src.template'),path.join(work,'flexplota.src.js'));
        // Use this host's actual View.extend automatic-handler implementation.
        const source = fs.readFileSync(runtime,'utf8');
        const marker = source.indexOf('this.handlers===void 0&&Object.assign(this,le)');
        assert.ok(marker > 0);
        const start = source.lastIndexOf('static extend(',marker)+'static '.length;
        const brace = source.indexOf('{',start);
        let depth=1,end=brace+1;
        while(depth) {if(source[end]==='{')depth++;if(source[end]==='}')depth--;end++;}
        const extend = vm.runInNewContext('('+source.slice(start,end).replace(/^extend/,'function')+')',{le:{}});
        const sandbox = vm.createContext({console,module:{exports:{}},exports:{},FormatDef:{},
            ui:{extend:x=>x},DefaultControls:new Proxy({},{get:(_,name)=>name}),View:{extend}});
        const browserify = require(require.resolve('browserify',{paths:[compiler]}));
        const bundle = await new Promise((resolve,reject)=>browserify(path.join(work,'flexplota.src.js'),
            {standalone:'module'}).bundle((error,data)=>error?reject(error):resolve(data)));
        vm.runInContext(bundle.toString(),sandbox);
        const compiled=sandbox.module.exports,view={_baseEvents:[],workspace:{}};compiled.view.call(view);
        const flatten=items=>items.flatMap(item=>[item,...flatten(item.controls||[])]);
        const controls=flatten(compiled.view.layout.controls);
        assert.ok(!controls.some(c=>c.name==='diff'),'Related-t is absent from compiled interface');
        assert.ok(compiled.options.some(o=>o.name==='diff'&&o.default===false),'Legacy option remains declared');
        const method=compiled.options.find(o=>o.name==='plmethod');
        assert.equal(method.default,'Jittered-density plot');
        assert.equal(method.options[0].name,'Jittered-density plot');
        assert.equal(method.options[0].title,'Beeswarm plot');
        const yaml=require(require.resolve('js-yaml',{paths:[compiler]}));
        const results=yaml.load(fs.readFileSync(path.join(root,'jamovi/flexplota.r.yaml'),'utf8'));
        assert.deepEqual(results.refs,['flexplot','graphicalAnalysis']);
        assert.ok(results.items.every(item=>!item.refs),'Citations stay at analysis level, as in 6a57fc36');
        const control=value=>({value:()=>value,setValue:v=>{value=v;},properties:{},
            setPropertyValue(name,v){this.properties[name]=v;}});
        let edits=0;
        const ui={view:{model:{options:{beginEdit(){edits++;},endEdit(){edits--;}}}}};
        for(const c of controls)if(c.name)ui[c.name]=control(null);
        ui.preds.setValue([]);ui.given.setValue([]);ui.bins.setValue(5);ui.suppr.setValue(true);ui.ghost.setValue(true);
        const columns={x:{dataType:'decimal',measureType:'continuous'},z:{dataType:'integer',measureType:'continuous'},
            a:{dataType:'text',measureType:'nominal'},b:{dataType:'integer',measureType:'ordinal'}};
        view.requestData=(_,q)=>Promise.resolve({columnFound:!!columns[q.columnName],...columns[q.columnName]});
        const settle=()=>new Promise(resolve=>setImmediate(resolve));
        const fire=name=>controls.find(c=>c.name===name).events[0].execute.call(view,ui);
        const check=async(preds,panels,enabled)=>{
            ui.preds.setValue(preds);ui.given.setValue(panels);fire('preds');await settle();
            assert.equal(ui.bins.properties.enable,enabled,JSON.stringify({preds,panels}));
            assert.equal(ui.suppr.properties.enable,true);assert.equal(ui.suppr.value(),true);
            assert.equal(ui.bins.value(),5);assert.equal(ui.ghost.value(),true);assert.equal(edits,0);
        };
        await check([],[],false);await check(['x'],[],false);await check(['x','a'],[],false);
        await check(['a','x'],[],true);await check(['a','b'],['a'],false);
        await check(['a'],['z'],true);await check(['a'],['a','z'],true);
        ui.given.setValue(['a']);fire('given');await settle();assert.equal(ui.bins.properties.enable,false);
        await check(['x','z'],[],true);await check(['z','x'],[],true);
        columns.x.measureType='nominal';view.remoteDataChanged(ui);await settle();assert.equal(ui.bins.properties.enable,false);
        columns.x.measureType='continuous';view.remoteDataChanged(ui);await settle();assert.equal(ui.bins.properties.enable,true);
        ui.preds.setValue(['x']);view.update(ui);await settle();assert.equal(ui.bins.properties.enable,false);
        await check(['a'],['missing'],false);
        for(const preference of ['Median + quartiles','Mean + sterr','Mean + stdev'])for(const ghost of [false,true]) {
            ui.center.setValue(preference);ui.ghost.setValue(ghost);
            ui.preds.setValue(['a']);ui.given.setValue(['a']);ui.suppr.setValue(false);fire('suppr');await settle();
            assert.equal(ui.center.properties.enable,true);assert.equal(ui.ghost.properties.enable,true);
            ui.suppr.setValue(true);fire('suppr');
            assert.equal(ui.center.properties.enable,false,'Center disables immediately');
            assert.equal(ui.ghost.properties.enable,false,'Ghost control disables immediately');await settle();
            ui.preds.setValue(['x']);fire('preds');await settle();
            assert.equal(ui.center.properties.enable,false);assert.equal(ui.ghost.properties.enable,false);
            ui.suppr.setValue(false);fire('suppr');await settle();
            assert.equal(ui.center.properties.enable,false,'Continuous predictor keeps existing center restriction');
            assert.equal(ui.ghost.properties.enable,true);
            ui.preds.setValue(['a']);fire('preds');await settle();assert.equal(ui.center.properties.enable,true);
            ui.given.setValue([]);fire('given');await settle();assert.equal(ui.ghost.properties.enable,false);
            assert.equal(ui.center.value(),preference);assert.equal(ui.ghost.value(),ghost);assert.equal(edits,0);
        }
        ui.suppr.setValue(true);ui.ghost.setValue(true);
        const replies=[];view.requestData=(_,q)=>new Promise(resolve=>replies.push(()=>resolve({columnFound:true,...columns[q.columnName]})));
        ui.preds.setValue(['a','x']);fire('preds');ui.preds.setValue(['x','a']);fire('preds');
        replies.reverse().forEach(reply=>reply());await settle();assert.equal(ui.bins.properties.enable,false,'Old eligible reply must not win');
        const pending=[];view.requestData=(_,q)=>new Promise(resolve=>pending.push(()=>resolve({columnFound:true,...columns[q.columnName]})));
        ui.preds.setValue(['a']);ui.given.setValue(['a']);ui.suppr.setValue(false);fire('suppr');
        ui.suppr.setValue(true);fire('suppr');pending.reverse().forEach(reply=>reply());await settle();
        assert.equal(ui.center.properties.enable,false,'Old metadata reply cannot re-enable suppressed center');
        assert.equal(ui.ghost.properties.enable,false);assert.equal(ui.ghost.value(),true);
        view.requestData=()=>Promise.reject(new Error('column unavailable'));view.remoteDataChanged(ui);await settle();
        assert.equal(ui.bins.properties.enable,false);assert.equal(ui.suppr.properties.enable,true);assert.equal(edits,0);
        console.log('PASS: compiled ordinary Flexplot controls, metadata changes, ordering, stale replies, saved preferences and citation declarations');
    } finally {fs.rmSync(work,{recursive:true,force:true});}
})().catch(error=>{console.error(error);process.exitCode=1;});
