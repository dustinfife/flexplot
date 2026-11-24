'use strict';
// Run installed jamovi rendering and binding methods with small DOM/grid stand-ins.
// In particular, do not supply _cell: the real renderer must create and attach it.
const assert = require('node:assert/strict');
const vm = require('node:vm');
module.exports = function(source, definition, seDefinition, compilerPath) {
    const acorn = require(require.resolve('acorn', {paths:[compilerPath]}));
    const method = (signature, code = source) => {
        const start = code.indexOf(signature);
        assert.ok(start >= 0, 'Installed UI method missing: ' + signature);
        const text = code.slice(start);
        const tokens = acorn.tokenizer(text, {ecmaVersion:2020});
        let depth = 0, begun = false, token;
        do {
            token = tokens.getToken();
            if (token.type.label === '{' || token.type.label === '${') { depth++; begun = true; }
            if (token.type.label === '}') depth--;
            assert.notEqual(token.type.label, 'eof');
        } while (!begun || depth !== 0);
        const body = text.slice(0, token.end);
        return vm.runInNewContext('(function' + body.slice(body.indexOf('(')) + ')', globals);
    };
    class Element {
        constructor() { this.classList = {add(){}}; this.listeners = {}; }
        addEventListener(name, handler) { this.listeners[name] = handler; }
    }
    class Grid extends Element {
        constructor() { super(); this.cells = []; }
        addCell(row, col, control) {
            const cell = {content:control.el || control, visible:true, setAlignment(){},
                setDimensionMinMax(){}, setVisibility(value){ this.visible = value; }};
            this.cells.push(cell);
            return cell;
        }
    }
    let id = 0;
    const globals = {be:Grid, ci:() => false, ee:{None:'none'},
        M:{getNextAriaElementId:() => 'test-' + (++id)},
        A:{parse:() => new Element()}, ne:value => value,
        Ps:value => value !== null && typeof value === 'object', console};
    const comboName = source.match(/ComboBox:([\w$]+)/)[1];
    const comboStart = source.indexOf('class ' + comboName + ' extends');
    const combo = source.slice(comboStart, source.indexOf('class ', comboStart + 6));
    const render = method('renderToGrid(e,t,s,r){let a=this.getSpans()');
    const apply = method('_applyCellProperties(e){');
    const onRender = method('onRenderToGrid(e,t,s,r){', combo);
    const getProperty = method('getPropertyValue(e){');
    const setProperty = method('setPropertyValue(e,t){');
    const getOptions = method('getOptionsProperty(){', combo);
    const updateDisplay = method('updateDisplayValue(){', combo);
    const usesSingleCell = method('usesSingleCell(){');
    const changeBranch = 'e==="contentLink"&&this._cell&&this._cell.setVisibility(this.getPropertyValue("contentLink"),!0)';
    assert.ok(source.includes(changeBranch));
    const changed = vm.runInNewContext('(function(e){' + changeBranch + '})');
    const create = wrapped => {
        let selection = /Scale$/.test(definition.name) ? 'probability' : 'raw';
        const values = {useSingleCell:wrapped, contentLink:null, margin:'normal',
            label:'Binomial visualization', minWidth:-1, maxWidth:-1, minHeight:-1, maxHeight:-1,
            options:/Scale$/.test(definition.name) ? [{name:'probability',title:'Probability'},{name:'logit',title:'Logit'}] : [{name:'auto',title:'Automatic'},{name:'raw',title:'Raw observations'}]};
        const properties = {};
        for (const [name, value] of Object.entries(values)) {
            const property = {value};
            property.get = () => property.value;
            property.set = value => {property.value = value;};
            properties[name] = property;
        }
        properties.contentLink.binding = definition.contentLink;
        return {properties, params:{}, getPropertyValue:getProperty, setPropertyValue:setProperty,
            firePropertyChangedEvent(){}, onPropertyChanged:changed,
            hasProperty:name => name === 'margin', isPropertyDefined:name => name === 'contentLink',
            getSpans:() => ({rows:1,columns:1}), usesSingleCell,
            _applyCellProperties:apply, onRenderToGrid:onRender, getOptionsProperty:getOptions,
            updateDisplayValue:updateDisplay, setRootElement(value){this.el = value;},
            translate:value => value, getSourceValue:() => selection, setValue:value => {selection = value;}};
    };
    if (['(interactions)','(comparisonInteractions)','(compare)','(!compare)','(polynomials)','(comparisonPolynomials)','(modifyPlotLabels)','(modifyComparisonPlotLabels)'].includes(definition.contentLink)) {
        assert.equal(definition.useSingleCell,true);
        const inverted = definition.contentLink === '(!compare)';
        const name = inverted ? 'compare' : definition.contentLink.slice(1,-1);
        let enabled = false;
        const control = create(true);
        const grid = new Grid(); render.call(control,grid,0,0,{});
        if(definition.name==='includeP')control.setValue(true);
        if (definition.name === 'comparisonGroupingLayout') {
            for (const available of [false,true,false,true]) {
                control.setPropertyValue('contentLink',available);
                assert.equal(control._cell.visible,available,'Server capability must hide/show the entire native target');
            }
        }
        const engine = {_resources:{interactionBuilder:control,[name]:{value:()=>enabled}},actions:[],
            addDirectAction(name,action){this.actions.push(action);}};
        for (const signature of ['_nextNonWhiteChar(e,t){','_arrayUnique(e){','_resolveBindPart(e,t){',
            '_resolveBinding(e,t){','bindActionParams(e,t,s){','bindingsToActions(){'])
            engine[signature.slice(0,signature.indexOf('('))] = method(signature);
        engine.bindingsToActions();
        for (enabled of [false,true,false,true]) {
            for (const action of engine.actions) action.execute(engine._resources);
            assert.equal(control._cell.visible,inverted ? !enabled : enabled);
            if(definition.name==='includeP')assert.equal(control.getSourceValue(),true,'Comparison toggles retain p-value preference');
        }
        return;
    }
    if (['(compare && comparisonMagnet)','(!compare || predictionType:cas)'].includes(definition.contentLink)) {
        const state={compare:false,comparisonMagnet:false,predictionType:'cluster'};
        const control=create(true),grid=new Grid();render.call(control,grid,0,0,{});
        const engine={_resources:{comparisonGrouping:control,...Object.fromEntries(Object.keys(state).map(name=>[name,{value:()=>state[name]}]))},actions:[],
            addDirectAction(name,action){this.actions.push(action);}};
        for (const signature of ['_nextNonWhiteChar(e,t){','_arrayUnique(e){','_resolveBindPart(e,t){',
            '_resolveBinding(e,t){','bindActionParams(e,t,s){','bindingsToActions(){'])
            engine[signature.slice(0,signature.indexOf('('))]=method(signature);
        engine.bindingsToActions();
        for (const compare of [false,true]) for (const magnet of [false,true]) for (const type of ['population','cluster','cas']) {
            Object.assign(state,{compare,comparisonMagnet:magnet,predictionType:type});
            for (const action of engine.actions) action.execute(engine._resources);
            assert.equal(control._cell.visible,definition.contentLink.includes('comparisonMagnet') ? compare && magnet : !compare || type==='cas');
        }
        return;
    }
    if (definition.name === 'casGhost') {
        const state = {compare:false,predictionType:'cluster'};
        const control = create(true);
        const grid = new Grid(); render.call(control,grid,0,0,{});
        const engine = {_resources:{casGhost:control,
            compare:{value:()=>state.compare},predictionType:{value:()=>state.predictionType}},actions:[],
            addDirectAction(name,action){this.actions.push(action);}};
        for (const signature of ['_nextNonWhiteChar(e,t){','_arrayUnique(e){','_resolveBindPart(e,t){',
            '_resolveBinding(e,t){','bindActionParams(e,t,s){','bindingsToActions(){'])
            engine[signature.slice(0,signature.indexOf('('))] = method(signature);
        engine.bindingsToActions();
        control.setValue(true);
        for (const compare of [false,true,false,true]) for (const type of ['cas','population','cluster','cas']) {
            state.compare=compare;state.predictionType=type;
            for (const action of engine.actions) action.execute(engine._resources);
            assert.equal(control._cell.visible,compare && type==='cas');
            assert.equal(control.getSourceValue(),true);
        }
        console.log('Installed native visibility bindings restrict CAS ghosts to CAS comparisons and retain the saved choice.');
        return;
    }
    let family = 'gaussian';
    const attach = control => {
        const engine = {_resources:{family:{value:() => family},[definition.name]:control}, actions:[],
            addDirectAction(name, action){ this.actions.push(action); }};
        for (const signature of ['_nextNonWhiteChar(e,t){','_arrayUnique(e){','_resolveBindPart(e,t){',
            '_resolveBinding(e,t){','bindActionParams(e,t,s){','bindingsToActions(){']) {
            engine[signature.slice(0,signature.indexOf('('))] = method(signature);
        }
        engine.bindingsToActions();
        assert.ok(engine.actions.some(action => action.onChange.includes('family')));
        return engine;
    };
    // Confirm the old layout fails even though the native expression resolves false.
    const old = create(false);
    const oldGrid = new Grid(); render.call(old, oldGrid, 0, 0, {});
    attach(old);
    assert.equal(old.getPropertyValue('contentLink'), false);
    assert.equal(old._cell, undefined);
    assert.ok(oldGrid.cells.every(cell => cell.visible));

    assert.equal(definition.useSingleCell, true, 'Conditional ComboBox needs a containing cell');
    const control = create(definition.useSingleCell);
    const grid = new Grid(); render.call(control, grid, 0, 0, {});
    const engine = attach(control);
    assert.ok(control._cell, 'Native renderer did not attach its visibility cell');
    assert.equal(grid.cells.length, 1);
    assert.equal(control._cell.content.cells.length, 2, 'Label and dropdown must share the wrapper');
    assert.equal(control._cell.visible, false);
    for (const [selection, index] of (/Scale$/.test(definition.name) ? [['probability',0],['logit',1]] : [['auto',0],['raw',1]])) {
        control.setValue(selection);
        control.updateDisplayValue();
        for (family of ['binomial','gaussian','poisson','gammaLog','Gamma','negativebinomial','binomial']) {
            for (const action of engine.actions) if (action.onChange.includes('family')) action.execute(engine._resources);
            assert.equal(control._cell.visible, family === 'binomial', family);
            assert.equal(control.getSourceValue(), selection, 'Hiding the dropdown changed the saved choice');
            assert.equal(control.input.selectedIndex, index);
        }
    }
    assert.ok(seDefinition && seDefinition.contentLink === undefined && seDefinition.enable === undefined,
        'Confidence intervals must not depend on the distribution');
    console.log('Installed renderer and binding handlers hide/show the wrapped binomial dropdown; selection is retained.');
};
