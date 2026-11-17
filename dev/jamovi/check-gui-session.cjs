// Exercise the actual JXA helper with mocked processes/windows. No GUI is launched.
const fs = require('fs');
const vm = require('vm');
const assert = require('assert/strict');
const source = fs.readFileSync(require('path').join(process.argv[2], 'dev/jamovi/gui-session.js'), 'utf8');
function scenario({running=true, ready=true, denied=false, duplicate=false}={}) {
    const state = {running, ready, userClosed:false, testClosed:false, polls:0};
    const user = {name:()=> 'My important analysis', buttons:()=> {state.userClosed=true;throw Error('User window touched');}};
    const test = {name:()=> 'flexplot-test-probe', buttons:()=> [{
        attributes:{byName:()=>({value:()=> 'AXCloseButton'})},
        actions:{byName:()=>({perform:()=>{state.testClosed=true;}})}
    }], sheets:()=>[]};
    const process = {windows:()=> {
        if (denied) throw Error('Automation permission denied');
        return [user].concat(state.ready && !state.testClosed ? [test] : []);
    }};
    const context = vm.createContext({Application:()=>({applicationProcesses:{whose:query=> {
        assert.equal(query.bundleIdentifier,'org.jamovi.jamovi');
        return ()=>state.running ? (duplicate ? [process,process] : [process]) : [];
    }}}),delay:()=>{state.polls++;if(!denied){state.running=true;state.ready=true;}}});
    vm.runInContext(source,context);
    return {state,run:action=>context.run([action,'flexplot-test-probe'])};
}
for (const running of [false,true]) {
    const test = scenario({running,ready:running});
    assert.equal(test.run('wait'),'open');
    assert.equal(test.state.testClosed,false);
    assert.equal(test.run('close'),'closed');
    assert.equal(test.state.userClosed,false);
}
assert.equal(scenario({running:false}).run('close'),'absent');
assert.equal(scenario({ready:false}).run('close'),'absent');
assert.throws(()=>scenario({denied:true}).run('wait'),/permission denied/);
assert.throws(()=>scenario({denied:true}).run('close'),/permission denied/);
assert.throws(()=>scenario({duplicate:true}).run('close'),/Multiple jamovi/);
console.log('PASS: actual window helper: startup readiness, running app, owned-only close, absent document, permissions, multiple instances.');
