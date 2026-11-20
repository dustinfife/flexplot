const assert=require('assert/strict');
const create=require('./cua-driver.js');
(async()=>{
 let checked=false, clicks=0;
 const app={getAXState:async()=>`0 standard window owned\n  4 checkbox Test, Value: ${Number(checked)}`,
  click:async(i)=>{assert.equal(i,4);clicks++;checked=!checked;},pressKey:async()=>{}};
 const driver=create(app);
 assert.equal((await driver.checkbox('Test',true)).status,'passed');
 assert.equal(clicks,1);
 await driver.checkbox('Test',true);assert.equal(clicks,1);
 assert.equal((await driver.checkbox('Missing',true)).status,'blocked');
 app.click=async()=>{};
 assert.equal((await driver.checkbox('Test',false)).status,'blocked');
 assert.equal((await driver.inspect()).source,'native_accessibility');
 let keys=0;
 const targeting=create({getAXState:async()=> '0 standard window owned\n  1 list Terms\n    2 text\n      3 container x:z\n  4 list Other\n    5 text (selected)\n      6 container x',
   click:async()=>{},pressKey:async()=>{keys++;}});
 assert.equal((await targeting.removeTerm('Terms','x:z')).status,'blocked');
 assert.equal(keys,0);
 let chosen=false, removed=false, calls=[];
 const native={getAXState:async()=>`0 standard window owned
  1 container glmgeneralized Options
    2 list Terms
${removed?'':`      3 text${chosen?' (selected)':''}
        4 container x:z`}
    5 button Remove x:z from Terms`,
 click:async(i)=>{calls.push(i);if(i===3)chosen=true;else if(i===5){assert.equal(chosen,true);removed=true;}else throw Error('Nested label must not be clicked');},
 pressKey:async()=>{throw Error('No destructive key fallback');}};
 assert.equal((await create(native,'glmgeneralized').removeTerm('Terms','x:z')).status,'observed');
 assert.deepEqual(calls,[3,5]);
 await assert.rejects(()=>create(native,'mixedmodel').inspect(),/Active analysis identity/);
 const ambiguous=create({getAXState:async()=> '0 window\n  1 list Terms\n    2 text\n      3 container x\n  4 list Terms\n    5 text\n      6 container x',click:async()=>{throw Error('Must not click an ambiguous list');}});
 await assert.rejects(()=>ambiguous.selectRow('Terms','x'),/ambiguous/);
 const adjacent=create({getAXState:async()=> '0 window\n  1 list Terms\n    2 text (selected)\n      3 container z\n    4 text\n      5 container x',click:async()=>{}});
 assert.equal((await adjacent.selectRow('Terms','x')).status,'blocked');
 const noExpansion=create({getAXState:async()=> '0 window\n  1 button Section',click:async()=>{}});
 assert.equal((await noExpansion.expand('Section')).status,'blocked');
 let focusValue=false, spaces=0;
 const focused={getAXState:async()=>`0 standard window owned\n  4 checkbox Test, Value: ${Number(focusValue)}\nThe focused UI element is 4 checkbox Test, Value: ${Number(focusValue)}`,
  pressKey:async(key)=>{assert.equal(key,'space');spaces++;focusValue=!focusValue;}};
 assert.equal((await create(focused).focusedCheckbox('Test',true)).status,'passed');
 assert.equal(spaces,1);
 assert.equal((await create(focused).focusedCheckbox('Test',true)).status,'passed');
 assert.equal(spaces,1);
 focused.getAXState=async()=> '0 standard window owned\n  4 checkbox Test, Value: 1\nThe focused UI element is 5 text Different';
 assert.equal((await create(focused).focusedCheckbox('Test',false)).status,'blocked');
 assert.equal(spaces,1);
 let movedId=4;
 const renumbered={getAXState:async()=>`0 window\n  ${movedId} checkbox Test, Value: 0\nThe focused UI element is ${movedId} checkbox Test, Value: 0`,pressKey:async()=>{movedId=5;}};
 assert.equal((await create(renumbered).focusedCheckbox('Test',true)).status,'blocked');
 console.log('PASS: CUA adapter uses fresh state, verifies clicks, avoids redundant edits and blocks unavailable/offscreen controls.');
})().catch(error=>{console.error(error);process.exitCode=1;});
