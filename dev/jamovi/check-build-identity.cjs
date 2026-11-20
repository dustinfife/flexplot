const assert=require('assert/strict'),fs=require('fs'),os=require('os'),path=require('path');
const {source,payload}=require('./build-identity.cjs');
(async()=>{
 const root=fs.mkdtempSync(path.join(os.tmpdir(),'flexplot-identity-'));
 try {
  const a=path.join(root,'first'),b=path.join(root,'second');
  for(const dir of [a,b]) {
   fs.mkdirSync(path.join(dir,'R'),{recursive:true});fs.mkdirSync(path.join(dir,'jamovi'));fs.mkdirSync(path.join(dir,'dev'));
   fs.writeFileSync(path.join(dir,'DESCRIPTION'),'Package: flexplot\nVersion: 0.27.0\n');
   fs.writeFileSync(path.join(dir,'jamovi/0000.yaml'),'version: 0.27.0\n');
   fs.writeFileSync(path.join(dir,'R/model.R'),'model = function(x) x\n');
  }
  assert.equal(source(a).fingerprint,source(b).fingerprint);
  fs.writeFileSync(path.join(b,'dev/tool.R'),'different developer tooling');
  assert.equal(source(a).fingerprint,source(b).fingerprint);
  fs.writeFileSync(path.join(b,'R/model.R'),'model = function(x) x+1\n');
  assert.notEqual(source(a).fingerprint,source(b).fingerprint);
  const compiler=process.argv[2],JSZip=require(require.resolve('jszip',{paths:[compiler]}));
  const zip=new JSZip(),install=path.join(root,'installed');fs.mkdirSync(install);
  const text='version: 0.27.0\n';zip.file('flexplot/jamovi.yaml',text);
  fs.writeFileSync(path.join(install,'jamovi.yaml'),text);
  const archive=path.join(root,'module.jmo');fs.writeFileSync(archive,await zip.generateAsync({type:'nodebuffer'}));
  assert.equal((await payload(archive,install,compiler)).files.length,1);
  fs.writeFileSync(path.join(install,'jamovi.yaml'),text+'changed: true\n');
  await assert.rejects(payload(archive,install,compiler),/payload mismatch/);
  console.log('PASS: source identity is path-independent, ignores dev-only edits, detects source edits and verifies actual archived/installed bytes.');
 } finally {fs.rmSync(root,{recursive:true,force:true});}
})().catch(error=>{console.error(error);process.exitCode=1;});
