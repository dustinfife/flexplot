// Validate and bundle in a temporary directory; never regenerate working-tree R files.
import fs from 'node:fs';
import path from 'node:path';
import {createRequire} from 'node:module';
const [root, lib, out] = process.argv.slice(2);
const require = createRequire(path.join(lib, 'index.js'));
const yaml = require('js-yaml'), browserify = require('browserify');
const {default:compile} = await import(path.join(lib,'compiler.js'));
const {default:compileUI} = await import(path.join(lib,'uicompiler.js'));
fs.mkdirSync(out,{recursive:true});
for (const file of fs.readdirSync(path.join(root,'jamovi/js')))
    fs.copyFileSync(path.join(root,'jamovi/js',file),path.join(out,file));
const refs = yaml.load(fs.readFileSync(path.join(root,'jamovi/00refs.yaml'),'utf8')).refs;
for (const {name} of yaml.load(fs.readFileSync(path.join(root,'jamovi/0000.yaml'),'utf8')).analyses) {
    for (const suffix of ['a.yaml','r.yaml','u.yaml'])
        fs.copyFileSync(path.join(root,'jamovi',name+'.'+suffix),path.join(out,name+'.'+suffix));
    const results = yaml.load(fs.readFileSync(path.join(out,name+'.r.yaml'),'utf8'));
    for (const item of [results,...results.items])
        for (const ref of item.refs || []) if (!refs[ref]) throw Error('Missing citation '+ref);
    compile('flexplot',path.join(out,name+'.a.yaml'),path.join(out,name+'.r.yaml'),
        path.join(lib,'header.template'),path.join(out,name+'.h.R'));
    compileUI(path.join(out,name+'.a.yaml'),path.join(out,name+'.u.yaml'),path.join(out,name+'.js'),
        name,path.join(lib,'src.template'),path.join(out,name+'.src.js'));
    const bundle = await new Promise((resolve,reject) => browserify(path.join(out,name+'.src.js'),
        {standalone:'module'}).bundle((error,data) => error ? reject(error) : resolve(data)));
    fs.writeFileSync(path.join(out,name+'.bundle.js'),bundle);
}
console.log('PASS: definitions, citations, generated headers, and bundled UI scripts.');
