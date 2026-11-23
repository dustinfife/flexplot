"use strict";
// Execute installed group/HTML rendering methods against real serialized results.
const fs=require("node:fs"),path=require("node:path"),vm=require("node:vm"),assert=require("node:assert/strict");
(async()=>{
 const [assets,fixtures]=process.argv.slice(2);
 const source=fs.readFileSync(path.join(assets,fs.readdirSync(assets).find(n=>/^resultsview-.*\.js$/.test(n))),"utf8");
 const extract=tag=>{
  const end=source.indexOf('customElements.define("'+tag+'"');
  const start=source.lastIndexOf('class ',end);
  assert.ok(start>=0 && end>start);
  return '('+source.slice(start,end).replace(/;\s*$/,'')+')';
 };
 class Annotation { update(){} }
 const Group=vm.runInNewContext(extract("jmv-results-group"),{
  ya:class {render(){}},It:{getControl:()=>null,activate:()=>{},create:()=>new Annotation()},
  Ic:class {},Fa:Annotation,_:text=>text});
 const Html=vm.runInNewContext(extract("jmv-results-html"),{Vt:{View:class {}}});
 const order={
  glmbasic:['waiting','specification','explanation','plot','glmcat','factorMeans','diff','rsq','comparisonplot','comparison','assumpplot'],
  glmgeneralized:['waiting','specification','explanation','plot','plotExplanation','coefficients','irt','probabilities','comparisonplot','comparisonPlotExplanation','comparison'],
  mixedmodel:['waiting','specification','comparisonSpecification','explanation','diagnostics','plot','plotExplanation','model1Heading','fixed','random','correlations','model2Heading','fixed2','random2','correlations2','comparisonHeading','comparisonplot','comparisonPlotNote','comparison']};
 for(const record of JSON.parse(fs.readFileSync(fixtures,'utf8'))){
  const annotations=new Map(), rendered=[];
  const host={model:{attributes:{title:'',element:{elements:[]},options:{}}},mode:'rich',hasTitle:false,
   address:()=>['analysis'],$container:{prepend:()=>{}},level:0,
   layout:{include:(key,create)=>{if(!annotations.has(key))annotations.set(key,create());return annotations.get(key);}},
   _includeItem:(previous,address,item)=>{rendered.push(item.name);return {item:{address:()=>address.split('/'),after:()=>{},hasAnnotations:()=>true},updated:()=>false,update:()=>true};},
   _includeBreak:previous=>previous,_includeAnnotation:Group.prototype._includeAnnotation,createElementTitle:()=>''};
  // Old cached order and annotation content are independent of the new position.
  host.model.attributes.element.elements=record.elements.filter(e=>e.name!=='specification').slice().reverse();
  Group.prototype.render.call(host);await host.ready;
  for(const [key,value] of annotations)value.text='annotation for '+key;
  const old=new Map(annotations);rendered.length=0;
  host.model.attributes.element.elements=record.elements;
  Group.prototype.render.call(host);await host.ready;
  const visible=e=>e.visible!==1 && e.visible!==3;
  assert.deepEqual(rendered,order[record.name].filter(name=>record.elements.some(e=>e.name===name && visible(e))));
  for(const [key,value] of old){assert.equal(annotations.get(key),value);assert.equal(value.text,'annotation for '+key);}
  const spec=record.elements.find(e=>visible(e) && (e.name==='specification'||e.name==='comparisonSpecification'));
  assert.ok(spec,'A fitted specification remains visible when plots are hidden');
  const node={innerHTML:'',querySelectorAll:()=>[]};
  const htmlHost={model:{attributes:{element:{content:spec.content}}},$head:{querySelector:()=>null},
   promises:[],ready:Promise.resolve(),querySelector:()=>node,querySelectorAll:()=>[]};
  Html.prototype.render.call(htmlHost);await htmlHost.ready;
  assert.ok(node.innerHTML.includes('Model'));
  if(record.compare)assert.ok(node.innerHTML.includes('Model 2'));
  if(record.name==='glmgeneralized'){assert.ok(node.innerHTML.includes('Family: binomial'));assert.ok(node.innerHTML.includes('Link: logit'));}
 }
 console.log('PASS: installed native ordering, specifications and annotation addresses across 12 real result states.');
})().catch(error=>{console.error(error);process.exitCode=1;});
