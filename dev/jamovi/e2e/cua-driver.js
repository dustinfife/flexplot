// Native CUA adapter: bind the existing App, never launch/move/raise a window.
function flexplotNativeDriver(app, analysis) {
 const redact=s=>s.replace(/([?&]access_key=)[^&\s]+/g,'$1<redacted>');
 const id=line=>Number(line.trim().match(/^\d+/)[0]);
 const indent=line=>line.match(/^\s*/)[0].length;
 const state=async()=>{
  const text=await app.getAXState({emit:false,disableDiffing:true});
  const lines=text.split('\n');
  const panes=lines.filter(l=>/container \w+ Options$/.test(l));
  if(analysis && (panes.length!==1 || !panes[0].endsWith(analysis+' Options')))
   throw Error('Active analysis identity is not uniquely verified');
  return {text,lines};
 };
 const block=(lines,title)=>{
  const matches=lines.map((l,i)=>l.trim().endsWith('list '+title)?i:-1).filter(i=>i>=0);
  if(matches.length!==1)throw Error('Native destination unavailable or ambiguous: '+title);
  const start=matches[0];
  let end=start+1;
  while(end<lines.length && (!lines[end].trim() || indent(lines[end])>indent(lines[start])))end++;
  return lines.slice(start,end);
 };
 const selected=(lines,title,label)=>{
  const rows=block(lines,title);
  return rows.some((l,i)=>{
   if(!/text \(selected\)$/.test(l.trim()))return false;
   let end=i+1;while(end<rows.length && indent(rows[end])>indent(l))end++;
   return rows.slice(i+1,end).some(child=>child.trim().endsWith('container '+label));
  });
 };
 return {
  async inspect(){return {source:'native_accessibility',text:redact((await state()).text)};},
  async checkbox(label,value,occurrence=0){
   const pattern='checkbox '+label+', Value: ';
   const before=(await state()).lines.filter(l=>l.includes(pattern))[occurrence];
   if(!before)return {status:'blocked',reason:'Checkbox unavailable: '+label};
   if(!before.endsWith(String(Number(value))))await app.click(id(before));
   const after=(await state()).lines.filter(l=>l.includes(pattern))[occurrence];
   return after && after.endsWith(String(Number(value)))?{status:'passed',control:label,value}:
    {status:'blocked',reason:'Intended checkbox change not observed'};
  },
  // Explicit keyboard action, not an automatic second toggle after a pending click.
  async focusedCheckbox(label,value){
   const before=await state();
   const focus=before.lines.find(l=>l.startsWith('The focused UI element is '));
   const current=before.lines.find(l=>l.includes('checkbox '+label+', Value: ') &&
    focus==='The focused UI element is '+l.trim());
   if(!current)return {status:'blocked',reason:'Exact native checkbox focus not verified; no key sent'};
   if(current.endsWith(String(Number(value))))return {status:'passed',control:label,value};
   await app.pressKey('space');
   const after=await state();
   const selected=after.lines.find(l=>/^\d+ checkbox /.test(l.trim()) &&
    l.includes('checkbox '+label+', Value: ') && id(l)===id(current));
   return selected && selected.endsWith(String(Number(value)))?{status:'passed',control:label,value}:
    {status:'blocked',reason:'Keyboard checkbox change not observed'};
  },
  async expand(label){
   const before=await state();
   if(before.lines.some(l=>l.trim().endsWith('container '+label)))return {status:'observed',accessibility:redact(before.text)};
   const item=before.lines.find(l=>l.trim().endsWith('button '+label));
   if(!item)return {status:'blocked',reason:'Section unavailable: '+label};
   await app.click(id(item));const after=await state();
   if(!after.lines.some(l=>l.trim().endsWith('container '+label)))
    return {status:'blocked',reason:'Native section expansion not observed; no coordinate fallback'};
   return {status:'observed',accessibility:redact(after.text)};
  },
  async save(){await state();await app.pressKey('super+s');return {source:'native_save_action',accessibility:redact((await state()).text)};},
  async selectRow(title,label){
   const before=await state(),rows=block(before.lines,title);
   const child=rows.findIndex(l=>l.trim().endsWith('container '+label));
   if(child<1)return {status:'blocked',reason:'Exact term absent: '+label};
   // The semantic parent row owns selection. The nested label's bounds are not reliable.
   let row=child-1;
   while(row>0 && !/\d+ text(?: \(selected\))?$/.test(rows[row].trim()))row--;
   if(!row)return {status:'blocked',reason:'Native selectable row not identified'};
   await app.click(id(rows[row]));
   const after=await state();
   return selected(after.lines,title,label)?{status:'passed',control:title,selected:label}:
    {status:'blocked',reason:'Intended row selection not observed; no removal attempted'};
  },
  async removeTerm(title,label){
   const selection=await this.selectRow(title,label);
   if(selection.status!=='passed')return selection;
   const before=await state();
   if(!selected(before.lines,title,label))return {status:'blocked',reason:'Selection changed before removal'};
   const button=before.lines.find(l=>l.trim().endsWith('button Remove '+label+' from '+title));
   if(!button)return {status:'blocked',reason:'Explicit native remove button unavailable; no key fallback'};
   await app.click(id(button));
   const after=await state();
   const remains=block(after.lines,title).some(l=>l.trim().endsWith('container '+label));
   return remains?{status:'blocked',reason:'Native removal not observed'}:
    {status:'observed',removed:label,requires_saved_option_delta:true,accessibility:redact(after.text)};
  }
 };
}
if(typeof module!=='undefined')module.exports=flexplotNativeDriver;
