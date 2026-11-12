'use strict';
// Verify the installed results-table renderer, not an assumed newline-to-HTML conversion.
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const assert = require('node:assert/strict');
const [assets, text] = process.argv.slice(2);
const files = fs.readdirSync(assets);
const source = fs.readFileSync(path.join(assets, files.find(name => /^highcontrast-.*\.js$/.test(name))), 'utf8');
const renderer = fs.readFileSync(path.join(assets, files.find(name => /^resultsview-.*\.js$/.test(name))), 'utf8');
assert.match(renderer, /Gp\(`<i>\$\{_\("Note"\)\}\.<\/i> \$\{on\(e\.notes\[R\]\.note\)\}`\)/);
const extract = name => {
    const start = source.indexOf('function ' + name + '(');
    assert.ok(start >= 0);
    let end = source.indexOf('{', start) + 1, depth = 1;
    while (depth) {
        if (source[end] === '{') depth++;
        if (source[end] === '}') depth--;
        end++;
    }
    return source.slice(start, end);
};
const code = extract('V') + '\n' + extract('$e') + '\n({escape:V, paragraphs:$e})';
const functions = vm.runInNewContext(code);
const html = functions.paragraphs('<i>Note.</i> ' + functions.escape(fs.readFileSync(text, 'utf8').trim()));
assert.match(html, /^<p><i>Note\.<\/i> Model 1:.*<\/p><p>Model 2:.*<\/p>$/);
assert.equal((html.match(/<p>/g) || []).length, 2);
console.log('Installed jamovi renderer produced separate Model 1 and Model 2 paragraphs.');
