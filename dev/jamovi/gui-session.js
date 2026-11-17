// macOS UI automation, restricted to one uniquely named disposable document.
// Never quits jamovi or closes an unregistered document.
function run(args) {
    const events = Application('System Events');
    // Evaluate the query: a JXA collection specifier is not an array.
    const findProcesses = () => events.applicationProcesses.whose({
        bundleIdentifier: 'org.jamovi.jamovi'
    })();
    let processes = findProcesses();
    if (processes.length > 1) throw Error('Multiple jamovi instances; refusing ambiguous window management.');
    if (!processes.length && args[0] === 'close') return 'absent';
    const stem = args[1];
    const titles = [stem, stem + '.omv'];
    const matches = window => titles.some(title => [title, title + ' — jamovi',
        title + ' – jamovi', title + ' - jamovi'].includes(window.name()));
    // Bounded readiness polling, not a prompt or an interactive session.
    for (let attempt = 0; attempt < 120; attempt++) {
        processes = findProcesses();
        if (processes.length > 1) throw Error('Multiple jamovi instances; refusing window management.');
        if (!processes.length) {
            if (args[0] === 'close') return 'absent';
        } else {
            const windows = processes[0].windows(); // Requires existing macOS permission.
            const owned = windows.filter(matches);
            if (owned.length > 1) throw Error('Ambiguous test window; refusing to close it.');
            if (owned.length === 1) {
                if (args[0] === 'wait') return 'open';
                break;
            }
            if (args[0] === 'close') return 'absent';
        }
        delay(0.25);
    }
    if (args[0] === 'wait') throw Error('Timed out waiting for the smoke document; registration retained for cleanup.');
    if (args[0] !== 'close') throw Error('Unknown window-management action.');
    const process = processes[0];
    const windows = process.windows();
    const owned = windows.filter(matches);
    if (!owned.length) return 'absent';
    const before = windows.map(window => window.name());
    const close = owned[0].buttons().filter(button => {
        try { return button.attributes.byName('AXSubrole').value() === 'AXCloseButton'; }
        catch (_) { return false; }
    });
    if (close.length !== 1) throw Error('Test window has no unique close button.');
    close[0].actions.byName('AXPress').perform();
    for (let attempt = 0; attempt < 20; attempt++) {
        delay(0.25);
        const current = process.windows();
        // Discard only a sheet attached to our registered document. A generic
        // application-level dialog cannot be attributed safely: fail instead.
        const candidates = current.filter(matches).flatMap(window => window.sheets());
        for (const dialog of candidates) {
            const discard = dialog.buttons().filter(button =>
                ["Don't Save", 'Don’t Save'].includes(button.name()));
            if (discard.length === 1) discard[0].click();
        }
        const remaining = process.windows();
        if (!remaining.some(matches) &&
            !remaining.some(window => !before.includes(window.name()))) return 'closed';
    }
    throw Error('Could not confirm test document closed; registry retained for cleanup.');
}
