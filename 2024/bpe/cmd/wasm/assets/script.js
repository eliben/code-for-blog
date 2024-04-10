'use strict';

const TextBox = document.querySelector('#text');
TextBox.addEventListener('input', onStateChange);
const Flipx = document.querySelector("#flipx");
Flipx.addEventListener("change", onStateChange);
const Flipy = document.querySelector("#flipy");
Flipy.addEventListener("change", onStateChange);

function init() {
    // Trigger a redraw to get started.
    onStateChange();
}

//------------------

function onStateChange() {
    console.log("state changed");
    const text = TextBox.value;

    // Measure the time it takes to invoke textToBPETokens
    const start = performance.now();
    let tokens = textToBPETokens(text);
    const end = performance.now();
    console.log("Time taken (ms): ", end - start);
    console.log(tokens);
}

