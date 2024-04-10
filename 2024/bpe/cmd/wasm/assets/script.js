'use strict';

const TextBox = document.querySelector('#text');
TextBox.addEventListener('input', onStateChange);
const Flipx = document.querySelector("#flipx");
Flipx.addEventListener("change", onStateChange);
const Flipy = document.querySelector("#flipy");
Flipy.addEventListener("change", onStateChange);

// Trigger a redraw to get started.
onStateChange();

//------------------

function onStateChange() {
    console.log("state changed");
}
