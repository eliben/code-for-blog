'use strict';

const TextBox = document.querySelector('#text');
TextBox.addEventListener('input', onStateChange);

const OutBox = document.querySelector('#tokens');

let radioText = document.querySelector('#showText');
let radioTokens = document.querySelector('#showTokens');
radioText.addEventListener('change', onStateChange);
radioTokens.addEventListener('change', onStateChange);

function init() {
    // Trigger a redraw to get started.
    onStateChange();
}

//------------------

function onStateChange() {
    console.log("state changed");
    const text = TextBox.value;

    if (radioTokens.checked) {
        const start = performance.now();
        let tokens = textToBPETokens(text);
        const end = performance.now();
        console.log("textToBPEToken elapsed (ms): ", end - start);
        OutBox.textContent = "[" + tokens.join(", ") + "]";
    } else {
        const start = performance.now();
        let fragments = textToBPEFragments(text);
        const end = performance.now();
        console.log("textToBPEFragments elapsed (ms): ", end - start);
        // set output text as a list of integers
        // OutBox.textContent = fragments.join('');
        OutBox.innerHTML = '';
        for (let i = 0; i < fragments.length; i++) {
            let color = i % 10;
            let span = document.createElement('span');
            span.textContent = fragments[i];
            span.style.backgroundColor = `hsl(${color * 36}, 100%, 80%)`;
            OutBox.appendChild(span);
        }
    }
}
