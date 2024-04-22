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
        console.log(fragments);

        OutBox.innerHTML = '';
        // To have different background colors for each fragment, we need to
        // wrap each fragment in a span. The color is cycled between 8 different
        // colors, in jumps of 135 degrees to make them sufficiently far apart
        // and not repeat for 8 cycles (since 360/8 = 45, we could use any
        // multiple of 45 that's not also a multiple of 180).
        for (let i = 0; i < fragments.length; i++) {
            let color = i % 8;
            let span = document.createElement('span');
            span.textContent = fragments[i];
            span.style.lineHeight = 1.5;
            span.style.backgroundColor = `hsl(${color * 135}, 40%, 70%)`;
            span.style.whiteSpace = 'pre';
            OutBox.appendChild(span);
        }
    }
}
