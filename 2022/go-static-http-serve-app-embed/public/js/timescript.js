let outputBox = document.querySelector('#output');

window.addEventListener('DOMContentLoaded', (event) => {
  outputBox.innerHTML = "initializing...";
  tick();
  setInterval(tick, 1000);
});

function tick() {
  fetch('/time')
    .then((response) => {
      if (!response.ok) {
        throw new Error("error response");
      }
      return response.text();
    })
    .then((text) => {
      outputBox.innerHTML = text;
    })
    .catch((error) => {
      outputBox.innerHTML = "network error";
    });
}
