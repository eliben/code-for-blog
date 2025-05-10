(() => {
  let canvas, ctx;
  let strokeColor = 'black';
  let strokeWeight = 1;

  window.mouseX = 0;
  window.mouseY = 0;

  window.createCanvas = (w, h) => {
    canvas = document.createElement('canvas');
    window.width = canvas.width = w;
    window.height = canvas.height = h;
    ctx = canvas.getContext('2d');
    document.body.appendChild(canvas);

    canvas.addEventListener('mousemove', e => {
      const r = canvas.getBoundingClientRect();
      mouseX = window.mouseX = e.clientX - r.left;
      mouseY = window.mouseY = e.clientY - r.top;
    });

    canvas.addEventListener('mousedown', () => {
      if (typeof mousePressed === 'function') {
        mousePressed();
      }
    });
  };

  window.background = (r, g = r, b = r, a = 1) => {
    ctx.fillStyle = color(r, g, b, a);
    ctx.fillRect(0, 0, canvas.width, canvas.height);
  };

  window.fill = (r, g = r, b = r, a = 1) => {
    ctx.fillStyle = color(r, g, b, a);
  };

  window.stroke = (r, g = r, b = r) => {
    ctx.strokeStyle = color(r,g,b);
  };

  window.strokeWeight = (w) => {
    strokeWeight = w;
    ctx.lineWidth = strokeWeight;
  };

  window.circle = (x, y, d) => {
    ctx.beginPath();
    ctx.arc(x, y, d / 2, 0, Math.PI * 2);
    ctx.fill();
  };

  window.ellipse = (x, y, w, h = w) => {
    const rx = w / 2, ry = h / 2;
    const cx = x;
    const cy = y;

    ctx.beginPath();
    ctx.ellipse(cx, cy, rx, ry, 0, 0, Math.PI * 2);
    ctx.fill();
  };

  window.map = (val, s1, e1, s2, e2, withinBounds = false) => {
    const newVal = (val - s1) / (e1 - s1) * (e2 - s2) + s2;

    if (!withinBounds) {
      return newVal;
    }

    // withinBounds = true, therefore we need to clamp the value.
    if (e2 > s2) {
      return Math.min(Math.max(newVal, s2), e2);
    } else {
      return Math.min(Math.max(newVal, e2), s2);
    }
  };

  window.random = (...args) => {
    const r = Math.random();
  
    // random() – no args
    if (args.length === 0) {
      return r;
    }
  
    const a = args[0];
  
    // random(array)
    if (Array.isArray(a)) {
      return a[Math.floor(r * a.length)];
    }
  
    // random(high)
    if (args.length === 1) {
      return r * a;
    }
  
    // random(low, high)
    const b = args[1];
    return r * (b - a) + a;
  };

  window.color = (r, g = r, b = r, a = 1) => {
    if (typeof r === 'string') {
      return r;
    } else {
      return `rgba(${r},${g},${b},${a})`;
    }
  };

  //----------------------

  function loop(t) {
    if (typeof draw === 'function') {
      draw(t);
    }
    requestAnimationFrame(loop);
  }

  document.addEventListener('DOMContentLoaded', () => {
    if (typeof setup === 'function') {
      setup();
    }
    requestAnimationFrame(loop);
  });
})();
