import { main } from "./output/Main";
import rough from 'roughjs'

class Rough extends HTMLElement {
  static get observedAttributes() {
    return [
      "color",
      "stroke-color",
      "hover-color",
      "hover-stroke-color",
      "down-color",
      "down-stroke-color",
    ];
  }
  constructor() {
    super();

    this.down = false;
    this.hover = false;
    this.shadow = this.attachShadow({ mode: "open" });
    this.styling = document.createElement("style");
    this.doStylesheet();
    this.shadow.appendChild(this.styling);
  }
  attributeChangedCallback(name, oldValue, newValue) {
    this.doStylesheet();
  }
  doStylesheet() {
    this.styling.textContent = `
svg g path[stroke-width="4"] {
  stroke: ${this.getAttribute("color")}
}

svg g path[stroke-width="1"] {
  stroke: ${this.getAttribute("stroke-color")}
}

svg:hover g path[stroke-width="4"] {
  stroke: ${this.getAttribute("hover-color")}
}

svg:hover g path[stroke-width="1"] {
  stroke: ${this.getAttribute("hover-stroke-color")}
}

svg:active g path[stroke-width="4"] {
  stroke: ${this.getAttribute("down-color")}
}

svg:active g path[stroke-width="1"] {
  stroke: ${this.getAttribute("down-stroke-color")}
}
`;
  }
  connectedCallback() {
    let shadowRoot = this.shadow;

    this.style.display = "block";
    this.style.width = "100%";
    this.style.height = "100%";
    this.el = document.createElementNS("http://www.w3.org/2000/svg", "svg");

    this.el.style.display = "block";
    this.el.style.overflow = "visible";
    this.rc = rough.svg(this.el);

    shadowRoot.appendChild(this.el);
    this.redraw();
    // const d = new Date()
    // const thingy = 250
    // const evened = d.getMilliseconds() % thingy
    // setTimeout(() => {
    //   this.timer = setInterval(() => this.redraw(), thingy)
    // }, evened)

    new ResizeObserver(() => this.redraw()).observe(this);
  }
  disconnectedCallback() {
    clearInterval(this.timer);
  }
  get fillColor() {
    return this.getAttribute("color");
  }
  get strokeColor() {
    return this.getAttribute("stroke-color");
  }
  redraw() {}
}
class RoughRectangle extends Rough {
  redraw() {
    const w = this.clientWidth;
    const h = this.clientHeight;

    let node = this.rc.rectangle(0, 0, w, h, {
      stroke: this.strokeColor,
      fill: this.fillColor,
      fillWeight: 4,
      roughness: 2.0,
    });
    this.el.setAttribute("viewBox", `0 0 ${w} ${h}`);
    this.el.setAttribute("width", `${w}`);
    this.el.setAttribute("height", `${h}`);
    this.el.style.width = `${w}px`;
    this.el.style.height = `${h}px`;
    if (this.el.firstChild) this.el.removeChild(this.el.firstChild);
    this.el.appendChild(node);
  }
}
class RougherRectangle extends Rough {
  redraw() {
    const w = this.clientWidth;
    const h = this.clientHeight;

    let node = this.rc.rectangle(0, 0, w, h, {
      stroke: this.strokeColor,
      fill: this.fillColor,
      fillWeight: 3,
      fillStyle: "zigzag",
      hachureAngle: 0.0,
      roughness: 1.0,
    });
    this.el.setAttribute("viewBox", `0 0 ${w} ${h}`);
    this.el.setAttribute("width", `${w}`);
    this.el.setAttribute("height", `${h}`);
    this.el.style.width = `${w}px`;
    this.el.style.height = `${h}px`;
    if (this.el.firstChild) this.el.removeChild(this.el.firstChild);
    this.el.appendChild(node);
  }
}
class RoughHorizontalLine extends Rough {
  redraw() {
    const w = this.clientWidth;
    const h = this.clientHeight;

    let node = this.rc.line(0, 0, w, 0, {
      stroke: this.strokeColor,
      roughness: 2.0,
    });
    this.el.setAttribute("viewBox", `0 0 ${w} ${h}`);
    this.el.setAttribute("width", `${w}`);
    this.el.setAttribute("height", `${h}`);
    this.el.style.width = `${w}px`;
    this.el.style.height = `${h}px`;
    if (this.el.firstChild) this.el.removeChild(this.el.firstChild);
    this.el.appendChild(node);
  }
}
window.customElements.define("rough-rectangle", RoughRectangle);
window.customElements.define("rougher-rectangle", RougherRectangle);
window.customElements.define("rough-horizontal-line", RoughHorizontalLine);

main();
