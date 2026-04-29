---
title: Scale Reliability
---

# Scale Reliability (Cronbach's α)

Cronbach's alpha measures internal consistency for each scale within each country. Values ≥ 0.70 are conventionally acceptable; values < 0.60 indicate poor reliability.

```js
const raw = FileAttachment("data/alpha_results.csv").csv({ typed: true });
```

```js
const schwartzSubscales = new Set([
  "Achievement", "Benevolence", "Conformity", "Hedonism", "Power",
  "Security", "Self_Direction", "Stimulation", "Tradition", "Universalism",
]);
const filtered_raw = raw.filter(d => !schwartzSubscales.has(d.Scale));

// Exclude "Overall" for the per-country heatmap
const byCountry = filtered_raw.filter(d => d.Country !== "Overall" && d.Alpha != null);

const scaleOrder = [...new Set(byCountry.map(d => d.Scale))].sort();
const countryOrder = [...new Set(byCountry.map(d => d.Country))].sort();

// Build z-matrix: rows = countries, cols = scales
const lookup = new Map(byCountry.map(d => [`${d.Country}|${d.Scale}`, d.Alpha]));
const z = countryOrder.map(c => scaleOrder.map(s => lookup.get(`${c}|${s}`) ?? null));
const text = countryOrder.map(c => scaleOrder.map(s => {
  const v = lookup.get(`${c}|${s}`);
  return v == null ? "No data" : v.toFixed(3);
}));
```

## Alpha Heatmap — All Countries × Scales

Each cell shows Cronbach's α. Hover for exact values. Grey cells = scale not administered in that country.

```js
const fg = getComputedStyle(document.documentElement).getPropertyValue("--theme-foreground").trim() || "#333";

const heatmapDiv = document.createElement("div");
Plotly.newPlot(heatmapDiv, [{
  type: "heatmap",
  z,
  x: scaleOrder.map(s => s.replace(/_/g, " ")),
  y: countryOrder,
  text,
  hovertemplate: "<b>%{y}</b><br>%{x}<br>α = %{text}<extra></extra>",
  colorscale: [
    [0,   "#d73027"],
    [0.43, "#fee08b"],
    [0.57, "#d9ef8b"],
    [0.71, "#1a9850"],
    [1,   "#004529"],
  ],
  zmin: 0,
  zmax: 1,
  colorbar: {
    title: { text: "α", font: { color: fg } },
    tickvals: [0, 0.3, 0.6, 0.7, 0.8, 1.0],
    ticktext: ["0", "0.3", "0.6 (poor)", "0.7 (ok)", "0.8 (good)", "1.0"],
    tickfont: { color: fg },
    len: 0.8,
  },
  zauto: false,
}], {
  title: { text: "Cronbach's α by Country and Scale", font: { size: 15, color: fg } },
  xaxis: { tickangle: -45, tickfont: { size: 10, color: fg }, automargin: true },
  yaxis: { tickfont: { size: 10, color: fg }, automargin: true },
  margin: { t: 50, b: 180, l: 160, r: 20 },
  height: Math.max(600, countryOrder.length * 12),
  plot_bgcolor: "#ffffff",
  paper_bgcolor: "transparent",
}, { responsive: true, displayModeBar: true });
display(heatmapDiv);
```

---

## Overall α by Scale

Average reliability across all countries for each scale.

```js
const overallAlpha = filtered_raw.filter(d => d.Country === "Overall").sort((a, b) => b.Alpha - a.Alpha);

const barDiv = document.createElement("div");
Plotly.newPlot(barDiv, [{
  type: "bar",
  x: overallAlpha.map(d => d.Scale.replace(/_/g, " ")),
  y: overallAlpha.map(d => d.Alpha),
  marker: {
    color: overallAlpha.map(d =>
      d.Alpha >= 0.8 ? "#1a9850" :
      d.Alpha >= 0.7 ? "#d9ef8b" :
      d.Alpha >= 0.6 ? "#fee08b" : "#d73027"
    )
  },
  hovertemplate: "<b>%{x}</b><br>α = %{y:.3f}<extra></extra>",
}], {
  title: { text: "Overall Cronbach's α (all countries pooled)", font: { size: 15, color: fg } },
  xaxis: { tickangle: -45, tickfont: { size: 11, color: fg }, automargin: true },
  yaxis: { title: { text: "Cronbach's α", font: { color: fg } }, tickfont: { color: fg }, range: [0, 1], gridcolor: "#eee" },
  shapes: [
    { type: "line", x0: -0.5, x1: overallAlpha.length - 0.5, y0: 0.7, y1: 0.7,
      line: { color: "#666", dash: "dash", width: 1 } },
    { type: "line", x0: -0.5, x1: overallAlpha.length - 0.5, y0: 0.6, y1: 0.6,
      line: { color: "#d73027", dash: "dot", width: 1 } },
  ],
  annotations: [
    { x: overallAlpha.length * 0.92, y: 0.71, text: "α = 0.70", showarrow: false, font: { size: 10, color: "#666" } },
    { x: overallAlpha.length * 0.92, y: 0.61, text: "α = 0.60", showarrow: false, font: { size: 10, color: "#d73027" } },
  ],
  margin: { t: 50, b: 160, l: 60, r: 20 },
  plot_bgcolor: "transparent",
  paper_bgcolor: "transparent",
  height: 420,
}, { responsive: true, displayModeBar: false });
display(barDiv);
```

---

## Full Reliability Table

```js
async function loadScript(src) {
  return new Promise((resolve, reject) => {
    if (document.querySelector(`script[src="${src}"]`)) return resolve();
    const s = document.createElement("script");
    s.src = src;
    s.onload = resolve;
    s.onerror = reject;
    document.head.appendChild(s);
  });
}
await loadScript("https://code.jquery.com/jquery-3.7.1.min.js");
await loadScript("https://cdn.datatables.net/2.0.8/js/dataTables.min.js");

const tableData = filtered_raw.filter(d => d.Alpha != null);

const wrapper = document.createElement("div");
wrapper.style.cssText = "overflow-x:auto;";

const tbl = document.createElement("table");
tbl.id = "alpha-table";
tbl.className = "display";
tbl.style.cssText = "width:100%;font-size:.88rem;";

tbl.innerHTML = `
  <thead>
    <tr>
      <th>Country</th>
      <th>Scale</th>
      <th>N</th>
      <th>Valid Items</th>
      <th>Total Items</th>
      <th>α</th>
    </tr>
  </thead>
  <tbody>
    ${tableData.map(d => `
      <tr>
        <td>${d.Country}</td>
        <td>${d.Scale.replace(/_/g, " ")}</td>
        <td>${d.N.toLocaleString()}</td>
        <td>${d.Valid_Items}</td>
        <td>${d.Total_Items}</td>
        <td style="color:${d.Alpha >= 0.7 ? '#1a9850' : d.Alpha >= 0.6 ? '#e6a817' : '#d73027'};font-weight:600;">
          ${d.Alpha.toFixed(3)}
        </td>
      </tr>
    `).join("")}
  </tbody>
`;
wrapper.appendChild(tbl);
display(wrapper);

window.$(tbl).DataTable({
  pageLength: 25,
  order: [[5, "asc"]],
  language: { search: "Filter:" },
  columnDefs: [{ targets: [2, 3, 4, 5], className: "dt-right" }],
});
```
