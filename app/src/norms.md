---
title: Country Level Norms
---

# Country Level Norms

What do people think is *normal* within their culture for Dignity, Honour, and Face? Countries are sorted by average norm score across all three constructs.

```js
const rawNorms = FileAttachment("data/country_norms.csv").csv({ typed: true });
const rawEnd   = FileAttachment("data/country_endorsement.csv").csv({ typed: true });
```

```js
const fg = getComputedStyle(document.documentElement).getPropertyValue("--theme-foreground").trim() || "#333";

const data = rawNorms
  .filter(d => d.country_res_full)
  .map(d => ({ ...d, avgMean: (d.D_norm_mean_country + d.H_norm_mean_country + d.F_norm_mean_country) / 3 }))
  .sort((a, b) => a.avgMean - b.avgMean);

const countries = data.map(d => d.country_res_full);

const dotDiv = document.createElement("div");
Plotly.newPlot(dotDiv, [
  {
    type: "scatter", mode: "markers", name: "Dignity",
    x: data.map(d => d.D_norm_mean_country),
    y: countries,
    error_x: { type: "data", array: data.map(d => d.D_norm_sd_country), visible: true, color: "#4e79a780", thickness: 1.5, width: 4 },
    marker: { color: "#4e79a7", size: 8, line: { color: fg, width: 1 } },
    hovertemplate: "<b>%{y}</b><br>Dignity Norm: %{x:.3f} ± %{error_x.array:.3f}<extra></extra>",
  },
  {
    type: "scatter", mode: "markers", name: "Honour",
    x: data.map(d => d.H_norm_mean_country),
    y: countries,
    error_x: { type: "data", array: data.map(d => d.H_norm_sd_country), visible: true, color: "#e1575980", thickness: 1.5, width: 4 },
    marker: { color: "#e15759", size: 8, line: { color: fg, width: 1 } },
    hovertemplate: "<b>%{y}</b><br>Honour Norm: %{x:.3f} ± %{error_x.array:.3f}<extra></extra>",
  },
  {
    type: "scatter", mode: "markers", name: "Face",
    x: data.map(d => d.F_norm_mean_country),
    y: countries,
    error_x: { type: "data", array: data.map(d => d.F_norm_sd_country), visible: true, color: "#59a14f80", thickness: 1.5, width: 4 },
    marker: { color: "#59a14f", size: 8, line: { color: fg, width: 1 } },
    hovertemplate: "<b>%{y}</b><br>Face Norm: %{x:.3f} ± %{error_x.array:.3f}<extra></extra>",
  },
], {
  title: { text: "Dignity, Honour & Face Norms by Country (±1 SD)", font: { size: 15, color: fg } },
  xaxis: { title: { text: "Mean Norm Score", font: { color: fg } }, tickfont: { color: fg }, gridcolor: "rgba(128,128,128,0.15)", zeroline: false },
  yaxis: { tickfont: { size: 10, color: fg }, automargin: true, gridcolor: "rgba(128,128,128,0.15)" },
  margin: { t: 60, b: 60, l: 200, r: 30 },
  height: Math.max(500, countries.length * 14 + 80),
  plot_bgcolor: "transparent",
  paper_bgcolor: "transparent",
  showlegend: true,
  legend: { font: { color: fg } },
}, { responsive: true, displayModeBar: true });
display(dotDiv);
```

---

## Endorsement vs. Norms

Does what people personally endorse match what they perceive as normal in their culture? Points above the diagonal line indicate higher norms than endorsement; points below indicate higher endorsement than norms.

```js
const constructInput = Inputs.select(["Dignity", "Honour", "Face"], {
  label: "Construct",
  value: "Dignity",
});
const selectedConstruct = view(constructInput);
```

```js
const prefixMap  = { "Dignity": "D", "Honour": "H", "Face": "F" };
const colorMap   = { "Dignity": "#4e79a7", "Honour": "#e15759", "Face": "#59a14f" };
const prefix     = prefixMap[selectedConstruct];
const dotColor   = colorMap[selectedConstruct];

const endMap = new Map(rawEnd.map(d => [d.country_res_full, d]));

const scatterData = data
  .map(d => ({
    country: d.country_res_full,
    norm: d[`${prefix}_norm_mean_country`],
    end:  endMap.get(d.country_res_full)?.[`${prefix}_end_mean_country`] ?? null,
  }))
  .filter(d => d.norm != null && d.end != null);

const allVals = [...scatterData.map(d => d.norm), ...scatterData.map(d => d.end)];
const axMin = Math.min(...allVals) - 0.3;
const axMax = Math.max(...allVals) + 0.3;

const scatterDiv = document.createElement("div");
Plotly.newPlot(scatterDiv, [
  {
    type: "scatter", mode: "lines",
    x: [axMin, axMax], y: [axMin, axMax],
    line: { color: "rgba(128,128,128,0.35)", dash: "dash", width: 1 },
    hoverinfo: "skip", showlegend: false,
  },
  {
    type: "scatter", mode: "markers",
    x: scatterData.map(d => d.end),
    y: scatterData.map(d => d.norm),
    text: scatterData.map(d => d.country),
    marker: { color: dotColor, size: 9, line: { color: fg, width: 1 } },
    hovertemplate: "<b>%{text}</b><br>Endorsement: %{x:.3f}<br>Norm: %{y:.3f}<extra></extra>",
    showlegend: false,
  },
], {
  title: { text: `${selectedConstruct} — Endorsement vs. Norm by Country`, font: { size: 15, color: fg } },
  xaxis: { title: { text: `${selectedConstruct} Endorsement`, font: { color: fg } }, tickfont: { color: fg }, range: [axMin, axMax], gridcolor: "rgba(128,128,128,0.15)" },
  yaxis: { title: { text: `${selectedConstruct} Norm`, font: { color: fg } }, tickfont: { color: fg }, range: [axMin, axMax], gridcolor: "rgba(128,128,128,0.15)" },
  margin: { t: 60, b: 80, l: 80, r: 30 },
  height: 520,
  plot_bgcolor: "transparent",
  paper_bgcolor: "transparent",
}, { responsive: true, displayModeBar: true });
display(scatterDiv);
```

---

## Norms Data Table

```js
async function loadScript(src) {
  return new Promise((resolve, reject) => {
    if (document.querySelector(`script[src="${src}"]`)) return resolve();
    const s = document.createElement("script");
    s.src = src; s.onload = resolve; s.onerror = reject;
    document.head.appendChild(s);
  });
}
await loadScript("https://code.jquery.com/jquery-3.7.1.min.js");
await loadScript("https://cdn.datatables.net/2.0.8/js/dataTables.min.js");

const normWrapper = document.createElement("div");
normWrapper.style.cssText = "overflow-x:auto;";
const normTbl = document.createElement("table");
normTbl.id = "norm-table";
normTbl.className = "display";
normTbl.style.cssText = "width:100%;font-size:.88rem;";
normTbl.innerHTML = `
  <thead>
    <tr>
      <th>Country</th>
      <th>Dignity Mean</th><th>Dignity SD</th>
      <th>Honour Mean</th><th>Honour SD</th>
      <th>Face Mean</th><th>Face SD</th>
      <th>Dignity Self Mean</th><th>Dignity Self SD</th>
      <th>Dignity Other Mean</th><th>Dignity Other SD</th>
      <th>Honour Self Mean</th><th>Honour Self SD</th>
      <th>Honour Other Mean</th><th>Honour Other SD</th>
      <th>Face Self Mean</th><th>Face Self SD</th>
      <th>Face Other Mean</th><th>Face Other SD</th>
    </tr>
  </thead>
  <tbody>
    ${data.map(d => `
      <tr>
        <td>${d.country_res_full}</td>
        <td>${d.D_norm_mean_country.toFixed(3)}</td><td>${d.D_norm_sd_country.toFixed(3)}</td>
        <td>${d.H_norm_mean_country.toFixed(3)}</td><td>${d.H_norm_sd_country.toFixed(3)}</td>
        <td>${d.F_norm_mean_country.toFixed(3)}</td><td>${d.F_norm_sd_country.toFixed(3)}</td>
        <td>${d.D_self_norm_mean_country.toFixed(3)}</td><td>${d.D_self_norm_sd_country.toFixed(3)}</td>
        <td>${d.D_other_norm_mean_country.toFixed(3)}</td><td>${d.D_other_norm_sd_country.toFixed(3)}</td>
        <td>${d.H_self_norm_mean_country.toFixed(3)}</td><td>${d.H_self_norm_sd_country.toFixed(3)}</td>
        <td>${d.H_other_norm_mean_country.toFixed(3)}</td><td>${d.H_other_norm_sd_country.toFixed(3)}</td>
        <td>${d.F_self_norm_mean_country.toFixed(3)}</td><td>${d.F_self_norm_sd_country.toFixed(3)}</td>
        <td>${d.F_other_norm_mean_country.toFixed(3)}</td><td>${d.F_other_norm_sd_country.toFixed(3)}</td>
      </tr>
    `).join("")}
  </tbody>
`;
normWrapper.appendChild(normTbl);
display(normWrapper);

window.$(normTbl).DataTable({
  pageLength: 25,
  order: [[0, "asc"]],
  language: { search: "Filter:" },
  columnDefs: [{ targets: Array.from({length: 18}, (_, i) => i + 1), className: "dt-right" }],
});
```
