---
title: Country Level Endorsement
---

# Country Level Endorsement

How strongly do people in each country personally endorse Dignity, Honour, and Face values? Countries are sorted by average endorsement across all three constructs.

```js
const raw = FileAttachment("data/country_endorsement.csv").csv({ typed: true });
```

```js
const fg = getComputedStyle(document.documentElement).getPropertyValue("--theme-foreground").trim() || "#333";

const data = raw
  .filter(d => d.country_res_full)
  .map(d => ({ ...d, avgMean: (d.D_end_mean_country + d.H_end_mean_country + d.F_end_mean_country) / 3 }))
  .sort((a, b) => a.avgMean - b.avgMean);

const countries = data.map(d => d.country_res_full);

const dotDiv = document.createElement("div");
Plotly.newPlot(dotDiv, [
  {
    type: "scatter", mode: "markers", name: "Dignity",
    x: data.map(d => d.D_end_mean_country),
    y: countries,
    error_x: { type: "data", array: data.map(d => d.D_end_sd_country), visible: true, color: "#4e79a780", thickness: 1.5, width: 4 },
    marker: { color: "#4e79a7", size: 8, line: { color: fg, width: 1 } },
    hovertemplate: "<b>%{y}</b><br>Dignity: %{x:.3f} ± %{error_x.array:.3f}<extra></extra>",
  },
  {
    type: "scatter", mode: "markers", name: "Honour",
    x: data.map(d => d.H_end_mean_country),
    y: countries,
    error_x: { type: "data", array: data.map(d => d.H_end_sd_country), visible: true, color: "#e1575980", thickness: 1.5, width: 4 },
    marker: { color: "#e15759", size: 8, line: { color: fg, width: 1 } },
    hovertemplate: "<b>%{y}</b><br>Honour: %{x:.3f} ± %{error_x.array:.3f}<extra></extra>",
  },
  {
    type: "scatter", mode: "markers", name: "Face",
    x: data.map(d => d.F_end_mean_country),
    y: countries,
    error_x: { type: "data", array: data.map(d => d.F_end_sd_country), visible: true, color: "#59a14f80", thickness: 1.5, width: 4 },
    marker: { color: "#59a14f", size: 8, line: { color: fg, width: 1 } },
    hovertemplate: "<b>%{y}</b><br>Face: %{x:.3f} ± %{error_x.array:.3f}<extra></extra>",
  },
], {
  title: { text: "Dignity, Honour & Face Endorsement by Country (±1 SD)", font: { size: 15, color: fg } },
  xaxis: { title: { text: "Mean Endorsement", font: { color: fg } }, tickfont: { color: fg }, gridcolor: "rgba(128,128,128,0.15)", zeroline: false },
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

## Self vs. Other Endorsement

How does personal endorsement compare to perceived endorsement by others within the same culture?

```js
const constructInput = Inputs.select(["Dignity", "Honour", "Face"], {
  label: "Construct",
  value: "Dignity",
});
const selectedConstruct = view(constructInput);
```

```js
const prefixMap = { "Dignity": "D", "Honour": "H", "Face": "F" };
const prefix = prefixMap[selectedConstruct];

const selfOtherData = [...data].sort((a, b) => a[`${prefix}_self_end_mean_country`] - b[`${prefix}_self_end_mean_country`]);
const soCountries = selfOtherData.map(d => d.country_res_full);

const selfOtherDiv = document.createElement("div");
Plotly.newPlot(selfOtherDiv, [
  {
    type: "scatter", mode: "markers", name: "Self",
    x: selfOtherData.map(d => d[`${prefix}_self_end_mean_country`]),
    y: soCountries,
    error_x: { type: "data", array: selfOtherData.map(d => d[`${prefix}_self_end_sd_country`]), visible: true, color: "#4e79a780", thickness: 1.5, width: 4 },
    marker: { color: "#4e79a7", size: 8, line: { color: fg, width: 1 } },
    hovertemplate: `<b>%{y}</b><br>${selectedConstruct} Self: %{x:.3f} ± %{error_x.array:.3f}<extra></extra>`,
  },
  {
    type: "scatter", mode: "markers", name: "Other",
    x: selfOtherData.map(d => d[`${prefix}_other_end_mean_country`]),
    y: soCountries,
    error_x: { type: "data", array: selfOtherData.map(d => d[`${prefix}_other_end_sd_country`]), visible: true, color: "#f28e2b80", thickness: 1.5, width: 4 },
    marker: { color: "#f28e2b", size: 8, line: { color: fg, width: 1 } },
    hovertemplate: `<b>%{y}</b><br>${selectedConstruct} Other: %{x:.3f} ± %{error_x.array:.3f}<extra></extra>`,
  },
], {
  title: { text: `${selectedConstruct} — Self vs. Other Endorsement by Country (±1 SD)`, font: { size: 15, color: fg } },
  xaxis: { title: { text: "Mean Endorsement", font: { color: fg } }, tickfont: { color: fg }, gridcolor: "rgba(128,128,128,0.15)", zeroline: false },
  yaxis: { tickfont: { size: 10, color: fg }, automargin: true, gridcolor: "rgba(128,128,128,0.15)" },
  margin: { t: 60, b: 60, l: 200, r: 30 },
  height: Math.max(500, soCountries.length * 14 + 80),
  plot_bgcolor: "transparent",
  paper_bgcolor: "transparent",
  showlegend: true,
  legend: { font: { color: fg } },
}, { responsive: true, displayModeBar: true });
display(selfOtherDiv);
```

---

## Endorsement Data Table

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

const endWrapper = document.createElement("div");
endWrapper.style.cssText = "overflow-x:auto;";
const endTbl = document.createElement("table");
endTbl.id = "end-table";
endTbl.className = "display";
endTbl.style.cssText = "width:100%;font-size:.88rem;";
endTbl.innerHTML = `
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
        <td>${d.D_end_mean_country.toFixed(3)}</td><td>${d.D_end_sd_country.toFixed(3)}</td>
        <td>${d.H_end_mean_country.toFixed(3)}</td><td>${d.H_end_sd_country.toFixed(3)}</td>
        <td>${d.F_end_mean_country.toFixed(3)}</td><td>${d.F_end_sd_country.toFixed(3)}</td>
        <td>${d.D_self_end_mean_country.toFixed(3)}</td><td>${d.D_self_end_sd_country.toFixed(3)}</td>
        <td>${d.D_other_end_mean_country.toFixed(3)}</td><td>${d.D_other_end_sd_country.toFixed(3)}</td>
        <td>${d.H_self_end_mean_country.toFixed(3)}</td><td>${d.H_self_end_sd_country.toFixed(3)}</td>
        <td>${d.H_other_end_mean_country.toFixed(3)}</td><td>${d.H_other_end_sd_country.toFixed(3)}</td>
        <td>${d.F_self_end_mean_country.toFixed(3)}</td><td>${d.F_self_end_sd_country.toFixed(3)}</td>
        <td>${d.F_other_end_mean_country.toFixed(3)}</td><td>${d.F_other_end_sd_country.toFixed(3)}</td>
      </tr>
    `).join("")}
  </tbody>
`;
endWrapper.appendChild(endTbl);
display(endWrapper);

window.$(endTbl).DataTable({
  pageLength: 25,
  order: [[0, "asc"]],
  language: { search: "Filter:" },
  columnDefs: [{ targets: Array.from({length: 18}, (_, i) => i + 1), className: "dt-right" }],
});
```
