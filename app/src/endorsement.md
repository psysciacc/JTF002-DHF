---
title: Country Level Endorsement
---

# Country Level Endorsement

How strongly do people in each country personally endorse Dignity, Honour, and Face values? Countries are sorted by average endorsement across all three constructs.

```js
const raw = FileAttachment("data/country_endorsement.csv").csv({ typed: true });
const isoLookup = FileAttachment("data/countries_iso.csv").csv({ typed: false });
```

```js
const fg = getComputedStyle(document.documentElement).getPropertyValue("--theme-foreground").trim() || "#333";

const data = raw
  .filter(d => d.country_res_full)
  .map(d => ({ ...d, avgMean: (d.D_end_mean_country + d.H_end_mean_country + d.F_end_mean_country) / 3 }))
  .sort((a, b) => a.avgMean - b.avgMean);

const countries = data.map(d => d.country_res_full);
```

## Endorsement Map

```js
const measureOptions = {
  "Dignity":          "D_end_mean_country",
  "Honour":           "H_end_mean_country",
  "Face":             "F_end_mean_country",
  "Dignity (Self)":   "D_self_end_mean_country",
  "Dignity (Other)":  "D_other_end_mean_country",
  "Honour (Self)":    "H_self_end_mean_country",
  "Honour (Other)":   "H_other_end_mean_country",
  "Face (Self)":      "F_self_end_mean_country",
  "Face (Other)":     "F_other_end_mean_country",
};
const measureInput = Inputs.select(Object.keys(measureOptions), { label: "Measure", value: "Dignity" });
const selectedMeasure = view(measureInput);
```

```js
const numericToIso3 = {
  8:"ALB", 32:"ARG", 51:"ARM", 36:"AUS", 40:"AUT", 50:"BGD",
  70:"BIH", 76:"BRA", 100:"BGR", 120:"CMR", 124:"CAN", 152:"CHL",
  156:"CHN", 170:"COL", 191:"HRV", 196:"CYP", 203:"CZE", 231:"ETH",
  250:"FRA", 268:"GEO", 276:"DEU", 288:"GHA", 300:"GRC", 344:"HKG",
  348:"HUN", 356:"IND", 360:"IDN", 364:"IRN", 368:"IRQ", 372:"IRL",
  376:"ISR", 380:"ITA", 392:"JPN", 398:"KAZ", 404:"KEN", 414:"KWT",
  422:"LBN", 458:"MYS", 484:"MEX", 504:"MAR", 516:"NAM", 528:"NLD",
  554:"NZL", 566:"NGA", 807:"MKD", 586:"PAK", 608:"PHL", 616:"POL",
  620:"PRT", 642:"ROU", 643:"RUS", 682:"SAU", 688:"SRB", 702:"SGP",
  703:"SVK", 710:"ZAF", 724:"ESP", 756:"CHE", 764:"THA", 792:"TUR",
  804:"UKR", 784:"ARE", 826:"GBR", 840:"USA", 860:"UZB", 704:"VNM",
};

const isoMap = new Map(isoLookup.map(d => [d.dhf_country_name, d.iso3c]));
const col = measureOptions[selectedMeasure];

const mapEndData = data
  .map(d => ({ country: d.country_res_full, value: d[col], iso3c: isoMap.get(d.country_res_full) ?? "" }))
  .filter(d => d.iso3c.length === 3 && typeof d.value === "number");

const endDataByIso3 = new Map(mapEndData.map(d => [d.iso3c, d]));
const endValues = mapEndData.map(d => d.value);
const vMin = Math.min(...endValues);
const vMax = Math.max(...endValues);

const constructColor = {
  "Dignity": [78,121,167], "Dignity (Self)": [78,121,167], "Dignity (Other)": [78,121,167],
  "Honour":  [225,87,89],  "Honour (Self)":  [225,87,89],  "Honour (Other)":  [225,87,89],
  "Face":    [89,161,79],  "Face (Self)":    [89,161,79],  "Face (Other)":    [89,161,79],
};
const [r2, g2, b2] = constructColor[selectedMeasure];

function getEndColor(v) {
  const t = (v - vMin) / (vMax - vMin || 1);
  return `rgb(${Math.round(240+(r2-240)*t)},${Math.round(240+(g2-240)*t)},${Math.round(240+(b2-240)*t)})`;
}

function fixRing(ring) {
  if (ring.length < 2) return ring;
  const out = [[...ring[0]]];
  for (let i = 1; i < ring.length; i++) {
    let lng = ring[i][0];
    const prev = out[i-1][0];
    while (lng - prev >  180) lng -= 360;
    while (lng - prev < -180) lng += 360;
    out.push([lng, ring[i][1]]);
  }
  return out;
}
function fixGeometry(geom) {
  if (!geom) return;
  if (geom.type === "Polygon")      geom.coordinates = geom.coordinates.map(fixRing);
  if (geom.type === "MultiPolygon") geom.coordinates = geom.coordinates.map(p => p.map(fixRing));
}

const world = await fetch("https://cdn.jsdelivr.net/npm/world-atlas@2/countries-110m.json").then(r => r.json());
const geojson = topojson.feature(world, world.objects.countries);
geojson.features.forEach(f => fixGeometry(f.geometry));

const endMapContainer = document.createElement("div");
endMapContainer.style.cssText = "height:480px;width:100%;border-radius:8px;overflow:hidden;";
display(endMapContainer);
await new Promise(r => requestAnimationFrame(r));

const lmap = L.map(endMapContainer, {
  center: [25, 10], zoom: 2, minZoom: 1, maxZoom: 6,
  zoomControl: true, attributionControl: false, scrollWheelZoom: false,
});

L.geoJSON(geojson, {
  style(feature) {
    const iso3 = numericToIso3[+feature.id];
    const d = iso3 ? endDataByIso3.get(iso3) : null;
    return { fillColor: d ? getEndColor(d.value) : "#f0f0f0", fillOpacity: 1, color: "#ccc", weight: 0.5 };
  },
  onEachFeature(feature, layer) {
    const iso3 = numericToIso3[+feature.id];
    const d = iso3 ? endDataByIso3.get(iso3) : null;
    if (d) {
      layer.bindTooltip(`<strong>${d.country}</strong><br>${selectedMeasure}: ${d.value.toFixed(3)}`, { sticky: true });
      layer.on("mouseover", function() { this.setStyle({ fillOpacity: 0.75, weight: 1.5, color: "#333" }); });
      layer.on("mouseout",  function() { this.setStyle({ fillOpacity: 1,    weight: 0.5, color: "#ccc" }); });
    }
  },
}).addTo(lmap);

const legend = L.control({ position: "bottomright" });
legend.onAdd = () => {
  const div = L.DomUtil.create("div");
  div.style.cssText = "background:rgba(255,255,255,0.92);padding:10px 14px;border-radius:7px;font:13px/1.8 system-ui,sans-serif;color:#333;box-shadow:0 2px 8px rgba(0,0,0,.15);";
  div.innerHTML = `
    <div style="font-size:11px;font-weight:600;letter-spacing:.06em;text-transform:uppercase;margin-bottom:6px;color:#666">${selectedMeasure}</div>
    <div style="display:flex;align-items:center;gap:6px;">
      <span style="font-size:11px">${vMin.toFixed(2)}</span>
      <div style="width:80px;height:12px;border-radius:3px;background:linear-gradient(to right,rgb(240,240,240),rgb(${r2},${g2},${b2}));border:1px solid rgba(0,0,0,.1)"></div>
      <span style="font-size:11px">${vMax.toFixed(2)}</span>
    </div>`;
  return div;
};
legend.addTo(lmap);
```

---

## Endorsement by Country

```js
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
