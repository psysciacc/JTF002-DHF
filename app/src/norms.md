---
title: Country Level Norms
---

# Country Level Norms

What do people think is *normal* within their culture for Dignity, Honour, and Face? Countries are sorted by average norm score across all three constructs.

```js
const rawNorms = FileAttachment("data/country_norms.csv").csv({ typed: true });
const rawEnd   = FileAttachment("data/country_endorsement.csv").csv({ typed: true });
const isoLookup = FileAttachment("data/countries_iso.csv").csv({ typed: false });
```

```js
const fg = getComputedStyle(document.documentElement).getPropertyValue("--theme-foreground").trim() || "#333";

const data = rawNorms
  .filter(d => d.country_res_full)
  .map(d => ({ ...d, avgMean: (d.D_norm_mean_country + d.H_norm_mean_country + d.F_norm_mean_country) / 3 }))
  .sort((a, b) => a.avgMean - b.avgMean);

const countries = data.map(d => d.country_res_full);
```

## Norms Map

```js
const measureOptions = {
  "Dignity":         "D_norm_mean_country",
  "Honour":          "H_norm_mean_country",
  "Face":            "F_norm_mean_country",
  "Dignity (Self)":  "D_self_norm_mean_country",
  "Dignity (Other)": "D_other_norm_mean_country",
  "Honour (Self)":   "H_self_norm_mean_country",
  "Honour (Other)":  "H_other_norm_mean_country",
  "Face (Self)":     "F_self_norm_mean_country",
  "Face (Other)":    "F_other_norm_mean_country",
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
  158:"TWN",
};

const isoMap = new Map(isoLookup.map(d => [d.dhf_country_name, d.iso3c]));
const col = measureOptions[selectedMeasure];

const mapNormData = data
  .map(d => ({ country: d.country_res_full, value: d[col], iso3c: isoMap.get(d.country_res_full) ?? "" }))
  .filter(d => d.iso3c.length === 3 && typeof d.value === "number");

const normDataByIso3 = new Map(mapNormData.map(d => [d.iso3c, d]));
const normValues = mapNormData.map(d => d.value);
const vMin = Math.min(...normValues);
const vMax = Math.max(...normValues);

const constructColor = {
  "Dignity": [78,121,167], "Dignity (Self)": [78,121,167], "Dignity (Other)": [78,121,167],
  "Honour":  [225,87,89],  "Honour (Self)":  [225,87,89],  "Honour (Other)":  [225,87,89],
  "Face":    [89,161,79],  "Face (Self)":    [89,161,79],  "Face (Other)":    [89,161,79],
};
const [r2, g2, b2] = constructColor[selectedMeasure];

function getNormColor(v) {
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

const world = await fetch("https://cdn.jsdelivr.net/npm/visionscarto-world-atlas@1/world/110m.json").then(r => r.json());
const geojson = topojson.feature(world, world.objects.countries);
geojson.features.forEach(f => fixGeometry(f.geometry));

const normMapContainer = document.createElement("div");
normMapContainer.style.cssText = "height:480px;width:100%;border-radius:8px;overflow:hidden;";
display(normMapContainer);
await new Promise(r => requestAnimationFrame(r));

const lmap = L.map(normMapContainer, {
  center: [25, 10], zoom: 2, minZoom: 1, maxZoom: 6,
  zoomControl: true, attributionControl: false, scrollWheelZoom: false,
});

L.geoJSON(geojson, {
  style(feature) {
    const iso3 = feature.properties?.a3 ?? numericToIso3[+feature.id];
    const d = iso3 ? normDataByIso3.get(iso3) : null;
    return { fillColor: d ? getNormColor(d.value) : "#f0f0f0", fillOpacity: 1, color: "#ccc", weight: 0.5 };
  },
  onEachFeature(feature, layer) {
    const iso3 = feature.properties?.a3 ?? numericToIso3[+feature.id];
    const d = iso3 ? normDataByIso3.get(iso3) : null;
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

## Norms by Country

```js
const flipNormsDot = view(Inputs.toggle({ label: "Flip axes" }));
```

```js
const normDotWrapper = document.createElement("div");
normDotWrapper.style.cssText = "overflow-x:auto;";
const dotDiv = document.createElement("div");
normDotWrapper.appendChild(dotDiv);
const normTraces = [
  { name: "Dignity", meanKey: "D_norm_mean_country", sdKey: "D_norm_sd_country", color: "#4e79a7", errColor: "#4e79a780" },
  { name: "Honour",  meanKey: "H_norm_mean_country", sdKey: "H_norm_sd_country", color: "#e15759", errColor: "#e1575980" },
  { name: "Face",    meanKey: "F_norm_mean_country", sdKey: "F_norm_sd_country", color: "#59a14f", errColor: "#59a14f80" },
].map(({ name, meanKey, sdKey, color, errColor }) => ({
  type: "scatter", mode: "markers", name,
  x: flipNormsDot ? countries : data.map(d => d[meanKey]),
  y: flipNormsDot ? data.map(d => d[meanKey]) : countries,
  ...(flipNormsDot
    ? { error_y: { type: "data", array: data.map(d => d[sdKey]), visible: true, color: errColor, thickness: 1.5, width: 4 } }
    : { error_x: { type: "data", array: data.map(d => d[sdKey]), visible: true, color: errColor, thickness: 1.5, width: 4 } }),
  marker: { color, size: 8, line: { color: fg, width: 1 } },
  hovertemplate: flipNormsDot
    ? `<b>%{x}</b><br>${name} Norm: %{y:.3f} ± %{error_y.array:.3f}<extra></extra>`
    : `<b>%{y}</b><br>${name} Norm: %{x:.3f} ± %{error_x.array:.3f}<extra></extra>`,
}));
const normDotWidth = flipNormsDot ? Math.max(900, countries.length * 14 + 80) : null;
Plotly.newPlot(dotDiv, normTraces, {
  title: { text: "Dignity, Honour & Face Norms by Country (±1 SD)", font: { size: 15, color: fg } },
  xaxis: {
    title: { text: flipNormsDot ? "Country" : "Mean Norm Score", font: { color: fg } },
    tickfont: { size: 10, color: fg }, automargin: true,
    gridcolor: "rgba(128,128,128,0.15)", zeroline: false,
    ...(flipNormsDot ? { tickangle: -45 } : {}),
  },
  yaxis: {
    title: flipNormsDot ? { text: "Mean Norm Score", font: { color: fg } } : undefined,
    tickfont: { size: 10, color: fg }, automargin: true,
    gridcolor: "rgba(128,128,128,0.15)",
  },
  margin: flipNormsDot ? { t: 60, b: 180, l: 60, r: 30 } : { t: 60, b: 60, l: 200, r: 30 },
  height: flipNormsDot ? 600 : Math.max(500, countries.length * 14 + 80),
  ...(normDotWidth ? { width: normDotWidth } : {}),
  plot_bgcolor: "transparent",
  paper_bgcolor: "transparent",
  showlegend: true,
  legend: { font: { color: fg } },
}, { responsive: !flipNormsDot, displayModeBar: true });
display(normDotWrapper);
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
const flipScatter = view(Inputs.toggle({ label: "Flip axes" }));
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
    x: flipScatter ? scatterData.map(d => d.norm) : scatterData.map(d => d.end),
    y: flipScatter ? scatterData.map(d => d.end) : scatterData.map(d => d.norm),
    text: scatterData.map(d => d.country),
    marker: { color: dotColor, size: 9, line: { color: fg, width: 1 } },
    hovertemplate: flipScatter
      ? "<b>%{text}</b><br>Norm: %{x:.3f}<br>Endorsement: %{y:.3f}<extra></extra>"
      : "<b>%{text}</b><br>Endorsement: %{x:.3f}<br>Norm: %{y:.3f}<extra></extra>",
    showlegend: false,
  },
], {
  title: { text: `${selectedConstruct} — Endorsement vs. Norm by Country`, font: { size: 15, color: fg } },
  xaxis: {
    title: { text: flipScatter ? `${selectedConstruct} Norm` : `${selectedConstruct} Endorsement`, font: { color: fg } },
    tickfont: { color: fg }, range: [axMin, axMax], gridcolor: "rgba(128,128,128,0.15)",
  },
  yaxis: {
    title: { text: flipScatter ? `${selectedConstruct} Endorsement` : `${selectedConstruct} Norm`, font: { color: fg } },
    tickfont: { color: fg }, range: [axMin, axMax], gridcolor: "rgba(128,128,128,0.15)",
  },
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
