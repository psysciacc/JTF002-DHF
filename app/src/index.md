---
title: DHF Cross-Cultural Study — Overview
---

# Dignity, Honour & Face: A Cross-Cultural Study

This dashboard presents data from a large-scale Psychological Science Accelerator (PSA) project examining how people across the world conceptualize and respond to threats to **Dignity**, **Honour**, and **Face**. Data were collected from **${totalN.toLocaleString()}** participants across **${countries.length}** countries using standardized surveys administered via Qualtrics and Gorilla.

---

## Sample Size by Country

```js
const desc = FileAttachment("data/scale_descriptives.csv").csv({ typed: true });
const isoLookup = FileAttachment("data/countries_iso.csv").csv({ typed: false });
const countryN = FileAttachment("data/country_n_app.csv").csv({ typed: true });
```

```js
const countrySizes = countryN
  .map(d => ({ country: d.country_res_full, n: d.n }))
  .sort((a, b) => b.n - a.n);

const countries = countrySizes.map(d => d.country);
const totalN = countrySizes.reduce((s, d) => s + d.n, 0);

// Join with ISO codes
const isoMap = new Map(isoLookup.map(d => [d.dhf_country_name, d.iso3c]));
const mapData = countrySizes
  .map(d => ({ ...d, iso3c: isoMap.get(d.country) ?? "" }))
  .filter(d => d.iso3c.length === 3);
```

```js
// ISO 3166-1 numeric → ISO3 for every study country
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

const mapRows = mapData.filter(d => d.n > 0);
const dataByIso3 = new Map(mapRows.map(d => [d.iso3c, d]));

const binDefs = [
  { min: 0,    max: 100,      label: "< 100",   color: "#deebf7" },
  { min: 100,  max: 250,      label: "100–249",  color: "#9ecae1" },
  { min: 250,  max: 500,      label: "250–499",  color: "#4292c6" },
  { min: 500,  max: 1000,     label: "500–999",  color: "#2171b5" },
  { min: 1000, max: Infinity, label: "1,000+",   color: "#084594" },
];

function getColor(n) {
  return (binDefs.find(b => n >= b.min && n < b.max) ?? binDefs.at(-1)).color;
}

// Fetch world atlas topojson at browser runtime.
// The Visionscarto variant includes Kosovo and explicit 3-letter codes.
const world = await fetch("https://cdn.jsdelivr.net/npm/visionscarto-world-atlas@1/world/110m.json")
  .then(r => r.json());
const geojson = topojson.feature(world, world.objects.countries);

// Fix antimeridian: Russia's ring goes from ~178°E to ~−168°W — coordinates are
// all within [−180, 180] but the polygon "wraps the long way" so Leaflet fills
// the entire northern hemisphere. Fix by unwrapping consecutive longitude jumps
// > 180°, turning −168° into +192° so the ring is one continuous eastward path.
function fixRing(ring) {
  if (ring.length < 2) return ring;
  const out = [[...ring[0]]];
  for (let i = 1; i < ring.length; i++) {
    let lng = ring[i][0];
    const prev = out[i - 1][0];
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
geojson.features.forEach(f => fixGeometry(f.geometry));

// Build container and initialise Leaflet after it's in the DOM
const mapContainer = document.createElement("div");
mapContainer.style.cssText = "height:560px;width:100%;border-radius:8px;overflow:hidden;";
display(mapContainer);
await new Promise(r => requestAnimationFrame(r));

const lmap = L.map(mapContainer, {
  center: [25, 10],
  zoom: 2,
  minZoom: 1,
  maxZoom: 6,
  zoomControl: true,
  attributionControl: false,
  scrollWheelZoom: false,
});

L.geoJSON(geojson, {
  style(feature) {
    const iso3 = feature.properties?.a3 ?? numericToIso3[+feature.id];
    const d = iso3 ? dataByIso3.get(iso3) : null;
    return {
      fillColor:   d ? getColor(d.n) : "#f0f0f0",
      fillOpacity: 1,
      color:       "#ccc",
      weight:      0.5,
    };
  },
  onEachFeature(feature, layer) {
    const iso3 = feature.properties?.a3 ?? numericToIso3[+feature.id];
    const d = iso3 ? dataByIso3.get(iso3) : null;
    if (d) {
      layer.bindTooltip(
        `<strong>${d.country}</strong><br>N = ${d.n.toLocaleString()}`,
        { sticky: true }
      );
      layer.on("mouseover", function() { this.setStyle({ fillOpacity: 0.75, weight: 1.5, color: "#333" }); });
      layer.on("mouseout",  function() { this.setStyle({ fillOpacity: 1,    weight: 0.5, color: "#ccc"  }); });
    }
  },
}).addTo(lmap);

// Inline legend (bottom-right)
const legend = L.control({ position: "bottomright" });
legend.onAdd = () => {
  const div = L.DomUtil.create("div");
  div.style.cssText = [
    "background:rgba(255,255,255,0.92)",
    "padding:10px 14px",
    "border-radius:7px",
    "font:13px/1.8 system-ui,sans-serif",
    "color:#333",
    "box-shadow:0 2px 8px rgba(0,0,0,.15)",
  ].join(";");
  div.innerHTML =
    `<div style="font-size:11px;font-weight:600;letter-spacing:.06em;text-transform:uppercase;margin-bottom:4px;color:#666">Sample size</div>` +
    binDefs.map(b =>
      `<div><span style="display:inline-block;width:13px;height:13px;background:${b.color};` +
      `border-radius:2px;margin-right:7px;vertical-align:middle;border:1px solid rgba(0,0,0,.1)"></span>${b.label}</div>`
    ).join("");
  return div;
};
legend.addTo(lmap);
```

---

## Quick Stats

```js
const schwartzSubscales = new Set([
  "Achievement", "Benevolence", "Conformity", "Hedonism", "Power",
  "Security", "Self_Direction", "Stimulation", "Tradition", "Universalism",
]);
const scales = [...new Set(desc.filter(d => d.Group !== "Overall").map(d => d.Scale))]
  .filter(s => !schwartzSubscales.has(s))
  .sort();
```

```js
const alpha = FileAttachment("data/alpha_results.csv").csv({ typed: true });
```

```js
const overallAlpha = alpha.filter(d => d.Country === "Overall" && !schwartzSubscales.has(d.Scale));
const avgAlpha = overallAlpha.reduce((s, d) => s + d.Alpha, 0) / overallAlpha.length;

const statsDiv = document.createElement("div");
statsDiv.style.cssText = "display:grid;grid-template-columns:repeat(4,1fr);gap:1rem;text-align:center;";

const stats = [
  { label: "Countries", value: countries.length },
  { label: "Total Participants", value: totalN.toLocaleString() },
  { label: "Scales Measured", value: scales.length },
  { label: "Avg. Cronbach's α", value: avgAlpha.toFixed(3) },
];

for (const s of stats) {
  const cell = document.createElement("div");
  cell.className = "card";
  cell.innerHTML = `
    <div style="font-size:2rem;font-weight:700;color:var(--theme-foreground-focus);">${s.value}</div>
    <div style="font-size:.9rem;color:var(--theme-foreground-muted);">${s.label}</div>
  `;
  statsDiv.appendChild(cell);
}
display(statsDiv);
```

---

_Data collected as part of the Psychological Science Accelerator project JTF002. Analyses run in R using lavaan for structural equation modelling._
