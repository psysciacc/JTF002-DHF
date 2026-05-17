---
title: Country Comparisons
---

# Country Comparisons

Explore how mean scale scores vary across countries. Points show the country mean; error bars show ±1 SD.

```js
const desc = FileAttachment("data/scale_descriptives.csv").csv({ typed: true });
```

```js
const schwartzSubscales = new Set([
  "Achievement", "Benevolence", "Conformity", "Hedonism", "Power",
  "Security", "Self_Direction", "Stimulation", "Tradition", "Universalism",
]);

const allScales = [...new Set(desc.filter(d => d.Group !== "Overall").map(d => d.Scale))]
  .filter(s => !schwartzSubscales.has(s))
  .sort();

const scaleDomainMap = {
  "Other_Face_Concern": "Face & Conflict",
  "Self_Face_Concern": "Face & Conflict",
  "Withdrawal": "Face & Conflict",
  "Retaliation": "Face & Conflict",
  "Humor": "Face & Conflict",
  "Positive_Reciprocity": "Violence Acceptance",
  "Negative_Reciprocity": "Violence Acceptance",
  "Penal_Code_Violence": "Violence Acceptance",
  "War_Acceptance": "Violence Acceptance",
  "Corporal_Punishment": "Violence Acceptance",
  "Intimate_Violence": "Violence Acceptance",
  "Total_Violence_Scale": "Violence Acceptance",
  "MFQ_Care": "Moral Foundations",
  "MFQ_Equality": "Moral Foundations",
  "MFQ_Proportionality": "Moral Foundations",
  "MFQ_Loyalty": "Moral Foundations",
  "MFQ_Authority": "Moral Foundations",
  "MFQ_Purity": "Moral Foundations",
  "Conservation": "Schwartz Values",
  "Openness_to_Change": "Schwartz Values",
  "Self_Enhancement": "Schwartz Values",
  "Self_Transcendence": "Schwartz Values",
  "Intrinsic_Religiosity": "Other",
  "Composite_Religiosity": "Other",
  "Subjective_Wellbeing": "Other",
};
```

## Select a Scale

```js
const scaleInput = Inputs.select(allScales, {
  label: "Scale",
  value: "Other_Face_Concern",
  format: s => s.replace(/_/g, " "),
});
const selectedScale = view(scaleInput);
```

```js
const flipCountries = view(Inputs.toggle({ label: "Flip axes" }));
```

```js
const domainColors = {
  "Face & Conflict": "#4e79a7",
  "Violence Acceptance": "#e15759",
  "Moral Foundations": "#59a14f",
  "Schwartz Values": "#f28e2b",
  "Other": "#9c755f",
};

const filtered = desc
  .filter(d => d.Scale === selectedScale && d.Group !== "Overall" && typeof d.Mean === "number")
  .sort((a, b) => a.Mean - b.Mean);

const domain = scaleDomainMap[selectedScale] ?? "Other";
const color = domainColors[domain];

const fg = getComputedStyle(document.documentElement).getPropertyValue("--theme-foreground").trim() || "#333";

const countryWrapper = document.createElement("div");
countryWrapper.style.cssText = "overflow-x:auto;";
const dotDiv = document.createElement("div");
countryWrapper.appendChild(dotDiv);

if (filtered.length === 0) {
  dotDiv.textContent = "No data available for this scale.";
} else {
  const countryPlotWidth = flipCountries ? Math.max(900, filtered.length * 14 + 80) : null;
  Plotly.newPlot(dotDiv, [
    {
      type: "scatter",
      mode: "markers",
      x: flipCountries ? filtered.map(d => d.Group) : filtered.map(d => d.Mean),
      y: flipCountries ? filtered.map(d => d.Mean) : filtered.map(d => d.Group),
      ...(flipCountries
        ? { error_y: { type: "data", array: filtered.map(d => d.SD), visible: true, color: color + "80", thickness: 1.5, width: 4 } }
        : { error_x: { type: "data", array: filtered.map(d => d.SD), visible: true, color: color + "80", thickness: 1.5, width: 4 } }),
      marker: {
        color,
        size: 10,
        line: { color: fg, width: 1.5 },
      },
      hovertemplate: flipCountries
        ? "<b>%{x}</b><br>Mean = %{y:.3f}<br>SD = %{error_y.array:.3f}<br>" + `N = %{customdata}<extra>${selectedScale.replace(/_/g, " ")}</extra>`
        : "<b>%{y}</b><br>Mean = %{x:.3f}<br>SD = %{error_x.array:.3f}<br>" + `N = %{customdata}<extra>${selectedScale.replace(/_/g, " ")}</extra>`,
      customdata: filtered.map(d => d.N.toLocaleString()),
      name: selectedScale.replace(/_/g, " "),
    },
  ], {
    title: {
      text: `${selectedScale.replace(/_/g, " ")} — Mean by Country (±1 SD)`,
      font: { size: 15, color: fg },
    },
    xaxis: {
      title: { text: flipCountries ? "Country" : "Mean Score", font: { color: fg } },
      tickfont: { size: 10, color: fg },
      automargin: true,
      gridcolor: "rgba(128,128,128,0.15)",
      zeroline: false,
      ...(flipCountries ? { tickangle: -45 } : {}),
    },
    yaxis: {
      title: flipCountries ? { text: "Mean Score", font: { color: fg } } : undefined,
      tickfont: { size: 10, color: fg },
      automargin: true,
      gridcolor: "rgba(128,128,128,0.15)",
    },
    margin: flipCountries ? { t: 60, b: 180, l: 60, r: 30 } : { t: 60, b: 60, l: 180, r: 30 },
    height: flipCountries ? 600 : Math.max(500, filtered.length * 14 + 80),
    ...(countryPlotWidth ? { width: countryPlotWidth } : {}),
    plot_bgcolor: "transparent",
    paper_bgcolor: "transparent",
    showlegend: false,
  }, { responsive: !flipCountries, displayModeBar: true });
}
display(countryWrapper);
```

---

## Descriptives Table

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

const descAll = desc.filter(d => d.Group !== "Overall" && typeof d.Mean === "number");

const dtWrapper = document.createElement("div");
dtWrapper.style.cssText = "overflow-x:auto;";
const dtTbl = document.createElement("table");
dtTbl.id = "desc-table";
dtTbl.className = "display";
dtTbl.style.cssText = "width:100%;font-size:.88rem;";
dtTbl.innerHTML = `
  <thead>
    <tr>
      <th>Country</th>
      <th>Scale</th>
      <th>Domain</th>
      <th>N</th>
      <th>Mean</th>
      <th>SD</th>
      <th>Min</th>
      <th>Max</th>
    </tr>
  </thead>
  <tbody>
    ${descAll.map(d => `
      <tr>
        <td>${d.Group}</td>
        <td>${d.Scale.replace(/_/g, " ")}</td>
        <td>${scaleDomainMap[d.Scale] ?? "Other"}</td>
        <td>${d.N.toLocaleString()}</td>
        <td>${d.Mean.toFixed(3)}</td>
        <td>${d.SD != null ? d.SD.toFixed(3) : "—"}</td>
        <td>${d.Min != null ? d.Min.toFixed(3) : "—"}</td>
        <td>${d.Max != null ? d.Max.toFixed(3) : "—"}</td>
      </tr>
    `).join("")}
  </tbody>
`;
dtWrapper.appendChild(dtTbl);
display(dtWrapper);

window.$(dtTbl).DataTable({
  pageLength: 25,
  order: [[0, "asc"]],
  language: { search: "Filter:" },
  columnDefs: [{ targets: [3, 4, 5, 6, 7], className: "dt-right" }],
});
```
