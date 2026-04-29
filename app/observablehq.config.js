export default {
  title: "DHF Cross-Cultural Study",
  root: "src",
  output: "../docs",
  pages: [
    { name: "Overview", path: "/" },
    { name: "Scale Reliability", path: "/reliability" },
    { name: "Country Comparisons", path: "/countries" },
  ],
  head: `
    <script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/leaflet@1.9.4/dist/leaflet.css">
    <script src="https://cdn.jsdelivr.net/npm/leaflet@1.9.4/dist/leaflet.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/topojson-client@3.1.0/dist/topojson-client.min.js"></script>
    <link rel="stylesheet" href="https://cdn.datatables.net/2.0.8/css/dataTables.dataTables.min.css">
    <style>
      :root { --theme-foreground-focus: #4e79a7; }
      .card { background: var(--theme-background-alt); border-radius: 8px; padding: 1.5rem; margin-bottom: 1.5rem; }
      table.dataTable thead th { background: var(--theme-background-alt); }
      .leaflet-container { background: #fff !important; }
    </style>
  `,
};
