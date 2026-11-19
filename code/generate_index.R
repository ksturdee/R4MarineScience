pages <- data.frame(
  href = c(
    "docs/workshop_01_penguins/penguin_analysis.html",
    "docs/workshop_02_datarescue/workshop_02_estuary_data_rescue.html",
    "docs/workshop_03_PlotDeconstruction/workshop_03_PlotDeconstruction.html",
    "docs/workshop_03_QLDdataDiscovery/workshop_03_qfish_data_discovery.html",
    "outputs/workshop4/copepod-richness-sst.png",
    "outputs/workshop4/copepod-richness-interactive.html"
  ),
  label = c(
    "Workshop 1 | Data Exploration and Visualisation",
    "Workshop 2 | Estuary Data Cleaning and Integration",
    "Workshop 3 | Scientific Figure Reconstruction",
    "Workshop 3 | Queensland Fisheries Data Discovery",
    "Workshop 4 | Copepod Richness and Sea-Surface Temperature",
    "Workshop 4 | Interactive Copepod Richness Map"
  ),
  stringsAsFactors = FALSE
)

html_content <- c(
  "<!DOCTYPE html>",
  "<html lang=\"en\">",
  "<head>",
  "  <meta charset=\"utf-8\">",
  "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "  <title>Marine Science | R Analysis &amp; Visualisation</title>",
  "  <style>",
  "    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; line-height: 1.6; max-width: 800px; margin: 40px auto; padding: 0 20px; color: #333; }",
  "    h1 { color: #24292e; border-bottom: 1px solid #eaecef; padding-bottom: 10px; }",
  "    p { color: #586069; }",
  "    ul { list-style: none; padding: 0; }",
  "    li { background: #f6f8fa; margin: 8px 0; padding: 12px; border: 1px solid #e1e4e8; border-radius: 6px; }",
  "    a { color: #0366d6; text-decoration: none; font-weight: bold; }",
  "    a:hover { text-decoration: underline; }",
  "  </style>",
  "</head>",
  "<body>",
  "  <h1>Marine Science: R Analysis &amp; Visualisation</h1>",
  "  <p>A collection of reproducible marine science analyses, scientific visualisations, and interactive spatial maps developed in R for MB5370: Techniques in Marine Science 1 at James Cook University.</p>",
  "  <ul>"
)

for (i in seq_len(nrow(pages))) {
  html_content <- c(
    html_content,
    sprintf(
      "    <li><a href=\"%s\">%s</a></li>",
      pages$href[[i]],
      pages$label[[i]]
    )
  )
}

html_content <- c(html_content, "  </ul>", "</body>", "</html>")
writeLines(html_content, "index.html", useBytes = TRUE)
