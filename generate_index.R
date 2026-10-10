# 1. Set the directory path where your HTML files are located
# (Leave as "." if running from the same directory, or change to your specific folder path)
target_dir <- "." 

# 2. Find all HTML files in the folder (including subfolders)
html_files <- list.files(
  path = target_dir, 
  pattern = "\\.html$", 
  recursive = TRUE, 
  ignore.case = TRUE
)

# 3. Exclude 'index.html' itself so it doesn't list itself in the links
html_files <- html_files[html_files != "index.html"]

# 4. Generate the HTML code for the index file
html_content <- c(
  "<!DOCTYPE html>",
  "<html lang='en'>",
  "<head>",
  "    <meta charset='UTF-8'>",
  "    <meta name='viewport' content='width=device-width, initial-scale=1.0'>",
  "    <title>Google Slides HTML Assets Directory</title>",
  "    <style>",
  "        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; line-height: 1.6; max-width: 800px; margin: 40px auto; padding: 0 20px; color: #333; }",
  "        h1 { color: #24292e; border-bottom: 1px solid #eaecef; padding-bottom: 10px; }",
  "        p { color: #586069; font-style: italic; }",
  "        ul { list-style-type: none; padding: 0; }",
  "        li { background: #f6f8fa; margin: 8px 0; padding: 12px; border: 1px solid #e1e4e6; border-radius: 6px; }",
  "        a { color: #0366d6; text-decoration: none; font-weight: bold; display: block; }",
  "        a:hover { text-decoration: underline; }",
  "    </style>",
  "</head>",
  "<body>",
  "    <h1>My Interactive HTML Assets</h1>",
  "    <p>Right-click any link below and select 'Copy Link Address' to grab the live URL for Google Slides.</p>",
  "    <ul>"
)

# 5. Dynamically add each file as a list item link
if (length(html_files) == 0) {
  html_content <- c(html_content, "        <li>No HTML files found in this directory.</li>")
} else {
  for (file in html_files) {
    # Visual cleanup: display the file name nicely but point to the actual path
    clean_name <- gsub("_", " ", basename(file))
    link_item <- paste0("        <li><a href='", file, "' target='_blank'>🔗 ", clean_name, "</a></li>")
    html_content <- c(html_content, link_item)
  }
}

# 6. Close the HTML tags
html_content <- c(html_content, "    </ul>", "</body>", "</html>")

# 7. Write the file to your folder
output_path <- file.path(target_dir, "index.html")
writeLines(html_content, output_path)

cat("Successfully generated directory file at:", normalizePath(output_path), "\n")
