# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Quarto-based research project analyzing wage share in value added (partage de la valeur ajoutée) through international comparisons. The project generates a reproducible research document published as a website at <https://xtimbeau.github.io/travail>.

**Language:** All content is in French. Code comments, commit messages, and documentation should be in French.

## Environment Setup

**R Version:** 4.5.2 (or higher)
**Quarto Version:** Must be >= 1.8.25

### Dependency Management

The project uses `renv` for package management. To set up:

```r
renv::restore()
```

Key GitHub packages that must be installed via `renv::install()` or `devtools::install_github()`:
- `ofce/ofce`
- `rstudio/gt`
- `nsgrantham/ggbraid`
- `expersso/OECD`
- `InseeFrLab/melodi`
- `jimjam-slam/ggflags`

## Project Structure

```
.
├── index.qmd           # Main document (working paper 2025-23)
├── annexes.qmd         # Appendices
├── NEWS.qmd            # Change history
├── seminaire_lunch_OFCE.qmd  # Presentation slides
├── blog.qmd            # Blog post version
├── _quarto.yml         # Quarto website configuration
├── rinit.r             # R initialization (loaded by all .qmd files)
├── R/                  # Data processing scripts
│   ├── va.r            # Main value added calculations
│   ├── vaq.r           # Quarterly value added data
│   ├── france-melodi.R # French data from Melodi package
│   └── ...             # Other data processing scripts
└── data/               # Downloaded/cached data files
```

## Development Workflow

### Rendering the Document

```bash
# Render main document
quarto render index.qmd

# Render entire website
quarto render

# Preview with auto-reload
quarto preview
```

### Data Processing

All R scripts in `R/` are executed using `ofce::source_data()`, which:
- Caches execution results automatically
- Hides execution output
- Returns data as a named list

**Important:** Each R script in `R/` must end with `return(list(name1 = obj1, name2 = obj2, ...))` to export data objects.

To refresh cached data before rendering:

```r
source("rinit.r")
ofce::source_data_refresh()
```

Then render the document.

### R Script Architecture

R scripts follow a consistent pattern:

1. Load required packages
2. Source dependencies via `source_data("script_name.r")$object_name`
3. Fetch data from Eurostat, OECD, or other sources
4. Process and transform data
5. Return named list of objects: `return(list(data1 = df1, data2 = df2))`

Example from `va.r`:
```r
# Dependencies
marchand <- source_data("nace.r")$marchand
naa_e <- source_data("naa_e")$naa_e

# Data fetching
naa_a20 <- get_eurostat("nama_10_a64", filters = list(...))

# Return results
return(list(naa = naa_ext, naaa = naa_ext2))
```

### Initialization File (`rinit.r`)

Executed at the start of every `.qmd` file via `ofce::init_qmd()`. Sets:
- Knitr chunk options (no messages/warnings, SVG output, transparent background)
- Theme to `theme_ofce()` with custom settings
- Font to OpenSans from the `ofce` package
- Locale to French (`fr_FR`)
- Custom helper functions (`lbl()`, `milliards()`, etc.)
- Package conflict preferences via `conflicted::conflicts_prefer()`
- Global options for the `ofce` package (plot sizes, caption settings, data caching)

### Key Helper Functions (defined in `rinit.r`)

- `lbl(x, format=NULL)`: Converts country codes to French country names using `countrycode`
- `milliards(x, n_signif = 3L)`: Formats numbers as "X milliards d'euros" with French number formatting
- `cols_hide_pdf(tbl, col)`: Conditionally hides columns in gt tables for PDF output
- `my_tab_options(data, ...)`: Standard gt table styling

### Quarto Configuration (`_quarto.yml`)

- Project type: `website`
- Format: `ofce-html` (custom OFCE format extension)
- Freeze mode: `auto` (caches computations)
- Language: French
- Includes custom OFCE branding and navigation

## Data Sources

Primary data sources accessed via R packages:
- **Eurostat:** via `eurostat::get_eurostat()`
- **OECD:** via custom OECD package
- **French data:** via `melodi` package (INSEE data)
- **National accounts:** Eurostat tables `nama_10_a64`, `namq_10_a10`, `nama_10_gdp`, `namq_10_gdp`, `nasa_10_nf_tr`

## Output Files

The project generates:
- Website published to `_site/` (deployed to GitHub Pages)
- PDF working paper: `OFCEWP2025-23.pdf`
- Data export: `lsh.csv` (wage share time series)
- Frozen computation cache in `_freeze/`

## Common Tasks

### Refresh all data and rebuild
```r
source("rinit.r")
ofce::source_data_refresh()  # Clear cache and re-run all R scripts
```
Then render with `quarto render`

### Add a new data script
1. Create `R/newscript.r`
2. End with `return(list(data_name = data_object))`
3. Source in other scripts or .qmd files via: `source_data("R/newscript.r")$data_name`

### Modify plots
- All plots use `theme_ofce()` with custom settings from `rinit.r`
- Interactive plots use `ggiraph::geom_*_interactive()` with tooltip styling defined in `tooltip_css`
- Color palettes: `ccsummer()` and `ccjoy()` from `PrettyCols`

### Testing changes
Use `quarto preview` to see live updates while editing `.qmd` files

## Git Workflow

The repository is version controlled with Git. Main branch: `master`

Typical workflow:
```bash
git status
git add .
git commit -m "description"
git push origin master
```

Website auto-deploys to GitHub Pages on push to master.
