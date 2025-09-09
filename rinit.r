library(knitr)
opts_chunk$set(
  fig.pos="htb",
  out.extra="",
  dev="ragg_png",
  dev.args = list(bg = "transparent"),
  out.width="100%",
  fig.showtext=TRUE,
  message = FALSE,
  warning = FALSE,
  echo = FALSE)

library(tidyverse, quietly = TRUE)
library(ofce, quietly = TRUE)
library(showtext, quietly = TRUE)
library(gt, quietly = TRUE)
library(readxl, quietly = TRUE)
library(ggiraph, quietly = TRUE)
library(scales, quietly = TRUE)
library(glue, quietly = TRUE)
library(patchwork, quietly = TRUE)
library(downloadthis, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(quarto, quietly = TRUE)
library(qs, quietly = TRUE)
library(conflicted, quietly = TRUE)
library(countrycode, quietly = TRUE)

options(
  ofce.base_size = 12,
  ofce.background_color = "transparent",
  ofce.marquee = TRUE,
  ofce.caption.ofce = FALSE,
  ofce.caption.wrap = 0,
  ofce.source_data.src_in = "file",
  ofce.source_data.force_exec = FALSE)
showtext_opts(dpi = 120)
showtext_auto()
options(cli.ignore_unknown_rstudio_theme = TRUE)
tooltip_css  <-
  "font-family:Open Sans;
  background-color:snow;
  border-radius:5px;
  border-color:gray;
  border-style:solid;
  border-width:0.5px;
  font-size:9pt;
  padding:4px;
  box-shadow: 2px 2px 2px gray;
  r:20px;"

gdtools::register_gfont("Open Sans")

girafe_opts <- function(x, ...) girafe_options(
  x,
  opts_hover(css = "stroke-width:1px;", nearest_distance = 60),
  opts_tooltip(css = tooltip_css, delay_mouseover = 10, delay_mouseout = 3000)) |>
  girafe_options(...)

girafy <- function(plot, r=2.5, o = 0.5,  ...) {
  if(knitr::is_html_output()| interactive()) {
    girafe(ggobj = plot) |>
      girafe_options(
        opts_hover_inv(css = glue("opacity:{o};")),
        opts_hover(css = glue("r:{r}px;")),
        opts_tooltip(css = tooltip_css)) |>
      girafe_options(...)
  } else {
    plot
  }
}

milliards <- function(x, n_signif = 3L) {
  stringr::str_c(
    format(
      x,
      digits = n_signif,
      big.mark = " ",
      decimal.mark = ","),
    " milliards d'euros")
}

if(.Platform$OS.type=="windows")
  Sys.setlocale(locale = "fr_FR.utf8") else
    Sys.setlocale(locale = "fr_FR")

margin_download <- function(data, output_name = "donnees", label = "données") {
  if(knitr::is_html_output()) {
    if(lobstr::obj_size(data)> 1e+5)
      cli::cli_alert("la taille de l'objet est supérieure à 100kB")
    fn <- str_c("travail-2025-", tolower(output_name))
    downloadthis::download_this(
      data,
      icon = "fa fa-download",
      class = "dbtn",
      button_label  = label,
      output_name = fn)
  } else
    return(invisible(NULL))
}

inline_download <- function(data, label = "données", output_name = "donnees") {
  downloadthis::download_this(
    data,
    icon = "fa fa-download",
    class = "dbtn-inline",
    button_label  = label,
    output_name = output_name
  )
}

ccsummer <- function(n=4) PrettyCols::prettycols("Summer", n=n)
ccjoy <- function(n=4) PrettyCols::prettycols("Joyful", n=n)

bluish <- ccjoy()[1]
redish <- ccjoy()[2]
yelish <- ccsummer()[2]
greenish <- ccsummer()[4]
darkgreenish <- ccsummer()[3]
darkbluish <- ccjoy()[4]

lbl <- function(x, format=NULL) {
  if(is.null(format))
    if(is.null(dim(x)))
      fmt <- ifelse(max(stringr::str_length(x))==2, "eurostat", "iso3c")
    else
      fmt <- ifelse(max(stringr::str_length(x[,1]))==2, "eurostat", "iso3c")
  else
    fmt <- format
  if(is.null(dim(x)))
    return(countrycode(x, fmt, "country.name.fr"))
  x |>
    mutate(across(1, ~countrycode(.x, fmt, "country.name.fr")))
}

date_trim <- function(date) {
  str_c("T", lubridate::quarter(date), " ", lubridate::year(date))
}

date_mois <- function(date) {
  str_c(lubridate::month(date,label = TRUE, abbr = FALSE), " ", lubridate::year(date))
}

date_jour <- function(date) {
  str_c(lubridate::day(date), " ", lubridate::month(date,label = TRUE, abbr = FALSE), " ", lubridate::year(date))
}

date1 <- function(x) {
  fnan <- which(!is.na(x)) |> min()
  r <- stringr::str_sub(x, 3)
  r[[fnan]] <- stringr::str_c(x[fnan])
  return(r)
}

date1d <- function(x) date1(year(x))

dates_breaks <- function(by=4) {
  function(x) {
    seq(ceiling(x[[1]]),x[[2]], by = by ) }
}
conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::lag, .quiet = TRUE)
conflicted::conflicts_prefer(lubridate::year, .quiet = TRUE)
conflicted::conflicts_prefer(lubridate::month, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::first, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::last, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::between, .quiet = TRUE)
conflicted::conflicts_prefer(lubridate::quarter, .quiet = TRUE)
