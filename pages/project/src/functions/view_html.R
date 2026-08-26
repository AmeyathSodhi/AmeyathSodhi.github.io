library(DT)
library(htmltools)

view_html <- function(
  object,
  rows = FALSE,
  show = 100,
  render_mode = "dark",
  font_color = "#00FF00",
  bg_color = "black",
  header_bg_color = "black",
  font_family = "system-ui, -apple-system, sans-serif", # NEW
  font_size = "13px",
  ...
) {
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("DT library not installed")
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("htmltools library not installed")
  }

  # Set defaults based on render_mode if colors not specified
  if (is.null(font_color)) {
    font_color <- ifelse(render_mode == "dark", "white", "black")
  }
  if (is.null(bg_color)) {
    bg_color <- ifelse(render_mode == "dark", "black", "white")
  }
  if (is.null(header_bg_color)) {
    header_bg_color <- ifelse(render_mode == "dark", "#333", "#f0f0f0")
  }

  # Calculate alternating row color based on bg_color
  row_alt_color <- if (render_mode == "dark") "grey10" else "#f5f5f5"
  row_hover_color <- if (render_mode == "dark") "grey15" else "#e8e8e8"
  input_bg_color <- if (render_mode == "dark") "black" else "white"

  if (inherits(object, "tbl_df")) {
    object <- as.data.frame(object)
  }

  apply_custom_theme <- function(datatable_object) {
    css_template <- sprintf(
      "
      .dataTables_wrapper {
        background-color: %s !important;
        color: %s !important;
        font-family: %s !important;
        font-size: %s !important;
      }
      table.dataTable {
        font-family: %s !important;
        font-size: %s !important;
      }
      table.dataTable thead {
        background-color: %s !important;
        color: %s !important;
      }
      table.dataTable tbody {
        background-color: %s !important;
        color: %s !important;
      }
      table.dataTable tbody tr {
        background-color: %s !important;
        color: %s !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: %s !important;
      }
      table.dataTable tbody tr:hover {
        background-color: %s !important;
      }
      table.dataTable tfoot {
        background-color: %s !important;
        color: %s !important;
      }
      .dataTables_filter input {
        background-color: %s !important;
        color: %s !important;
        font-family: %s !important;
        font-size: %s !important;
      }
      .dataTables_filter label {
        color: %s !important;
        font-family: %s !important;
        font-size: %s !important;
      }
      .dataTables_length select {
        background-color: %s !important;
        color: %s !important;
        font-family: %s !important;
        font-size: %s !important;
      }
      .dataTables_length label {
        color: %s !important;
        font-family: %s !important;
        font-size: %s !important;
      }
      table.dataTable td,
      table.dataTable th {
        color: %s !important;
        font-family: %s !important;
        font-size: %s !important;
      }
      .dataTables_info, .dataTables_paginate {
        font-family: %s !important;
        font-size: %s !important;
      }
    ",

      bg_color,
      font_color,
      font_family,
      font_size, # wrapper
      font_family,
      font_size, # table
      header_bg_color,
      font_color, # thead
      bg_color,
      font_color, # tbody
      bg_color,
      font_color, # tbody tr
      row_alt_color, # even rows
      row_hover_color, # hover
      header_bg_color,
      font_color, # tfoot
      input_bg_color,
      font_color,
      font_family,
      font_size, # filter input
      font_color,
      font_family,
      font_size, # filter label
      input_bg_color,
      font_color,
      font_family,
      font_size, # length select
      font_color,
      font_family,
      font_size, # length label
      font_color,
      font_family,
      font_size, # cells
      font_family,
      font_size # info/pagination
    )

    datatable_object |>
      htmlwidgets::prependContent(
        tags$style(HTML(css_template))
      )
  }

  if (is.null(dim(object)) && is.list(object) && !is.data.frame(object)) {
    message("Object is a list. Viewer displays last list element.")
    x <- object[[length(object)]]

    dt <- datatable(
      as.data.frame(x),
      rownames = rows,
      options = list(
        pageLength = show,
        searching = TRUE,
        dom = 'lfrtip',
        scrollX = TRUE
      )
    )

    if (render_mode != "light" || !is.null(font_color) || !is.null(bg_color)) {
      dt <- apply_custom_theme(dt)
    }
    return(dt)
  }

  dt <- datatable(
    as.data.frame(object),
    rownames = rows,
    options = list(
      pageLength = show,
      searching = TRUE,
      dom = 'lfrtip',
      scrollX = TRUE
    )
  )

  if (render_mode != "light" || !is.null(font_color) || !is.null(bg_color)) {
    dt <- apply_custom_theme(dt)
  }

  return(dt)
}


view_html_old <- function(object, rows = F, show = 100, ...) {
  if (!require(DT)) {
    stop("DT library not installed. Please install first.")
  } else {
    if (tibble::is_tibble(object)) {
      object <- as.data.frame(object)
      # message("converted tibble to dataframe for viewing")
    }

    if (is.null(dim(object)) & class(object) == "list") {
      message(
        "Object is a list. Viewer displays last list element. Consider passing each element to view()."
      )

      lapply(object, function(x) {
        DT::datatable(
          x,
          rownames = rows,
          filter = "top",
          options = list(pageLength = show)
        )
      })
    } else {
      DT::datatable(
        object,
        rownames = rows,
        filter = "top",
        options = list(pageLength = show)
      )
    }
  }
}
