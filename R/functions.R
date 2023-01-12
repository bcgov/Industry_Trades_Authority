read_clean_join <- function(pat) {
  read_xlsx(here("data", "current_ita", list.files(here("data", "current_ita"), pattern = pat))) %>%
    clean_names() %>%
    mutate(noc_code = as.character(noc_code)) %>%
    full_join(mapping, by = "noc_code") %>%
    rename(
      year = contains("year"),
      month = contains("month")
    )
}

filter_and_aggregate <- function(tbbl, var, type_of_filter, value = NULL, grp = NULL) {
  if (is.null(value)) {
    tbbl %>%
      group_by({{ grp }}, {{ var }}, year, month) %>%
      summarize(count = sum(count)) %>%
      na.omit()
  } else {
    if (type_of_filter == "equal") {
      tbbl %>%
        filter({{ var }} == value) %>%
        group_by({{ grp }}, {{ var }}, year, month) %>%
        summarize(count = sum(count)) %>%
        na.omit()
    } else if (type_of_filter == "not equal") {
      tbbl %>%
        filter({{ var }} != value) %>%
        group_by({{ grp }}, {{ var }}, year, month) %>%
        summarize(count = sum(count)) %>%
        na.omit()
    } else {
      stop("type of filter must be either equal or not equal")
    }
  }
}

just_aggregate <- function(tbbl, grp = NULL) {
  temp <- tbbl %>%
    group_by({{ grp }}, month, year) %>%
    summarize(count = sum(count)) %>%
    mutate(group = "total") %>%
    na.omit()
}

make_plt <- function(tbbl, label = NULL, smooth = TRUE, ttl) {
  gg <- tbbl %>%
    mutate(date = lubridate::ym(paste(year, str_sub(month, 1, 2), sep = "-"))) %>%
    ggplot(aes(date, count, colour = {{ label }})) +
    labs(title = ttl,
        caption = "Source: Industry Trades Authority") +
    ggthemes::theme_excel_new() +
    scale_y_continuous(labels = scales::comma)
  if (smooth == FALSE) {
    gg <- gg +
      geom_line()
  } else {
    gg <- gg +
      geom_line(alpha = .5) +
      geom_smooth(se = FALSE)
  }
  wrapR::fix_labs(gg)
}

wider_with_totals <- function(tbbl) {
  temp <- tbbl %>%
    pivot_wider(names_from = month, values_from = count) %>%
    mutate(year = as.character(year)) %>%
    adorn_totals(c("row", "col"))
  colnames(temp) <- wrapR::make_title(colnames(temp))
  return(temp)
}

filter_and_select <- function(tbbl, var, value) {
  temp <- tbbl %>%
    filter({{ var }} == value) %>%
    select(-construction_trades, -trades, -stc)
  colnames(temp) <- wrapR::make_title(colnames(temp))
  return(temp)
}

add_plot <- function(plt, sheet, wb) {
  print(plt)
  insertPlot(wb = wb, sheet = sheet, startCol = 10, startRow = 19)
}

addsheet <- function(sheetname, wb) {
  addWorksheet(wb = wb, sheetName = sheetname)
}

adjust_width <- function(sheet, wb) {
  setColWidths(wb = wb, sheet = sheet, cols = 1:8, widths = rep(14, 8))
}
