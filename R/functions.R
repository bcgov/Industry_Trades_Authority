read_clean_join <- function(pat) {
  temp <- read_xlsx(here("current_data", "ita", list.files(here("current_data", "ita"), pattern = pat))) %>%
    clean_names() %>%
    mutate(noc_code = as.character(noc_code_2021)) %>%
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
    na.omit()%>%
    arrange(year)
}

make_plt <- function(tbbl, label = NULL, smooth = TRUE, title=NULL) {
  gg <- tbbl %>%
    mutate(date = lubridate::ym(paste(year, str_sub(month, 1, 2), sep = "-"))) %>%
    ggplot(aes(date, count, colour = {{ label }})) +
    labs(title = title,
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
    mutate(year = as.character(year))|>
    adorn_totals(c("row", "col"))
  colnames(temp) <- wrapR::make_title(colnames(temp))
  return(temp)
}

filter_and_select <- function(tbbl, var, value) {
  tbbl %>%
    filter({{ var }} == value) %>%
    select(-construction_trades, -trades, -stc, -non_construction_trades)
}

add_plot <- function(plt, sheet, wb) {
  print(plt)
  insertPlot(wb = wb, sheet = sheet, startCol = 10, startRow = 25)
}

addsheet <- function(sheetname, wb) {
  addWorksheet(wb = wb, sheetName = sheetname)
}

adjust_width <- function(sheet, wb) {
  setColWidths(wb = wb, sheet = sheet, cols = 1:8, widths = rep(14, 8))
}
get_growth <- function(tbbl, period, n){
  latest <- tbbl%>%
    filter(date==max(date))%>%
    pull(count)
  yago <- tbbl%>%
    filter(date==max(date)-period(n))%>%
    pull(count)
  latest/yago-1
}

fcast_growth <- function(tbbl, plus, minus){
  p <- tbbl%>%
    filter(year==plus)%>%
    pull(value)
  m <- tbbl%>%
    filter(year==minus)%>%
    pull(value)
  (p/m)^(1/(plus-minus))-1
}

describe_change <- function(num){
  case_when(
    num > .01 ~ paste0(abs_per(num)," higher"),
    num < -.01 ~ paste0(abs_per(num)," lower"),
    TRUE ~ "unchanged"
  )
}

abs_per <- function(num){
  scales::percent(abs(num), accuracy=1)
}
fix_dates <- function(tbbl){
  tbbl%>%
    ungroup()%>%
    mutate(month=str_sub(month,1,2),
           date=lubridate::ym(paste(year, month, sep="/")))%>%
    arrange(date)
}

forecast_plot <- function(tbbl, grp){
  tbbl <- tbbl%>%
    filter(group==grp)
  text_y <- (min(tbbl$forecast)+max(tbbl$forecast))/2
  text_x <- (rect_df$xmin+rect_df$xmax)/2
  ggplot()+
    geom_rect(data=rect_df, mapping=aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf), alpha=.1)+
    geom_line(data=filter(tbbl, group==grp), mapping=aes(year, forecast, group=1))+
    annotate("text", x=text_x, y=text_y, label="Forecast")+
    scale_y_continuous(labels=scales::comma)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 16))+
    labs(x="",y="",colour="")+
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))+
    theme(text = element_text(size = 20))
}

decomp_plot <- function(tbbl){
  components <- tbbl%>%
    mutate(date=yearmonth(date))%>%
    as_tsibble(index=date)%>%
    model(stl = STL(count))%>%
    components()%>%
    select(date, count, trend)%>%
    pivot_longer(cols=-date)%>%
    mutate(date=lubridate::ym(date))
  x_breaks <- as_date(seq(floor_date(min(components$date),"year"), ceiling_date(max(components$date),"year"), by="6 months"))
  ggplot(components, aes(date, value, alpha=name))+
    geom_line(show.legend = FALSE)+
    scale_y_continuous(labels=scales::comma)+
    scale_x_date(breaks=x_breaks, date_labels = "%b %Y")+
    labs(x="",y="")+
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))+
    theme(text = element_text(size = 20))
}

fix_column_names <- function(tbbl){
  tbbl%>%
    rename(
      "Job openings: {lmo_edition}-{plus_ten}":=ten_year_jo,
      "Employment: {lmo_edition}":=emp_now,
      "Employment: {plus_five}":=emp_five,
      "Employment: {plus_ten}":=emp_ten,
      "Growth: {lmo_edition}-{plus_five}":=ffy_cagr,
      "Growth: {plus_five}-{plus_ten}":= sfy_cagr,
      "Growth: {lmo_edition}-{plus_ten}":= ty_cagr)
}





