library(shiny)
library(data.table)
library(ggplot2)
library(colourpicker)
library(shinymanager)
library(readxl)
library(DT)
library(plotly)

# --- Labels (hergebruikt in titels/legenda) ---
TYPE_LABELS  <- c(allcause = "All-cause", covid = "COVID", cancer_cvd = "Cancer/CVD")
GROUP_LABELS <- c(dialyse = "Dialyse", transplant = "Transplantatie")

# --- Kolomnaam-helpers (nieuwe datastructuur) ---
# SMR:      smr_{groep}_{outcome}_{citype}      (waarde identiek over citypes)
# CI:       ci_diff_{groep}_{outcome}_{citype}  -> CI = smr +/- ci_diff
# Mort.rate: mort_ratio_{groep}_{outcome}, met 1 uitzondering hieronder
smr_colname  <- function(g, type, ci) sprintf("smr_%s_%s_%s", g, type, ci)
diff_colname <- function(g, type, ci) sprintf("ci_diff_%s_%s_%s", g, type, ci)
mr_colname   <- function(g, type, available = NULL) {
  primary <- sprintf("mort_ratio_%s_%s", g, type)            # bv. mort_ratio_transplant_cancer_cvd
  if (!is.null(available) && primary %in% available) return(primary)
  # In de huidige CSV mist transplant-kanker het _cvd-achtervoegsel;
  # val terug op die naam als de correcte (nog) niet bestaat.
  if (g == "transplant" && type == "cancer_cvd" &&
      (is.null(available) || "mort_ratio_transplant_cancer" %in% available)) {
    return("mort_ratio_transplant_cancer")
  }
  primary
}

# --- Stringency data ---
stringency <- data.table(
  date = as.Date(c(
    "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
    "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01",
    "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
    "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01", "2021-09-01",
    "2021-10-01", "2021-11-01", "2021-12-01", "2022-01-01", "2022-02-01",
    "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01", "2022-07-01",
    "2022-08-01", "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01"
  )),
  stringency = c(1, 47, 79, 74, 61, 45, 48, 51, 62, 62, 71,
                 80, 80, 75, 74, 68, 61, 37, 42, 42, 37, 43,
                 58, 58, 41, 24, 18, 16, 16, 16, 16, 14, 11, 11, 11)
)

# --- COVID rioolwater (landelijk): maandgemiddelde van RNA_flow_per_100000, in x10^12 ---
WW_FILE <- "COVID-19_rioolwaterdata_landelijk.xlsx"   # naast app.R plaatsen
.ww_empty <- data.table(date = as.Date(character(0)), value = numeric(0))
.ww_load <- function() {
  if (!file.exists(WW_FILE)) {
    return(list(data = .ww_empty,
                msg = paste0("Rioolwaterbestand niet gevonden: ",
                             normalizePath(file.path(getwd(), WW_FILE), mustWork = FALSE),
                             "  (werkmap: ", getwd(), ")")))
  }
  tryCatch({
    ww <- as.data.table(readxl::read_excel(WW_FILE))
    if (!all(c("Date_measurement", "RNA_flow_per_100000") %in% names(ww))) {
      return(list(data = .ww_empty,
                  msg = paste0("Verwachte kolommen ontbreken. Aanwezig: ",
                               paste(names(ww), collapse = ", "))))
    }
    ww <- ww[!is.na(Date_measurement) & !is.na(RNA_flow_per_100000)]
    ww[, ym := format(as.Date(Date_measurement), "%Y-%m")]
    mon <- ww[, .(value = mean(RNA_flow_per_100000, na.rm = TRUE) / 1e12), by = ym]
    mon[, date := as.Date(paste0(ym, "-01"))]
    setorder(mon, date)
    if (nrow(mon) == 0) return(list(data = .ww_empty, msg = "Rioolwaterbestand bevat geen bruikbare rijen."))
    list(data = mon[, .(date, value)], msg = NULL)
  }, error = function(e) {
    list(data = .ww_empty, msg = paste0("Fout bij inlezen rioolwater: ", conditionMessage(e)))
  })
}
.ww <- .ww_load()
rioolwater_monthly <- .ww$data
rioolwater_msg     <- .ww$msg

# --- Vaccinatiegraad: eerste dag van elke maand, 3 kolommen ---
VACC_FILE <- "20260713_Vaccination_percentage_by_day.xlsx"
.vacc_load <- function() {
  if (!file.exists(VACC_FILE)) {
    # Toon ook welke xlsx-bestanden wél aanwezig zijn zodat naam-mismatch duidelijk is
    xlsxs <- list.files(getwd(), pattern = "\\.xlsx$", ignore.case = TRUE)
    found_str <- if (length(xlsxs) > 0) paste(xlsxs, collapse = ", ") else "(geen .xlsx bestanden gevonden)"
    return(list(data = NULL, msg = paste0(
      "Vaccinatiebestand niet gevonden: '", VACC_FILE, "'  ",
      "(werkmap: ", getwd(), ")  |  ",
      "Aanwezige .xlsx bestanden: ", found_str)))
  }
  # Check tab-namen voor duidelijkere fout als sheet ontbreekt
  sheets <- tryCatch(readxl::excel_sheets(VACC_FILE), error = function(e) character(0))
  sheet  <- "20260713_Vaccination_percentage"
  if (!sheet %in% sheets) {
    return(list(data = NULL, msg = paste0(
      "Tabblad '", sheet, "' niet gevonden in '", VACC_FILE, "'.  ",
      "Beschikbare tabbladen: ", paste(sheets, collapse = ", "))))
  }
  tryCatch({
    raw <- as.data.table(readxl::read_excel(VACC_FILE, sheet = sheet))
    setnames(raw, c("date", "vaccin1", "vaccin2", "vaccin3_more"))
    raw[, date := as.Date(date)]
    mon <- raw[as.integer(format(date, "%d")) == 1,
               .(date, vaccin1, vaccin2, vaccin3_more)]
    list(data = mon, msg = NULL)
  }, error = function(e)
    list(data = NULL, msg = paste0("Fout bij laden vaccinatiedata: ", conditionMessage(e))))
}
.vacc   <- .vacc_load()
vacc_monthly <- .vacc$data
vacc_msg     <- .vacc$msg

# --- Excess deaths berekening: obs_allcause - maandgemiddelde 2016-2019 ---
# Geeft data.table met kolommen: date, pgroup, value (excess per maand)
calc_excess <- function(dt, groups, plot_years, baseline_years = 2016:2019) {
  rbindlist(lapply(groups, function(g) {
    col <- sprintf("obs_%s_allcause", g)
    if (!col %in% names(dt)) return(NULL)
    bl <- dt[year %in% baseline_years & !is.na(get(col)),
             .(bl_mean = mean(get(col), na.rm = TRUE)), by = month]
    d  <- dt[year %in% plot_years & !is.na(get(col)),
             .(date, month, obs = get(col))]
    d  <- bl[d, on = "month"]
    d[, .(date, pgroup = g, value = obs - bl_mean)]
  }), fill = TRUE)
}

# --- Plot functions ---

plot_smr <- function(dt,
                     groups = "dialyse",
                     type = c("allcause", "covid", "cancer_cvd"),
                     show_baseline = TRUE,
                     baseline_range = TRUE,
                     baseline_only = FALSE,
                     show_ci = TRUE,
                     ci_type = c("O_E", "E", "O"),
                     show_waves = TRUE,
                     sec_type = "none",
                     alpha_waves = 0.15,
                     border_width = 0.5,
                     bold_axis_text = FALSE,
                     show_axis_titles = TRUE,
                     show_wave_legend = TRUE,
                     baseline_years = 2016:2019,
                     plot_years = 2020:2023,
                     group_cols = c(dialyse = "steelblue"),
                     col_ci = NULL,
                     col_baseline_mean = "grey50",
                     col_baseline_range = "grey70",
                     col_waves = NULL,
                     sec_data = NULL,
                     sec_lo = 0,
                     sec_hi = 100,
                     sec_label = "Stringency Index",
                     sec_breaks = NULL,
                     sec_log = FALSE,
                     col_sec = "darkgreen",
                     title = NULL) {
  
  type    <- match.arg(type)
  ci_type <- match.arg(ci_type)
  n_groups <- length(groups)
  
  # --- Long-format data: 1 rij per groep x maand ---
  build_one <- function(g) {
    smr_col  <- smr_colname(g, type, ci_type)
    diff_col <- diff_colname(g, type, ci_type)
    d <- dt[year %in% plot_years & !is.na(get(smr_col)),
            .(date, month, wave,
              pgroup = g,
              smr     = get(smr_col),
              ci_low  = get(smr_col) - get(diff_col),
              ci_high = get(smr_col) + get(diff_col))]
    if (nrow(d) == 0) return(d)
    setorder(d, date)
    d[, gap_group := cumsum(c(1, diff(as.numeric(date)) > 35))]
    d[, line_grp := paste(pgroup, gap_group)]
    d[]
  }
  plot_dt <- rbindlist(lapply(groups, build_one), fill = TRUE)
  
  x_limits <- c(as.Date(sprintf("%d-01-01", min(plot_years))),
                as.Date(sprintf("%d-12-31", max(plot_years))))
  
  if (show_waves) {
    waves <- dt[!is.na(wave) & wave != "no covid period" & year %in% plot_years,
                .(start = min(date), end = max(date) + 31), by = wave]
    waves[, wave := factor(wave, levels = c("lockdown1", "inter-lockdown",
                                            "lockdown2", "after vacc & delta",
                                            "omicron"))]
  }
  
  if (show_baseline) {
    build_bl <- function(g) {
      smr_col <- smr_colname(g, type, ci_type)
      bl <- dt[year %in% baseline_years, .(
        bl_mean = mean(get(smr_col), na.rm = TRUE),
        bl_min  = min(get(smr_col),  na.rm = TRUE),
        bl_max  = max(get(smr_col),  na.rm = TRUE)
      ), by = month]
      out <- dt[year %in% plot_years, .(date, month)][bl, on = "month"][order(date)]
      out[, pgroup := g][]
    }
    baseline_plot <- rbindlist(lapply(groups, build_bl), fill = TRUE)
    baseline_plot <- baseline_plot[is.finite(bl_mean)]  # bv. covid pre-2020: geen baseline
  }
  
  sec_active <- sec_type != "none" && !is.null(sec_data) && nrow(sec_data) > 0 &&
    is.finite(sec_hi) && is.finite(sec_lo) && (sec_hi != sec_lo)
  
  if (sec_active && nrow(plot_dt) > 0) {
    smr_range <- range(plot_dt$smr, na.rm = TRUE)
    if (sec_log) {
      llo <- log10(sec_lo); lhi <- log10(sec_hi)
      scale_factor <- diff(smr_range) / (lhi - llo)
      if (!is.finite(scale_factor) || scale_factor == 0) scale_factor <- 1
      offset <- smr_range[1] - llo * scale_factor
      sec_plot <- copy(sec_data)[value > 0]
      sec_plot[, scaled := log10(value) * scale_factor + offset]
    } else {
      scale_factor <- diff(smr_range) / (sec_hi - sec_lo)
      if (!is.finite(scale_factor) || scale_factor == 0) scale_factor <- 1
      offset <- smr_range[1] - sec_lo * scale_factor
      sec_plot <- copy(sec_data)
      sec_plot[, scaled := value * scale_factor + offset]
    }
  }
  
  type_label <- TYPE_LABELS[[type]]
  grp_label  <- paste(GROUP_LABELS[groups], collapse = " & ")
  if (is.null(title)) title <- sprintf("SMR %s \u2013 %s (%s-%s)",
                                       type_label, grp_label,
                                       min(plot_years), max(plot_years))
  
  p <- ggplot()
  
  if (show_waves) {
    p <- p + geom_rect(data = waves,
                       aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = wave),
                       alpha = alpha_waves)
    if (!is.null(col_waves)) {
      p <- p + scale_fill_manual(values = col_waves, name = "COVID wave")
    } else {
      p <- p + scale_fill_brewer(palette = "Set2", name = "COVID wave")
    }
  }
  
  if (show_baseline && nrow(baseline_plot) > 0) {
    for (g in groups) {
      bp <- baseline_plot[pgroup == g]
      if (nrow(bp) == 0) next
      mean_col <- if (n_groups > 1) group_cols[[g]] else col_baseline_mean
      band_col <- if (n_groups > 1) group_cols[[g]] else col_baseline_range
      if (baseline_range) {
        p <- p + geom_ribbon(data = bp, aes(x = date, ymin = bl_min, ymax = bl_max),
                             fill = band_col, alpha = 0.3)
      }
      p <- p + geom_line(data = bp, aes(x = date, y = bl_mean),
                         colour = mean_col, linetype = "dashed", linewidth = 0.5)
    }
  }
  
  if (show_ci && !baseline_only && nrow(plot_dt) > 0) {
    # Losse punten (geen aangrenzende maand) -> ribbon is onzichtbaar; teken een whisker
    plot_dt[, .n_in_grp := .N, by = line_grp]
    iso_dt <- plot_dt[.n_in_grp == 1]
    for (g in groups) {
      gd <- plot_dt[pgroup == g]
      if (nrow(gd) == 0) next
      ci_fill <- if (n_groups > 1) group_cols[[g]]
      else if (!is.null(col_ci)) col_ci else group_cols[[g]]
      p <- p + geom_ribbon(data = gd,
                           aes(x = date, ymin = ci_low, ymax = ci_high, group = line_grp),
                           fill = ci_fill, alpha = 0.2)
      gi <- iso_dt[pgroup == g]
      if (nrow(gi) > 0) {
        p <- p + geom_errorbar(data = gi,
                               aes(x = date, ymin = ci_low, ymax = ci_high),
                               colour = ci_fill, width = 8, linewidth = 0.6, alpha = 0.9,
                               inherit.aes = FALSE)
      }
    }
  }
  
  if (sec_active && nrow(plot_dt) > 0) {
    if (sec_type == "excess" && "pgroup" %in% names(sec_plot) && !all(is.na(sec_plot$pgroup))) {
      n_grps <- length(unique(sec_plot$pgroup))
      for (g in unique(sec_plot$pgroup)) {
        gd <- sec_plot[pgroup == g]
        gc <- if (n_grps > 1 && !is.null(group_cols) && g %in% names(group_cols))
          group_cols[[g]] else col_sec
        p <- p +
          geom_line(data = gd, aes(x = date, y = scaled),
                    colour = gc, linewidth = 0.6, alpha = 0.6) +
          geom_point(data = gd, aes(x = date, y = scaled),
                     colour = gc, size = 1, alpha = 0.6)
      }
    } else {
      p <- p +
        geom_line(data = sec_plot, aes(x = date, y = scaled),
                  colour = col_sec, linewidth = 0.6, alpha = 0.6) +
        geom_point(data = sec_plot, aes(x = date, y = scaled),
                   colour = col_sec, size = 1, alpha = 0.6)
    }
  }
  if (!baseline_only) {
    p <- p +
      geom_line(data = plot_dt,
                aes(x = date, y = smr, colour = pgroup, group = line_grp),
                linewidth = 0.8) +
      geom_point(data = plot_dt,
                 aes(x = date, y = smr, colour = pgroup),
                 size = 1.5)
  }
  p <- p +
    scale_colour_manual(values = group_cols, name = "Pati\u00ebntgroep",
                        labels = GROUP_LABELS[names(group_cols)]) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y",
                 limits = x_limits, expand = c(0.01, 0),
                 oob = scales::oob_keep) +
    labs(title = title, x = NULL, y = if (show_axis_titles) "SMR" else NULL) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(linewidth = border_width, colour = "black"),
          axis.line.y.left = element_line(linewidth = border_width, colour = "black"),
          axis.line.y.right = if (sec_active) element_line(linewidth = border_width, colour = "black") else element_blank(),
          axis.text = element_text(face = if (bold_axis_text) "bold" else "plain"))
  
  if (n_groups == 1)   p <- p + guides(colour = "none")  # 1 groep -> geen groep-legenda (oude look)
  if (!show_axis_titles) p <- p + guides(colour = "none")  # as-labels uit -> ook geen patientgroep-legenda
  if (!show_wave_legend) p <- p + guides(fill = "none")
  
  if (sec_active && nrow(plot_dt) > 0) {
    if (sec_log) {
      br <- scales::breaks_log(n = 6)(c(sec_lo, sec_hi))
      p <- p + scale_y_continuous(
        sec.axis = sec_axis(~ 10^((. - offset) / scale_factor),
                            name = if (show_axis_titles) sec_label else NULL,
                            breaks = br, labels = scales::label_number()))
    } else {
      p <- p + scale_y_continuous(
        sec.axis = sec_axis(~ (. - offset) / scale_factor,
                            name = if (show_axis_titles) sec_label else NULL,
                            breaks = if (is.null(sec_breaks)) waiver() else sec_breaks,
                            labels = scales::label_number()))
    }
  }
  
  return(p)
}


plot_mr <- function(dt,
                    groups = "dialyse",
                    type = c("allcause", "covid", "cancer_cvd"),
                    show_waves = TRUE,
                    sec_type = "none",
                    show_baseline = FALSE,
                    baseline_range = TRUE,
                    per_1000 = FALSE,
                    adjust_days = c("none", "monthly", "daily"),
                    ref_2019 = FALSE,
                    alpha_waves = 0.15,
                    border_width = 0.5,
                    bold_axis_text = FALSE,
                    show_axis_titles = TRUE,
                    show_wave_legend = TRUE,
                    baseline_years = 2016:2019,
                    plot_years = 2020:2023,
                    group_cols = c(dialyse = "steelblue"),
                    col_waves = NULL,
                    sec_data = NULL,
                    sec_lo = 0,
                    sec_hi = 100,
                    sec_label = "Stringency Index",
                    sec_breaks = NULL,
                    sec_log = FALSE,
                    col_sec = "darkgreen",
                    col_baseline_mean = "grey50",
                    col_baseline_range = "grey70",
                    title = NULL) {
  
  type        <- match.arg(type)
  adjust_days <- match.arg(adjust_days)
  n_groups    <- length(groups)
  
  # Dagcorrectie wordt afgeleid uit de al-berekende mort_ratio (= obs/alive),
  # zodat een aparte 'alive'-kolom niet meer nodig is:
  #   monthly = mr * (30.4375 / dagen),  daily = mr / dagen
  transform_val <- function(mr, days) {
    v <- switch(adjust_days,
                none    = mr,
                monthly = mr * (30.4375 / days),
                daily   = mr / days)
    if (per_1000) v * 1000 else v
  }
  
  build_one <- function(g) {
    mr_col <- mr_colname(g, type, names(dt))
    d <- dt[year %in% plot_years & !is.na(get(mr_col)),
            .(date, month, wave, days_in_month, pgroup = g, mr = get(mr_col))]
    if (nrow(d) == 0) return(d)
    d[, plot_val := transform_val(mr, days_in_month)]
    setorder(d, date)
    d[, gap_group := cumsum(c(1, diff(as.numeric(date)) > 35))]
    d[, line_grp := paste(pgroup, gap_group)]
    d[]
  }
  plot_dt <- rbindlist(lapply(groups, build_one), fill = TRUE)
  
  # 2019-gemiddelde referentielijn (alleen all-cause), in dezelfde eenheid als de plot
  ref2019_dt <- NULL
  if (isTRUE(ref_2019) && type == "allcause") {
    ref2019_dt <- rbindlist(lapply(groups, function(g) {
      mr_col <- mr_colname(g, type, names(dt))
      sub <- dt[year == 2019 & !is.na(get(mr_col)), .(mr = get(mr_col), days = days_in_month)]
      if (nrow(sub) == 0) return(NULL)
      data.table(pgroup = g, yref = mean(transform_val(sub$mr, sub$days), na.rm = TRUE))
    }), fill = TRUE)
  }
  
  x_limits <- c(as.Date(sprintf("%d-01-01", min(plot_years))),
                as.Date(sprintf("%d-12-31", max(plot_years))))
  y_label <- if (per_1000) "Deaths per 1,000" else "Mortality Rate"
  
  if (show_waves) {
    waves <- dt[!is.na(wave) & wave != "no covid period" & year %in% plot_years,
                .(start = min(date), end = max(date) + 31), by = wave]
    waves[, wave := factor(wave, levels = c("lockdown1", "inter-lockdown",
                                            "lockdown2", "after vacc & delta",
                                            "omicron"))]
  }
  
  if (show_baseline) {
    build_bl <- function(g) {
      mr_col <- mr_colname(g, type, names(dt))
      tmp <- dt[year %in% baseline_years,
                .(month, v = transform_val(get(mr_col), days_in_month))]
      bl <- tmp[, .(bl_mean = mean(v, na.rm = TRUE),
                    bl_min  = min(v,  na.rm = TRUE),
                    bl_max  = max(v,  na.rm = TRUE)), by = month]
      out <- dt[year %in% plot_years, .(date, month)][bl, on = "month"][order(date)]
      out[, pgroup := g][]
    }
    baseline_plot <- rbindlist(lapply(groups, build_bl), fill = TRUE)
    baseline_plot <- baseline_plot[is.finite(bl_mean)]
  }
  
  sec_active <- sec_type != "none" && !is.null(sec_data) && nrow(sec_data) > 0 &&
    is.finite(sec_hi) && is.finite(sec_lo) && (sec_hi != sec_lo)
  
  if (sec_active && nrow(plot_dt) > 0) {
    val_range <- range(plot_dt$plot_val, na.rm = TRUE)
    if (sec_log) {
      llo <- log10(sec_lo); lhi <- log10(sec_hi)
      scale_factor <- diff(val_range) / (lhi - llo)
      if (!is.finite(scale_factor) || scale_factor == 0) scale_factor <- 1
      offset <- val_range[1] - llo * scale_factor
      sec_plot <- copy(sec_data)[value > 0]
      sec_plot[, scaled := log10(value) * scale_factor + offset]
    } else {
      scale_factor <- diff(val_range) / (sec_hi - sec_lo)
      if (!is.finite(scale_factor) || scale_factor == 0) scale_factor <- 1
      offset <- val_range[1] - sec_lo * scale_factor
      sec_plot <- copy(sec_data)
      sec_plot[, scaled := value * scale_factor + offset]
    }
  }
  
  type_label <- TYPE_LABELS[[type]]
  grp_label  <- paste(GROUP_LABELS[groups], collapse = " & ")
  rate_label <- ifelse(per_1000, "per 1,000", "")
  if (is.null(title)) title <- sprintf("Mortality Rate %s %s \u2013 %s (%s-%s)",
                                       type_label, rate_label, grp_label,
                                       min(plot_years), max(plot_years))
  
  p <- ggplot()
  
  if (show_waves) {
    p <- p + geom_rect(data = waves,
                       aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = wave),
                       alpha = alpha_waves)
    if (!is.null(col_waves)) {
      p <- p + scale_fill_manual(values = col_waves, name = "COVID wave")
    } else {
      p <- p + scale_fill_brewer(palette = "Set2", name = "COVID wave")
    }
  }
  
  if (show_baseline && nrow(baseline_plot) > 0) {
    for (g in groups) {
      bp <- baseline_plot[pgroup == g]
      if (nrow(bp) == 0) next
      mean_col <- if (n_groups > 1) group_cols[[g]] else col_baseline_mean
      band_col <- if (n_groups > 1) group_cols[[g]] else col_baseline_range
      if (baseline_range) {
        p <- p + geom_ribbon(data = bp, aes(x = date, ymin = bl_min, ymax = bl_max),
                             fill = band_col, alpha = 0.3)
      }
      p <- p + geom_line(data = bp, aes(x = date, y = bl_mean),
                         colour = mean_col, linetype = "dashed", linewidth = 0.5)
    }
  }
  
  if (sec_active && nrow(plot_dt) > 0) {
    if (sec_type == "excess" && "pgroup" %in% names(sec_plot) && !all(is.na(sec_plot$pgroup))) {
      n_grps <- length(unique(sec_plot$pgroup))
      for (g in unique(sec_plot$pgroup)) {
        gd <- sec_plot[pgroup == g]
        gc <- if (n_grps > 1 && !is.null(group_cols) && g %in% names(group_cols))
          group_cols[[g]] else col_sec
        p <- p +
          geom_line(data = gd, aes(x = date, y = scaled),
                    colour = gc, linewidth = 0.6, alpha = 0.6) +
          geom_point(data = gd, aes(x = date, y = scaled),
                     colour = gc, size = 1, alpha = 0.6)
      }
    } else {
      p <- p +
        geom_line(data = sec_plot, aes(x = date, y = scaled),
                  colour = col_sec, linewidth = 0.6, alpha = 0.6) +
        geom_point(data = sec_plot, aes(x = date, y = scaled),
                   colour = col_sec, size = 1, alpha = 0.6)
    }
  }
  
  if (!is.null(ref2019_dt) && nrow(ref2019_dt) > 0) {
    for (g in groups) {
      yv <- ref2019_dt[pgroup == g, yref]
      if (length(yv) == 1 && is.finite(yv)) {
        p <- p + geom_hline(yintercept = yv, colour = group_cols[[g]],
                            linetype = "dotted", linewidth = 0.7)
      }
    }
  }
  
  p <- p +
    geom_line(data = plot_dt,
              aes(x = date, y = plot_val, colour = pgroup, group = line_grp),
              linewidth = 0.8) +
    geom_point(data = plot_dt,
               aes(x = date, y = plot_val, colour = pgroup),
               size = 1.5) +
    scale_colour_manual(values = group_cols, name = "Pati\u00ebntgroep",
                        labels = GROUP_LABELS[names(group_cols)]) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y",
                 limits = x_limits, expand = c(0.01, 0),
                 oob = scales::oob_keep) +
    labs(title = title, x = NULL, y = if (show_axis_titles) y_label else NULL) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(linewidth = border_width, colour = "black"),
          axis.line.y.left = element_line(linewidth = border_width, colour = "black"),
          axis.line.y.right = if (sec_active) element_line(linewidth = border_width, colour = "black") else element_blank(),
          axis.text = element_text(face = if (bold_axis_text) "bold" else "plain"))
  
  if (n_groups == 1)   p <- p + guides(colour = "none")
  if (!show_axis_titles) p <- p + guides(colour = "none")  # as-labels uit -> ook geen patientgroep-legenda
  if (!show_wave_legend) p <- p + guides(fill = "none")
  
  if (sec_active && nrow(plot_dt) > 0) {
    if (sec_log) {
      br <- scales::breaks_log(n = 6)(c(sec_lo, sec_hi))
      p <- p + scale_y_continuous(
        sec.axis = sec_axis(~ 10^((. - offset) / scale_factor),
                            name = if (show_axis_titles) sec_label else NULL,
                            breaks = br, labels = scales::label_number()))
    } else {
      p <- p + scale_y_continuous(
        sec.axis = sec_axis(~ (. - offset) / scale_factor,
                            name = if (show_axis_titles) sec_label else NULL,
                            breaks = if (is.null(sec_breaks)) waiver() else sec_breaks,
                            labels = scales::label_number()))
    }
  }
  
  return(p)
}


# =============================================================================
# AANTAL PER MAAND (tab 3) - observed counts
# =============================================================================

OBS_TYPE_LABELS  <- c(allcause = "All-cause", covid = "COVID", cancer_cvd = "Cancer/CVD")

plot_obs <- function(dt, groups, outcome, plot_years,
                     chart_type = c("bar", "line"),
                     bar_position = c("dodge", "stack"),
                     group_cols = c(dialyse = "steelblue"),
                     show_axis_titles = TRUE, bold_axis_text = FALSE,
                     border_width = 0.5, title = NULL) {
  
  chart_type   <- match.arg(chart_type)
  bar_position <- match.arg(bar_position)
  n_groups <- length(groups)
  
  build_one <- function(g) {
    col <- sprintf("obs_%s_%s", g, outcome)
    if (!col %in% names(dt)) return(NULL)
    dt[year %in% plot_years & !is.na(get(col)),
       .(date, month, pgroup = g, count = get(col))]
  }
  pd <- rbindlist(lapply(groups, build_one), fill = TRUE)
  
  x_limits <- c(as.Date(sprintf("%d-01-01", min(plot_years))),
                as.Date(sprintf("%d-12-31", max(plot_years))))
  
  type_lab <- OBS_TYPE_LABELS[[outcome]]
  grp_lab  <- paste(GROUP_LABELS[groups], collapse = " & ")
  if (is.null(title)) title <- sprintf("Aantal sterfgevallen %s per maand \u2013 %s (%s-%s)",
                                       type_lab, grp_lab, min(plot_years), max(plot_years))
  
  p <- ggplot(pd)
  
  if (chart_type == "bar") {
    pos <- if (n_groups > 1) bar_position else "stack"
    p <- p +
      geom_col(aes(x = date, y = count, fill = pgroup),
               position = pos, width = 24) +
      scale_fill_manual(values = group_cols, name = "Pati\u00ebntgroep",
                        labels = GROUP_LABELS[names(group_cols)])
  } else {
    p <- p +
      geom_line(aes(x = date, y = count, colour = pgroup), linewidth = 0.8) +
      geom_point(aes(x = date, y = count, colour = pgroup), size = 1.6) +
      scale_colour_manual(values = group_cols, name = "Pati\u00ebntgroep",
                          labels = GROUP_LABELS[names(group_cols)])
  }
  
  p <- p +
    scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
                 limits = x_limits, expand = c(0.01, 0), oob = scales::oob_keep) +
    labs(title = title, x = NULL,
         y = if (show_axis_titles) "Aantal sterfgevallen" else NULL) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(linewidth = border_width, colour = "black"),
          axis.line.y.left = element_line(linewidth = border_width, colour = "black"),
          axis.text = element_text(face = if (bold_axis_text) "bold" else "plain"))
  
  # 1 groep -> geen legenda; as-labels uit -> ook geen legenda (consistent met SMR-tab)
  if (n_groups == 1 || !show_axis_titles) {
    p <- p + guides(fill = "none", colour = "none")
  }
  p
}


VACC_DOSE_LABELS <- c(vaccin1 = "Dose 1", vaccin2 = "Dose 2", vaccin3_more = "Dose 3+")
VACC_DOSE_LTYPE  <- c(vaccin1 = "solid",  vaccin2 = "dashed", vaccin3_more = "dotdash")

plot_vacc <- function(vacc_dt, doses, plot_years,
                      col_line = "steelblue",
                      show_waves = TRUE, wave_dt = NULL,
                      alpha_waves = 0.15,
                      col_waves = NULL,
                      show_wave_legend = TRUE,
                      show_axis_titles = TRUE,
                      bold_axis_text = FALSE,
                      border_width = 0.5,
                      title = NULL) {
  
  cols <- intersect(doses, names(VACC_DOSE_LABELS))
  if (length(cols) == 0 || is.null(vacc_dt) || nrow(vacc_dt) == 0)
    return(ggplot() + labs(title = "Geen vaccinatiedata beschikbaar"))
  
  dv <- vacc_dt[data.table::year(date) %in% plot_years]
  if (nrow(dv) == 0)
    return(ggplot() + labs(subtitle = "Geen vaccinatiedata in de gekozen jaren (data loopt 2021\u20132022)"))
  
  # lang formaat
  pd <- rbindlist(lapply(cols, function(col) {
    data.table(date = dv$date, dose = VACC_DOSE_LABELS[[col]], value = dv[[col]],
               ltype = VACC_DOSE_LTYPE[[col]])
  }))
  pd[, dose := factor(dose, levels = VACC_DOSE_LABELS[cols])]
  
  x_limits <- c(as.Date(sprintf("%d-01-01", min(plot_years))),
                as.Date(sprintf("%d-12-31", max(plot_years))))
  
  if (is.null(title)) {
    dose_str <- paste(VACC_DOSE_LABELS[cols], collapse = " + ")
    title <- sprintf("Vaccination coverage \u2013 %s (%s\u2013%s)",
                     dose_str, min(plot_years), max(plot_years))
  }
  
  p <- ggplot()
  
  if (show_waves && !is.null(wave_dt) && nrow(wave_dt) > 0) {
    p <- p + geom_rect(data = wave_dt,
                       aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = wave),
                       alpha = alpha_waves)
    if (!is.null(col_waves)) {
      p <- p + scale_fill_manual(values = col_waves, name = "COVID wave")
    } else {
      p <- p + scale_fill_brewer(palette = "Set2", name = "COVID wave")
    }
  }
  
  p <- p +
    geom_line(data = pd, aes(x = date, y = value, linetype = dose),
              colour = col_line, linewidth = 0.8) +
    geom_point(data = pd, aes(x = date, y = value, shape = dose),
               colour = col_line, size = 1.8) +
    scale_linetype_manual(values = setNames(VACC_DOSE_LTYPE[cols], VACC_DOSE_LABELS[cols]),
                          name = NULL) +
    scale_shape_manual(values = setNames(c(16L, 17L, 15L)[seq_along(cols)],
                                         VACC_DOSE_LABELS[cols]), name = NULL) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                       labels = function(x) paste0(x, "%")) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y",
                 limits = x_limits, expand = c(0.01, 0), oob = scales::oob_keep) +
    labs(title = title, x = NULL,
         y = if (show_axis_titles) "Vaccination coverage (%)" else NULL) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(linewidth = border_width, colour = "black"),
          axis.line.y.left  = element_line(linewidth = border_width, colour = "black"),
          axis.text = element_text(face = if (bold_axis_text) "bold" else "plain"))
  
  if (!show_wave_legend) p <- p + guides(fill = "none")
  if (length(cols) == 1)  p <- p + guides(linetype = "none", shape = "none")
  p
}


# =============================================================================
# REGRESSIE-RESULTATEN (tab 2)
# =============================================================================

REG_DIR <- "reg_data"   # map met de 4 Excel-bestanden ({periode}_volw_met_df.xlsx) naast app.R
REG_FILE_SUFFIX <- "_volw_met_df.xlsx"
reg_file <- function(period) file.path(REG_DIR, paste0(period, REG_FILE_SUFFIX))

# Tabbladen zijn covariaat-major, uitkomst-minor; ophalen via vaste index
REG_OUTCOMES <- c("dood","covid_dood","opname","ic_opname","covid_dood_opname","positieve_test")

# Covariatensets worden OVERGENOMEN uit de tabnamen: elke 'dood_*'-tab is het begin
# van een covariaatblok, dus de covariaatnaam = tabnaam zonder de 'dood_'-prefix.
REG_COVS <- local({
  fb <- c("uni","demog","demog_ses","demog_comorb","demog_opn","demog_frail","comorb_opn")
  sn <- tryCatch(readxl::excel_sheets(reg_file("p1_2020")), error = function(e) character(0))
  if (length(sn) == 0) return(fb)
  covs <- sub("^dood_", "", sn[startsWith(sn, "dood_")])
  if (length(covs) == 0) fb else covs
})

# Nette labels, afgeleid van de tabnaam-tokens (bijv. "demog_frail" -> "Demografie + frailty")
reg_cov_label <- function(tok) {
  m <- c(uni = "Univariaat", demog = "Demografie", ses = "SES-WOA",
         comorb = "comorbiditeiten", opn = "opname", frail = "frailty")
  parts <- strsplit(tok, "_", fixed = TRUE)[[1]]
  mapped <- ifelse(parts %in% names(m), m[parts], parts)
  out <- paste(mapped, collapse = " + ")
  substr(out, 1, 1) <- toupper(substr(out, 1, 1))
  out
}
REG_COV_CHOICES <- stats::setNames(REG_COVS, vapply(REG_COVS, reg_cov_label, character(1)))

REG_MODEL_LABELS <- c(
  bevolking     = "General population model (ref: general population)",
  nierpatienten = "Kidney patient model (ref: KTR)"
)

REG_PERIOD_LABELS <- c(
  p1_2020 = "March cohort 2020", p1_2021 = "March cohort 2021",
  p2_2020 = "June cohort 2020",  p2_2021 = "June cohort 2021"
)

REG_MODEL_SHORT <- c(bevolking = "General population", nierpatienten = "Kidney patients")

REG_TERM_ORDER <- c("Dialysis","KTR","Age (per year)","Gender (male)",
                    "SES 20-50%","SES 10-20%","SES 0-10%","SES unknown",
                    "Immunocompromised","Asthma/COPD","Diabetes","Frailty (middle)","Frailty (high)",
                    "Hospitalization")

reg_file_and_index <- function(period, outcome, cov) {
  o_i <- match(outcome, REG_OUTCOMES)
  c_i <- match(cov, REG_COVS)
  list(file = reg_file(period), sheet = (c_i - 1) * length(REG_OUTCOMES) + o_i)
}

reg_term_label <- function(term) {
  term <- as.character(term)
  out <- term
  out[grepl("^status_(maart|juni)1$", term)] <- "Dialysis"
  out[grepl("^status_(maart|juni)2$", term)] <- "KTR"
  map <- c(leeftijd = "Age (per year)", geslachtMannen = "Gender (male)",
           `seswoa_small20-50%` = "SES 20-50%", `seswoa_small10-20%` = "SES 10-20%",
           `seswoa_small0-10%` = "SES 0-10%", seswoa_smallOnbekend = "SES unknown",
           immuno = "Immunocompromised", astma_copd = "Asthma/COPD",
           diabetes_combi = "Diabetes", opname_algemeen = "Hospitalization",
           frailty_groupmiddle = "Frailty (middle)", frailty_grouphigh = "Frailty (high)",
           `(Intercept)` = "Intercept")
  hit <- term %in% names(map)
  out[hit] <- map[term[hit]]
  out
}

# Lees 1 tabblad -> tidy data.table met kolom 'model' (bevolking / nierpatienten)
parse_reg_sheet <- function(file, sheet_idx) {
  raw <- suppressMessages(as.data.table(read_excel(file, sheet = sheet_idx, col_names = TRUE)))
  setnames(raw, make.names(names(raw)))
  blank <- is.na(raw$term) & is.na(raw$OR)
  nb <- which(!blank)
  if (length(nb) == 0) return(NULL)
  runs <- split(nb, cumsum(c(1, diff(nb) > 1)))   # opeenvolgende niet-lege rijen = 1 deelmodel
  rbindlist(lapply(runs, function(idx) {
    b   <- raw[idx]
    ref <- b$referentie[!is.na(b$referentie)][1]
    model <- if (!is.na(ref) && grepl("geen nierpatient", ref)) "bevolking"
    else if (!is.na(ref) && grepl("transplant", ref)) "nierpatienten"
    else NA_character_
    b <- b[!is.na(term)]
    data.table(model = model, term = b$term, OR = b$OR,
               conf.low = b$conf.low, conf.high = b$conf.high, p.value = b$p.value)
  }), fill = TRUE)
}

build_reg_data <- function(periods, outcome, cov, which_model, pop_filter = "both") {
  d <- rbindlist(lapply(periods, function(pe) {
    fi <- reg_file_and_index(pe, outcome, cov)
    one <- parse_reg_sheet(fi$file, fi$sheet)
    if (is.null(one) || nrow(one) == 0) return(NULL)
    one[, period := pe]
    one
  }), fill = TRUE)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  if (which_model != "beide") d <- d[model == which_model]
  d <- d[term != "(Intercept)"]
  if (nrow(d) == 0) return(NULL)
  d[, term_label := reg_term_label(term)]
  # bevolkingsmodel: optioneel alleen Dialysis of alleen KTR tonen
  if (pop_filter != "both") {
    remove_lbl <- if (pop_filter == "dialysis") "KTR" else "Dialysis"
    d <- d[!(model == "bevolking" & term_label == remove_lbl)]
    if (nrow(d) == 0) return(NULL)
  }
  lev <- intersect(REG_TERM_ORDER, unique(d$term_label))
  lev <- c(lev, setdiff(unique(d$term_label), lev))
  d[, term_label := factor(term_label, levels = rev(lev))]
  d[, model_label := factor(REG_MODEL_LABELS[model],
                            levels = REG_MODEL_LABELS[c("bevolking","nierpatienten")])]
  d[, model_label_short := factor(REG_MODEL_SHORT[model],
                                  levels = REG_MODEL_SHORT[c("bevolking","nierpatienten")])]
  d[, period_label := factor(REG_PERIOD_LABELS[period],
                             levels = REG_PERIOD_LABELS[intersect(names(REG_PERIOD_LABELS), unique(period))])]
  d[, sig := ifelse(p.value < 0.05, "p < 0.05", "n.s.")]
  d[]
}

plot_reg <- function(d, col_sig = "steelblue", col_ns = "grey60", title = NULL,
                     or_cap = TRUE, or_max = 100,
                     bold_axis_text = FALSE, show_axis_titles = TRUE,
                     border_width = 0.7,
                     fixed_xlim = NULL,
                     n_y_ticks = NULL) {
  n_terms_total <- nrow(d)
  # alleen zinvol plotbare schattingen (separation/niet-schatbaar -> weg uit plot, tabel houdt alles)
  dd <- d[is.finite(OR) & OR > 0 & is.finite(conf.low) & conf.low > 0 & is.finite(conf.high) & conf.high > 0]
  if (or_cap && is.finite(or_max)) {
    dd <- dd[OR <= or_max & OR >= 1 / or_max]
    dd[, ci_lo_d := pmax(conf.low, 1 / or_max)]
    dd[, ci_hi_d := pmin(conf.high, or_max)]
  } else {
    dd[, ci_lo_d := conf.low]; dd[, ci_hi_d := conf.high]
  }
  n_hidden <- n_terms_total - nrow(dd)
  
  # tooltip-tekst voor hover (plotly) -> toont de ECHTE waarden
  dd[, .tt := sprintf("%s\nOR %.2f (95%% BI %.2f\u2013%.2f)\np = %s",
                      as.character(term_label), OR, conf.low, conf.high,
                      ifelse(p.value < 0.001, "<0.001", formatC(p.value, format = "f", digits = 3)))]
  
  # Vaste y-as: lege padding-rijen zodat de y-as altijd n_y_ticks termen heeft,
  # ongeacht hoeveel termen in het huidige model zitten.
  # Techniek: voeg echte datarijen toe (met OR=NA en alpha=0) zodat ggplot/plotly
  # de lege y-posities reserveert; louter factor-levels zonder datapunt worden genegeerd.
  dummy_rows <- NULL
  if (!is.null(n_y_ticks) && is.finite(n_y_ticks) && n_y_ticks > 0) {
    cur_terms <- levels(dd$term_label)
    n_pad <- max(0L, as.integer(n_y_ticks) - length(cur_terms))
    if (n_pad > 0) {
      pad_labels <- paste0(strrep("\u00a0", seq_len(n_pad)))   # unieke lege labels (non-breaking spaces)
      # template-rij: zelfde kolommen als dd maar alle waarden NA/leeg
      tmpl <- dd[1L]
      dummy_rows <- rbindlist(lapply(pad_labels, function(lbl) {
        r <- copy(tmpl)
        r[, term_label := lbl]
        r[, OR := NA_real_][, ci_lo_d := NA_real_][, ci_hi_d := NA_real_]
        r[, .tt := ""][, sig := "n.s."]
        r
      }))
      new_levels <- c(pad_labels, cur_terms)
      dd[,         term_label := factor(as.character(term_label), levels = new_levels)]
      dummy_rows[, term_label := factor(as.character(term_label), levels = new_levels)]
    }
  }
  
  cap_note <- if (n_hidden > 0)
    sprintf("%d estimate(s) not shown (extreme/non-estimable OR or CI); see table for exact values.", n_hidden) else NULL
  
  plot_data <- if (!is.null(dummy_rows)) rbindlist(list(dd, dummy_rows), fill = TRUE) else dd
  p <- ggplot(plot_data, aes(x = OR, y = term_label, colour = sig, text = .tt)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(aes(xmin = ci_lo_d, xmax = ci_hi_d), height = 0.2, linewidth = 0.6, na.rm = TRUE) +
    geom_point(size = 1.8, na.rm = TRUE) +
    scale_x_log10(labels = scales::label_number(drop0trailing = TRUE)) +
    scale_colour_manual(values = c("p < 0.05" = col_sig, "n.s." = col_ns), name = NULL) +
    labs(title = title, x = if (show_axis_titles) "Odds ratio (95% CI, log scale)" else NULL,
         y = NULL, caption = cap_note) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey40", fill = NA, linewidth = border_width),
          strip.background = element_rect(fill = "grey95", colour = "grey40"),
          strip.text = element_text(face = "bold"),
          axis.text = element_text(face = if (bold_axis_text) "bold" else "plain"))
  if (nrow(dd) > 0) {
    xlim <- if (!is.null(fixed_xlim) && length(fixed_xlim) == 2 && all(is.finite(fixed_xlim))) {
      fixed_xlim
    } else {
      c(min(c(dd$ci_lo_d, dd$OR), na.rm = TRUE),
        max(c(dd$ci_hi_d, dd$OR), na.rm = TRUE))
    }
    dd[, ci_lo_d := pmax(ci_lo_d, xlim[1])]
    dd[, ci_hi_d := pmin(ci_hi_d, xlim[2])]
    p <- p + coord_cartesian(xlim = xlim)
  }
  n_models  <- length(unique(d$model))
  n_periods <- length(unique(d$period))
  if (n_periods > 1 && n_models > 1) {
    p <- p + facet_grid(model_label_short ~ period_label)
  } else if (n_periods > 1) {
    p <- p + facet_wrap(~ period_label, ncol = 2)
  } else if (n_models > 1) {
    p <- p + facet_wrap(~ model_label, ncol = 2)
  }
  p
}

reg_table <- function(d) {
  fmt <- function(x) formatC(x, format = "f", digits = 2)
  pf  <- function(p) ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))
  out <- d[, .(Periode = as.character(REG_PERIOD_LABELS[period]),
               Term = as.character(term_label),
               `OR (95% BI)` = sprintf("%s (%s\u2013%s)", fmt(OR), fmt(conf.low), fmt(conf.high)),
               `p-waarde` = pf(p.value),
               Model = as.character(REG_MODEL_LABELS[model]))]
  out <- out[order(match(Periode, REG_PERIOD_LABELS), match(Model, REG_MODEL_LABELS))]
  if (length(unique(d$model)) == 1)  out[, Model := NULL]
  if (length(unique(d$period)) == 1) out[, Periode := NULL]
  out
}


# =============================================================================
# SHINY APP
# =============================================================================

# Lokale modus: automatisch aan als het sqlite-wachtwoordbestand niet aanwezig is.
# Op de server staat het bestand wel -> normaal gedrag met login.
# Lokaal ontbreekt het -> login overgeslagen, app direct zichtbaar.
LOCAL_MODE <- !file.exists("/srv/shiny-server/passwords.sqlite")

ui <- navbarPage(
  title = "SMR & Regressie Explorer",
  header = tags$head(
    tags$style("body { visibility: hidden; }"),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('show_app', function(x) {
          document.body.style.visibility = 'visible';
        });
        Shiny.addCustomMessageHandler('no_access_redirect', function(x) {
          document.body.style.visibility = 'visible';
          document.body.innerHTML = '<div style=\"display:flex;align-items:center;justify-content:center;height:100vh;flex-direction:column;background:white;\"><h1>Geen toegang</h1><p>Je hebt geen toegang tot deze app.</p><p>Je wordt teruggestuurd naar het loginscherm...</p></div>';
          setTimeout(function(){ location.reload(); }, 5000);
        });
      });
    "))
  ),
  
  # ---- TAB 1: SMR & Mortality Rate ----
  tabPanel(
    "SMR & Mortality Rate",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        # --- SMR databestand ---
        radioButtons("smr_file", "SMR-data",
                     choices = c("Standaard"   = "standaard",
                                 "Zonder SES"  = "no_ses",
                                 "Oude versie" = "oud"),
                     selected = "standaard"),
        
        # --- Plot type ---
        radioButtons("plot_func", "Plot type",
                     choices = c("SMR" = "smr", "Mortality Rate" = "mr")),
        
        radioButtons("type", "Outcome",
                     choices = c("All-cause" = "allcause",
                                 "COVID" = "covid",
                                 "Cancer/CVD" = "cancer_cvd")),
        
        # --- Patientgroep ---
        radioButtons("group", "Pati\u00ebntgroep",
                     choices = c("Beide groepen (overlay)" = "both",
                                 "Dialyse" = "dialyse",
                                 "Transplantatie" = "transplant"),
                     selected = "dialyse"),
        
        sliderInput("plot_years", "Jaren",
                    min = 2016, max = 2023, value = c(2020, 2023),
                    step = 1, sep = ""),
        
        hr(),
        
        # --- Toggle opties ---
        h4("Weergave opties"),
        
        checkboxInput("show_waves", "COVID waves", value = TRUE),
        conditionalPanel(
          condition = "input.show_waves",
          sliderInput("alpha_waves", "Wave transparantie",
                      min = 0.05, max = 0.5, value = 0.15, step = 0.05),
          checkboxInput("show_wave_legend", "Wave legenda tonen", value = TRUE)
        ),
        
        sliderInput("border_width", "Plot rand dikte",
                    min = 0, max = 3, value = 0.5, step = 0.1),
        
        checkboxInput("bold_axis_text", "Vette astekst", value = FALSE),
        
        checkboxInput("show_axis_titles", "As labels tonen", value = TRUE),
        
        checkboxInput("show_title", "Titel tonen", value = TRUE),
        
        radioButtons("secondary", "Secundaire as",
                     choices = c("Geen" = "none",
                                 "Stringency Index" = "stringency",
                                 "Rioolwater (RNA)" = "rioolwater",
                                 "Excess deaths (all-cause)" = "excess",
                                 "Vaccinatie 1e dosis (%)" = "vacc1",
                                 "Vaccinatie 2e doses (%)" = "vacc2",
                                 "Vaccinatie 3+ doses (%)" = "vacc3"),
                     selected = "none"),
        conditionalPanel(
          condition = "input.secondary == 'rioolwater'",
          checkboxInput("sec_log", "Rioolwater op log-schaal", value = FALSE)
        ),
        conditionalPanel(
          condition = "input.secondary == 'vacc1' || input.secondary == 'vacc2' || input.secondary == 'vacc3'",
          checkboxInput("vacc_only", "Alleen vaccinatiegraad plotten", value = FALSE),
          conditionalPanel(
            condition = "input.vacc_only",
            checkboxGroupInput("vacc_doses", "Toon doses",
                               choices = c("1e dosis"  = "vaccin1",
                                           "2e dosis"  = "vaccin2",
                                           "3+ doses"  = "vaccin3_more"),
                               selected = "vaccin1")
          )
        ),
        
        # Baseline (voor beide plot types)
        checkboxInput("show_baseline", "Baseline (2016-2019)", value = TRUE),
        conditionalPanel(
          condition = "input.show_baseline",
          checkboxInput("baseline_range", "Min/Max band", value = TRUE),
          conditionalPanel(
            condition = "input.plot_func == 'smr'",
            checkboxInput("baseline_only", "Alleen baseline tonen", value = FALSE)
          )
        ),
        
        # SMR-specifieke opties
        conditionalPanel(
          condition = "input.plot_func == 'smr'",
          checkboxInput("show_ci", "Confidence intervals (SMR \u00b1 ci_diff)", value = TRUE),
          conditionalPanel(
            condition = "input.show_ci",
            radioButtons("ci_type", "CI type (kiest ci_diff)",
                         choices = c("O en E onzeker" = "O_E",
                                     "Alleen E onzeker" = "E",
                                     "Alleen O onzeker" = "O"),
                         selected = "O_E")
          )
        ),
        
        # MR-specifieke opties
        conditionalPanel(
          condition = "input.plot_func == 'mr'",
          checkboxInput("per_1000", "Per 1,000", value = FALSE),
          radioButtons("adjust_days", "Dagcorrectie",
                       choices = c("Geen" = "none",
                                   "Maandrate (\u00d730.4375)" = "monthly",
                                   "Dagrate" = "daily"),
                       selected = "none"),
          conditionalPanel(
            condition = "input.type == 'allcause'",
            checkboxInput("mr_ref2019", "2019-gemiddelde referentielijn", value = FALSE)
          )
        ),
        
        hr(),
        
        # --- Kleuren ---
        h4("Kleuren"),
        colourInput("col_line", "Lijn kleur (groep 1 / dialyse)", value = "steelblue"),
        
        conditionalPanel(
          condition = "input.group == 'both'",
          colourInput("col_line2", "Lijn kleur groep 2 (transplantatie)", value = "darkorange")
        ),
        
        conditionalPanel(
          condition = "input.plot_func == 'smr' && input.show_ci && input.group != 'both'",
          colourInput("col_ci", "CI kleur", value = "steelblue")
        ),
        
        conditionalPanel(
          condition = "input.show_baseline",
          colourInput("col_baseline_mean", "Baseline mean", value = "grey50"),
          conditionalPanel(
            condition = "input.baseline_range",
            colourInput("col_baseline_range", "Baseline band", value = "grey70")
          )
        ),
        
        conditionalPanel(
          condition = "input.secondary != 'none'",
          colourInput("col_sec", "Kleur secundaire as", value = "darkgreen")
        ),
        
        hr(),
        
        # --- Export ---
        h4("Export"),
        checkboxInput("transparent_bg", "Transparante achtergrond", value = FALSE),
        checkboxInput("no_grid", "Geen raster", value = FALSE),
        numericInput("plot_width", "Breedte (cm)", value = 30, min = 10, max = 60),
        numericInput("plot_height", "Hoogte (cm)", value = 15, min = 5, max = 40),
        numericInput("plot_dpi", "DPI", value = 300, min = 72, max = 600),
        downloadButton("download_plot", "Download PNG")
      ),
      
      mainPanel(
        width = 9,
        plotOutput("main_plot", height = "600px"),
        conditionalPanel(
          condition = "input.secondary == 'excess'",
          hr(),
          div(style = "display:flex; justify-content:space-between; align-items:center;",
              h5("Excess deaths per maand (all-cause)"),
              downloadButton("excess_download_csv", "Download CSV", style = "margin-bottom:6px;")),
          DT::dataTableOutput("excess_table")
        )
      )
    )
  ),
  
  # ---- TAB 2: Regressie-resultaten ----
  tabPanel(
    "Regressie-resultaten",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons("reg_period", "Periode",
                     choices = c("Maart 2020" = "p1_2020",
                                 "Maart 2021" = "p1_2021",
                                 "Juni 2020"  = "p2_2020",
                                 "Juni 2021"  = "p2_2021")),
        checkboxInput("reg_compare_periods", "P1 vs P2 naast elkaar (zelfde jaar)", value = FALSE),
        selectInput("reg_outcome", "Uitkomstvariabele",
                    choices = c("All-cause sterfte" = "dood",
                                "COVID-sterfte" = "covid_dood",
                                "COVID-opname" = "opname",
                                "COVID IC-opname" = "ic_opname",
                                "COVID-sterfte of (IC-)opname" = "covid_dood_opname",
                                "Positieve test" = "positieve_test")),
        selectInput("reg_cov", "Covariatenset",
                    choices = REG_COV_CHOICES),
        radioButtons("reg_model", "Regressie",
                     choices = c("Bevolking" = "bevolking",
                                 "Nierpati\u00ebnten" = "nierpatienten",
                                 "Beide naast elkaar" = "beide")),
        conditionalPanel(
          condition = "input.reg_model == 'bevolking' || input.reg_model == 'beide'",
          radioButtons("reg_pop_filter", "Toon in bevolkingsmodel",
                       choices = c("Dialysis + KTR" = "both",
                                   "Alleen Dialysis" = "dialysis",
                                   "Alleen KTR"      = "ktr"),
                       selected = "both")
        ),
        
        hr(),
        checkboxInput("reg_show_title", "Titel tonen", value = TRUE),
        colourInput("reg_col_sig", "Kleur significant (p<0.05)", value = "steelblue"),
        colourInput("reg_col_ns", "Kleur niet-significant", value = "grey60"),
        
        checkboxInput("reg_cap_or", "Grote OR's beperken (schaal leesbaar houden)", value = TRUE),
        conditionalPanel(
          condition = "input.reg_cap_or",
          numericInput("reg_or_max", "Max OR in plot", value = 100, min = 2, max = 100000)
        ),
        checkboxInput("reg_fixed_x", "Vaste x-as (zelfde bereik over plots)", value = FALSE),
        conditionalPanel(
          condition = "input.reg_fixed_x",
          fluidRow(
            column(6, numericInput("reg_xmin", "Min OR", value = 0.1, min = 0.001, max = 1,   step = 0.05)),
            column(6, numericInput("reg_xmax", "Max OR", value = 10,  min = 1,     max = 1000, step = 1))
          )
        ),
        checkboxInput("reg_fixed_y", "Vaste y-as (vast aantal termen)", value = FALSE),
        conditionalPanel(
          condition = "input.reg_fixed_y",
          numericInput("reg_n_ticks", "Aantal termen op y-as", value = 9, min = 1, max = 30, step = 1)
        ),
        
        hr(),
        h4("Weergave"),
        checkboxInput("reg_show_title", "Titel tonen", value = TRUE),
        checkboxInput("reg_bold_axis", "Vette astekst", value = FALSE),
        checkboxInput("reg_show_axis_titles", "As labels tonen", value = TRUE),
        sliderInput("reg_border_width", "Plot rand dikte",
                    min = 0, max = 3, value = 0.7, step = 0.1),
        
        hr(),
        h4("Export"),
        checkboxInput("reg_transparent_bg", "Transparante achtergrond", value = FALSE),
        checkboxInput("reg_no_grid", "Geen raster", value = FALSE),
        numericInput("reg_plot_width", "Breedte (cm)", value = 28, min = 10, max = 60),
        numericInput("reg_plot_height", "Hoogte (cm)", value = 14, min = 5, max = 40),
        numericInput("reg_plot_dpi", "DPI", value = 300, min = 72, max = 600),
        downloadButton("reg_download_plot", "Download plot (PNG)"),
        br(), br(),
        downloadButton("reg_download_table", "Download tabel (CSV)")
      ),
      
      mainPanel(
        width = 9,
        plotly::plotlyOutput("reg_plot", height = "520px"),
        hr(),
        DT::dataTableOutput("reg_table")
      )
    )
  ),
  
  # ---- TAB 3: Aantal per maand ----
  tabPanel(
    "Aantal per maand",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons("obs_group", "Pati\u00ebntgroep",
                     choices = c("Beide groepen" = "both",
                                 "Dialyse" = "dialyse",
                                 "Transplantatie" = "transplant"),
                     selected = "dialyse"),
        radioButtons("obs_outcome", "Outcome",
                     choices = c("All-cause" = "allcause",
                                 "COVID" = "covid",
                                 "Cancer/CVD" = "cancer_cvd")),
        sliderInput("obs_years", "Jaren",
                    min = 2016, max = 2023, value = c(2016, 2023),
                    step = 1, sep = ""),
        radioButtons("obs_chart", "Grafiektype",
                     choices = c("Staaf" = "bar", "Lijn" = "line")),
        conditionalPanel(
          condition = "input.obs_chart == 'bar' && input.obs_group == 'both'",
          radioButtons("obs_position", "Staven",
                       choices = c("Naast elkaar" = "dodge", "Gestapeld" = "stack"))
        ),
        
        hr(),
        checkboxInput("obs_show_title", "Titel tonen", value = TRUE),
        checkboxInput("obs_axis_titles", "As labels tonen", value = TRUE),
        checkboxInput("obs_bold_axis", "Vette astekst", value = FALSE),
        sliderInput("obs_border_width", "Plot rand dikte",
                    min = 0, max = 3, value = 0.5, step = 0.1),
        colourInput("obs_col1", "Kleur (groep 1 / dialyse)", value = "steelblue"),
        conditionalPanel(
          condition = "input.obs_group == 'both'",
          colourInput("obs_col2", "Kleur groep 2 (transplantatie)", value = "darkorange")
        ),
        
        hr(),
        h4("Export"),
        numericInput("obs_plot_width", "Breedte (cm)", value = 28, min = 10, max = 60),
        numericInput("obs_plot_height", "Hoogte (cm)", value = 13, min = 5, max = 40),
        numericInput("obs_plot_dpi", "DPI", value = 300, min = 72, max = 600),
        downloadButton("obs_download_plot", "Download plot (PNG)"),
        br(), br(),
        downloadButton("obs_download_table", "Download data (CSV)")
      ),
      
      mainPanel(
        width = 9,
        plotOutput("obs_plot", height = "560px")
      )
    )
  )
)


server <- function(input, output, session) {
  
  # --- Lokale modus: shinymanager overslaan als sqlite-bestand ontbreekt ---
  if (LOCAL_MODE) {
    session$sendCustomMessage("show_app", TRUE)
  } else {
    res_auth <- secure_server(
      check_credentials = check_credentials(
        db = "/srv/shiny-server/passwords.sqlite"
      )
    )
    auth_ok <- reactiveVal(FALSE)
    
    observe({
      req(reactiveValuesToList(res_auth)$user)
      user_apps <- reactiveValuesToList(res_auth)$apps
      if (user_apps == "all") {
        auth_ok(TRUE)
        session$sendCustomMessage("show_app", TRUE)
      } else {
        allowed <- unlist(strsplit(user_apps, ","))
        app_name <- basename(getwd())
        if (app_name %in% allowed) {
          auth_ok(TRUE)
          session$sendCustomMessage("show_app", TRUE)
        } else {
          session$sendCustomMessage("no_access_redirect", TRUE)
        }
      }
    })
  }
  
  
  # --- Data: twee SMR-bestanden (standaard = met SES-correctie; en zonder SES) ---
  load_smr <- function(path) {
    d <- fread(path)
    # naamloze index-kolom + dubbele mort_ratio-kolommen -> hou eerste van elke naam
    d <- d[, !duplicated(names(d)), with = FALSE]
    d[, date := as.Date(sprintf("%d-%02d-01", year, month))]
    d[, days_in_month := as.integer(difftime(
      as.Date(sprintf("%d-%02d-01", ifelse(month == 12, year + 1, year),
                      ifelse(month == 12, 1, month + 1))),
      date, units = "days"))]
    d[]
  }
  smr_data_files <- c(standaard = "smr_data_updated.csv",
                      no_ses    = "smr_data_updated_no_ses.csv",
                      oud       = "smr_data_output.csv")
  smr_sets <- lapply(smr_data_files, function(p) tryCatch(load_smr(p), error = function(e) NULL))
  
  smr_dt_r <- reactive({
    sel <- if (is.null(input$smr_file)) "standaard" else input$smr_file
    d <- smr_sets[[sel]]
    validate(need(!is.null(d),
                  paste0("SMR-bestand niet gevonden/leesbaar: ", smr_data_files[[sel]])))
    d
  })
  
  # --- Update lijn-/CI-kleur default bij outcome switch ---
  observeEvent(input$type, {
    default <- switch(input$type,
                      allcause   = "steelblue",
                      covid      = "firebrick",
                      cancer_cvd = "#6a3d9a")
    updateColourInput(session, "col_line", value = default)
    updateColourInput(session, "col_ci",   value = default)
  })
  
  # --- Build plot ---
  current_plot <- reactive({
    dt <- smr_dt_r()
    
    # Vacc-only modus: SMR/MR vervangen door pure vaccinatiegraad-plot
    if (isTRUE(input$vacc_only) &&
        !is.null(input$secondary) && input$secondary %in% c("vacc1","vacc2","vacc3")) {
      doses <- if (is.null(input$vacc_doses) || length(input$vacc_doses) == 0)
        "vaccin1" else input$vacc_doses
      yrs <- input$plot_years[1]:input$plot_years[2]
      wave_dt <- NULL
      if (isTRUE(input$show_waves)) {
        wave_dt <- dt[!is.na(wave) & wave != "no covid period" & year %in% yrs,
                      .(start = min(date), end = max(date) + 31), by = wave]
        wave_dt[, wave := factor(wave, levels = c("lockdown1","inter-lockdown",
                                                  "lockdown2","after vacc & delta","omicron"))]
      }
      p <- plot_vacc(vacc_monthly, doses = doses, plot_years = yrs,
                     col_line       = input$col_sec,
                     show_waves     = isTRUE(input$show_waves),
                     wave_dt        = wave_dt,
                     alpha_waves    = input$alpha_waves,
                     show_wave_legend = isTRUE(input$show_wave_legend),
                     show_axis_titles = isTRUE(input$show_axis_titles),
                     bold_axis_text = isTRUE(input$bold_axis_text),
                     border_width   = input$border_width)
      if (!isTRUE(input$show_title)) p <- p + labs(title = NULL)
      return(p)
    }
    
    # Groepen + kleuren
    grp_sel <- if (is.null(input$group)) "dialyse" else input$group
    if (grp_sel == "both") {
      groups_sel <- c("dialyse", "transplant")
      group_cols <- c(dialyse = input$col_line, transplant = input$col_line2)
    } else {
      groups_sel <- grp_sel
      group_cols <- setNames(input$col_line, grp_sel)
    }
    
    # Baseline jaren: sluit plot_years uit zodat er geen overlap is
    bl_years <- setdiff(2016:2019, input$plot_years[1]:input$plot_years[2])
    
    # Secundaire as: geen / stringency / rioolwater / excess (altijd of-of)
    yrs <- input$plot_years[1]:input$plot_years[2]
    ww_note <- NULL
    ss <- if (identical(input$secondary, "stringency")) {
      list(type = "stringency", data = stringency[, .(date, pgroup = NA_character_, value = stringency)],
           lo = 0, hi = 100, label = "Stringency Index", breaks = seq(0, 100, 25))
    } else if (identical(input$secondary, "rioolwater")) {
      dw <- rioolwater_monthly[data.table::year(date) %in% yrs]
      if (nrow(dw) > 0) {
        lab <- if (isTRUE(input$sec_log)) "Rioolwater (\u00d710\u00b9\u00b2 RNA/100.000, log)"
        else "Rioolwater (\u00d710\u00b9\u00b2 RNA/100.000)"
        list(type = "rioolwater",
             data = dw[, .(date, pgroup = NA_character_, value)],
             lo = min(dw$value), hi = max(dw$value),
             label = lab, breaks = NULL)
      } else {
        ww_note <- if (!is.null(rioolwater_msg)) rioolwater_msg
        else "Geen rioolwaterdata in de gekozen jaren."
        list(type = "none", data = NULL, lo = 0, hi = 100, label = "", breaks = NULL)
      }
    } else if (identical(input$secondary, "excess")) {
      ex <- calc_excess(dt, groups_sel, yrs)
      if (!is.null(ex) && nrow(ex) > 0) {
        list(type = "excess", data = ex,
             lo = min(ex$value, na.rm = TRUE), hi = max(ex$value, na.rm = TRUE),
             label = "Excess deaths (all-cause)", breaks = NULL)
      } else {
        list(type = "none", data = NULL, lo = 0, hi = 100, label = "", breaks = NULL)
      }
    } else if (input$secondary %in% c("vacc1","vacc2","vacc3")) {
      vcol <- c(vacc1 = "vaccin1", vacc2 = "vaccin2", vacc3 = "vaccin3_more")[[input$secondary]]
      vlab <- c(vacc1 = "Vaccination dose 1 (%)",
                vacc2 = "Vaccination dose 2 (%)",
                vacc3 = "Vaccination 3+ doses (%)")[[input$secondary]]
      if (is.null(vacc_monthly)) {
        ww_note <- if (!is.null(vacc_msg)) vacc_msg else "Vaccinatiedata niet beschikbaar."
        list(type = "none", data = NULL, lo = 0, hi = 100, label = "", breaks = NULL)
      } else {
        dv <- vacc_monthly[data.table::year(date) %in% yrs,
                           .(date, pgroup = NA_character_, value = get(vcol))]
        if (nrow(dv) > 0) {
          list(type = "vacc", data = dv, lo = 0, hi = 100,
               label = vlab, breaks = seq(0, 100, 25))
        } else {
          ww_note <- "Geen vaccinatiedata in de gekozen jaren (data loopt 2021\u20132022)."
          list(type = "none", data = NULL, lo = 0, hi = 100, label = "", breaks = NULL)
        }
      }
    } else {
      list(type = "none", data = NULL, lo = 0, hi = 100, label = "", breaks = NULL)
    }
    
    p <- if (input$plot_func == "smr") {
      plot_smr(dt,
               groups = groups_sel,
               type = input$type,
               plot_years = input$plot_years[1]:input$plot_years[2],
               show_baseline = input$show_baseline && length(bl_years) > 0,
               baseline_range = input$baseline_range,
               baseline_only = isTRUE(input$baseline_only),
               baseline_years = bl_years,
               show_ci = input$show_ci,
               ci_type = input$ci_type,
               show_waves = input$show_waves,
               sec_type = ss$type,
               alpha_waves = input$alpha_waves,
               border_width = input$border_width,
               bold_axis_text = input$bold_axis_text,
               show_axis_titles = input$show_axis_titles,
               show_wave_legend = input$show_wave_legend,
               group_cols = group_cols,
               col_ci = input$col_ci,
               col_baseline_mean = input$col_baseline_mean,
               col_baseline_range = input$col_baseline_range,
               sec_data = ss$data, sec_lo = ss$lo, sec_hi = ss$hi,
               sec_label = ss$label, sec_breaks = ss$breaks,
               sec_log = identical(input$secondary, "rioolwater") && isTRUE(input$sec_log),
               col_sec = input$col_sec)
    } else {
      plot_mr(dt,
              groups = groups_sel,
              type = input$type,
              plot_years = input$plot_years[1]:input$plot_years[2],
              show_waves = input$show_waves,
              sec_type = ss$type,
              show_baseline = input$show_baseline && length(bl_years) > 0,
              baseline_range = input$baseline_range,
              baseline_years = bl_years,
              per_1000 = input$per_1000,
              adjust_days = input$adjust_days,
              ref_2019 = isTRUE(input$mr_ref2019),
              alpha_waves = input$alpha_waves,
              border_width = input$border_width,
              bold_axis_text = input$bold_axis_text,
              show_axis_titles = input$show_axis_titles,
              show_wave_legend = input$show_wave_legend,
              group_cols = group_cols,
              sec_data = ss$data, sec_lo = ss$lo, sec_hi = ss$hi,
              sec_label = ss$label, sec_breaks = ss$breaks,
              sec_log = identical(input$secondary, "rioolwater") && isTRUE(input$sec_log),
              col_sec = input$col_sec,
              col_baseline_mean = input$col_baseline_mean,
              col_baseline_range = input$col_baseline_range)
    }
    
    if (!input$show_title) p <- p + labs(title = NULL)
    if (!is.null(ww_note)) p <- p + labs(subtitle = ww_note)
    p
  })
  
  output$main_plot <- renderPlot({
    current_plot()
  })
  
  # --- Download ---
  output$download_plot <- downloadHandler(
    filename = function() {
      grp <- if (is.null(input$group)) "dialyse" else input$group
      paste0(input$plot_func, "_", input$type, "_", grp, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- current_plot()
      
      # Transparante achtergrond
      if (input$transparent_bg) {
        p <- p + theme(
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA)
        )
      }
      
      # Geen raster
      if (input$no_grid) {
        p <- p + theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
      }
      
      ggsave(file, plot = p,
             width = input$plot_width, height = input$plot_height,
             units = "cm", dpi = input$plot_dpi,
             bg = if (input$transparent_bg) "transparent" else "white")
    }
  )
  
  # --- Excess tabel ---
  excess_table_r <- reactive({
    req(identical(input$secondary, "excess"))
    dt <- smr_dt_r()
    grp_sel <- if (is.null(input$group)) "dialyse" else input$group
    groups_sel <- if (grp_sel == "both") c("dialyse", "transplant") else grp_sel
    yrs <- input$plot_years[1]:input$plot_years[2]
    ex <- calc_excess(dt, groups_sel, yrs)
    if (is.null(ex) || nrow(ex) == 0) return(NULL)
    ex[, .(
      Jaar    = data.table::year(date),
      Maand   = month.abb[data.table::month(date)],
      Groep   = fifelse(pgroup == "dialyse", "Dialyse", "Transplantatie"),
      `Excess deaths` = round(value, 2)
    )][order(Jaar, data.table::month(ex$date), Groep)]
  })
  
  output$excess_table <- DT::renderDataTable({
    d <- excess_table_r(); req(!is.null(d))
    DT::datatable(d, rownames = FALSE,
                  options = list(pageLength = 24, dom = "tip"),
                  caption = "Excess deaths = geobserveerde sterfgevallen minus maandgemiddelde 2016\u20132019")
  })
  
  output$excess_download_csv <- downloadHandler(
    filename = function() {
      grp <- if (is.null(input$group)) "dialyse" else input$group
      paste0("excess_deaths_allcause_", grp, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      d <- excess_table_r(); req(!is.null(d))
      fwrite(d, file)
    }
  )
  
  # ============================ TAB 2: REGRESSIE ============================
  
  reg_periods <- reactive({
    req(input$reg_period)
    if (isTRUE(input$reg_compare_periods)) {
      yr <- sub("^p[12]_", "", input$reg_period)
      c(paste0("p1_", yr), paste0("p2_", yr))
    } else input$reg_period
  })
  
  reg_data_r <- reactive({
    req(input$reg_outcome, input$reg_cov, input$reg_model)
    tryCatch(
      build_reg_data(reg_periods(), input$reg_outcome, input$reg_cov, input$reg_model,
                     pop_filter = if (is.null(input$reg_pop_filter)) "both" else input$reg_pop_filter),
      error = function(e) NULL
    )
  })
  
  reg_title <- reactive({
    out_lab <- c(dood = "All-cause mortality", covid_dood = "COVID mortality",
                 opname = "COVID hospitalisation", ic_opname = "COVID ICU admission",
                 covid_dood_opname = "COVID mortality or ICU admission",
                 positieve_test = "Positive test")[[input$reg_outcome]]
    pers <- reg_periods()
    per_lab <- if (length(pers) > 1) {
      sprintf("March vs June %s", sub("^p[12]_", "", pers[1]))
    } else REG_PERIOD_LABELS[[input$reg_period]]
    cov_lab <- names(REG_COV_CHOICES)[REG_COV_CHOICES == input$reg_cov]
    if (length(cov_lab) == 0) cov_lab <- input$reg_cov
    sprintf("%s  \u2013  %s  \u2013  %s", out_lab, per_lab, cov_lab)
  })
  
  reg_plot_obj <- reactive({
    d <- reg_data_r()
    validate(need(!is.null(d) && nrow(d) > 0,
                  "Geen data voor deze selectie. Staan de 4 Excel-bestanden in de map 'reg_data'?"))
    ttl <- if (isTRUE(input$reg_show_title)) reg_title() else NULL
    plot_reg(d, col_sig = input$reg_col_sig, col_ns = input$reg_col_ns, title = ttl,
             or_cap = isTRUE(input$reg_cap_or),
             or_max = if (isTRUE(input$reg_cap_or)) input$reg_or_max else Inf,
             bold_axis_text = isTRUE(input$reg_bold_axis),
             show_axis_titles = isTRUE(input$reg_show_axis_titles),
             border_width = input$reg_border_width,
             fixed_xlim = if (isTRUE(input$reg_fixed_x))
               c(input$reg_xmin, input$reg_xmax) else NULL,
             n_y_ticks = if (isTRUE(input$reg_fixed_y)) input$reg_n_ticks else NULL)
  })
  
  output$reg_plot <- plotly::renderPlotly({
    p <- reg_plot_obj()
    gp <- suppressWarnings(plotly::ggplotly(p, tooltip = "text"))
    gp <- plotly::plotly_build(gp)
    # rand om ELK facet-paneel: rechthoek op de paneel-domeinen (ook de binnenranden)
    ax_key <- function(a) if (grepl("^x", a)) sub("^x", "xaxis", a) else sub("^y", "yaxis", a)
    pairs <- unique(do.call(rbind, lapply(gp$x$data, function(tr) data.frame(
      xa = if (is.null(tr$xaxis)) "x" else tr$xaxis,
      ya = if (is.null(tr$yaxis)) "y" else tr$yaxis,
      stringsAsFactors = FALSE))))
    borders <- lapply(seq_len(nrow(pairs)), function(i) {
      xd <- gp$x$layout[[ax_key(pairs$xa[i])]]$domain; if (is.null(xd)) xd <- c(0, 1)
      yd <- gp$x$layout[[ax_key(pairs$ya[i])]]$domain; if (is.null(yd)) yd <- c(0, 1)
      list(type = "rect", xref = "paper", yref = "paper",
           x0 = xd[1], x1 = xd[2], y0 = yd[1], y1 = yd[2],
           line = list(color = "grey40", width = 1),
           fillcolor = "rgba(0,0,0,0)", layer = "above")
    })
    gp$x$layout$shapes <- c(gp$x$layout$shapes, borders)
    plotly::layout(gp, legend = list(orientation = "h", x = 0, y = -0.15))
  })
  
  output$reg_table <- DT::renderDataTable({
    d <- reg_data_r()
    validate(need(!is.null(d) && nrow(d) > 0, "Geen data voor deze selectie."))
    DT::datatable(reg_table(d), rownames = FALSE,
                  options = list(pageLength = 25, dom = "tip"))
  })
  
  output$reg_download_plot <- downloadHandler(
    filename = function() {
      per_tok <- if (isTRUE(input$reg_compare_periods))
        paste0("p1p2_", sub("^p[12]_", "", input$reg_period)) else input$reg_period
      paste0("regressie_", per_tok, "_", input$reg_outcome, "_",
             input$reg_cov, "_", input$reg_model, ".png")
    },
    content = function(file) {
      p <- reg_plot_obj()
      if (isTRUE(input$reg_transparent_bg)) {
        p <- p + theme(
          plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          legend.background     = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA))
      }
      if (isTRUE(input$reg_no_grid)) {
        p <- p + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())
      }
      suppressWarnings(ggsave(file, plot = p,
                              width = input$reg_plot_width, height = input$reg_plot_height,
                              units = "cm", dpi = input$reg_plot_dpi,
                              bg = if (isTRUE(input$reg_transparent_bg)) "transparent" else "white"))
    }
  )
  
  output$reg_download_table <- downloadHandler(
    filename = function() {
      per_tok <- if (isTRUE(input$reg_compare_periods))
        paste0("p1p2_", sub("^p[12]_", "", input$reg_period)) else input$reg_period
      paste0("regressie_", per_tok, "_", input$reg_outcome, "_",
             input$reg_cov, "_", input$reg_model, ".csv")
    },
    content = function(file) {
      d <- reg_data_r(); req(d)
      fwrite(reg_table(d), file)
    }
  )
  
  # ============================ TAB 3: AANTAL PER MAAND ============================
  
  obs_data_r <- reactive({
    req(input$obs_group, input$obs_outcome)
    dt <- smr_dt_r()
    grp <- if (input$obs_group == "both") c("dialyse", "transplant") else input$obs_group
    rbindlist(lapply(grp, function(g) {
      col <- sprintf("obs_%s_%s", g, input$obs_outcome)
      dt[year %in% input$obs_years[1]:input$obs_years[2] & !is.na(get(col)),
         .(jaar = year, maand = month, datum = date, groep = g, aantal = get(col))]
    }), fill = TRUE)
  })
  
  obs_plot_obj <- reactive({
    grp <- if (input$obs_group == "both") c("dialyse", "transplant") else input$obs_group
    group_cols <- if (input$obs_group == "both")
      c(dialyse = input$obs_col1, transplant = input$obs_col2)
    else setNames(input$obs_col1, input$obs_group)
    p <- plot_obs(smr_dt_r(), groups = grp, outcome = input$obs_outcome,
                  plot_years = input$obs_years[1]:input$obs_years[2],
                  chart_type = input$obs_chart,
                  bar_position = if (is.null(input$obs_position)) "dodge" else input$obs_position,
                  group_cols = group_cols,
                  show_axis_titles = input$obs_axis_titles,
                  bold_axis_text = input$obs_bold_axis,
                  border_width = input$obs_border_width)
    if (!input$obs_show_title) p <- p + labs(title = NULL)
    p
  })
  
  output$obs_plot <- renderPlot({ obs_plot_obj() })
  
  output$obs_download_plot <- downloadHandler(
    filename = function() {
      grp <- if (input$obs_group == "both") "both" else input$obs_group
      paste0("aantal_", input$obs_outcome, "_", grp, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      suppressWarnings(ggsave(file, plot = obs_plot_obj(),
                              width = input$obs_plot_width, height = input$obs_plot_height,
                              units = "cm", dpi = input$obs_plot_dpi, bg = "white"))
    }
  )
  
  output$obs_download_table <- downloadHandler(
    filename = function() {
      grp <- if (input$obs_group == "both") "both" else input$obs_group
      paste0("aantal_", input$obs_outcome, "_", grp, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(obs_data_r(), file)
    }
  )
}

shinyApp(if (LOCAL_MODE) ui else secure_app(ui, enable_admin = TRUE), server)