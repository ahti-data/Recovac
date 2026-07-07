# PLOTTING
library(ggplot2)
library(data.table)
smr_dt <- fread("C:/Users/TijnSchickendantzaht/OneDrive - AHTI/Documenten/recovac/output1/recovac output 260412/smr_data.csv")
setDT(smr_dt)
smr_dt[, date := as.Date(sprintf("%d-%02d-01", year, month))]

# Stringency index data
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

# --- COVID waves ---
waves <- smr_dt[!is.na(wave) & wave != "no covid period", 
                .(start = min(date), end = max(date) + 31), by = wave]
waves[, wave := factor(wave, levels = c("lockdown1", "inter-lockdown", 
                                        "lockdown2", "after vacc & delta", 
                                        "omicron"))]

# --- Baseline 2016-2019 per maand ---
baseline <- smr_dt[year %in% 2016:2019, .(
  mean_ac = mean(smr_obs_dialyses_allcause, na.rm = TRUE),
  min_ac  = min(smr_obs_dialyses_allcause,  na.rm = TRUE),
  max_ac  = max(smr_obs_dialyses_allcause,  na.rm = TRUE)
), by = month]

# Koppel baseline aan 2020-2023 dates
plot_dt <- smr_dt[year %in% 2020:2023]
baseline_plot <- plot_dt[, .(date, month)][baseline, on = "month"][order(date)]


plot_smr <- function(dt,
                     type = c("allcause", "covid"), # kies tussen deze 2
                     show_baseline = TRUE, # mean 2016-2019
                     baseline_range = TRUE, # min max 2016-2019
                     show_ci = TRUE, # smr confidence interval
                     show_waves = TRUE, # waves op achtergrond
                     alpha_waves = 0.25, # hoe licht zijn de waves aanwezig
                     show_stringency = FALSE, # stringency index
                     baseline_years = 2016:2019,
                     plot_years = 2020:2023,
                     col_smr_line = NULL,
                     col_ci = NULL,
                     col_baseline_mean = "grey50",
                     col_baseline_range = "grey70",
                     col_waves = NULL,
                     col_stringency = "darkgreen",
                     title = NULL) {
  
  type <- match.arg(type)
  
  if (type == "allcause") {
    smr_col  <- "smr_obs_dialyses_allcause"
    low_col  <- "ci_low_dialyse_allcause"
    high_col <- "ci_high_dialyse_allcause"
    if (is.null(col_smr_line)) col_smr_line <- "steelblue"
  } else {
    smr_col  <- "smr_obs_dialyses_covid"
    low_col  <- "ci_low_dialyse_covid"
    high_col <- "ci_high_dialyse_covid"
    if (is.null(col_smr_line)) col_smr_line <- "firebrick"
  }
  
  if (is.null(col_ci)) col_ci <- col_smr_line
  
  plot_dt <- dt[year %in% plot_years & !is.na(get(smr_col))]
  plot_dt[, gap_group := cumsum(c(1, diff(as.numeric(date)) > 35))]
  
  if (show_waves) {
    waves <- dt[!is.na(wave) & wave != "no covid period",
                .(start = min(date), end = max(date) + 31), by = wave]
    waves[, wave := factor(wave, levels = c("lockdown1", "inter-lockdown",
                                            "lockdown2", "after vacc & delta",
                                            "omicron"))]
  }
  
  if (show_baseline) {
    baseline <- dt[year %in% baseline_years, .(
      bl_mean = mean(get(smr_col), na.rm = TRUE),
      bl_min  = min(get(smr_col),  na.rm = TRUE),
      bl_max  = max(get(smr_col),  na.rm = TRUE)
    ), by = month]
    baseline_plot <- dt[year %in% plot_years, .(date, month)
    ][baseline, on = "month"][order(date)]
  }
  
  # Schaalfactor voor secundaire as
  if (show_stringency) {
    smr_range <- range(plot_dt[, get(smr_col)], na.rm = TRUE)
    scale_factor <- diff(smr_range) / 100
    offset <- smr_range[1]
    str_plot <- copy(stringency)
    str_plot[, scaled := stringency * scale_factor + offset]
  }
  
  type_label <- ifelse(type == "allcause", "All-cause", "COVID")
  if (is.null(title)) title <- sprintf("SMR %s – Dialyse patiënten (%s-%s)",
                                       type_label, min(plot_years), max(plot_years))
  
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
  
  if (show_baseline) {
    if (baseline_range) {
      p <- p + geom_ribbon(data = baseline_plot,
                           aes(x = date, ymin = bl_min, ymax = bl_max),
                           fill = col_baseline_range, alpha = 0.3)
    }
    p <- p + geom_line(data = baseline_plot,
                       aes(x = date, y = bl_mean),
                       colour = col_baseline_mean, linetype = "dashed", linewidth = 0.5)
  }
  
  if (show_ci) {
    p <- p + geom_ribbon(data = plot_dt,
                         aes(x = date, ymin = get(low_col), ymax = get(high_col),
                             group = gap_group),
                         fill = col_ci, alpha = 0.2)
  }
  
  if (show_stringency) {
    p <- p +
      geom_line(data = str_plot, aes(x = date, y = scaled),
                colour = col_stringency, linewidth = 0.6, alpha = 0.6) +
      geom_point(data = str_plot, aes(x = date, y = scaled),
                 colour = col_stringency, size = 1, alpha = 0.6)
  }
  
  p <- p +
    geom_line(data = plot_dt,
              aes(x = date, y = get(smr_col), group = gap_group),
              colour = col_smr_line, linewidth = 0.8) +
    geom_point(data = plot_dt,
               aes(x = date, y = get(smr_col)),
               colour = col_smr_line, size = 1.5) +
    #geom_hline(yintercept = 1, linetype = "dotted", colour = "red", linewidth = 0.4) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    labs(title = title, x = NULL, y = "SMR") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  
  if (show_stringency) {
    p <- p + scale_y_continuous(
      sec.axis = sec_axis(~ (. - offset) / scale_factor, 
                          name = "Stringency Index",
                          breaks = seq(0, 100, by = 25))
    )
  }
  
  return(p)
}


plot_smr(smr_dt, type = "allcause")

plot_smr(smr_dt, type = "allcause", show_stringency = T,  col_stringency = "red")
plot_smr(smr_dt, type = "allcause", show_ci = F)
plot_smr(smr_dt, type = "allcause", baseline_range = F)
plot_smr(smr_dt, type = "allcause", show_baseline = F, baseline_range = F)
plot_smr(smr_dt, type = "allcause", show_waves = F, plot_years = 2020:2022)

plot_smr(smr_dt, type = "allcause", show_baseline = F, baseline_range = F, 
         show_ci = F, show_waves = F)

plot_smr(smr_dt, type = "allcause", 
         col_smr_line = "darkblue",
         col_ci = "blue",
         col_baseline_mean = "orange",
         col_baseline_range = "orange",
         col_waves = c("lockdown1"           = "#e41a1c",
                       "inter-lockdown"      = "#377eb8",
                       "lockdown2"           = "#ff7f00",
                       "after vacc & delta"  = "#4daf4a",
                       "omicron"             = "#984ea3"))

plot_smr(smr_dt, type = "covid")


 
#### SLA AFBEELDING OP ####
# vervang door p <- gewenste plot
p <- plot_smr(smr_dt, type = "allcause", show_baseline = F, baseline_range = F)

# Vervang "smr_allcause.png" door map waar jij wil opslaan
ggsave("smr_allcause.png", plot = p, width = 12, height = 6, dpi = 300)





# FUNCTIE VOOR mortality ratios
plot_mr <- function(dt,
                    type = c("allcause", "covid"),
                    show_waves = TRUE,
                    show_stringency = FALSE,
                    per_1000 = FALSE,
                    plot_years = 2020:2023,
                    col_line = NULL,
                    col_waves = NULL,
                    alpha_waves = 0.25,
                    col_stringency = "darkgreen",
                    title = NULL) {
  
  type <- match.arg(type)
  
  if (type == "allcause") {
    mr_col <- "mort_ratio_dialyse_allcause"
    if (is.null(col_line)) col_line <- "steelblue"
  } else {
    mr_col <- "mort_ratio_dialyse_covid"
    if (is.null(col_line)) col_line <- "firebrick"
  }
  
  plot_dt <- dt[year %in% plot_years & !is.na(get(mr_col))]
  plot_dt[, gap_group := cumsum(c(1, diff(as.numeric(date)) > 35))]
  
  if (per_1000) {
    plot_dt[, plot_val := get(mr_col) * 1000]
    y_label <- "Deaths per 1,000"
  } else {
    plot_dt[, plot_val := get(mr_col)]
    y_label <- "Mortality Rate"
  }
  
  if (show_waves) {
    waves <- dt[!is.na(wave) & wave != "no covid period",
                .(start = min(date), end = max(date) + 31), by = wave]
    waves[, wave := factor(wave, levels = c("lockdown1", "inter-lockdown",
                                            "lockdown2", "after vacc & delta",
                                            "omicron"))]
  }
  
  # Schaalfactor voor secundaire as
  if (show_stringency) {
    val_range <- range(plot_dt$plot_val, na.rm = TRUE)
    scale_factor <- diff(val_range) / 100
    offset <- val_range[1]
    str_plot <- copy(stringency)
    str_plot[, scaled := stringency * scale_factor + offset]
  }
  
  type_label <- ifelse(type == "allcause", "All-cause", "COVID")
  rate_label <- ifelse(per_1000, "per 1,000", "")
  if (is.null(title)) title <- sprintf("Mortality Rate %s %s – Dialyse patiënten (%s-%s)",
                                       type_label, rate_label,
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
  
  if (show_stringency) {
    p <- p +
      geom_line(data = str_plot, aes(x = date, y = scaled),
                colour = col_stringency, linewidth = 0.6, alpha = 0.6) +
      geom_point(data = str_plot, aes(x = date, y = scaled),
                 colour = col_stringency, size = 1, alpha = 0.6)
  }
  
  p <- p +
    geom_line(data = plot_dt,
              aes(x = date, y = plot_val, group = gap_group),
              colour = col_line, linewidth = 0.8) +
    geom_point(data = plot_dt,
               aes(x = date, y = plot_val),
               colour = col_line, size = 1.5) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    labs(title = title, x = NULL, y = y_label) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  
  if (show_stringency) {
    p <- p + scale_y_continuous(
      sec.axis = sec_axis(~ (. - offset) / scale_factor, 
                          name = "Stringency Index",
                          breaks = seq(0, 100, by = 25))
    )
  }
  
  return(p)
}


# Ruwe ratio (als percentage)
plot_mr(smr_dt, type = "allcause")

# Per 1000
plot_mr(smr_dt, type = "allcause", per_1000 = TRUE)

plot_mr(smr_dt, type = "allcause", per_1000 = TRUE, show_waves = FALSE)
plot_mr(smr_dt, type = "covid", per_1000 = TRUE, show_waves = FALSE)


# OPSLAAN
p <- plot_mr(smr_dt, type = "allcause")
ggsave("mr_allcause.png", plot = p, width = 12, height = 6, dpi = 300)




#### OUDE PLOT CODE ####
# #smr_dt <- smr_dt[year > 2019]
# waves <- smr_dt[!is.na(wave), .(start=min(date), end = max(date) + 31), by = wave]
# waves[, wave := factor(wave, levels = c("no covid period", "lockdown1", 
#                                         "inter-lockdown", "lockdown2",
#                                         "after vacc & delta", "omicron"))]
# 
# 
# baseline <- smr_dt[year %in% 2016:2019, .(
#   mean_dia_ac = mean(smr_obs_dialyses_allcause, na.rm=T),
#   min_dia_ac = min(smr_obs_dialyses_allcause, na.rm=T),
#   max_dia_ac = max(smr_obs_dialyses_allcause, na.rm=T)
# ), by = month]
# 
# baseline_plot <- smr_dt[year %in% 2020:2023, .(date, month)][baseline, on = "month"][order(date)]
# 
# p <- ggplot(smr_dt[year > 2019], aes(x=date)) +
#   geom_rect(data = waves,
#             aes(xmin=start, xmax = end, ymin = 0, ymax=30, fill = wave),
#             alpha= 0.2,
#             color = NA,
#             inherit.aes = F) +
#   #scale_fill_brewer(palette = "Pastel1", name = "wave") +
#   geom_line(aes(y = smr_obs_dialyses_allcause, color = "ac"), size = 1) +
#   #geom_line(aes(y = smr_obs_dialyses_covid, , color = "covid"), size = 1) +
#   #geom_point() +
#   #geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
#   scale_y_continuous(
#     breaks = seq(0,12, by = 1)
#   ) +
#   scale_x_date(
#     date_breaks = "1 months",
#     date_labels = "%m-%y"
#   ) +
#   labs(
#     x = "Period",
#     y = "SMR",
#     color = "kidney patient type",
#     title = "Monthly SMR over time"
#   ) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# p
# 
# 
# ggsave(filename = "data/plots/smr_nefro.png",
#        plot = p,
#        width = 12,
#        height = 5,
#        dpi = 300)
# 
# 
# # PLOT 2016-2019 as seasonal comparison in background
# ggplot(smr_dt[year %in% 2020:2023], aes(x = date)) +
#   geom_ribbon(data = baseline_plot, aes(x = date, ymin = min_dia_ac, ymax = max_dia_ac),
#               fill = "orange", alpha = .2, inherit.aes = F) +
#   geom_line(data = baseline_plot, aes(x = date, y = mean_dia_ac),
#             color = "orange", linetype = "dashed", inherit.aes = F) +
#   # geom_ribbon(data = baseline_plot, aes(x = date, ymin = min_trans, ymax = max_trans),
#   #             fill = "lightblue", alpha = .2, inherit.aes = F) +
#   # geom_line(data = baseline_plot, aes(x = date, y = mean_trans),
#   #            color = "lightblue", linetype = "dashed", inherit.aes = F) +
#   geom_line(aes(y = smr_obs_dialyses_allcause, color = "dialyses"), size = 1) +
#   #geom_line(aes(y = smr_transplant, , color = "transplant"), size = 1) +
#   scale_y_continuous(
#     breaks = seq(0,12, by = 1)) +
#   scale_x_date(
#     date_breaks = "1 months",
#     date_labels = "%m-%y") +
#   labs(
#     x = "Period",
#     y = "SMR",
#     color = "kidney patient type",
#     title = "Monthly SMR over time") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 
# 
# # Crude rates
# p <- ggplot(smr_dt, aes(x=date)) +
#   geom_line(aes(y = mort_ratio_dialyse * 1000, color = "dialyses"), size = 1) + 
#   geom_line(aes(y = mort_ratio_transplant * 1000, , color = "transplant"), size = 1) +
#   #geom_point() +
#   #geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
#   # scale_y_continuous(
#   #   breaks = seq(0,12, by = 1)
#   # ) +
#   scale_x_date(
#     date_breaks = "1 months",
#     date_labels = "%m-%y"
#   ) + 
#   labs(
#     x = "Period",
#     y = "Number of deaths per 1000 persons",
#     color = "kidney patient type",
#     title = "Mortality rate kidney patients"
#   ) + 
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# p
# 
# ggsave(filename = "data/plots/mortrate_nefro.png",
#        plot = p,
#        width = 12,
#        height = 5,
#        dpi = 300)
# 
# 
# 
# 
# #### Per wave ####
# # wave_smr <- smr_dt[!is.na(wave), .(
# #   date = as.Date(min(date) + (max(date) - min(date)) / 2),
# #   dia = mean(smr_dialyses, na.rm=T),
# #   trans = mean(smr_transplant, na.rm=T)
# # ), by = wave]
# # 
# # wave_smr_long <- melt(wave_smr, id.vars = c("wave", "date"),
# #                       variable.name = "type",
# #                       value.name = "smr")
# # 
# # ggplot(smr_dt[year %in% 2020:2023], aes(x = date)) +
# #   # geom_rect(data = waves,
# #   #           aes(xmin=start, xmax = end, ymin = 2, ymax=6, fill = wave),
# #   #           alpha= 0.2,
# #   #           color = NA,
# #   #           inherit.aes = F) +
# #   geom_point(data = wave_smr_long, aes(x = date, y = smr, color = type),
# #              size = 2, inherit.aes = F) +
# #   geom_line(data = wave_smr_long, aes(x = date, y = smr, color = type, group = type),
# #             inherit.aes = F) +
# #   theme_bw() + 
# #   theme_minimal()
