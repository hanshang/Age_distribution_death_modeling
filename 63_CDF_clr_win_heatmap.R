## ---------------------------------------------------------------------------
## Win heat map for "Forecasting multiple density-valued time series"
##
## Each cell is the rank (1 = best) of a forecasting method within one
## evaluation scenario, where a scenario is
##      metric x nominal level x transformation x sex x univariate forecaster.
## Values are the "Mean" summary rows of Tables 1-9 (averages over the 20
## forecast horizons and the 47 prefectures).  Ranks use ties.method = "min",
## so the region-gap / double-gap ties for the female data share a rank.
##
## Outputs, as two standalone figures:
##   Fig_win_heatmap.pdf  rank of every method in every scenario
##   Fig_win_counts.pdf   how often each method ranks first / second-third
## ---------------------------------------------------------------------------

## ---- 1. Method labels, in the column order used in Tables 1-9 --------------

methods <- c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
             "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)",
             "Gender gap", "Region gap", "Double gap")

## ---- 2. The "Mean" rows of Tables 1-9 --------------------------------------
## One row per scenario; 11 columns in the order of `methods`.

vals <- rbind(
  ## ------------------------- ETS -------------------------------------------
  ## Table 1: KLD
  c(0.0614, 0.0388, 0.0447, 0.0166, 0.0233, 0.0605, 0.0411, 0.0448, 0.0611, 0.0249, 0.0249),
  c(0.0176, 0.0146, 0.0178, 0.0322, 0.0105, 0.0175, 0.0150, 0.0175, 0.1155, 0.0177, 0.0461),
  c(0.1533, 0.0093, 0.0648, 0.0161, 0.0163, 0.1620, 0.0412, 0.0581, 0.0464, 0.0175, 0.0175),
  c(0.0876, 0.0104, 0.0533, 0.0229, 0.0072, 0.0384, 0.0172, 0.0745, 0.0284, 0.0174, 0.0128),
  ## Table 2: CPD, alpha = 0.2
  c(0.0531, 0.0652, 0.0617, 0.0811, 0.0831, 0.0534, 0.0587, 0.0589, 0.0535, 0.0365, 0.0365),
  c(0.0698, 0.0783, 0.0817, 0.0688, 0.1096, 0.0649, 0.0668, 0.0734, 0.0431, 0.0476, 0.0422),
  c(0.0615, 0.1103, 0.0742, 0.0869, 0.1187, 0.0785, 0.0848, 0.0627, 0.0637, 0.0638, 0.0638),
  c(0.1230, 0.0833, 0.0845, 0.0881, 0.1467, 0.0825, 0.0753, 0.0851, 0.0834, 0.0585, 0.0968),
  ## Table 3: CPD, alpha = 0.05
  c(0.0662, 0.0580, 0.0436, 0.0335, 0.0382, 0.0656, 0.0506, 0.0473, 0.0663, 0.0513, 0.0513),
  c(0.0309, 0.0526, 0.0422, 0.0355, 0.0379, 0.0305, 0.0415, 0.0405, 0.0516, 0.0438, 0.0382),
  c(0.0396, 0.0291, 0.0457, 0.0321, 0.0370, 0.0972, 0.0335, 0.0493, 0.0560, 0.0241, 0.0241),
  c(0.0669, 0.0376, 0.0686, 0.0360, 0.0428, 0.0451, 0.0418, 0.0710, 0.0686, 0.0289, 0.0585),
  ## Table 4: MIS, alpha = 0.2
  c(1066,  795,  869,  593,  690, 1051,  762,  874, 1054,  611,  611),
  c( 557,  539,  532,  558,  616,  550,  521,  533, 1177,  543,  848),
  c(1616,  523,  884,  607,  647, 1497,  680,  901,  914,  533,  533),
  c(1377,  496,  759,  560,  655,  635,  517,  815,  707,  564,  548),
  ## Table 5: MIS, alpha = 0.05
  c(2400, 1703, 1799, 1122, 1236, 2364, 1613, 1839, 2373, 1079, 1079),
  c( 930,  975,  924,  978, 1204,  908,  937,  938, 2457,  958, 1684),
  c(2677,  925, 1839, 1211, 1244, 3723, 1404, 1879, 1899,  902,  902),
  c(2205,  821, 1597,  964, 1139, 1277,  894, 1785, 1347, 1013,  929),
  ## ------------------------ ARIMA ------------------------------------------
  ## Table 6: KLD
  c(0.1108, 0.1190, 0.1008, 0.0243, 0.1181, 0.1107, 0.1179, 0.1043, 0.1111, 0.0971, 0.0971),
  c(0.0315, 0.0354, 0.0428, 0.0322, 0.0355, 0.0314, 0.0349, 0.0411, 0.2247, 0.0295, 0.2019),
  c(0.1598, 0.0491, 0.0597, 0.0201, 0.0975, 0.1229, 0.0680, 0.0706, 0.0839, 0.0687, 0.0687),
  c(0.0911, 0.0255, 0.0455, 0.0284, 0.0319, 0.0354, 0.0248, 0.0763, 0.0512, 0.0240, 0.0440),
  ## Table 7: CPD, alpha = 0.2
  c(0.0697, 0.1361, 0.0915, 0.0680, 0.1043, 0.0694, 0.1344, 0.0958, 0.0694, 0.1365, 0.1365),
  c(0.0446, 0.0694, 0.0425, 0.0524, 0.0797, 0.0454, 0.0656, 0.0463, 0.0348, 0.0678, 0.0338),
  c(0.0545, 0.0906, 0.0862, 0.0817, 0.1268, 0.0918, 0.0917, 0.1077, 0.0695, 0.1481, 0.1481),
  c(0.1141, 0.0993, 0.0660, 0.0593, 0.1079, 0.0662, 0.0917, 0.0690, 0.0524, 0.0455, 0.0936),
  ## Table 8: CPD, alpha = 0.05
  c(0.1247, 0.1544, 0.1351, 0.0616, 0.1162, 0.1245, 0.1523, 0.1401, 0.1244, 0.1542, 0.1542),
  c(0.0726, 0.0783, 0.0909, 0.0679, 0.0530, 0.0744, 0.0739, 0.0935, 0.0953, 0.0925, 0.0813),
  c(0.0406, 0.0605, 0.0845, 0.0480, 0.0999, 0.1091, 0.0772, 0.1043, 0.1110, 0.1260, 0.1260),
  c(0.0686, 0.0367, 0.0953, 0.0488, 0.0439, 0.0729, 0.0395, 0.0952, 0.0899, 0.0546, 0.0989),
  ## Table 9: MIS, alpha = 0.2
  c(1636, 1775, 1587,  738, 1662, 1636, 1749, 1629, 1636, 1562, 1562),
  c( 705,  727,  781,  676,  733,  701,  716,  773, 1836,  666, 1662),
  c(1656,  866,  997,  673, 1332, 1632, 1022, 1137, 1303, 1145, 1145),
  c(1422,  572,  839,  620,  681,  775,  582,  883,  904,  556,  803),
  ## Table 9: MIS, alpha = 0.05
  c(4108, 4478, 3815, 1528, 3716, 4106, 4386, 3990, 4101, 3824, 3824),
  c(1304, 1383, 1477, 1218, 1233, 1302, 1350, 1479, 4094, 1123, 3353),
  c(2781, 1649, 1920, 1353, 2580, 3957, 2054, 2336, 2918, 2265, 2265),
  c(2308,  996, 1628, 1047, 1072, 1535, 1008, 1767, 1779,  936, 1429)
)
colnames(vals) <- methods

## ---- 3. Scenario key (same order as the rows of `vals`) --------------------

panel_lev <- c("CDF, F", "CDF, M", "clr, F", "clr, M")
key <- expand.grid(panel      = panel_lev,
                   metric     = c("KLD", "CPD (80%)", "CPD (95%)",
                                  "MIS (80%)", "MIS (95%)"),
                   forecaster = c("ETS", "ARIMA"),
                   stringsAsFactors = FALSE)
stopifnot(nrow(key) == nrow(vals))

dat <- cbind(key, as.data.frame(vals)) |>
  pivot_longer(all_of(methods), names_to = "method", values_to = "value") |>
  group_by(forecaster, metric, panel) |>
  mutate(rank = rank(value, ties.method = "min")) |>   # 1 = smallest error
  ungroup() |>
  mutate(
    method     = factor(method, levels = rev(methods)),
    panel      = factor(panel, levels = panel_lev),
    metric     = factor(metric, levels = c("KLD", "CPD (80%)", "CPD (95%)",
                                           "MIS (80%)", "MIS (95%)")),
    forecaster = factor(forecaster, levels = c("ETS", "ARIMA"))
  ) |>
  separate_wider_delim(panel, delim = ", ", names = c("transf", "sex"),
                       cols_remove = FALSE) |>
  mutate(
    transf = factor(transf, levels = c("CDF", "clr")),
    sex    = factor(sex, levels = c("F", "M"),
                    labels = c("Female", "Male"))
  )

## ---- 4. Panel (a): the rank heat map ---------------------------------------
## Sequential single hue, dark = best.  Prints correctly in grayscale.

ramp_lo <- "#F2F6FA"   # rank 11
ramp_hi <- "#0B3B5E"   # rank 1

p_heat <- ggplot(dat, aes(panel, method, fill = rank)) +
  geom_tile(colour = "white", linewidth = 0.9) +
  geom_text(aes(label = rank, colour = rank > 5),
            size = 2.5, show.legend = FALSE) +
  facet_grid(forecaster ~ metric, switch = "y") +
  scale_fill_gradient(low = ramp_hi, high = ramp_lo,
                      breaks = c(1, 4, 8, 11),
                      labels = c("1 (best)", "4", "8", "11 (worst)"),
                      guide = guide_colourbar(barwidth  = unit(4.2, "cm"),
                                              barheight = unit(0.32, "cm"),
                                              ticks = FALSE, reverse = TRUE,
                                              title.vjust = 0.9)) +
  scale_colour_manual(values = c(`TRUE` = "#1B2733", `FALSE` = "#FFFFFF")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = "Rank",
       title = "Rank of each method within a scenario (1 = smallest error)") +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid       = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7.5,
                                    colour = "#3D4B5A"),
    axis.text.y      = element_text(size = 8, colour = "#1B2733"),
    strip.text       = element_text(size = 8.5, face = "bold",
                                    colour = "#1B2733"),
    strip.background = element_blank(),
    panel.spacing.x  = unit(0.45, "lines"),
    panel.spacing.y  = unit(0.65, "lines"),
    plot.title       = element_text(size = 9.5, face = "bold", hjust = 0,
                                    colour = "#1B2733"),
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 7.5),
    legend.position  = "bottom",
    legend.margin    = margin(t = -4)
  )

## ---- 5. Panel (b): how often each method wins ------------------------------

tot <- dat |>
  group_by(method) |>
  summarise(first = sum(rank == 1), top3 = sum(rank <= 3), .groups = "drop")

wins <- dat |>
  group_by(method) |>
  summarise(first = sum(rank == 1),
            top3  = sum(rank <= 3), .groups = "drop") |>
  mutate(other3 = top3 - first) |>
  select(method, `Ranked 1st` = first, `Ranked 2nd-3rd` = other3) |>
  pivot_longer(-method, names_to = "band", values_to = "n") |>
  mutate(band = factor(band, levels = c("Ranked 1st", "Ranked 2nd-3rd")))

p_wins <- ggplot(wins, aes(n, method, fill = band)) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.7,
           position = position_stack(reverse = TRUE)) +
  geom_text(data = subset(tot, first >= 2),
            aes(x = first / 2, y = method, label = first),
            inherit.aes = FALSE, size = 2.5, colour = "#FFFFFF") +
  geom_text(data = tot, aes(x = top3, y = method, label = top3),
            inherit.aes = FALSE, hjust = -0.45, size = 2.4, colour = "#6B7A8A") +
  scale_fill_manual(values = c("Ranked 1st" = "#0B3B5E",
                               "Ranked 2nd-3rd" = "#9DBBD2")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Number of the 40 scenarios", y = NULL, fill = NULL,
       title = "Wins and near-wins across the 40 scenarios") +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "#E6EBF0", linewidth = 0.3),
    axis.text.y        = element_text(size = 8, colour = "#1B2733"),
    axis.text.x        = element_text(size = 7.5, colour = "#3D4B5A"),
    axis.title.x       = element_text(size = 8, colour = "#3D4B5A"),
    plot.title         = element_text(size = 9.5, face = "bold", hjust = 0,
                                      colour = "#1B2733"),
    legend.position    = "bottom",
    legend.text        = element_text(size = 7.5),
    legend.key.size    = unit(0.35, "cm")
  )

## ---- 5b. Win counts broken down by forecaster and sex ----------------------
## Each facet contains 10 scenarios (5 metrics x 2 transformations).

tot_by <- dat |>
  group_by(forecaster, sex, method) |>
  summarise(first = sum(rank == 1), top3 = sum(rank <= 3), .groups = "drop")

wins_by <- tot_by |>
  mutate(other3 = top3 - first) |>
  select(forecaster, sex, method,
         `Ranked 1st` = first, `Ranked 2nd-3rd` = other3) |>
  pivot_longer(c(`Ranked 1st`, `Ranked 2nd-3rd`),
               names_to = "band", values_to = "n") |>
  mutate(band = factor(band, levels = c("Ranked 1st", "Ranked 2nd-3rd")))

p_wins_by <- ggplot(wins_by, aes(n, method, fill = band)) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.6,
           position = position_stack(reverse = TRUE)) +
  geom_text(data = subset(tot_by, first >= 2),
            aes(x = first / 2, y = method, label = first),
            inherit.aes = FALSE, size = 2.3, colour = "#FFFFFF") +
  facet_grid(forecaster ~ sex) +
  scale_fill_manual(values = c("Ranked 1st" = "#0B3B5E",
                               "Ranked 2nd-3rd" = "#9DBBD2")) +
  scale_x_continuous(breaks = c(0, 5, 10), limits = c(0, 10.4),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Number of the 10 scenarios in each panel", y = NULL, fill = NULL,
       title = "Wins and near-wins by univariate time-series forecasting method and gender") +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "#E6EBF0", linewidth = 0.3),
    axis.text.y        = element_text(size = 8, colour = "#1B2733"),
    axis.text.x        = element_text(size = 7.5, colour = "#3D4B5A"),
    axis.title.x       = element_text(size = 8, colour = "#3D4B5A"),
    strip.text         = element_text(size = 8.5, face = "bold",
                                      colour = "#1B2733"),
    strip.background   = element_blank(),
    panel.spacing      = unit(0.8, "lines"),
    plot.title         = element_text(size = 9.5, face = "bold", hjust = 0,
                                      colour = "#1B2733"),
    legend.position    = "bottom",
    legend.text        = element_text(size = 7.5),
    legend.key.size    = unit(0.35, "cm")
)

## ---- 6. Assemble and write --------------------------------------------------

pdf("Fig_win_heatmap.pdf", width = 9.4, height = 5.7)
print(p_heat)
dev.off()

# Win counts

pdf("Fig_win_counts.pdf", width = 5.2, height = 4.4)
print(p_wins)
dev.off()

# Win counts by forecasting method and gender

pdf("Fig_win_counts_by.pdf", width = 7.6, height = 6.0)
print(p_wins_by)
dev.off()

## Companion table: the rank of every method in every scenario.
write.csv(dat[, c("forecaster", "metric", "panel", "method", "value", "rank")],
          "win_heatmap_ranks.csv", row.names = FALSE)

## Console summary
dat |>
  filter(rank == 1) |>
  count(method, sort = TRUE) |>
  print(n = Inf)
