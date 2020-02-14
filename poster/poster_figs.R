# poster/poster_figs.R
# This script contains the code used to create the custom figures for the landscape poster


# Setup -------------------------------------------------------------------

source("code/functions.R")


# Figure 1 ----------------------------------------------------------------

# Study area
NWA_study_area <- ggplot(data = NWA_coords, aes(x = lon, y = lat)) +
  geom_polygon(aes(colour = region, fill = region), size = 1.5, alpha = 0.2) +
  geom_polygon(data = map_base, aes(group = group), show.legend = F) +
  coord_cartesian(xlim = c(min(NWA_coords$lon)-2, max(NWA_coords$lon)+2),
                  ylim = c(min(NWA_coords$lat)-2, max(NWA_coords$lat)+0.5),
                  expand = FALSE) +
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "top") +
  scale_y_continuous(breaks = c(40, 50),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = 'Dark2')[c(1,2,4,5,3,6)]) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = 'Dark2')[c(1,2,4,5,3,6)]) +
  labs(x = NULL, y = NULL, colour = "Region", fill = "Region") +
  theme_bw() +
  theme(legend.position = c(0.6, 0.2),
        legend.background = element_rect(colour = "black"),
        legend.direction = "horizontal")
NWA_study_area

# Lollis
MHW_lolli_plot <- ggplot(data = OISST_MHW_event , aes(x = date_peak, y = intensity_cumulative)) +
  geom_lolli(aes(colour = region), colour_n = "red", n = 0, show.legend = F) +
  labs(x = "Peak Date", y = "Cum. Intensity (°C x days)") +
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = 'Dark2')[c(1,2,4,5,3,6)]) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(50, 200, 50), expand = c(0,0)) +
  facet_wrap(~region, ncol = 2) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
MHW_lolli_plot

# COmbine
MHW_region_lolli <- cowplot::plot_grid(NWA_study_area, MHW_lolli_plot, labels = c('A)', 'B)'), label_size = 10,
                                       align = 'hv', rel_widths = c(1.2, 1), nrow = 1, axis = "l")

# Blank bit at top for posterdown formatting
top_row <- ggplot() + theme_void()

# Steek'em
MHW_region_lolli_pad <- cowplot::plot_grid(top_row, MHW_region_lolli, ncol = 1, rel_heights = c(1, 10))
ggsave(plot = MHW_region_lolli_pad, filename = "poster/MHW_region_lolli_pad.png", height = 4.1, width = 9)


# Figure 2 ----------------------------------------------------------------

# NB: Must be run on tikoraluk
fig_data_packet("gm",	14)


# Figure 3 ----------------------------------------------------------------

# The base packet info
fig_packet <- fig_data_prep(readRDS("data/SOM/som.Rda"))

# Filter out non-target nodes
fig_packet$som_data_wide <- filter(fig_packet$som_data_wide, node == 9)
fig_packet$som_data_sub <- filter(fig_packet$som_data_sub, node == 9)
fig_packet$other_data_wide <- filter(fig_packet$other_data_wide, node == 9)
fig_packet$OISST_MHW_meta <- filter(fig_packet$OISST_MHW_meta, node == 9)
fig_packet$node_season_info <- filter(fig_packet$node_season_info, node == 9)
fig_packet$node_region_info <- filter(fig_packet$node_region_info, node == 9)
fig_packet$region_prop_label <- filter(fig_packet$region_prop_label, node == 9)
fig_packet$node_h_lines <- filter(fig_packet$node_h_lines, node == 9)
fig_packet$eddy_data <- filter(fig_packet$eddy_data, node == 9)

## The anomaly states for node 9
# SST + U + V (anom)
sst_u_v_anom_sub <- fig_sst_u_v_anom(fig_packet, 1, 1, 1)
# Air Temp + U + V (anom)
air_u_v_mslp_anom_sub <- fig_air_u_v_mslp_anom(fig_packet, 1, 1, 1)
# Net downward heat flux and MLD (anom)
qnet_mld_anom_sub <- fig_qnet_mld_anom(fig_packet, 1, 1, 1)

# Combine
node_9_anom <- cowplot::plot_grid(sst_u_v_anom_sub, air_u_v_mslp_anom_sub,  qnet_mld_anom_sub,
                                  labels = c('A)', 'B)', 'C)'), nrow = 1)
ggsave(node_9_anom, filename = paste0("poster/node_9_anom.png"), height = 5, width = 16)


# Figure 4 ----------------------------------------------------------------

# Change factor order of seasons for colour palette
fig_packet$OISST_MHW_meta$season_peak <- factor(fig_packet$OISST_MHW_meta$season_peak,
                                                levels = c("Autumn", "Winter", "Spring", "Summer"))

## The MHW event info for node 9
# Events per region and season per node
region_season_sub <- fig_region_season(fig_packet, 1, 1, 1)
# Lollis showing cum.int. + season
cum_int_season_sub <- fig_cum_int_season(fig_packet, 1, 1, 1) +
  guides(colour = guide_legend(override.aes = list(size = 4, linetype = 0)))
# Lollis showing max.int + region
max_int_region_sub <- fig_max_int_region(fig_packet, 1, 1, 1) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = 'Dark2')[c(1,2,4,3,6)]) +
  guides(colour = guide_legend(override.aes = list(size = 4, linetype = 0)))

# Stick them together
node_9_event <- cowplot::plot_grid(region_season_sub, cum_int_season_sub, max_int_region_sub,
                                  labels = c('A)', 'B)', 'C)'), nrow = 1)
ggsave(node_9_event, filename = paste0("poster/node_9_event.png"), height = 5, width = 16)

