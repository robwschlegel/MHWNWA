# talk/IMBeR_2019_figures.R
# The purpose of this script is to house the code used to create the figures
# seen in the talk within this same folder.


# Libraries ---------------------------------------------------------------

source("code/functions.R")


# Figure 1 ----------------------------------------------------------------

# The figure of the study area
system.time(
  var_mean_states <- readRDS("data/NAPA_clim_vars.Rda") %>%
    dplyr::select(-doy) %>%
    group_by(lon, lat) %>%
    summarise_all(.funs = "mean") %>%
    ungroup() %>%
    left_join(lon_lat_NAPA_OISST, by = c("lon", "lat")) %>%
    dplyr::select(-lon, -lat) %>%
    dplyr::rename(lon = lon_O, lat = lat_O) %>%
    group_by(lon, lat) %>%
    summarise_all(.funs = "mean", na.rm = T)
) # 12 seconds

# Vector mean states
system.time(
  vec_mean_states <- readRDS("data/NAPA_clim_vecs.Rda") %>%
    dplyr::select(-doy, -wo_clim) %>%
    group_by(lon, lat) %>%
    summarise_all(.funs = "mean") %>%
    ungroup() %>%
    left_join(lon_lat_NAPA_OISST, by = c("lon", "lat")) %>%
    dplyr::select(-lon, -lat) %>%
    dplyr::rename(lon = lon_O, lat = lat_O) %>%
    group_by(lon, lat) %>%
    summarise_all(.funs = "mean", na.rm = T) %>%
    dplyr::rename(u = uoce_clim, v = voce_clim) %>%
    mutate(arrow_size = ((abs(u*v)/ max(abs(u*v)))+0.2)/6)
) # 7 seconds

# Reduce wind/ current vectors
lon_sub <- seq(min(var_mean_states$lon), max(var_mean_states$lon), by = 1)
lat_sub <- seq(min(var_mean_states$lat), max(var_mean_states$lat), by = 1)
# currents <- currents[(currents$lon %in% lon_sub & currents$lat %in% lat_sub),]
vec_mean_states_sub <- vec_mean_states[(vec_mean_states$lon %in% lon_sub & vec_mean_states$lat %in% lat_sub),]

# Establish the vector scalar for the currents
current_uv_scalar <- 4

# Establish the vector scalar for the wind
# wind_uv_scalar <- 0.5

# Wind feature vector coordinates
# cyc_atlantic <- data.frame(x = c(14.0, 16.1, 16.0), y = c(-36.0, -34.4, -32.1),
#                            xend = c(16.0, 16.1, 14.0), yend = c(-34.5, -32.2, -30.6))
# cyc_indian <- data.frame(x = c(36.0, 33.9, 34.0), y = c(-31.5, -33.1, -35.4),
#                          xend = c(34.0, 33.9, 36.0), yend = c(-33.0, -35.3, -36.9))
# westerlies <- data.frame(x = c(18.0, 21.1, 24.2), y = c(-38.0, -37.8, -37.8),
#                          xend = c(21.0, 24.1, 27.2), yend = c(-37.8, -37.8, -38.0))

# The top figure (sea)
  # NB: Add bathymetry as a contour here
fig_1_top <- ggplot(data = map_base, aes(x = lon, y = lat)) +
  # The ocean temperature
  geom_raster(data = var_mean_states, aes(fill = sst_clim)) +
  # The bathymetry
  # stat_contour(data = bathy[bathy$depth < -100 & bathy$depth > -300,],
  # aes(x = lon, y = lat, z = depth), alpha = 0.5,
  # colour = "ivory", size = 0.5, binwidth = 200, na.rm = TRUE, show.legend = FALSE) +
  # The current vectors
  geom_segment(data = vec_mean_states_sub, aes(xend = lon + u * current_uv_scalar, yend = lat + v * current_uv_scalar),
               arrow = arrow(angle = 40, length = unit(vec_mean_states_sub$arrow_size, "cm"), type = "open"),
               linejoin = "mitre", size = 0.4) +
  # The land mass
  geom_polygon(aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # The legend for the vector length
  # geom_label(aes(x = 37.0, y = -38.0, label = "1.0 m/s\n"), size = 3, label.padding = unit(0.5, "lines")) +
  # geom_segment(aes(x = 36.0, y = -38.3, xend = 38.0, yend = -38.3), linejoin = "mitre",
  #              arrow = arrow(angle = 40, length = unit(0.2, "cm"), type = "open")) +
  # Halifax point and label
  # geom_point(data = SACTN_site_list, shape = 19,  size = 2.8, colour = "ivory") +
  # geom_text(data = SACTN_site_list[-c(3,4,7:9,18,21,23:24),], aes(label = order), size = 1.9, colour = "red") +
  # Ocean label
  annotate("text", label = "ATLANTIC\nOCEAN", x = -48, y = 35.0, size = 4.0, angle = 0, colour = "ivory") +
  # Labrador Sea label
  annotate("text", label = "Labrador\nSea", x = -54, y = 58, size = 3.0, angle = 0, colour = "ivory") +
  # Labrador Current line and label
  geom_curve(aes(x = -63, y = 59, xend = -51, yend = 46),
             arrow = arrow(angle = 40, type = "open", length = unit(0.3, "cm")),
             size = 0.5, colour = "ivory", curvature = -0.2) +
  geom_curve(aes(x = -52, y = 45, xend = -69, yend = 41),
             arrow = arrow(angle = 40, type = "open", length = unit(0.3, "cm")),
             size = 0.5, colour = "ivory", curvature = 0.1) +
  annotate("text", label = "Labrador Current", x = -58, y = 54, size = 3.0, angle = 330, colour = "ivory") +
  # Gulf Stream label
  annotate("text", label = "Gulf Stream", x = -60, y = 37, size = 3.0, angle = 10, colour = "ivory") +
  geom_curve(aes(x = -77, y = 32, xend = -50, yend = 40), curvature = -0.3, angle = 160, colour = "ivory",
             arrow = arrow(angle = 40, type = "open", length = unit(0.25,"cm")), alpha = 0.4) +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = scales::unit_format(suffix = "°E", sep = ""),
                     position = "top") +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  labs(x = NULL, y = NULL) +
  # Slightly shrink the plotting area
  coord_cartesian(xlim = NWA_corners_sub[1:2], ylim = NWA_corners_sub[3:4], expand = F) +
  # Use viridis colour scheme
  scale_fill_viridis_c(name = "SST (°C)", option = "D", breaks = seq(0, 25, 5)) +
  # Adjust the theme
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.position = "top")
# fig_1_top
ggsave(fig_1_top, filename = "talk/graph/fig_1_top.png", height = 4, width = 5)

# The bottom figure (heat flux)
  # NB: Rather show the MLD as a contour here
  # Also add labels for oceanographic/shelf/floor featurs
fig_1_bottom <- ggplot(data = map_base, aes(x = lon, y = lat)) +
  # The ocean temperature
  geom_raster(data = var_mean_states, aes(fill = qt_clim)) +
  # The land mass
  geom_polygon(aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # The bathymetry contours
  geom_contour(data = bathy, aes(z = depth),
               breaks = c(-50),  size = c(0.3),  colour = "black") +
  geom_contour(data = bathy, aes(z = depth),
               breaks = c(-200),  size = c(0.3),  colour = "grey30") +
  geom_contour(data = bathy, aes(z = depth),
               breaks = c(-2000),  size = c(0.3),  colour = "grey70") +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = scales::unit_format(suffix = "°E", sep = "")) +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  labs(x = NULL, y = NULL) +
  # Slightly shrink the plotting area
  coord_cartesian(xlim = NWA_corners_sub[1:2], ylim = NWA_corners_sub[3:4], expand = F) +
  # Use viridis colour scheme
  scale_fill_viridis_c(name = "Net downward\nheat flux (W/m2)", option = "A") +
  # Adjust the theme
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.position = "bottom")
# fig_1_bottom
ggsave(fig_1_bottom, filename = "talk/graph/fig_1_bottom.png", height = 4, width = 5)

# Convert the figures to grobs
fig_1_top_grob <- ggplotGrob(fig_1_top)
# fb_inset_grob <- ggplotGrob(fb_inset)
fig_1_bottom_grob <- ggplotGrob(fig_1_bottom)

# Stick them together
fig_1 <- ggplot() +
  # First set the x and y axis values so we know what the ranges are
  # in order to make it easier to place our facets
  coord_equal(xlim = c(1, 10), ylim = c(1, 10), expand = F) +
  # Then we place our facetsover one another using the coordinates we created
  annotation_custom(fig_1_top_grob,
                    xmin = 1, xmax = 10, ymin = 5.5, ymax = 10) +
  # annotation_custom(fb_inset_grob,
  # xmin = 3.5, xmax = 5.5, ymin = 7.2, ymax = 8.8) +
  annotation_custom(fig_1_bottom_grob,
                    xmin = 1, xmax = 10, ymin = 1, ymax = 5.5)
# fig_1
# save
ggsave(plot = fig_1, filename = "talk/graph/fig_1.png", height = 8, width = 8)


# Figure 2 ----------------------------------------------------------------

# Plot the regions
fig_2 <- ggplot(NWA_coords, aes(x = lon, y = lat)) +
  geom_polygon(data = map_base, aes(group = group), show.legend = F) +
  geom_polygon(aes(colour = region), alpha = 0.2, fill = NA, show.legend = F) +
  geom_point(data = NWA_NAPA_info, aes(colour = region, alpha = sub_region),
             shape = 15, size = 0.5) +
  guides(colour = guide_legend(override.aes = list(size = 5), order = 1),
         alpha = guide_legend(override.aes = list(size = 5), order = 2)) +
  scale_alpha_manual(values = c(0.4, 0.7, 1)) +
  coord_cartesian(xlim = NWA_corners_sub[1:2],
                  ylim = NWA_corners_sub[3:4]) +
  labs(x = NULL, y = NULL, colour = "Region", alpha = "Sub-region")
fig_2
ggsave(fig_2, filename = "talk/graph/fig_2.png", height = 5, width = 6)


# Figure 3 ----------------------------------------------------------------

# The synoptic summary figure

# Find a large-ish MHW
sub_event_meta <- filter(NAPA_MHW_meta, intensity_cumulative == 102.4355)

# Extract the data packet for this event
sub_synoptic_var_state <- synoptic_states_anom_cartesian %>%
  ungroup() %>%
  filter(region == sub_event_meta$region,
         sub_region == sub_event_meta$sub_region,
         event_no == sub_event_meta$event_no)
sub_synoptic_vec_state <- synoptic_vec_states_anom_cartesian %>%
  ungroup() %>%
  filter(region == sub_event_meta$region,
         sub_region == sub_event_meta$sub_region,
         event_no == sub_event_meta$event_no)
sub_synoptic_state <- left_join(sub_synoptic_var_state, sub_synoptic_vec_state,
                                by = colnames(sub_synoptic_var_state)[1:5]) %>%
  dplyr::rename(u_anom = uoce_anom, v_anom = voce_anom) %>%
  group_by(lon, lat) %>%
  mutate(arrow_size = ((abs(u_anom*v_anom)/ max(abs(u_anom*v_anom)))+0.2)/6) %>%
  ungroup()

# Reduce wind/ current vectors
lon_sub <- seq(min(sub_synoptic_state$lon), max(sub_synoptic_state$lon), by = 1)
lat_sub <- seq(min(sub_synoptic_state$lat), max(sub_synoptic_state$lat), by = 1)
vec_sub <- sub_synoptic_state %>%
  filter(lon %in% lon_sub, lat %in% lat_sub) %>%
  na.omit()


# Establish the vector scalar for the currents
current_uv_scalar <- 4

# SST and ocean current panel
sst_U_V_panel <- ggplot(sub_synoptic_state, aes(x = lon, y = lat)) +
  geom_raster(data = sub_synoptic_state, aes(fill = sst_anom)) +
  geom_segment(data = vec_sub, aes(xend = lon + u_anom * current_uv_scalar, yend = lat + v_anom * current_uv_scalar),
               arrow = arrow(angle = 40, length = unit(vec_sub$arrow_size, "cm"), type = "open"),
               linejoin = "mitre", size = 0.4) +
  # The land mass
  geom_polygon(data = map_base, aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # The region of the MHW
  geom_polygon(data = filter(NWA_coords, region == sub_event_meta$region), fill = NA, colour = "black", size = 2) +
  # Colour scale
  scale_fill_gradient2(low = "blue", high = "red") +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = scales::unit_format(suffix = "°E", sep = ""),
                     position = "top") +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  labs(x = NULL, y = NULL, fill = "SST anom. (°C)") +
  # Slightly shrink the plotting area
  coord_cartesian(xlim = NWA_corners_sub[1:2], ylim = NWA_corners_sub[3:4], expand = F)
sst_U_V_panel

qt_taum_mld_panel <- ggplot(sub_synoptic_state, aes(x = lon, y = lat)) +
  geom_raster(data = sub_synoptic_state, aes(fill = qt_anom)) +
  # The land mass
  geom_polygon(data = map_base, aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # The MLD contours
    # NB: Rather show this as three distinct colours
  geom_contour(aes(z = mldr10_1_anom), breaks = c(-0.5, 0, 0.5),  size = c(0.3),  colour = "black") +
  # The vectors
  geom_segment(data = vec_sub, aes(xend = lon, yend = lat + taum_anom * 50),
               arrow = arrow(angle = 40, length = unit(0.2, "cm"), type = "open"),
               linejoin = "mitre", size = 0.4) +
  # The region of the MHW
  geom_polygon(data = filter(NWA_coords, region == sub_event_meta$region), fill = NA, colour = "black", size = 2) +
  # Colour scale
  scale_fill_gradient2(low = "blue", high = "red") +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = scales::unit_format(suffix = "°E", sep = ""),
                     position = "top") +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  labs(x = NULL, y = NULL, fill = "Net downward\nheat flux\nanom. (W/m2)") +
  # Slightly shrink the plotting area
  coord_cartesian(xlim = NWA_corners_sub[1:2], ylim = NWA_corners_sub[3:4], expand = F)
qt_taum_mld_panel
