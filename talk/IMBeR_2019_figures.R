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
    summarise_all(.funs = "mean", na.rm = T) %>%
    ungroup()
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
    mutate(arrow_size = ((abs(u*v)/ max(abs(u*v)))+0.2)/6) %>%
    ungroup()
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
# ggsave(fig_1_top, filename = "talk/graph/fig_1_top.png", height = 4, width = 5)

# The bottom figure (heat flux)
# NB: Rather show the MLD as a contour here
# Also add labels for oceanographic/shelf/floor featurs
fig_1_bottom <- ggplot(data = map_base, aes(x = lon, y = lat)) +
  # The ocean temperature
  geom_raster(data = var_mean_states, aes(fill = qt_clim)) +
  # The land mass
  geom_polygon(aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # The bathymetry contours
  geom_contour(data = NAPA_bathy_sub, aes(z = bathy),
               breaks = c(-50),  size = c(0.3),  colour = "black") +
  geom_contour(data = NAPA_bathy_sub, aes(z = bathy),
               breaks = c(-200),  size = c(0.3),  colour = "grey30") +
  geom_contour(data = NAPA_bathy_sub, aes(z = bathy),
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
# ggsave(fig_1_bottom, filename = "talk/graph/fig_1_bottom.png", height = 4, width = 5)

# Convert the figures to grobs
fig_1_top_grob <- ggplotGrob(fig_1_top)
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
ggsave(plot = fig_1, filename = "talk/graph/fig_1.png", height = 8, width = 6)


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
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  # coord_cartesian(xlim = NWA_corners_sub[1:2],
                  # ylim = NWA_corners_sub[3:4]) +
  coord_equal(xlim = NWA_corners_sub[1:2],
              ylim = NWA_corners_sub[3:4], expand = F) +
  labs(x = NULL, y = NULL, colour = "Region", alpha = "Sub-region")
# fig_2
ggsave(fig_2, filename = "talk/graph/fig_2.png", height = 4, width = 6)


# Figure 3 ----------------------------------------------------------------

# The synoptic summary figure

# Load data packet
all_anom <- readRDS("data/packet_all_anom.Rda")

# Load SOM packet for anomaly data
som_all_anom <- readRDS("data/som_all_anom.Rda")

# Determine node index
node_index_all_anom <- event_node_index(all_anom, som_all_anom) %>%
  mutate(event_no = as.numeric(event_no))
# saveRDS(node_index_all_anom, "data/node_index_all_anom.Rda")

# MHW season of (peak) occurrence and other meta-data
NAPA_MHW_meta <- NAPA_MHW_event %>%
  mutate(month_peak = lubridate::month(date_peak, label = T),
         season_peak = case_when(month_peak %in% c("Jan", "Feb", "Mar") ~ "Winter",
                                 month_peak %in% c("Apr", "May", "Jun") ~ "Spring",
                                 month_peak %in% c("Jul", "Aug", "Sep") ~ "Summer",
                                 month_peak %in% c("Oct", "Nov", "Dec") ~ "Autumn"),
         sub_region = as.character(sub_region)) %>%
  left_join(node_index_all_anom, by = c("region", "sub_region", "event_no"))


# Find a large-ish MHW
sub_event_meta <- filter(NAPA_MHW_meta, intensity_cumulative == 102.4355)

# Load the synoptic states data packet
system.time(
  synoptic_states <- readRDS("data/synoptic_states.Rda")
) # 30 seconds

# Unnest the synoptic data
system.time(
  synoptic_states_unnest <- synoptic_states %>%
    select(region, sub_region, event_no, synoptic) %>%
    unnest()
) # 30 seconds

# Load the synoptic vector states data packet
system.time(
  synoptic_vec_states <- readRDS("data/synoptic_vec_states.Rda")
) # 10 seconds

# Unnest the synoptic vector data
system.time(
  synoptic_vec_states_unnest <- synoptic_vec_states %>%
    select(region, sub_region, event_no, synoptic) %>%
    unnest()
) # 27 seconds

system.time(
  synoptic_states_anom_fix <- synoptic_states_unnest %>%
    select(region:lat, sst_anom, taum_anom, qt_anom, mldr10_1_anom) %>%
    filter(lon >= NWA_corners_sub[1], lon <= NWA_corners_sub[2],
           lat >= NWA_corners_sub[3], lat <= NWA_corners_sub[4]) %>%
    group_by(lon, lat) %>%
    mutate(mldr10_1_anom = mldr10_1_anom/max(abs(mldr10_1_anom), na.rm = T)) %>%
    # na.omit() %>%
    ungroup()
) # 3 seconds

# NB: There is an issue downstream with one or more of the vector pixels always having a value of 0
# This needs to be accounted for here before joining the data further
system.time(
  synoptic_vec_states_anom_fix <- synoptic_vec_states_unnest %>%
    select(region:lat, uoce_anom:voce_anom) %>%
    filter(lon >= NWA_corners_sub[1], lon <= NWA_corners_sub[2],
           lat >= NWA_corners_sub[3], lat <= NWA_corners_sub[4]) %>%
    group_by(lon, lat) %>%
    mutate(uoce_anom = case_when(min(uoce_anom, na.rm = T) != 0 & max(uoce_anom, na.rm = T) != 0 ~ uoce_anom),
           voce_anom = case_when(min(voce_anom, na.rm = T) != 0 & max(voce_anom, na.rm = T) != 0 ~ voce_anom)) %>%
    # na.omit() %>%
    ungroup()
) # 8 seconds

system.time(
  synoptic_states_anom_cartesian <- synoptic_states_anom_fix %>%
    # First create means based on the nearest NAPA pixel to OISST pixel
    left_join(lon_lat_NAPA_OISST, by = c("lon", "lat")) %>%
    dplyr::select(-lon, -lat) %>%
    dplyr::rename(lon = lon_O, lat = lat_O) %>%
    group_by(region, sub_region, event_no, lon, lat) %>%
    summarise_all(.funs = "mean", na.rm = T) #%>%
  # Then interpolate the few pixels that aren't filled
  # NB: After spending a day working on interpolation techniques
  # I decided to leave it as this creates more artefacts than it removes
  # group_by(region, sub_region, event_no)
) # 34 seconds

# Constrain vectors
system.time(
  synoptic_vec_states_anom_cartesian <- synoptic_vec_states_anom_fix %>%
    # First create means based on the nearest NAPA pixel to OISST pixel
    left_join(lon_lat_NAPA_OISST, by = c("lon", "lat")) %>%
    dplyr::select(-lon, -lat) %>%
    dplyr::rename(lon = lon_O, lat = lat_O) %>%
    group_by(region, sub_region, event_no, lon, lat) %>%
    summarise_all(.funs = "mean", na.rm = T)
) # 32 seconds

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

# Transformation function for long decimal place legends
scale_decimal <- function(x) sprintf("%.2f", x)

# SST and ocean current panel
sst_U_V_panel <- ggplot(sub_synoptic_state, aes(x = lon, y = lat)) +
  geom_raster(data = sub_synoptic_state, aes(fill = sst_anom)) +
  geom_segment(data = vec_sub, aes(xend = lon + u_anom * current_uv_scalar, yend = lat + v_anom * current_uv_scalar),
               arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
               linejoin = "mitre", size = 0.2, alpha = 0.5) +
  # The land mass
  geom_polygon(data = map_base, aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # The region of the MHW
  geom_polygon(data = filter(NWA_coords, region == sub_event_meta$region), fill = NA, colour = "black", size = 2) +
  # Colour scale
  scale_fill_gradient2(low = "blue", high = "red") +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  # Slightly shrink the plotting area
  coord_equal(xlim = NWA_corners_sub[1:2],
              ylim = NWA_corners_sub[3:4], expand = F) +
  labs(x = NULL, y = NULL, fill = "SST anom. (°C)") +
  theme(legend.position = "bottom")
# sst_U_V_panel

# The net downward heatflux panel
qt_panel <- ggplot(sub_synoptic_state, aes(x = lon, y = lat)) +
  geom_raster(data = sub_synoptic_state, aes(fill = qt_anom)) +
  # The land mass
  geom_polygon(data = map_base, aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # The region of the MHW
  geom_polygon(data = filter(NWA_coords, region == sub_event_meta$region), fill = NA, colour = "black", size = 2) +
  # Colour scale
  scale_fill_gradient2(low = "blue", high = "red") +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  # Slightly shrink the plotting area
  # coord_cartesian(xlim = NWA_corners_sub[1:2], ylim = NWA_corners_sub[3:4], expand = F) +
  coord_equal(xlim = NWA_corners_sub[1:2],
              ylim = NWA_corners_sub[3:4], expand = F) +
  labs(x = NULL, y = NULL,
       fill = "Net downward\nheat flux\nanom. (W/m2)") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8))
# qt_panel

# The wind stress and mixed layer depth panel
taum_mld_panel <- ggplot(sub_synoptic_state, aes(x = lon, y = lat)) +
  geom_raster(data = sub_synoptic_state, aes(fill = taum_anom)) +
  # The land mass
  geom_polygon(data = map_base, aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # The MLD contours
  geom_contour(aes(z = round(mldr10_1_anom, 2), colour = ..level..),
               breaks = c(seq(-0.5, 0.5, 0.1)), size = 1) +
  # The region of the MHW
  geom_polygon(data = filter(NWA_coords, region == sub_event_meta$region), fill = NA, colour = "black", size = 2) +
  # Colour scale
  scale_fill_gradient2(low = "blue", high = "red", labels = scale_decimal) +
  scale_colour_gradient2("MLD\nrel. anom.", low = "yellow", high = "green", labels = scale_decimal) +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  # Slightly shrink the plotting area
  coord_equal(xlim = NWA_corners_sub[1:2],
              ylim = NWA_corners_sub[3:4], expand = F) +
  labs(x = NULL, y = NULL,
       fill = "Wind stress \n(N/m2)") +
  theme(legend.position = "bottom",
        legend.key.width = unit(0.5, "cm"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 5))
# taum_mld_panel

# Extract time series data for top panel
sub_NAPA_MHW_ts <- NAPA_MHW_sub %>%
  select(-clims, -cats) %>%
  unnest(events) %>%
  filter(row_number() %% 2 == 1) %>%
  unnest(events) %>%
  filter(region == sub_event_meta$region,
         sub_region == sub_event_meta$sub_region)

# Subset further only one year for plotting
sub_NAPA_MHW_ts_1_year <- sub_NAPA_MHW_ts %>%
  filter(t >= sub_event_meta$date_peak-45,
         t <= sub_event_meta$date_peak+45)

peak_event <- sub_NAPA_MHW_ts %>%
  filter(t >= sub_event_meta$date_start-1,
         t <= sub_event_meta$date_end+1) %>%
  mutate(start_point = thresh[t == min(t)],
         peak_point = thresh[t == sub_event_meta$date_peak],
         end_point = thresh[t == max(t)])

ts_panel <- ggplot(data = sub_NAPA_MHW_ts_1_year, aes(x = t, y = temp)) +
  geom_line() +
  geom_line(aes(y = seas), col = "skyblue") +
  geom_line(aes(y = thresh), col = "navy") +
  geom_flame(aes(y2 = thresh)) +
  geom_flame(data = peak_event, aes(y2 = thresh), fill = "red") +
  geom_segment(data = peak_event, colour = "springgreen",
               aes(x = sub_event_meta$date_start-1, xend = sub_event_meta$date_start-1,
                   y = start_point-1, yend = start_point+1)) +
  geom_segment(data = peak_event, colour = "springgreen",
               aes(x = sub_event_meta$date_end+1, xend = sub_event_meta$date_end+1,
                   y = end_point-1,
                   yend = end_point+1)) +
  geom_segment(data = peak_event, colour = "forestgreen",
               aes(x = sub_event_meta$date_peak, xend = sub_event_meta$date_peak,
                   y = peak_point-1,
                   yend = peak_point+1)) +
  labs(x = NULL, y = "Temp. (°C)") +
  scale_x_date(expand = c(0,0))
# ts_panel

# Merge the panels together
bottom_row <- cowplot::plot_grid(sst_U_V_panel, qt_panel, taum_mld_panel, labels = c('B', 'C', 'D'),
                                 align = 'h', rel_widths = c(1, 1, 1), nrow = 1)
top_row <- cowplot::plot_grid(NULL, ts_panel, NULL, labels = c('', 'A', ''), nrow = 1, rel_widths = c(1,2,1))
fig_3 <- cowplot::plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1.5, 3))
# fig_3
ggsave(fig_3, filename = "talk/graph/fig_3.png", height = 6, width = 12)


# Figure 4 ----------------------------------------------------------------

# The summary of a single node

# Find the index of node 9 only
# node_index_9 <- node_index_all_anom %>%
#   filter(node == 9)

# Extract the data packet for this event
sub_synoptic_var_state <- synoptic_states_anom_cartesian %>%
  ungroup() %>%
  left_join(node_index_all_anom, by = c("region", "sub_region", "event_no")) %>%
  filter(node == 9) %>%
  dplyr::select(-region, -sub_region, -event_no, -node, -count) %>%
  group_by(lon, lat) %>%
  summarise_all(.funs = "mean")
sub_synoptic_vec_state <- synoptic_vec_states_anom_cartesian %>%
  ungroup() %>%
  left_join(node_index_all_anom, by = c("region", "sub_region", "event_no")) %>%
  filter(node == 9) %>%
  dplyr::select(-region, -sub_region, -event_no, -node, -count) %>%
  group_by(lon, lat) %>%
  summarise_all(.funs = "mean")
sub_synoptic_state <- left_join(sub_synoptic_var_state, sub_synoptic_vec_state,
                                by = colnames(sub_synoptic_var_state)[1:2]) %>%
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
               linejoin = "mitre", size = 0.4, alpha = 0.5) +
  # The land mass
  geom_polygon(data = map_base, aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # The region of the MHW
  # geom_polygon(data = filter(NWA_coords, region == sub_event_meta$region), fill = NA, colour = "black", size = 2) +
  # Colour scale
  scale_fill_gradient2(low = "blue", high = "red") +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  # Slightly shrink the plotting area
  # coord_cartesian(xlim = NWA_corners_sub[1:2], ylim = NWA_corners_sub[3:4], expand = F) +
  coord_equal(xlim = NWA_corners_sub[1:2],
              ylim = NWA_corners_sub[3:4], expand = F) +
  labs(x = NULL, y = NULL, fill = "SST anom. (°C)") +
  theme(legend.position = "bottom")
sst_U_V_panel

qt_taum_mld_panel <- ggplot(sub_synoptic_state, aes(x = lon, y = lat)) +
  geom_raster(data = sub_synoptic_state, aes(fill = qt_anom)) +
  # The land mass
  geom_polygon(data = map_base, aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # The MLD contours
  geom_contour(aes(z = mldr10_1_anom), breaks = 0,  size = c(0.3),  colour = "black") +
  geom_contour(aes(z = mldr10_1_anom), breaks = 0.1,  size = c(0.3),  colour = "green") +
  geom_contour(aes(z = mldr10_1_anom), breaks = 0.2,  size = c(0.3),  colour = "pink") +
  # Showing taum via size of alpha dot
  geom_point(data = filter(vec_sub, taum_anom > 0.0), aes(size = taum_anom), shape = 1, alpha = 0.4) +
  geom_point(data = filter(vec_sub, taum_anom < 0.0), aes(size = taum_anom), shape = 4, alpha = 0.4) +
  # The vectors
  # geom_segment(data = vec_sub, aes(xend = lon, yend = lat + taum_anom * 50),
  # arrow = arrow(angle = 40, length = unit(0.2, "cm"), type = "open"),
  # linejoin = "mitre", size = 0.4) +

  # The region of the MHW
  # geom_polygon(data = filter(NWA_coords, region == sub_event_meta$region), fill = NA, colour = "black", size = 2) +
  # Colour scale
  scale_fill_gradient2(low = "blue", high = "red") +
  # Size scale
  scale_size_continuous(range = c(0,5), breaks = c(0.0, 0.025, 0.05)) +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  # Slightly shrink the plotting area
  # coord_cartesian(xlim = NWA_corners_sub[1:2], ylim = NWA_corners_sub[3:4], expand = F) +
  coord_equal(xlim = NWA_corners_sub[1:2],
              ylim = NWA_corners_sub[3:4], expand = F) +
  labs(x = NULL, y = NULL,
       fill = "Net downward\nheat flux\nanom. (W/m2)",
       size = "N/m2") +
  theme(legend.position = "bottom")
qt_taum_mld_panel

# The proportion plot
# MHW season of (peak) occurrence and other meta-data
NAPA_MHW_meta <- NAPA_MHW_event %>%
  mutate(month_peak = lubridate::month(date_peak, label = T),
         season_peak = case_when(month_peak %in% c("Jan", "Feb", "Mar") ~ "Winter",
                                 month_peak %in% c("Apr", "May", "Jun") ~ "Spring",
                                 month_peak %in% c("Jul", "Aug", "Sep") ~ "Summer",
                                 month_peak %in% c("Oct", "Nov", "Dec") ~ "Autumn"),
         sub_region = as.character(sub_region)) %>%
  left_join(node_index_all_anom, by = c("region", "sub_region", "event_no"))

# Proportion of MHWs in each season in each node
node_prop_info <- NAPA_MHW_meta %>%
  dplyr::select(region:event_no, month_peak:count) %>%
  group_by(node, season_peak) %>%
  mutate(node_season_prop = round(n()/count, 2)) %>%
  select(season_peak:node_season_prop) %>%
  unique() %>%
  ungroup()

# Fill in the blanks
node_prop_grid <- expand.grid(unique(node_prop_info$season_peak), 1:12) %>%
  dplyr::rename(season_peak = Var1, node = Var2) %>%
  mutate(season_peak = as.character(season_peak)) %>%
  # left_join(NWA_coords, by = "") %>%
  left_join(node_prop_info, by = c("node", "season_peak")) %>%
  mutate(count = replace_na(count, 0),
         node_season_prop = replace_na(node_season_prop, 0))

# Proportion of MHWs in each season in each region
region_prop_info <- NAPA_MHW_meta %>%
  dplyr::select(region:event_no, month_peak:count) %>%
  group_by(node, region) %>%
  mutate(region_node_prop = round(n()/count, 2)) %>%
  select(region, node, count, region_node_prop) %>%
  unique() %>%
  ungroup() #%>%
# right_join(data.frame(node = 1:12), by = "node")
# right_join(NWA_coords, by = "region")

# Fill in the blanks
region_prop_grid <- expand.grid(unique(region_prop_info$region), 1:12) %>%
  dplyr::rename(region = Var1, node = Var2) %>%
  mutate(region = as.character(region)) %>%
  left_join(NWA_coords, by = "region") %>%
  left_join(region_prop_info, by = c("region", "node")) %>%
  mutate(count = replace_na(count, 0),
         region_node_prop = replace_na(region_node_prop, 0)) %>%
  filter(node == 9)

# Join node info to region coordinates to keep ggplot happy
# NWA_coords_more <- left_join(NWA_coords, region_prop_info)

# som_season_runner <- ggplot()

som_prop_plot <- ggplot() +
  # geom_point(aes(colour = val)) +
  # geom_raster(aes(fill  = val)) +
  geom_polygon(data = map_base, aes(group = group, x = lon, y = lat), show.legend = F) +
  geom_polygon(data = region_prop_grid, aes(group = region, x = lon, y = lat, fill = region_node_prop), colour = "black") +
  geom_label(data = region_prop_grid, aes(x = -60, y = 35, label = paste0("n = ",count,"/611"))) +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(35, 55, 10),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  # Slightly shrink the plotting area
  # coord_cartesian(xlim = NWA_corners_sub[1:2], ylim = NWA_corners_sub[3:4], expand = F) +
  coord_equal(xlim = NWA_corners_sub[1:2],
              ylim = NWA_corners_sub[3:4], expand = F) +
  scale_fill_distiller(palette = "BuPu", direction = -1) +
  labs(x = NULL, y = NULL, fill = "Proportion of events\nper region") +
  # facet_wrap(~node, ncol = 4) +
  theme(legend.position = "bottom")
som_prop_plot

# The meta plot

# Calculate mean and median per node for plotting
node_h_lines <- NAPA_MHW_meta %>%
  filter(node == 9) %>%
  group_by(node) %>%
  summarise(mean_int_cum = mean(intensity_cumulative, na.rm = T),
            median_int_cum = median(intensity_cumulative, na.rm = T))

# Create the figure
som_lolli_plot <- ggplot(data = NAPA_MHW_meta, aes(x = date_peak, y = intensity_cumulative)) +
  geom_lolli() +
  geom_point(aes(colour = season_peak)) +
  geom_label(aes(x = as.Date("2007-01-01"), y = 450, label = paste0("n = ", count,"/",length(node))),
             size = 3, label.padding = unit(0.5, "lines")) +
  geom_hline(data = node_h_lines, aes(yintercept = mean_int_cum), linetype = "dashed") +
  # geom_hline(data = node_h_lines, aes(yintercept = median_int_cum), linetype = "dotted") +
  # facet_wrap(~node) +
  labs(x = "", y = "Cumulative intensity (°C x days)", colour = "Season") +
  theme(legend.position = "bottom")
som_lolli_plot

# Stick them together
# bottom_row <- cowplot::plot_grid(sst_U_V_panel, qt_taum_mld_panel, labels = c('B', 'C'), align = 'h', rel_widths = c(1, 1))
fig_4 <- cowplot::plot_grid(sst_U_V_panel, qt_taum_mld_panel,
                            som_prop_plot, som_lolli_plot,
                            labels = c('A', 'B', 'C', 'D'), ncol = 2, rel_heights = c(1, 1))
# fig_4
ggsave(fig_4, filename = "talk/graph/fig_4.png", height = 10, width = 12)

