# Create lank ownership map

#####################################################
# Load packages
#####################################################
lapply(c("grid", "gridExtra", "tidyterra", "terra", # new to pipeline
         "targets", "tidyverse","sbtools","sf",'dataRetrieval',"nhdplusTools",'dplyr','readxl','readr','stringr','mapview','leaflet', 'httr', 'scico', 'openxlsx', 'rmapshaper', 'scales'),
       library, character.only = T)

# Load color ramp
# Colors come from BatlowS, scientific categorical color ramp
# Not available in scico, but it's from the same palette series
# Available from https://doi.org/10.5281/zenodo.5501399
color_pal <- data.frame(MngNm_L = c("Bureau of Land Management", "U.S. Forest Service", "National Park Service", "U.S. Fish and Wildlife Service", "U.S. Bureau of Reclamation", "Department of Defense", "Federal - other", "Native American areas", "Regional entities", "State entities", "County entities", "Municipal entities", "NGO", "Private"),
                        color = c("#A18A2B", "#667B3E", "#356A59", "#134B61", "#C09036", "#011959", "#226061", "#91862D", "#F19D6B", "#FDB4B4", "#FBC9F1", "#E7985A", "#1E5D62", "#0E365E"))

# Import intersected PAD and Lakes dataset
gdb <- st_read("1_fetch/in/Shapefiles_220721/GreatBasinDessertBnd.shp") 

pad <- st_read('1_fetch/in/Shapefiles_220721/PADUS_3_0VA.shp') |>
  filter(ST_Name %in% c('California','Utah','Nevada','Oregon'))|> 
  filter(Mng_Typ == 'TRIB') |>
  st_transform(st_crs(watersheds)) |> st_make_valid()
glimpse(pad)
unique(pad$Mng_Typ)

pad_trib <- pad |> st_buffer(0) |> st_intersection(watersheds)
pad_trib |> distinct(NAME, lk_w_st,REG_NUM, MngTp_D, Unit_Nm) |> write_csv('saline_lakes_tribes.csv')
pad |> filter(Mng_Typ == 'TRIB')

watersheds <- st_read('1_fetch/in/Saline_lake_watershed_extents/lake_watersheds.shp') |> st_buffer(0) |> st_make_valid()
glimpse(watersheds)


# Download basemap
basemap <- get_tiles(x = pad, provider = "CartoDB.PositronNoLabels", crop = T, verbose = T, zoom = 7, forceDownload = T)

#####################################################
# Define functions
#####################################################
# Create square bounding box (so lake maps plot square)
square_bbox <- function(input){
  # Adapted from FlowmindeR::expand_bbox, https://github.com/Flowminder/FlowmindeR/blob/master/R/expand_bbox.R
  
  # Extract bounding box and convert to data frame
  bbox_input <- sf::st_bbox(input)  #%>%
  #as.numeric() %>%
  #  `names<-`(c("xmin", "ymin", "xmax", "ymax")) %>%
  #  t() %>%
  #  as.data.frame()
  
  # Calculate dimensions
  width <- as.numeric(bbox_input[3] - bbox_input[1])
  height <- as.numeric(bbox_input[4] - bbox_input[2])
  
  # Creates a square bounding box
  #correction <- function(x) x #1/cos(x * pi/180)
  
  # Calculates y centre point
  y_centroid <- mean(bbox_input[4], bbox_input[2])
  x_centroid <- mean(bbox_input[3], bbox_input[1])
  
  desired_width <- max(c(width, height))
  desired_height <- max(c(width, height))
  
  #desired_width <- height * correction(y_centroid)
  #desired_height <- width / correction(y_centroid)
  
  # Only one of these numbers will be positive
  width_diff <- desired_width - width
  height_diff <- desired_height - height
  
  out <- bbox_input %>%
    as.numeric() %>%
    `names<-`(c("xmin", "ymin", "xmax", "ymax")) %>%
    t() %>%
    as.data.frame()
  
  if(width_diff > 0){
    out[3] <- out[3] + width_diff/2
    out[1] <- out[1] - width_diff/2
  }
  
  if(height_diff > 0){
    out[4] <- out[4] + height_diff/2
    out[2] <- out[2] - height_diff/2
  }
  
  return(out)
}

# Create lake labels (that are all 2 lines high to prevent unevenly shaped insets)
lake_label <- function(x){
  ifelse(str_detect(x, "; "), gsub(x = x, pattern = "; ", replacement = "\n"), paste0(x, "\n "))
}

# Plot individual lakes (or groups of lakes)
plot_lakes <- function(data, lake){
  data <- data %>%
    filter(assc_lk == {{lake}})
  
  bbox <- square_bbox(data)
 # basemap <- get_tiles(x = data, provider = "CartoDB.PositronNoLabels", crop = T, verbose = T, zoom = 8, forceDownload = T)
  basemap <- basemap %>%
    crop(data)
  
  ggplot() +
    geom_rect(data = bbox, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, col = NA) +
    geom_spatraster_rgb(data = basemap) +
    geom_sf(data = data,
            aes(fill = I(color)),
            col = NA,
            alpha = 0.7) +
    labs(title = {{lake_label(lake)}}) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 8, hjust = 0.1),
          plot.background = element_rect(colour = "black", fill = NA, size=1))
}

# Plot all lakes
build_main_plot <- function(data){
  ggplot(data = data) +
    geom_spatraster_rgb(data = basemap) +
    geom_sf(aes(fill = I(color)),
            col = NA,
            alpha = 0.7) +
    theme_void() +
    theme(legend.position = "none")
}

# Plot bar plot showing overall area by stakeholder (land manager)
build_bar_plot <- function(data, mapcolor_lookup){
  pad_df <- data %>%
    mutate(Area = st_area(geometry)) %>%
    st_drop_geometry() %>%
    group_by(MngNm_L) %>%
    summarize(Area_gr = sum(Area)) %>%
    mutate(Proportion = as.numeric(100 * (Area_gr / sum(Area_gr))),
           Label = sprintf("  %s%%", round(Proportion, 0))) %>%
    arrange(Proportion) %>%
    left_join(mapcolor_lookup, by = "MngNm_L")
  
  
  ggplot(data = pad_df,
         aes(x =  Proportion, 
             y = reorder(MngNm_L, Proportion))) +
    geom_bar(stat = 'identity', 
             aes(fill = I(color)),
             alpha = 0.7) +
    geom_rect(xmin = -4, xmax = -1, ymin = seq(0.545, 13.545), ymax = seq(1.455, 14.455),
              aes(fill = I(color)),
              alpha = 0.7) +
    geom_text(aes(label = Label), fontface = "plain", position = position_stack(vjust = 1)) +
    labs(x = "Percent of land within lake basins", 
         y = NULL,
         title = NULL)
}

# Locate center of lake basin
center <- function(sf, lake = NULL){
  if(!is.null(lake)){
    sf <- sf %>%
      filter(assc_lk == lake)
  }
  
  sf %>%
    st_bbox() %>%
    st_as_sfc(crs = st_crs(sf)) %>%
    st_centroid() %>%
    st_as_sf() %>%
    st_coordinates() %>%
    as.data.frame()
}

# Define lake plotting order (starting at 9 o'clock position, moving clockwise)
lake_plot_order <- function(sf, lakes){
  cent_overall <- center(sf = pad)
  
  output <- map(lakes,
                function(x) center(sf = pad, lake = x)) %>%
    bind_rows() %>%
    #st_coordinates() %>%
    #as.data.frame()
    add_column(lakes, .before = "X") %>%
    mutate(angle = atan2(Y - cent_overall$Y,
                         X - cent_overall$X)) %>%
    arrange(desc(angle)) %>%
    #mutate(order = 1:n()) %>%
    #select(lakes, order)
    pull(lakes)
}

#####################################################
# Generate maps
#####################################################

# Define lake list (in order)
lake_list <- unique(pad$assc_lk) %>%
  lake_plot_order(pad, .)

main <- build_main_plot(data = pad)

legend <- build_bar_plot(data = pad, mapcolor_lookup = color_pal)

inset_plots <- map(lake_list,
                  function(x) plot_lakes(data = pad, lake = x))

plot_list <- c(list(main), list(legend), inset_plots)

plot_title <- textGrob("Land management across saline lake basins\n ",
                       hjust = 0.75,
                       gp = gpar(fontsize = 18))

# Compile maps into final figure
layout <- rbind(c( 3,  4,  5,  6,  7,  8,  9),
                c(17,  1,  1,  1,  1,  1, 10),
                c(16,  1,  1,  1,  1,  1, 11),
                c(15,  1,  1,  1,  1,  1, 12),
                c(14,  1,  1,  1,  1,  1, 13),
                c( 2,  2,  2,  2,  2,  2,  2),
                c( 2,  2,  2,  2,  2,  2,  2))



output <- grid.arrange(grobs = plot_list,
             layout_matrix = layout,
             top = plot_title) %>%
  gtable_add_padding(padding = unit(c(0.5, 0.25, 0.5, 0.25), "in"))

ggsave("3_visualize/out/ProtectedLands.png", output, 
       width = 8.5, height = 11.5, units = "in")