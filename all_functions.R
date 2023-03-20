## All functions gathered together in one R file


oct_to_df <- function(prob_path, x_path, y_path) {
  # Import data -----------------------
  prob <- readr::read_csv(prob_path, col_names = FALSE, col_types = cols())

  x <- readr::read_csv(x_path,
                       col_names = FALSE,
                       col_types = cols()) %>%
    pivot_longer(everything(),
                 names_to = "col_name",
                 values_to = "x_degrees")

  y <- readr::read_csv(y_path,
                       col_names = FALSE,
                       col_types = cols()) %>%
    pivot_longer(everything(),
                 names_to = "col_name",
                 values_to = "y_degrees")

  # Check -----------------------
  stopifnot(nrow(prob) == nrow(y),
            ncol(prob) == nrow(x))

  # Set coordinates -----------------------
  colnames(prob) <- as.character(x$x_degrees)
  prob <- prob %>% dplyr::mutate(y = y$y_degrees)

  # Transform to data frame -----------------------
  oct <- prob %>%
    pivot_longer(-y, names_to = "x", values_to = "prob")
  oct$x <- as.double(oct$x)

  # Clean, Reorder Columns, Return -----------------------
  nan_area <- oct %>% dplyr::filter(is.na(prob))

  oct <- oct %>% dplyr::filter(!is.na(prob))

  rnfl_list <- list("clean" = oct, "nans" = nan_area)
}



create_sf_dframe <- function(oct, vf, distance = 0.5, morph = TRUE) {

  sf_dframe <- vf %>% dplyr::mutate(oct_superpxl_prob = as.numeric(NA))

  for (j in 1:nrow(sf_dframe)) {

    oct_selection <- oct %>%
      dplyr::mutate(euc_dist = sqrt((oct$x - sf_dframe$x_morphed[j])^2 + (oct$y - sf_dframe$y_morphed[j])^2))

    within_range <- oct_selection %>% dplyr::filter(euc_dist <= distance)

    if (nrow(within_range) >= 40) {

      sf_dframe$oct_superpxl_prob[j] <- median(within_range$prob)

    }
  }

  sf_dframe


}


plot_oct <- function(oct_data, nan_oct_data) {

  # Define colors and shapes
  colorscale <- c("#610007", "#C42400", "#C64B00", "#C87300",
                           "#FF7F00", "#FFEC0F", "#ACCE00", "#84D000",
                           "#5DD200", "#34D400", "#0ABD00")

  # Define plot
  p <- ggplot(oct_data, aes(x, y, fill = prob))

  # Plot OCT thickness map
  p <- p + geom_tile(alpha = 0.8) # RNFL heatmap
  # p <- p + geom_tile(data = rgcp, aes(fill = prob), alpha = 0.8) # RGCP heatmap
  #
  p <- p + geom_tile(data = nan_oct_data, aes(x = x, y=y), fill = "white")


  # Specify heatmap options
  p <- p + scale_fill_gradientn(limits = c(0, 0.1),
                                oob = scales::squish, # set > 0.1 to green
                                colors = colorscale)

  # Specify chart options
  p <- p + labs(x = "", y = "") +
    theme_minimal(base_size = 8) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.position = "none") +
    scale_x_continuous(breaks = seq(from = -27, to = 27, by = 6),
                       limits = c(-27, 27)) +
    scale_y_continuous(breaks = seq(from = -21, to = 21, by = 6),
                       limits = c(-21, 21))

  # Return plot
  p
}


plot_vf_on_oct <- function(p_oct, vf_dframe, plot_overlap = TRUE) {

  shapes <- c("0.5" = 19, "1" = 19, "2" = 19, "5" = 19, "95" = 1)

  colors <- c("0.5" = "black", "1" = "black",
              "2" = "black", "5" = "black",
              "95" = "#595959")

  p <- p_oct + geom_point(aes(x_morphed, y_morphed,
                              color = factor(prob),
                              shape = factor(prob)),
                          size = 4,
                          data = vf_dframe,
                          show.legend = FALSE) +
    scale_shape_manual(drop = FALSE,
                       #name = legend_title,
                       values = shapes,
                       breaks = c("0.5", "1", "2", "5", "95"),
                       labels = c("< 0.5%", "< 1%", "< 2%", "< 5%", "100% (unspecified)")) +
    scale_color_manual(drop = FALSE,
                       #name = legend_title,
                       values = colors,
                       breaks = c("0.5", "1", "2", "5", "95"),
                       labels = c("< 0.5%", "< 1%", "< 2%", "< 5%", "100% (unspecified)"))

  if(plot_overlap){

    sf_overlap <- vf_dframe %>% filter(prob < 95 & oct_superpxl_prob < 0.10)

    p <- p + geom_point(data = sf_overlap, aes(x_morphed, y_morphed),
                        size = 6, shape = 5, stroke = 1,
                        color = "black")

  }

  p
}
