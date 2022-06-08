#' Interactive 3D plot for visualizing results
#'
#' Interactive 3D plot for visualizing computation results
#'
#' @param networkData List with the coordinates matrix, the node size vector
#'   and the Edge matrix data, usually as returned by \code{\link{getPlotNetworkData}}
#' @param points3Dargs List with arguments for \code{\link[plot3D]{points3D}}
#' @param text3Dargs List with arguments for \code{\link[plot3D]{text3D}}
#' @param segments3Dargs List with arguments for \code{\link[plot3D]{segments3D}}
#'
#' @return plotly object
#'
#' @export
#' @examples
plotNetworkData <- function(
  networkData,
  points3Dargs = list(cex = 5, add = FALSE, colkey = FALSE),
  text3Dargs = list(add = TRUE, cex = 2, colkey = FALSE),
  segments3Dargs = list(col = c('red', 'green'), lwd = 5, add = TRUE, colkey = FALSE)
) {

  # We create a tibble with the coordinates data, as well as the node name/ID
  # and the node size

  coords_data <-
    networkData$coordinates %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      node = rownames(networkData$coordinates),
      size = networkData$NodeSize[node]
    )

  # We also create the tibble with the segments data, using the Edges slot
  networkData$Edges[upper.tri(networkData$Edges)] <- NA
  networkData$Edges[networkData$Edges == 0] <- NA

  segment_data <-
    expand.grid(rownames(networkData$Edges), colnames(networkData$Edges)) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(
      From = Var1,
      To = Var2
    ) %>%
    dplyr::mutate(
      corr = purrr::map2_dbl(From, To, ~networkData$Edges[.x, .y]),
      colvar = dplyr::if_else(corr < 0, 'red', 'green')
    ) %>%
    dplyr::filter(!is.na(corr)) %>%
    dplyr::left_join(coords_data, by = c('From' = 'node')) %>%
    dplyr::rename(
      x_from = x,
      y_from = y,
      z_from = z,
      size_from = size
    ) %>%
    dplyr::left_join(coords_data, by = c('To' = 'node')) %>%
    dplyr::rename(
      x_to = x,
      y_to = y,
      z_to = z,
      size_to = size
    ) %>%
    dplyr::filter()

  coords_data <<- coords_data
  segment_data <<- segment_data


  coords_data %>%
    plotly::plot_ly(
      x = ~x, y = ~y, z = ~z, color = ~size
    ) %>%
    plotly::add_markers(
      size = ~size, sizes = c(10, 20),
      marker = list(symbol = 'circle', sizemode = 'diameter')
    ) %>%
    plotly::add_text(text = ~node, size = 0.8, color = I('black')) %>% {
      temp_plot <- .
      for (i in 1:nrow(segment_data)) {
        temp_plot <- temp_plot %>%
          plotly::add_lines(
            data = coords_data %>%
              dplyr::filter(node %in% c(segment_data$From[i], segment_data$To[i])),
            x = ~x, y = ~y, z = ~z, color = I(segment_data$colvar[i]),
            line = list(width = abs(segment_data$corr[i])*10)
          )
      }
      temp_plot
    }
  #   plotly::add_paths(
  #
  #   )
  #
  #
  #
  # threejs::scatterplot3js(
  #   x = coords_data$x, y = coords_data$y, z = coords_data$z,
  #   axis = FALSE, grid = TRUE, pch = 'o'
  # )
  #
  #
  # relations_df <-
  #   expand.grid(rownames(networkData$Edges), colnames(networkData$Edges)) %>%
  #   dplyr::as_tibble() %>%
  #   dplyr::rename(
  #     from = Var1,
  #     to = Var2
  #   ) %>%
  #   dplyr::mutate(
  #     corr = purrr::map2_dbl(from, to, ~networkData$Edges[.x, .y]),
  #     colvar = dplyr::if_else(corr < 0, 0, 1)
  #   ) %>%
  #   dplyr::filter(corr > 0 | corr < 0)
  #
  # vertices_df <-
  #   networkData$coordinates %>%
  #   dplyr::as_tibble() %>%
  #   dplyr::mutate(
  #     node = rownames(networkData$coordinates),
  #     size = networkData$NodeSize[node]
  #   ) %>%
  #   dplyr::select(node, size)
  #
  #
  # igraph_object <- igraph::graph_from_data_frame(
  #   relations_df, vertices = vertices_df, directed = TRUE
  # )
  #
  # threejs::graphjs(igraph_object)
  #
  # # points
  # do.call(
  #   plot3D::points3D,
  #   c(
  #     list(
  #       x = coords_data$x, y = coords_data$y, z = coords_data$z,
  #       colvar = coords_data$size
  #     ), points3Dargs
  #   )
  # )
  # # text
  # do.call(
  #   plot3D::text3D,
  #   c(
  #     list(
  #       x = coords_data$x, y = coords_data$y, z = coords_data$z,
  #       labels = coords_data$node
  #     ), text3Dargs
  #   )
  # )
  # # segments
  # do.call(
  #   plot3D::segments3D,
  #   c(
  #     list(
  #       x0 = segment_data$x_from, x1 = segment_data$x_to,
  #       y0 = segment_data$y_from, y1 = segment_data$y_to,
  #       z0 = segment_data$z_from, z1 = segment_data$z_to,
  #       colvar = segment_data$colvar
  #     ), segments3Dargs
  #   )
  # )
  #
  # # convert to rgl
  # plot3Drgl::plotrgl(zoom = 1)
#
  # return(invisible(networkData))
}
