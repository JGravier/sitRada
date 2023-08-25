# HCA on PCA coords: returning object hclust or object dendrogram
hca_on_pca <- function(df, objet, nclust){
  pca_dudi <- ade4::dudi.pca(df = df, center = TRUE, scale = TRUE, scannf = FALSE, nf = ncol(df))
  dist_pca <- ade4::dist.dudi(pca_dudi) # distance on coord of PCA indivuduals
  clustering <- hclust(dist_pca, method = "ward.D2") # hca on distances
  
  if (objet == TRUE) {
    returning <- clustering
  } else {
    dendro <- as.dendrogram(clustering)
    returning <- dendextend::color_branches(dend = dendro, k = nclust)
  }
  
  return(returning)
}

# qulification of clusters: from mean of sacle-center residuals
residuals_diff <- function(df, nclust, mean_clusters){
  pca_dudi <- ade4::dudi.pca(df = df, center = TRUE, scale = TRUE, scannf = FALSE, nf = ncol(df))
  center_reduced_df <- pca_dudi$tab
  dist_pca <- ade4::dist.dudi(pca_dudi) # distance on coord of PCA indivuduals
  clustering <- hclust(dist_pca, method = "ward.D2") # hca on distances
  
  if (mean_clusters == TRUE) {
    returning <- center_reduced_df %>%
      mutate(clusters = factor(x = cutree(clustering, k = nclust), levels = 1:nclust)) %>%
      group_by(clusters) %>%
      summarise_all(list(mean))
  } else {
    returning <- tibble(clusters = factor(x = cutree(clustering, k = nclust), levels = 1:nclust))
  }
  
  return(returning)
}


#' Shortest paths distances between two points pattern on a planar network
#'
#' @description
#' Computation of shortest path pairwise distances between between two point patterns of a planar network.
#' Input point patterns data must be snapped on linear networks.
#'
#' @param pp1 A sf point patterns
#' @param pp2 A sf point patterns
#' @param network A sf planar networks
#' @return A tibble of shortest paths
#'
#' @details 
#' * `pp1`/`pp2` points numbers in output tibble refer to initial orders of rows in point patterns data.
#'
#' @seealso [snapPointsToLines()] for snapping points on lines.
#'
#' @importFrom Rdpack reprompt
#' @export

dist_2_pp <- function(pp1, pp2, network) {
  
  #### Calculating distances on observed point patterns ####
  ## creation of an lpp object
  observed_lpp1 <- spatstat.linnet::lpp(
    X = spatstat.geom::as.ppp(X = pp1 %>% dplyr::select(geometry)),
    L = maptools::as.linnet.SpatialLines(X = as(network, "Spatial"))
  )
  
  observed_lpp2 <- spatstat.linnet::lpp(
    X = spatstat.geom::as.ppp(X = pp2 %>% dplyr::select(geometry)),
    L = maptools::as.linnet.SpatialLines(X = as(network, "Spatial"))
  )
  
  ## calculating shortest paths for observed data
  matrix_observed_dist_pi_p <- spatstat.linnet::crossdist.lpp(X = observed_lpp1, Y = observed_lpp2)
  
  observed_dist <- matrix_observed_dist_pi_p %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column(var = "pp1") %>%
    tidyr::pivot_longer(cols = -pp1, names_to = "pp2", values_to = "dist_pi_p") %>%
    dplyr::filter(!is.na(dist_pi_p)) %>%
    dplyr::mutate(pp2 = stringr::str_replace_all(string = pp2, pattern = "V", replacement = ""))
  
  return(observed_dist)
}
