options(tidyverse.quiet = TRUE)
options(scipen = 999)
if( !"here" %in% installed.packages()[,'Package'] )  { install.packages("here") }
source(here::here("R", "package_list.R"))
source(here::here("R", "install_packages.R"))


pca_arc_plot <- function(pca_augmented) {
  
  var_exp <- pca_augmented %>% 
    unnest(pca_aug) %>%
    summarize_at(.vars = vars(contains("PC", ignore.case = FALSE)), .funs = list(var)) %>%
    gather(key = pc, value = variance) %>% 
    mutate(var_exp = variance/sum(variance),
           cum_var_exp = cumsum(var_exp),
           pc = str_replace(pc, ".fitted", ""))
  
  
  arc_pc_plot <- var_exp %>% 
    rename(
      `Объясняемая дисперсия` = var_exp,
      `Накопленная объясняемая дисперсия` = cum_var_exp
    ) %>% 
    gather(key = key, value = value, `Объясняемая дисперсия`:`Накопленная объясняемая дисперсия`) %>% 
    mutate(pc_order = 1:nrow(.)) %>% 
    ggplot(aes(x = reorder(gsub("PC", "", pc), pc_order), y = value, group = key)) + 
    geom_point() + 
    geom_line() + 
    facet_wrap(~key, scales = "free_y") +
    theme_pubclean(base_size = 12) +
    lims(y = c(0, 1)) +
    labs(y = "Дисперсия", x = "Главный компонент (PC)",
         title = "Доля объяснения дисперсии главными компонентами")
  
  return(arc_pc_plot)
}


helper_kmeans_sil_and_clust <- function(x, k, seed, iter_max = 200L) {
  set.seed(seed)
  x_kmeans <- kmeans(x, centers = k, iter.max = iter_max)
  x_kmeans_sil <- silhouette(x_kmeans$cluster, dist = dist(x))
  
  # set.seed(seed)
  fsil <- fviz_silhouette(x_kmeans_sil, print.summary = F)
  # set.seed(seed)
  fclust <- fviz_cluster(x_kmeans, data = x, repel = T) + coord_cartesian(clip = "off")
  
  return(list(sil_plot = fsil, clust_plot = fclust))
}

helper_gmm_clust <- function(x, kk, seed) {
  set.seed(seed)
  gmm_clust <- Mclust(as.matrix(x), G = kk, verbose = F)
  
  fgmm <- fviz_mclust_bic(gmm_clust)
  fclust <- fviz_cluster(gmm_clust, data = x, repel = T) + coord_cartesian(clip = "off")
  
  return(list(bic_plot = fgmm, clust_plot = fclust, clust = gmm_clust))
}

helper_kmeans_sil_and_clust_on_pca <- function(x, k, seed, iter_max = 200L) {
  set.seed(seed)
  x_kmeans <- kmeans(x, centers = k, iter.max = iter_max)
  x_kmeans_sil <- silhouette(x_kmeans$cluster, dist = dist(x))
  
  # set.seed(seed)
  fsil <- fviz_silhouette(x_kmeans_sil, print.summary = FALSE)
  # set.seed(seed)
  x$Group <- as.factor(x_kmeans$cluster)
  x$Settlement_Name <- row.names(x)
  fclust <- x %>%
    ggplot(aes(x = PC1, y = PC2, col = Group)) +
    geom_point(alpha = 0.8) +
    geom_mark_hull(aes(fill = Group, label = Group), concavity = 2.8) +
    geom_text_repel(aes(label = Settlement_Name)) +
    coord_cartesian(clip = "off") +
    theme_pubclean()
  
  return(list(sil_plot = fsil, clust_plot = fclust))
}

helper_tsne_df <- function(x, seed, dims) {
  
  set.seed(seed)
  tsne_r <- Rtsne::Rtsne(X = x,
                         dims = dims,
                         perplexity = 2,
                         check_duplicates = T,
                         max_iter = 5000,
                         eta = 200,
                         num_threads = 5,
                         pca = T,
                         pca_center = TRUE, # def = TRUE
                         pca_scale = TRUE, # def = FALSE
                         normalize = TRUE, # def = TRUE
                         verbose = F)
  
  tsne_df <- as.data.frame(tsne_r$Y)
  setnames(tsne_df, old = 1:dims, new = paste0("tsne_", 1:dims))
  row.names(tsne_df) <- dt$Settlement_Name
  
  return(tsne_df)
}


helper_hists_with_clusters <- function(df, clust, n_col = 3){
  
  clust_hist <- df %>%
    mutate(Group = as.factor(clust),
           Name = row.names(.)) %>%
    group_by(Group) %>%
    pivot_longer(cols = -matches("Group|Name")) %>%
    ggplot(aes(y = value, x = Group, col = Group, text = Name)) +
    geom_beeswarm(groupOnX = TRUE) +
    facet_wrap(~name, scales = 'free', ncol = n_col ) +
    labs(x = "", y = "") +
    theme_pubclean()
  
  return(clust_hist)
}

helper_umap_df <- function(x, seed, dims = 2){
  
  set.seed(seed)
  dtx_umap <- uwot::umap(x, n_neighbors = 3, init = "spca", n_components = dims)
  umap_df <- as.data.frame(dtx_umap, row.names = dt$Settlement_Name)
  setnames(umap_df, old = 1:dims, new = paste0("umap_", 1:dims))
  
  return(umap_df)
}


helper_create_city_pairs <- function(city_names, cluster_vector){
  
  DF_g <- data.table(SN = city_names, Group = cluster_vector)
  
  DF_g <- DF_g[ ! Group %in% DF_g[ , .N, by = Group][N < 2, Group] ]
  
  DF_list_matrix <- DF_g[ , list(list(t(combn(SN, 2 )))), by = .(Group)]$V1
  DX_list_DT <- DF_list_matrix %>% purrr::map(~ as.data.table(.x))
  DX_DT <- rbindlist(DX_list_DT, fill = FALSE)
  
  return(DX_DT)
}



vis_net <- function(netw,
                    cut_off_weight = 20,
                    font_size = 32, width = "2000px", height = "1200px",
                    legend_font_size = "60px", physics_on = TRUE) {
  
  legend_edge_colours <- levels(netw %>% activate(edges) %>% pull(color))
  
  netvis <- netw %>%
    activate(nodes) %>% 
    mutate(font.size = font_size,
           smooth = T) %>% 
    activate(edges) %>%
    filter(weight > cut_off_weight) %>%
    visIgraph(idToLabel = F, layout = "layout_with_kk", physics = physics_on) %>%
    visPhysics(enabled = T, maxVelocity = 5, solver = "forceAtlas2Based", timestep = 0.01,
               forceAtlas2Based = list(gravitationalConstant = -1000, centralGravity = 0.001)) %>%
    visNodes(color = list(background = "grey", border = "#5B5B5B", highlight = 'darkblue'), font = list(strokeWidth = 20, strokeColor = "white")) %>%
    visEdges(arrows = NULL, color = list(highlight = 'red'),
             smooth = list(enabled = T, type = "continuous")) %>%
    visLayout(randomSeed = 3) %>% 
    visInteraction(hover = T, hoverConnectedEdges = T, multiselect = T, selectConnectedEdges = T, navigationButtons = F, selectable = T) %>%
    visLegend(main = list(text = "Times a neighbour\n(out of 1000 runs)",
                          style = glue::glue('font-family:Tahoma, sans;font-weight:bold;font-size={legend_font_size};text-align:center') ),
              addEdges = data.frame(label = c("< 85%", "[ 85%, 90% )", "[ 90%, 95% )", "> 95%"),
                                    color = legend_edge_colours),
              zoom = F
              ) %>%
    visOptions(highlightNearest = list(enabled =TRUE, degree = 1),
               width = width, height = height)
  
  return(netvis)
}

prep_graph <- function(DF, cut_off, colour_scale = c('lightblue', 'lightpink', 'hotpink', 'brown') ) {
  DF <- DF %>% 
    mutate(weight = scales::rescale(weight, to = c(0, 100)) )
  
  gg <- graph.data.frame(DF, directed = F)
  E(gg)$width <- E(gg)$weight / 5
  # hist(E(gg)$width)
  E(gg)[ width <= 69/5 ]$width <- 1
  
  ggt <- as_tbl_graph(gg)
  
  ggt <- assign_colours(ggt, colour_scale = colour_scale, cut_off = cut_off)
  
  
  
  return(ggt)
}

assign_colours <- function(ggt, colour_scale, cut_off){
  E(ggt)[ weight <= cut_off[1] ]$color <- colour_scale[1]
  E(ggt)[ weight > cut_off[1] ]$color <- colour_scale[2]
  E(ggt)[ weight > cut_off[2] ]$color <- colour_scale[3]
  E(ggt)[ weight > cut_off[3] ]$color <- colour_scale[4]
  
  ggt <- ggt %>%
    activate(edges) %>%
    mutate(color = factor(color, levels = colour_scale, ordered = T))
  
  return(ggt)
}