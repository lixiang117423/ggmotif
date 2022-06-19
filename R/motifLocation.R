#' @name motifLocation
#' @author Xiang LI <lixiang117423@@gmail.com>
#'
#' @title Extract and Visualize Motif Information from MEME Software
#' @description
#' \code{motifLocation} Visualize motif location in a specificial sequences..
#'
#' @param data A data frame file from getMotifFromXML function.
#' @param tree.path A file path of the correponding phylogenetic tree.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate
#' @importFrom ggplot2 ggplot aes geom_segment geom_rect scale_x_continuous labs theme theme_classic element_blank
#' @importFrom ggtree ggtree geom_tiplab
#' @importFrom ape read.tree
#' @importFrom grid unit
#' @importFrom patchwork align_plots
#' @importFrom tidyverse tidyverse_conflicts
#'
#' @examples
#' # without phylogenetic tree
#' filepath <- system.file("examples", "meme.xml", package = "ggmotif")
#' motif_extract <- getMotifFromMEME(data = filepath, format="xml")
#' motif_plot <- motifLocation(data = motif_extract)
#'
#' # with phylogenetic tree
#' filepath <- system.file("examples", "meme.xml", package = "ggmotif")
#' treepath <- system.file("examples", "ara.nwk", package="ggmotif")
#' motif_extract <- getMotifFromMEME(data = filepath, format="xml")
#' motif_plot <- motifLocation(data = motif_extract, tree = treepath)
#'
#' @export
#'
#' @return Return a plot
utils::globalVariables(c(
  "seq.id", "position", "width", "input.seq.id", "motif_id",".",
  "start.position", "end.position", "start", "end", "y", "Genes",
  "Motif", "x.min", "x.max", "y.min", "y.max","isTip","label","motif_extract"
))

motifLocation <- function(data, tree = NULL) {
  if (is.null(tree)) {
    my.gene <- data %>%
      dplyr::select(
        "input.seq.id", "length", "motif_id",
        "start.position", "end.position"
      ) %>%
      dplyr::rename(
        Genes = input.seq.id, Motif = motif_id,
        start = start.position, end = end.position
      )

    my.gene.list <- data.frame(
      Genes = unique(my.gene$Genes),
      y = 1:length(unique(my.gene$Genes))
    )

    my.gene.final <- merge(my.gene, my.gene.list, by = "Genes") %>%
      dplyr::mutate(length = as.numeric(length)) %>%
      dplyr::mutate(x = max(length)) %>%
      dplyr::mutate(
        x.min = start,
        x.max = end,
        y.min = y - 0.4,
        y.max = y + 0.4,
        group = paste0(Genes, Motif)
      ) %>%
      dplyr::arrange(y) %>%
      dplyr::mutate(Genes = factor(Genes, levels = unique(Genes))) %>%
      dplyr::arrange(length)

    ggplot(my.gene.final) +
      geom_segment(aes(
        x = 0, xend = length,
        y = Genes, yend = Genes
      ),
      size = 0.8
      ) +
      geom_rect(aes(
        xmin = x.min, xmax = x.max,
        ymin = y.min, ymax = y.max,
        fill = Motif
      )) +
      scale_x_continuous(expand = c(0, 0), breaks = seq(0, max(my.gene.final$length), 100)) +
      labs(x = "", y = "Name") +
      theme_classic() -> p

    return(p)
  } else {
    p.tree <- ape::read.tree(tree) %>%
      ggtree::ggtree(branch.length = "none") +
      theme(plot.margin = unit(c(0, -3, 0, 0), "cm"))

    my.tree <- ape::read.tree(file = tree) %>% ggtree::ggtree()

    tree.location <- my.tree[["data"]] %>%
      dplyr::filter(isTip == "TRUE") %>%
      dplyr::select("label", "y") %>%
      dplyr::rename(Genes = label) %>%
      dplyr::arrange(y)

    motif_extract %>%
      dplyr::select(
        "input.seq.id", "length",
        "motif_id", "start.position", "end.position"
      ) %>%
      dplyr::rename(
        Genes = input.seq.id,
        Motif = motif_id, start = start.position, end = end.position
      ) %>%
      dplyr::mutate(Genes = factor(Genes, levels = tree.location$Genes)) %>%
      merge(tree.location, by = "Genes") %>%
      dplyr::mutate(length = as.numeric(length)) %>%
      dplyr::mutate(x = max(length)) %>%
      dplyr::mutate(x.min = start, x.max = end, y.min = y -
        0.4, y.max = y + 0.4, group = paste0(Genes, Motif)) %>%
      dplyr::arrange(y) %>%
      dplyr::mutate(Genes = factor(Genes,
        levels = unique(Genes)
      )) %>%
      dplyr::arrange(length) -> my.gene.final
    ggplot(my.gene.final) +
      geom_segment(aes(
        x = 0, xend = length,
        y = Genes, yend = Genes
      ), size = 0.8) +
      geom_rect(aes(
        xmin = x.min,
        xmax = x.max,
        ymin = y.min,
        ymax = y.max,
        fill = Motif
      )) +
      scale_x_continuous(expand = c(0, 0), breaks = seq(
        0,
        max(my.gene.final$length), 100
      )) +
      labs(x = "", y = "Name") +
      theme_classic() +
      theme(
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, -3), "cm")
      ) -> p.motif

    p <- p.tree + p.motif
    return(p)
  }
}
