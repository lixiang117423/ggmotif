#' @name motif_with_tree
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Extract and Visualize Motif Information from MEME Software
#' @description
#' \code{getMotifFromXML} Visualize motif location with a phylogenetic tree object..
#'
#' @param data A data frame file from getMotifFromXML function.
#' @param tree A tree file.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate filter rename
#' @importFrom ggplot2 ggplot aes geom_segment geom_rect scale_x_continuous labs theme_classic theme element_blank
#' @importFrom ggtree ggtree geom_tiplab
#' @importFrom ape read.tree
#' @importFrom aplot insert_right
#'
#' @examples
#' filepath <- system.file("examples", "meme.xml", package="ggmotif")
#' treepath <- system.file("examples", "tree.nwk", package="ggmotif")
#' motif_extract <- getMotifFromXML(XMLfile = filepath)
#' motif_plot_with_tree <- motif_with_tree(data = motif_extract, tree = treepath)
#'
#' @export
#'
#' @return Return a datafram
utils::globalVariables(c('seq.id','position','width','input.seq.id','motif_id',
                         'start.position','end.position','start','end','y','Genes',
                         'Motif','x.min','x.max','y.min','y.max', 'tree','label',"isTip"))

motif_with_tree <- function(data, tree) {
  
  my.tree <- ape::read.tree(file = tree) %>%
    ggtree::ggtree()

  tree.location <- my.tree[["data"]] %>%
    dplyr::filter(isTip == "TRUE") %>%
    dplyr::select("label", "y") %>%
    dplyr::rename(Genes = label)

  my.gene <- data %>%
    dplyr::select(
      "input.seq.id", "length", "motif_id",
      "start.position", "end.position"
    ) %>%
    dplyr::rename(
      Genes = input.seq.id, Motif = motif_id,
      start = start.position, end = end.position
    )

  my.gene.final <- merge(my.gene, tree.location, by = "Genes") %>%
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
      y = y, yend = y
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
    theme_classic() +
    theme(axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()) -> p

  return(p)
}
