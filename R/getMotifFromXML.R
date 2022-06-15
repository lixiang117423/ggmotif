#' @name getMotifFromXML
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Extract and Visualize Motif Information from MEME Software
#' @description
#' \code{getMotifFromXML} Extract motif information from the MEME software results, XML file.
#'
#' @param XMLfile A XML file from MEME software.
#' 
#' @importFrom magrittr %>%
#' @importFrom XML xmlParse xmlRoot xmlSize xmlToList
#' @importFrom dplyr select filter mutate
#'
#' @examples
#' filepath <- system.file("examples", "meme.xml", package="ggmotif")
#' motif_extract <- getMotifFromXML(XMLfile = filepath)
#' @export
#'
#' @return Return a vector or a datafram
utils::globalVariables(c('seq.id','position','width','input.seq.id','motif_id',
                         'start.position','end.position','start','end','y','Genes',
                         'Motif','x.min','x.max','y.min','y.max'))


getMotifFromXML <- function(XMLfile) {

  df <- XML::xmlParse(file = XMLfile)

  df.root <- XML::xmlRoot(df)

  # 提取输入序列汇总信息
  seq.inf <- df.root[[1]]

  df.seq.info <- NULL

  for (i in 2:(XML::xmlSize(seq.inf) - 1)) {
    df.seq.info.temp <- seq.inf[[i]] %>%
      XML::xmlToList() %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame()
    df.seq.info <- rbind(df.seq.info, df.seq.info.temp)
  }

  colnames(df.seq.info)[1:2] <- c("seq.id", "input.seq.id")


  # 提取motif信息
  motif.seq <- df.root[[3]]

  df.motif.info <- NULL

  for (i in 1:XML::xmlSize(motif.seq)) {
    temp <- motif.seq[[i]] %>%
      XML::xmlToList()

    df.motif.info <- temp[[".attrs"]] %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      rbind(df.motif.info)
  }

  colnames(df.motif.info)[1:2] <- c("motif_id", "motif_name")

  # 提取基因序列上的motif信息
  gene.motif <- df.root[[4]]

  df.gene.motif.info <- NULL

  for (i in 1:XML::xmlSize(gene.motif)) {
    temp <- gene.motif[[i]] %>%
      XML::xmlToList()

    df.gene.motif.info.temp <- NULL

    for (j in 1:(length(temp) - 1)) {
      df.gene.motif.info.temp.2 <- temp[[j]] %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame()
      df.gene.motif.info.temp <- rbind(df.gene.motif.info.temp, df.gene.motif.info.temp.2)
    }

    df.gene.motif.info <- df.gene.motif.info.temp %>%
      dplyr::mutate(
        seq.id = temp[[".attrs"]][[1]],
        p.value.seq = temp[[".attrs"]][[2]],
        num_site4seq = temp[[".attrs"]][[3]]
      ) %>%
      rbind(df.gene.motif.info)
  }

  df.gene.motif.info <- df.gene.motif.info %>%
    dplyr::select('seq.id', 'num_site4seq', 'p.value.seq', 'motif_id', 
                  'strand', 'position', 'pvalue')


  all.info <- df.gene.motif.info %>%
    merge(df.motif.info, by = "motif_id") %>%
    merge(df.seq.info[, 1:3], by = "seq.id") %>%
    dplyr::select(-seq.id)

  col.name <- colnames(all.info)

  all.info.final <- all.info %>%
    dplyr::select('input.seq.id', 'length', col.name[1:16]) %>%
    dplyr::mutate(
      position = as.numeric(position),
      width = as.numeric(width)
    ) %>%
    dplyr::mutate(
      start.position = position,
      end.position = position + width
    ) %>%
    dplyr::select(
      'input.seq.id',
      'length',
      'num_site4seq',
      'p.value.seq',
      'motif_id',
      'position',
      'width',
      'start.position',
      'end.position',
      'pvalue',
      'ic',
      're',
      'llr',
      'p_value',
      'e_value',
      'bayes_threshold'
    )
  return(all.info.final)
}
