## ----eval=FALSE---------------------------------------------------------------
#  if(!require(devtools)) install.packages("devtools")
#  devtools::install_github("lixiang117423/ggmotif")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("ggmotif")

## ----echo=TRUE----------------------------------------------------------------
library(ggmotif)

## ----echo=TRUE----------------------------------------------------------------
filepath <- system.file("examples", "meme.txt", package = "ggmotif")
motif.info <- getMotifFromMEME(data = filepath, format="txt")

## ----echo=TRUE----------------------------------------------------------------
filepath <- system.file("examples", "meme.xml", package="ggmotif")
motif.info.2 <- getMotifFromMEME(data = filepath, format="xml")

## ----echo=TRUE----------------------------------------------------------------
filepath <- system.file("examples", "meme.xml", package = "ggmotif")
motif_extract <- getMotifFromMEME(data = filepath, format="xml")
motif_plot <- motifLocation(data = motif_extract)
motif_plot +
  ggsci::scale_fill_aaas()

ggplot2::ggsave(filename = "1.png", width = 6, height = 6, dpi = 300)

## ----echo=TRUE, fig.height=4--------------------------------------------------
filepath <- system.file("examples", "meme.xml", package = "ggmotif")
treepath <- system.file("examples", "ara.nwk", package="ggmotif")
motif_extract <- getMotifFromMEME(data = filepath, format="xml")
motif_plot <- motifLocation(data = motif_extract, tree = treepath)
motif_plot +
  ggsci::scale_fill_aaas()

ggplot2::ggsave(filename = "2.png", width = 8, height = 6, dpi = 300)

## ----echo=TRUE----------------------------------------------------------------
library(tidyverse)
library(ggseqlogo)

filepath <- system.file("examples", "meme.txt", package = "ggmotif")
motif.info <- getMotifFromMEME(data = filepath, format = "txt")

# show one motif
motif.info %>%
  dplyr::select(2, 4) %>%
  dplyr::filter(motif.num == "Motif.2") %>%
  dplyr::select(2) %>%
  ggseqlogo::ggseqlogo() +
  theme_bw()

## ----echo=TRUE, fig.height=8--------------------------------------------------
filepath <- system.file("examples", "meme.txt", package = "ggmotif")
motif.info <- getMotifFromMEME(data = filepath, format = "txt")

# show all motif
plot.list <- NULL

for (i in unique(motif.info$motif.num)) {
  motif.info %>%
    dplyr::select(2, 4) %>%
    dplyr::filter(motif.num == i) %>%
    dplyr::select(2) %>%
    ggseqlogo::ggseqlogo() +
    labs(title = i) +
    theme_bw() -> plot.list[[i]]
}

cowplot::plot_grid(plotlist = plot.list, ncol = 2)

## ----echo=TRUE----------------------------------------------------------------
library(tidyverse)
library(memes)

meme.res = memes::importMeme("meme.txt",combined_sites = TRUE)

# table from memes::importMeme function
meme.res[["meme_data"]] %>% 
  dplyr::select_if(~ !any(is.na(.))) %>% 
  dplyr::select(-bkg,-motif)

## ----echo=TRUE----------------------------------------------------------------
meme.res[["combined_sites"]]

## ----echo=TRUE----------------------------------------------------------------
uni.res = universalmotif::read_meme("meme.txt")
uni.res[[1]]

## ----eval=FALSE---------------------------------------------------------------
#  memes::importMeme("meme.xml")

## ----echo=TRUE, message=TRUE, warning=TRUE------------------------------------
universalmotif::read_meme("meme.xml")

## ----echo=TRUE, message=TRUE, warning=TRUE------------------------------------
sessionInfo()

