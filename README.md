# ggmotif: An R Package for the extraction and visualization of motifs from MEME software

[![](https://img.shields.io/badge/release version-0.1.2-green.svg)](https://cran.r-project.org/web/packages/ggmotif/index.html)

MEME Suit is a most used tool to identify motifs within deoxyribonucleic acid (DNA) or protein sequences. However, the results generated by the MEME Suit are saved using file formats, *.xml* and *.txt,* that are difficult to read, visualize or integrate with other wide used phylogenetic tree packages such as *ggtree*. To overcome this problem, we developed the *ggmotif* R package that provides a set of easy-to-use functions that can be used to facilitate the extraction and visualization of motifs from the results files generated by the MEME Suit. *ggmotif* can extract the information of the location of motif(s) on the corresponding sequence(s) from the *.xml* format file and visualize it. Additionally, the data extracted by *ggmotif* can be easily integrated with the phylogenetic data generated by *ggtree*. On the other hand, *ggmotif* can get the sequence of each motif from the *.txt* format file and draw the sequence logo with the function *ggseqlogo* from *ggseqlogo* R package. 

# :writing\_hand: Authors

Xiang LI

College of Plant Protection, Yunnan Agricultural University

<a href="https://blog.web4xiang.top/" class="uri">https://blog.web4xiang.top/</a>

# Identification of motifs

The demo data, the AP2 gene family of *Arabidopsis thaliana*, was downloaded from [Plant Transcription Factor Database](http://planttfdb.gao-lab.org/family.php?sp=Ath&fam=AP2). The latest version MEME, v5.4.1, was used search motifs from the demo data using the fellow code:

```bash
meme ara.fa -protein -o meme_out -mod zoops -nmotifs 10 -minw 4 -maxw 7 -objfun classic -markov_order 0
```

The output files, `html`file, `txt` file and `xml` file, can be found at [GitHub](https://github.com/lixiang117423/ggmotif/tree/main/inst/demo_data).

# Construction of phylogenetic tree

clustalo (V1.2.4) and FastTree (V2.1.10) were used  to align the sequences and construct the phylogenetic tree.

```bash
clustalo -i ara.fa > ara.aligned.fa
FastTree ara.aligned.fa > ara.twk
```

The output files can be found at [GitHub](https://github.com/lixiang117423/ggmotif/tree/main/inst/demo_data).

# Installation and loading

- Install the latest developmental version from [GitHub](https://github.com/lixiang117423/ggmotif) as follow:

```R
if(!require(devtools)) install.packages("devtools")
devtools::install_github("lixiang117423/ggmotif")
```

- Or install from [CRAN](https://cran.r-project.org/web/packages/ggmotif/index.html) as follow:

```R
install.packages("ggmotif")
```

- Loading package

```R
library(ggmotif)
```

# Parse motif information from MEME results

The results generated by MEME Suit include a lot of files, including figures of each motif and three other files, a `html`file, a `txt`file and a `xml` file. The `html` file contain some figures of motifs. The `txt` file is for the sequences' information and the `xml` for  other information including position, length, p-value and so on.

The main function of `ggmotif` is to parse the information and plot the position of each motif on the corresponding sequences.

## Parse information

### information of sequences of motifs

```R
filepath <- system.file("examples", "meme.txt", package = "ggmotif")
motif.info <- getMotifFromMEME(data = filepath, format="txt")
```

### information of other detail information of motifs

```R
filepath <- system.file("examples", "meme.xml", package="ggmotif")
motif.info.2 <- getMotifFromMEME(data = filepath, format="xml")
```

# Plot location

The figures from MEME only contain the location. It is difficult to combine the location figure to the corresponding phylogenetic tree. In `ggmotif`, the function `motifLocation`  can visualize the location of each motif on its corresponding sequences, almost same as the `html` file. If user have the corresponding phylogenetic tree,  the function can combine the tree and the location.

### Without tree

```R
filepath <- system.file("examples", "meme.xml", package = "ggmotif")
motif_extract <- getMotifFromMEME(data = filepath, format="xml")
motif_plot <- motifLocation(data = motif_extract)
motif_plot

ggsave(filename = "1.png", width = 6, height = 6, dpi = 300)
```

<img src="https://github.com/lixiang117423/ggmotif/blob/main/inst/image/1.png" style="zoom:25%;" />

### With tree

```R
filepath <- system.file("examples", "meme.xml", package = "ggmotif")
treepath <- system.file("examples", "ara.nwk", package="ggmotif")
motif_extract <- getMotifFromMEME(data = filepath, format="xml")
motif_plot <- motifLocation(data = motif_extract, tree = treepath)
motif_plot

ggsave(filename = "2.png", width = 8, height = 6, dpi = 300)
```

<img src="https://github.com/lixiang117423/ggmotif/blob/main/inst/image/2.png" style="zoom: 25%;" />

# show motif(s)

```R
filepath <- system.file("examples", "meme.txt", package = "ggmotif")
motif.info <- getMotifFromMEME(data = filepath, format = "txt")

# show one motif
motif.info %>%
  dplyr::select(2, 4) %>%
  dplyr::filter(motif.num == "Motif.2") %>%
  dplyr::select(2) %>%
  ggseqlogo::ggseqlogo() +
  theme_bw()
```

<img src="https://github.com/lixiang117423/ggmotif/blob/main/inst/image/3.png" style="zoom:25%;" />

```
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

cowplot::plot_grid(plotlist = plot.list, ncol = 5)
```

![](https://github.com/lixiang117423/ggmotif/blob/main/inst/image/4.png)

# :sparkling\_heart: Contributing

We welcome any contributions! 
