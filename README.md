# ggmotif
Extract and Visualize Motif Information from MEME Software

# Extract and visualize motif information from XML file from MEME software. 
In biology, a motif is a nucleotide or amino acid sequence pattern that is widespread and usually assumed to be related to specifical biological functions. There exist many software was used to discover motif sequences from a set of nucleotide or amino acid sequences. MEME is almost the most used software to detect motif. It's difficult for biologists to extract and visualize the location of a motif on sequences from the results from MEME software. The package can extract and visualize the location of a motif on sequences.

# Extract and visualize motif location in sequences

```R
filepath <- system.file("examples", "meme.xml", package="ggmotif")
motif_extract <- getMotifFromXML(XMLfile = filepath)
motif_plot <- motif_location(data = motif_extract)
motif_plot
```

# Extract and visualize motif sequence logos

```R
library(tidyverse)

filepath <- system.file("examples", "meme.txt", package = "ggmotif")
motif_extract <- getMotifFromTxt(data = filepath)

motif_extract %>% 
  dplyr::filter(motif.num == 'Motif.1') %>% 
  dplyr::select(input.seq.motif) %>% 
  ggseqlogo::ggseqlogo() +
  theme_classic()
```

# Extract and visualize motif location in sequences with a phylogenetic tree

```R
filepath <- system.file("examples", "meme.xml", package="ggmotif")
treepath <- system.file("examples", "tree.nwk", package="ggmotif")
motif_extract <- getMotifFromXML(XMLfile = filepath)
motif_plot_with_tree <- motif_with_tree(data = motif_extract, tree = treepath)


p.tree = ape::read.tree(treepath) %>% 
  ggtree::ggtree() +
  ggtree::geom_tiplab()

p.tree  %>% 
  aplot::insert_right(motif_plot_with_tree, width = 0.8)
```

