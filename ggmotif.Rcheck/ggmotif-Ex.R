pkgname <- "ggmotif"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "ggmotif-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ggmotif')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("getMotifFromMEME")
### * getMotifFromMEME

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getMotifFromMEME
### Title: Extract and Visualize Motif Information from MEME Software
### Aliases: getMotifFromMEME

### ** Examples

filepath <- system.file("examples", "meme.txt", package = "ggmotif")
motif.info <- getMotifFromMEME(data = filepath, format = "txt")

filepath <- system.file("examples", "meme.xml", package = "ggmotif")
motif.info <- getMotifFromMEME(data = filepath, format = "xml")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getMotifFromMEME", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("motifLocation")
### * motifLocation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: motifLocation
### Title: Extract and Visualize Motif Information from MEME Software
### Aliases: motifLocation

### ** Examples

# without phylogenetic tree
filepath <- system.file("examples", "meme.xml", package = "ggmotif")
motif_extract <- getMotifFromMEME(data = filepath, format="xml")
motif_plot <- motifLocation(data = motif_extract)

# with phylogenetic tree
filepath <- system.file("examples", "meme.xml", package = "ggmotif")
treepath <- system.file("examples", "ara.nwk", package="ggmotif")
motif_extract <- getMotifFromMEME(data = filepath, format="xml")
motif_plot <- motifLocation(data = motif_extract, tree = treepath)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("motifLocation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
