##
## 
##
library(RCurl)
library(XML)
library(Sxslt)
library(stringi)

######
##
## répertoire script
wd      <- ""
## répertoire des fichiers xml compressés
tar.dir <- ""
## répertoire des fichiers xml décompressés
xml.dir <- ""

##
setwd(wd)

##
source("./cnil-délibérations-utils.R")

##
## Liste des fichiers dans le répertoire distant
##
delib.fnm0 <- list.delib()
##
## Téléchargement dans le répertoire tar.dir et extraction des fichiers dans le répertoire xml.dir
##
rv <- get.delib(
  delib.fnm0$fname
  , tar.dir
  , xml.dir 
  , extract = T
)
##
## Conversion des fichiers xml en data.frame
##
delib0 <- delib.toDataFrame(
  xml.dir
  , verbose = 100
)
##
## Conversion des fichiers xml en texte brut
##
delib0$delibtxt <- delib.toText(
  delib0$BLOC_TEXTUEL
  , paste( wd, "xsl/cnil-délibérations-toText.xsl", sep= "/")
)
