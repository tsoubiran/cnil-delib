##
## Fichier de démonstration d'importation des délibérations de la Cnil dans un data.frame
##
## ¡¡¡ ATTENTION !!!
##
## Des modifications dans la structure des fichiers des délibération peuvent être intervenues
## depuis la publication de ce script. Donc, même si aucune erreur ne surgit, 
## il vaut mieux bien vérifier le résultat (doublons,…) avant utilisation
## 
##
library(RCurl)
library(XML)
library(xslt)
## optionnel: pour utiliser `Sxslt` à la place de `xslt` dans `xmlDelibToText()` : library(Sxslt) 
library(stringi)
##
## optionnel: Le package `parallel` doit aussi être installé pour paralléliser la conversion des fichiers xml

##
##
## répertoire script
wd       <- ""
## répertoire des fichiers xml compressés
tar.dir  <- ""
## répertoire des fichiers xml décompressés
xml.dir  <- ""
## répertoire des sorties html
html.dir <- ""

##
setwd(wd)
##
source("./cnil-délibérations-utils.R")

##
## Liste des fichiers dans le répertoire distant
##
dlbTarFnm <- lsDelib()
##
head(dlbTarFnm)
##
## Sauvegarde pour la mise à jour
##
## saveRDS(dlbTarFnm, "…")
##
## Téléchargement dans le répertoire tar.dir et extraction des fichiers dans le répertoire xml.dir
## 
##
rv <- getDelib(
  dlbTarFnm 
  , dstdir  = tar.dir
  , exdir   = xml.dir 
  , extract = T
  , force   = F
  , verbose = 1
  ## options passées à download.file
  , quiet = F
)
##
## OU 
##
## pour seulement mettre à jour le dépôt avec les nouvelles délibérations
## si la liste des délibérations déjà téléchargées se trouve dans p. ex. `dlbTarFnm0`
##
# rv <- getDelib0(
#   subset(
#     dlbTarFnm
#     , as.integer(timestamp) %in% setdiff(timestamp, dlbTarFnm0$timestamp)
#   )
#   , dstdir = tar.dir
#   , exdir  = xml.dir 
#   , extract = T
#   , verbose = 1
#   ## options passées à download.file
#   , quiet = F
# )

##
## Conversion des fichiers xml en data.frame
##
## 
##
dlb0 <- xmlDelibToDataFrame(
  xml.dir
  , ncores = 1 ## à adapter en fonction des ressources disponibles
  , verbose = 100
)
##
## Mise à jour
##
## Note: une autre possibilité est d'utiliser rv
##
# dlbMajFinfo0 <- do.call(
#   rbind
#   , lapply(
#     list.dirs(xml.dir, recursive=F)
#     , file.info
#   )
# ) |>  ( \(x) {
#   x <- cbind(fpath=rownames(x), x)
#   rownames(x) <- basename(rownames(x))
#   x
# })()
# ##
# ##
# ## présuppose que mtime a bien été préservé
# ##
# rpathMaj0 <- (
#   xmlDlbFnmMaj0 <- subset(dlbMajFinfo0, as.Date(mtime)  >= as.Date("2023-02-10", format="%Y-%m-%d") ) 
#   ) |> (
#   \(x) paste(
#     rownames(x)
#     , list.files(
#       x$fpath
#       , pattern = "\\.xml"
#       , recursive = T
#     )
#     , sep ="/"
#   )
# )()
# ##
# dlbMaj0 <- xmlDelibToDataFrame(
#   xml.dir
#   , xmlDlbRpath = rpathMaj0
#   , ncores = 1 ## à adapter en fonction des ressources disponibles et du nombre de fichiers
#   , verbose = 100
# )
# ##
# ##
# ##
# xmlrpath0 <- list.files(
#   xml.dir
#   , pattern = "\\.xml"
#   , recursive = T
# )
# ## ASSERT
# length(xmlrpath0) - ( nrow(dlb1) + nrow(dlbMaj0) )

##
## En cas de duplicata, Ajout d'un indice pour gérer les duplicata
##
##
dlb1 <- delibDupIdxInsert(dlb0)

##
## Conversion des fichiers xml en texte brut avec une feuille de style xsl
## 
dlb1$dlbTxt <- xmlDelibToText(
  dlb1$BLOC_TEXTUEL
  , paste( wd, "xsl", "cnil-délibérations-toText.xsl", sep= "/")
  , ncores = 1 ## à adapter en fonction des ressources disponibles
)

##
## Exemple de recherche de texte brut : les délibérations mentionnant l'Ined
##
## 
## 
m0 <-  grepl(
  paste(
    "(\\P{L}(I\\.?N\\.?E\\.?D)\\P{L})"
    , "(Institut\\P{L}*national\\P{L}*d\\P{L}*\\p{L}*tudes\\P{L}*d\\p{L}*mographiques)"
    , sep = "|"
  )
  , dlb1$dlbTxt
  , perl = T
  , ignore.case = T
)
##
table(m0)
##
xmlDelibToHtml(
  dlb1[m0  & dlb1$dlbIdx==1,]
  , file = paste(
      html.dir
    , "délib-ined-0.html"
    , sep ="/"
  )
  , doc.tit = "Délibérations de la Cnil : Ined"
  , css.path = paste( wd, "css", "cnil--délibérations-style-0.css", sep="/")
  , css.internal = F ## ¡¡¡ petit bœugue avec T !!!
  , sort.by = "DATE_TEXTE"
)
