# Délibérations de la Cnil

Ce dépôt contient une version mise au format `data.frame` du logiciel R des fichiers `xml` des délibérations émises par la [Commission nationale informatique et des libertés](ttps://www.cnil.fr/) de 1979 à février 2022 et rendues publiques via [la page d'Open Data](https://www.cnil.fr/fr/opendata) de la Commission.

Le script cnil-délibérations.R utilisé pour la conversion en `data.frame` se trouve dans le répertoire ./script/ accompagné du fichier cnil-délibérations-utils.R qui contient les fonctions utilisées pour la conversion. Ce script nécessite les packages `RCurl`, `stringi`, `XML` et `xslt`.

Le `data.frame` cnil-delib.Rds au format de sérialisation `RDS` du logiciel R se trouve dans le répertoire ./délibérations/. 

Pour charger directement les données :

```
dlb <- readRDS(co <- url("https://github.com/tsoubiran/cnil-delib/raw/master/d%C3%A9lib%C3%A9rations/cnil-delib.Rds?raw=true")); close(co)
```

Le noms colonnes du `data.frame` reprennent le noms des champs des fichiers `xml` :

|nom|type|
|---|----|
|ID|character|
|dlbIdx|numeric|
|dlbIdx.1|numeric|
|ANCIEN_ID|character|
|ORIGINE|character|
|URL|character|
|NATURE|character|
|TITRE|character|
|TITREFULL|character|
|NUMERO|character|
|NOR|character|
|NATURE_DELIB|character|
|DATE_TEXTE|Date|
|ORIGINE_PUBLI|character|
|PAGE_DEB_PUBLI|character|
|PAGE_FIN_PUBLI|character|
|NUM_SEQUENCE|character|
|DATE_PUBLI|Date|
|ETAT_JURIDIQUE|character|
|BLOC_TEXTUEL|character|
|LIENS|list|
|rpath|character|
|dlbTxt|character|

La colonne LIENS contient une liste de vecteurs —et non un vecteur— de références à des textes mentionnés dans la délibération. 

```
table(l<-sapply(dlb$LIENS,length))
head(dlb$LIENS[which(l>1)])
```

La colonne BLOC_TEXTUEL conserve le texte des délibérations au format `xml` original. 

Les colonnes rpath et dlbTxt ont été ajoutées et contiennent, respectivement, le chemin relatif de la délibération extraite des archives `tar.gz` et le texte brut des délibérations converti avec la feuille de style `xsl` ./xsl/cnil-délibérations-toText.xsl.

Le fichier cnil-délibérations-utils.R contient aussi une fonction `xmlDelibToHtml` qui permet de convertir les délibérations en `html`. 

L'analyse d'une version antérieure du fichier a été présentée au colloque colloque du PIREH [Histoire, langues et textométrie](https://histlangtexto.sciencesconf.org/resource/page/id/5). Le support de la présentation pireh2019--cnil-délib--présentation.pdf se trouve dans le répertoire ./pdf/.

## Licences

Les fichiers des délibérations de la Cnil sont diffusés sous licence [etalab-2.0](https://www.etalab.gouv.fr/wp-content/uploads/2017/04/ETALAB-Licence-Ouverte-v2.0.pdf). Les scripts sont, eux, diffusés sous licence [Creative Commons Attribution-ShareAlike (BY-SA) ](https://creativecommons.org/licenses/by-sa/4.0/).
