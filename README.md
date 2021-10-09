# Délibérations de la Cnil

Ce dépôt contient une version mise au format `data.frame` du logiciel R des fichiers `xml` des délibérations émises par la [Commission nationale informatique et libertés](ttps://www.cnil.fr/) de 1979 à septembre 2021 et rendues publiques via [la page d'Open Data](https://www.cnil.fr/fr/opendata) de la Commission.

Le script cnil-délibérations.R utilisé pour la conversion en `data.frame` se trouve dans le répertoire ./script/ accompagné du fichier cnil-délibérations-utils.R qui contient les fonctions utilisées pour la conversion. Ce script nécessite les packages `RCurl`, `stringi`, `XML` et `Sxslt`.

Le `data.frame` cnil-delib.Rds au format de sérialisation `RDS` du logiciel R se trouve dans le répertoire ./délibérations/. 
Pour charger les données :

```
delib <- readRDS("cnil-delib.Rds")
```

Les colonnes du `data.frame` reprennent les champs des fichiers `xml` :

|nom|type|
|---|----|
|ID|character|
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
|delibtxt|character|

La colonne LIENS contient une liste de vecteurs —et non un vecteur— de références à des textes mentionnés dans la délibération. 

```
table(l<-sapply(delib$LIENS,length))
head(delib$LIENS[which(l>1)])
```

La colonne BLOC_TEXTUEL conserve le texte des délibérations au format `xml` original. 

La colonne delibtxt a été ajoutée et contient, elle, le texte brut des délibérations converti avec la feuille de style `xsl` ./xsl/cnil-délibérations-toText.xsl.

L'analyse d'une version antérieure du fichier a été présentée au colloque colloque du PIREH [Histoire, langues et textométrie](https://histlangtexto.sciencesconf.org/resource/page/id/5). Le support de la présentation pireh2019--cnil-délib--présentation.pdf se trouve dans le répertoire ./pdf/.

## Licences

Les fichiers des délibérations de la Cnil sont diffusés sous licence [etalab-2.0](https://www.etalab.gouv.fr/wp-content/uploads/2017/04/ETALAB-Licence-Ouverte-v2.0.pdf). Les scripts sont, eux, diffusés sous licence [Creative Commons Attribution-ShareAlike (BY-SA) ](https://creativecommons.org/licenses/by-sa/4.0/).
