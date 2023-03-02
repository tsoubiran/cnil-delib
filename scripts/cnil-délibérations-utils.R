##
##
##
pBlkIdx <- function(n, ncores, nblk){
  ## 
  nblk <- ncores*nblk 
  ##
  blksz <- n %/% nblk
  ##
  blkIdx <- seq(0, nblk * blksz, blksz)
  ## reste
  if( (n %% nblk) ) blkIdx <- c(blkIdx, n )
  ##
  structure(
    blkIdx + 1
    , n = n
    , nblk = nblk
    , blksz = blksz
  )
}
##
##
##
lsDelib <- function(dlbUrl='https://echanges.dila.gouv.fr/OPENDATA/CNIL/', decreasing=T){
  ##
  r <- RCurl::getURL(dlbUrl)
  ## parse html 
  dlbTarFnm <- htmlParse(r)
  ## liste des noeud url des fichiers
  dlbTarFnm <- getNodeSet(
    dlbTarFnm
    , "//a[starts-with(@href,'CNIL') or starts-with(@href,'Freemium')]"
  )
  ## extraction des url
  dlbTarFnm <- data.frame(
    matrix(sapply(
      dlbTarFnm
      , function(node){
        ##
        c(
          ## url
          xmlAttrs(node)
          ## dt
          , xmlValue(getSibling(node))
        )
      }
    )
    , ncol=2, byrow=T
    ##
    , dimnames = list(NULL,c("fname", "tm_sz"))
    )
    , stringsAsFactors = F
  )
  ## extraction horodatage + taille du fichier
  tm_sz <- stringi::stri_match(
    dlbTarFnm$tm_sz
    ##
    , regex = paste(
      '(\\d{4}-\\d{2}-\\d{2}\\W+\\d{2}:\\d{2})'
      # , '\\W+'
      # , '(\\d{2}:\\d{2})'
      , '\\W+'
      , '([\\d\\.]+)(K|M)*'
      , sep = ''
    )
  )[,2:4]
  ##
  tm_sz[is.na(tm_sz[,3]), 3] <- 'o'
  ##
  dlbTarFnm <- data.frame(
    ##
    dlbTarFnm
    ##
    , timestamp = strptime(tm_sz[,1], "%Y-%m-%d %H:%M")
    ## semble en base 10
    , sz = as.integer(as.numeric(tm_sz[,2]) * ifelse(
      tm_sz[,3]=='K'
      , 1e3 ## 2^10 ## 
      , ifelse(
        tm_sz[,3]=='M'
        , 1e6 ## 2^20 ## 
        , 1
      )
    ))
  )
  ## 
  class(dlbTarFnm$sz) <- c('object_size', class(dlbTarFnm$sz))
  ##
  dlbTarFnm$tm_sz <- NULL
  ##
  dlbTarFnm[order(dlbTarFnm$timestamp, decreasing=decreasing), ]
}
##
##
##
##
getDelib <- function(
  x
  , urlPath = "https://echanges.dila.gouv.fr/OPENDATA/CNIL"
  , dstdir, exdir
  , extract=F, force=F
  , verbose=0,...
){
  sapply(
    x$fname
    , function(fname){
      ##
      if(verbose) cat(fname)
      ##
      dst.fname <- paste(dstdir, fname, sep='/')
      ##
      x <- file.exists(dst.fname)
      ##
      if(verbose) cat(":", if(x) "existe" else "n'existe pas", "\n")
      ##
      # cat( extract, x, force, "\n")
      ##
      rv <- if( 
        # extract && ( x | force ) 
        extract <- extract && (
          !x || ( x && force ) 
        )
      ){
        ##
        cat("téléchargement\n")
        ##
        download.file(
          paste(urlPath, fname, sep='/')
          , dst.fname
          ,...
        )+10
      }else as.integer(x) 
      ##
      if(rv>10) stop("error while dowloading file with code ", x - 10)
      ##
      if(extract){
        ##
        cat("extraction\n")
        ##
        untar(
          dst.fname
          , exdir = exdir
          # , compressed = "gzip"
        )+100
      }else rv##
    }
  )
}
##
##
##
xmlDelibParse <- function(fname){
  ##
  delib.xml <- xmlTreeParse(
    fname
    , asText = F
    , useInternalNodes = T
  )
  ## métadonnées
  meta <- getNodeSet(
    delib.xml
    , "//*[not(child::*) and not(ancestor::BLOC_TEXTUEL) and not(ancestor::LIENS) and not(self::LIENS) ]"
  )
  ## 
  xval <- xmlApply(meta,xmlValue)
  ##
  names(xval) <- xmlApply(meta,xmlName)
  ## texte de la délibération
  txt <- xpathApply(
    delib.xml
    , "//BLOC_TEXTUEL/*" ## 
  )
  ##
  if( (n <- length(txt))!=1){
    warning("# txt ", n)
    return(NULL)
  }
  ## 
  xmltxt <- saveXML(txt[[1]]) ## 
  ##
  xmltxt <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n", xmltxt)
  ##
  liens  <- xpathApply(
    delib.xml
    , "//LIENS/*"
    , xmlValue
  )
  ##
  liens <- list(
    if( !length(liens) ) character(0) else liens
  )
  ## 
  data.frame(
    xval
    , BLOC_TEXTUEL = xmltxt
    , LIENS = I((liens))
    , stringsAsFactors = F
  )
}
##
##
##
xmlDelibToDataFrame <- function(
  xmlDlbDir
  , xmlDlbRpath
  , verbose=0
  , ncores = getOption("mc.cores", 1L)
  , nblk = 2
  ,...
){
  ## 
  assertDlb <- function(delib){
    n <- table(sapply(delib,length))##
    rv <- !( length(n)==1  )
    if( rv ){
      warning("nombre variable de champs : ", paste(head(n), collapse = ", "))
    }
    rv
  }
  ## 
  toDataFrame <- function(fpath, verbose){
    ##
    i <- 0
    ##
    if(verbose>0) t0 <- Sys.time()
    ##
    delib0 <- lapply(
      fpath
      , function(fpath, verbose){
        ##
        if(verbose>0){
          i <<- i+1
          if( (i%%verbose)==0  )cat(i, " ")
          if( (i%%(verbose*10) )==0 )cat("\n")
        }
        ##
        xmlDelibParse(fpath)
      }
      ##
      , verbose = verbose
    )
    ##
    if(verbose>0) cat( "\nDurée:", format(Sys.time() - t0 ), "\n")
    ##
    if( nn <- sum(  sapply(delib0, function(x) inherits(x, "XMLDocument") ) ) ) warning( nn, " erreur(s) durant l'analyse des fichiers xml" )
    ##
    if( nn || ( nf <- assertDlb(delib0) ) ){
      ##
      if(verbose>0){
        cat( "\nDurée:", format(Sys.time() - t0 ), "\n")
      }
      ##
      if( nn) attr(delib0, "xmlParseError") <- nn
      ##
      if( nf) attr(delib0, "varyingNumberOfFieldsError") <- T
      ##
      return(delib0)
    }
    ##
    if(verbose>0) cat("Concaténation des délibérations\n")
    ##
    do.call(rbind, delib0)
  }
  ## 
  if( !missing(xmlDlbDir) ){
    ##
    if( missing(xmlDlbRpath) ){
      xmlDlbRpath <- list.files(
        xmlDlbDir
        , pattern = "\\.xml"
        , recursive = T
      )
    }
  } else {
    stop("xmlDlbDir manquant")
  }
  ##
  ##
  fpath <- paste(
    xmlDlbDir
    , xmlDlbRpath ##
    , sep='/'
  )
  ##
  ##
  if(verbose>0){
    cat("Nombre de fichiers : ", length(xmlDlbRpath) , "\n", sep="")
    cat("Début de la importation: ", as.character(t0 <- Sys.time()), "\n", sep="")
  }
  ##
  delib0 <- if( ncores==1 ){
    ##
    toDataFrame(fpath, verbose=verbose)
    
  }else if( ncores >1 ){
    ##
    require(parallel)
    ##
    blkIdx <- pBlkIdx(length(fpath), ncores, nblk)
    ##
    print(blkIdx)
    ##
    parallel::mclapply(
      ##
      ( 1:( length(blkIdx)-1 ) )##[
      ##
      , function(i, fpath, blkIdx){
        ## 
        toDataFrame(
          fpath[(blkIdx[i]):(blkIdx[i+1]-1)]
          , verbose = 0
        )
      }
      ##
      , fpath, blkIdx
      ##
      , mc.cores = ncores
      ##,...
    )
    
  }else stop("wrong number of cores:", ncores)
  ##
  if( ( ncores>1 ) && all( sapply(delib0, is.data.frame)) ){
    ##
    delib0 <- do.call(rbind, delib0)
  }
  ##
  if( !is.data.frame(delib0) ){
    ##
    if( ncores>1 ) warning("erreur(s) durant l'analyse des fichiers xml" )
    ##
    if(verbose>0) cat( "\nDurée totale: ", format(Sys.time()- t0), "\n", sep="")
    ##
    return(delib0)
  }
  ##
  delib0$DATE_TEXTE <- as.Date(delib0$DATE_TEXTE, "%Y-%m-%d")
  delib0$DATE_PUBLI <- as.Date(delib0$DATE_PUBLI, "%Y-%m-%d")
  ## chemin relatif pour debœug
  delib0$rpath <- xmlDlbRpath
  ##
  if(verbose>0) cat( "\nDurée totale: ", format(Sys.time()- t0), "\n", sep="")
  ##
  delib0
}
##
##
##
delibDupIdxInsert <- function(dlb, where="ID"){
  ##
  j <- match(where, colnames(dlb), nomatch = 0L)[1]
  ##
  if( j==0 ) stop(where, " not found")
  ##
  dlb <- dlb[do.call(
    order
    ,  c(
      dlb[ c("ID", "DATE_PUBLI")] 
      , list(decreasing = F, na.last = T)
    )
  ),]
  ##
  d0 <- with(dlb, ID!=c("", ID[-length(ID)]))
  ##
  i <- which(d0)
  ##
  d1 <- i - c(-1, i[-length(i)])
  ##
  d2 <- rep(1,length(d0))
  ##
  d2[i[-1]] <- -d1[-1] +1
  ##
  nc <- ncol(dlb)
  ##
  if( j < nc ) data.frame(
    dlb[1:j]
    , dlbIdx = cumsum(d2)
    , dlb[(j+1):nc]
  ) else data.frame(
    dlb
    , dlbIdx = cumsum(d2)
  )
}
##
##
##
##
xmlDelibToText <- function(
  txt
  , xsl
  , ncores= getOption("mc.cores", 2L)
  , nblk=2
  , xsltlib = c("xslt", "Sxslt")
){
  ##
  toText <- function(txt, xsl){
    ##
    sapply(
      txt##[1:length(txt)] ## 
      , .toText
      ##
      # , txt = txt
      , xsl = xsl
      ##
      , USE.NAMES = F
    )
  }
  ##
  toTextxml <- function(txt, xsl){
    ##
    xml <- xmlTreeParse(
      txt
      , asText = T
      , useInternalNodes = T
    )
    ##
    if( xpathSApply(xml, "count(//*)>1", xmlValue ) ){
      ##
      rv <- xsltApplyStyleSheet(
        xml
        , xsl
      )
      ##
      try(saveXML(rv))

    }else ""
  }
  ##
  toTextxml2 <- function(txt, xsl){
    ##
    xml <- read_xml(
      txt
    )
    if(  xml_find_lgl(xml, "count(//*)>1") ){
      try(xml_xslt(
        xml
        , xsl
      ))
    }else ""
  }
  ##
  xsltlib <- match.arg(xsltlib)
  ##
  xsl <- if( xsltlib=="xslt"){
    require(xslt)
    .toText <- toTextxml2
    read_xml(xsl)
  }else{
    require(Sxslt)
    .toText <- toTextxml
    xsltParseStyleSheet(xsl)
  }
  ##
  ##
  txt0 <- if( ncores==1 ){
    ##, verbose=verbose
    toText(txt, xsl)
    
  }else if( ncores >1 ){
    ##
    blkIdx <- pBlkIdx(length(txt), ncores, nblk)
    ##
    # print(blkIdx)
    ##
    do.call(
      c
      ##
      , parallel::mclapply(
        ##
        (1:( length(blkIdx)-1 ))##
        ##
        , function(i, txt, xsl, blkIdx){
          ## 
          # cat(i, blkIdx[i], blkIdx[i+1]-1, "\n")
          ## 
          toText(
            txt[(blkIdx[i]):(blkIdx[i+1]-1)]
            , xsl
            ##, verbose = 0
          )
        }
        ##
        , txt, xsl, blkIdx
        ##
        , mc.cores = ncores
        ##,...
      )
    )
  }
}
##
##
##
# transforme une liste de délib au format xml en un document html
##
xmlDelibToHtml  <- function(
    delib, file=NULL, doc.tit = "Délibérations de la Cnil"
  , css.path=NULL, css.internal = F
  , sort.by=NULL
){
  ## crée un nœud XML en y ajoutant des descendants
  XMLNode <- function(name, subnodes, parent=NULL, attrs=NULL, cdata=F){
    ##
    n <- newXMLNode(name, parent = parent, attrs = attrs, cdata = cdata)
    ##
    addChildren(
      n
      , kids = as.list(subnodes)
    )
    ##
    n
  }
  ## transforme le xml du texte en html en ajoutant quelques métadonnées
  ##
  ## ¡¡¡ retourne une balise <a> pour l'insérer dans la TdM !!!
  ##
  # - delib
  #   - h2
  #   - delib-container
  #     - delib-info-container
  #       - delib-info
  #         - delib-info-key
  #         - delib-info-val
  #     - delib-txt
  ##
  toHtml <- function(delib, parent=NULL){
    ## ajout des métadonnées
    appendInfoNode <- function(kv, parent){
      ##
      XMLNode(
        "div"
        , list(
          XMLNode("span", sprintf("%s :", names(kv)), attrs=c(class="delib-info-key"))
          , XMLNode("span", kv, attrs=c(class="delib-info-val")) ##
        )
        , parent = parent
        , attrs=c(class="delib-info")
      )
    }
    ## conteneur de la délibération
    container0 <- newXMLNode("div", parent = parent, attrs=c(class="delib"))
    ## titre de la délibération
    tit1 <- XMLNode("h2", delib$TITREFULL, parent = container0, attrs = c(id = delib$ID) )
    ##
    container1 <- newXMLNode("div", parent = container0, attrs=c(class="delib-container"))
    ## 
    info.container0 <- newXMLNode("div", parent = container1, attrs=c(class="delib-info-container"))
    ##
    ## Métadonnées de la délib
    ##
    ## ID
    appendInfoNode(c(ID=delib$ID), info.container0)
    ## NUMERO
    appendInfoNode(c(NUMERO=delib$NUMERO), info.container0)
    ## DATE_TEXTE
    appendInfoNode(c(DATE_TEXTE = as.character(delib$DATE_TEXTE)), info.container0)
    ## NATURE_DELIB
    appendInfoNode(c(NATURE_DELIB=delib$NATURE_DELIB), info.container0)
    ## ETAT_JURIDIQUE
    appendInfoNode(c(ETAT_JURIDIQUE=delib$ETAT_JURIDIQUE), info.container0)
    ##
    xmldoc <- xmlTreeParse(
      delib$BLOC_TEXTUEL
      , asText = T
      , useInternalNodes = T
    )
    ##
    # print(xmldoc)
    ##
    txt <- xpathApply(
      xmldoc
      , "//CONTENU" ## 
    )
    ##
    xmlName(txt[[1]]) <- "div"
    ## 
    xmlAttrs(txt[[1]]) <- c(class="delib-txt" )
    ##
    addChildren(
      container1
      , txt
    )
    ##
    ## Toc entry
    ##
    XMLNode(
      "a"
      # , sprintf("%s — %s", as.character(delib$DATE_TEXTE), delib$TITREFULL)
      , list(
        XMLNode("span", as.character(delib$DATE_TEXTE), attr = c(class="toc-delib-dt"))
        # , " — "
        , XMLNode("span", delib$TITREFULL, attr = c(class="toc-delib-tit"))
      )
      , attrs= c(href= paste0("#", delib$ID))
    )
  }
  ##
  ## Tri
  ## 
  if(!is.null(sort.by)){
    if( !all(m <- sort.by %in% colnames(delib)) ){
      stop("sort column(s) : ", delib[!m], " not found")
    }
    # print(str(as.list(delib[sort.by])))
    i <- do.call(order, as.list(delib[sort.by]))
    ##
    delib <- delib[i,]
  }
  ##
  html0 = newXMLNode("html")
  ##
  head0 <- newXMLNode("head", parent =html0)
  ##
  tit0 <- newXMLNode("title", parent =head0)
  ##
  addChildren(
    tit0
    , doc.tit ## paste0(delib$ID, ": ", delib$TITREFULL )
  )
  ##
  ## Métadonnées du doc
  ## 
  meta0 <- newXMLNode("meta", attrs = list(charset="utf-8"), parent = head0) ## <meta charset="utf-8">
  ## Feuille de style
  if(!is.null(css.path)){
    ##
    if( !css.internal ){
      ## <link rel="stylesheet" type="text/css" href="/delib-css-0.css" />
      css0 <- newXMLNode("link", attrs = c(rel="stylesheet", type="text/css", href=css.path), parent = meta0)
    }
    else if( file.exists(css.path)) {
      ##
      css.txt0 <- readChar(css.path, file.info(css.path)$size)
      ##  ¡¡¡FIXME: empêcher conversion en entités html !!!
      css0 <- newXMLNode("style", newXMLTextNode(css.txt0, escapeEntities=T), parent = meta0)
      
    }else{
      ##
      warning("fichier css ", css.path, " introuvable")
    }
  }
  ## Doctype(name="html")
  ##
  body0 <- newXMLNode("body", parent =html0)
  ##
  container0 <- newXMLNode("div", parent =body0, attrs=c(class="container"))
  ##
  XMLNode(
    "h1"
    , doc.tit
    , container0
  )
  ##
  # toc-container
  #   toc-list
  #     toc-entry
  #       toc-delib-dt
  #       toc-delib-tit
  ##
  toc.container0 <- newXMLNode("div", parent =container0, attrs=c(class="toc-container"))
  ## 
  toc0 <- newXMLNode("ol", parent = toc.container0, attrs= c(class="toc-list"))
  ##
  ## cvt des délib en html
  ##
  ## rv contient l'entrée pour la TdM
  ##
  rv <- lapply(
    1:nrow(delib)
    , function(i, sup0){##, toc0
      toHtml(delib[i,], sup0)##, toc0
    }
    , container0
    # , toc0
  )
  ##
  # print(rv)
  ##
  ## ajout des entrées dans la TdM
  ##
  lapply(rv, function(x, toc){
    ## 
    n <- newXMLNode("li", parent =toc, attrs= c(class="toc-entry"))
    ##
    addChildren(
      n
      , x
    )
  }, toc0)
  ##
  saveXML(
    html0
    , file = file
    , doctype =  Doctype(name = "html") ## 
  )
}