##
##
##
list.delib <- function(delib.url='https://echanges.dila.gouv.fr/OPENDATA/CNIL/'){
  ##
  r <- RCurl::getURL(delib.url)
  ## parse html
  delib.tar.url0.0 <- htmlParse(r)
  ## liste des noeud url des fichiers
  delib.tar.url0.1 <- getNodeSet(
    delib.tar.url0.0
    , "//a[starts-with(@href,'CNIL') or starts-with(@href,'Freemium')]"
  )
  ## extraction des url
  delib.tar.url0.2 <- data.frame(
    matrix(sapply(
      delib.tar.url0.1
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
    , dimnames = list(NULL,c("fname", "tm.sz"))
    )
    , stringsAsFactors = F
  )
  ## extraction horodatage + taille du fichier
  tm.sz0 <- stringi::stri_match(
    delib.tar.url0.2$tm.sz
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
  tm.sz0[is.na(tm.sz0[,3]), 3] <- 'o'
  ##
  delib.tar.url0 <- data.frame(
    ##
    delib.tar.url0.2
    ##
    , timestamp = strptime(tm.sz0[,1], "%Y-%m-%d %H:%M")
    ## sz
    , sz = as.numeric(tm.sz0[,2]) * ifelse(
      tm.sz0[,3]=='K'
      , 2^10 ## 1e3
      , ifelse(
        tm.sz0[,3]=='M'
        , 2^20 ## 1e6
        , 1
      )
    )
  )
  ## 
  class(delib.tar.url0$sz) <- c('object_size', class(delib.tar.url0$sz))
  ##
  delib.tar.url0
}
##
##
##
get.delib <- function(fnames, dst.dir, exdir, force=F, extract=F,verbose=0,...){
  sapply(
    fnames
    , function(fname){
      ##
      if(verbose) cat(fname)
      ##
      dst.fname <- paste(dst.dir, fname, sep='/')
      ##
      x <- !file.exists(dst.fname)
      ##
      if(verbose) cat("  ", x, "\n")
      ##
      rv <- if( !extract && ( x | force ) ){
        download.file(
          paste("https://echanges.dila.gouv.fr/OPENDATA/CNIL", fname, sep='/')
          , dst.fname
          ,...
        )+10
      }else as.integer(x) 
      ##
      if(extract){
        untar(
          dst.fname
          , exdir = exdir
        # , compressed = "gzip"
       )
      }else rv+100
    }
  )
}
##
##
##
delib.xmlparse <- function(fname){
  ##
  delib.xml <- xmlTreeParse(
    fname
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
delib.toDataFrame <- function(src.dir,  verbose=0){
  ##
  delib.assert <- function(delib){
    n <- table(sapply(delib,length))##
    rv <- !( length(n)==1  )
    if( rv ){
      warning("longueurs inégales: ")
      print(n)
    }
    rv
  }
  ##
  delib.fnames <- list.files(
    src.dir
    , recursive = T
  )
  ##
  delib.fnames <- delib.fnames[grepl( "\\.xml", delib.fnames)]
  ##
  if(verbose>0){
    cat(as.character(t0 <- Sys.time()), "\n", sep="")
    i <- 0
  }
  ##
  delib0 <- lapply(
    paste(
      src.dir
      , delib.fnames##
      , sep='/'
    )
    , function(fpath){
      if(verbose>0){
        i <<- i+1
        if( (i%%verbose)==0  )cat(i, " ")
        if( (i%%(verbose*10) )==0 )cat("\n")
      }
      delib.xmlparse(fpath)
    }
  )
  ##
  if( any(sapply(delib0,is.null)) || delib.assert(delib0) ){
    return(delib0)
  }
  ##
  if(verbose>0) cat( "\n", as.character(Sys.time() - t0 ), "\n", "Concaténation des délibérations\n", sep="")
  ##
  delib1 <- do.call(rbind, delib0)
  ##
  delib1$DATE_TEXTE <- as.Date(delib1$DATE_TEXTE, "%Y-%m-%d")
  delib1$DATE_PUBLI <- as.Date(delib1$DATE_PUBLI, "%Y-%m-%d")
  ##
  if(verbose>0) cat( "\n",as.character(Sys.time()- t0), "\n", sep="")
  ##
  delib1
}
##
##
##
delib.toText <- function(txt, xsl){
  ##
  sapply(
    1:length(txt) ## 
    , function(i){
      xml <- xmlTreeParse(
        txt[i] ##
        , useInternalNodes = T
      )
      if( length( xpathSApply(xml, "//*", xmlName) )>1 ){
        rv <- xsltApplyStyleSheet(
          xml
          , xsl
        )
        saveXML(rv)
      }else ""
    }
    , USE.NAMES = F
  )
}
##
##
##
delib.toHtml <- function(delib, file=NULL){
  ##
  html0 = newXMLNode("html")
  ##
  head0 <- newXMLNode("head", parent =html0)
  ##
  tit0 <- newXMLNode("title", parent =head0)
  ##
  addChildren(
    tit0
    , paste0(delib$ID, ": ", delib$TITREFULL )
  )
  ## <meta charset="utf-8">
  meta0 <- newXMLNode("meta", attrs = list(charset="utf-8"), parent = head0)
  ##
  body0 <- newXMLNode("body", parent =html0)
  ##
  xmldoc0 <- xmlTreeParse(
    delib$BLOC_TEXTUEL
    , useInternalNodes = T
  )
  ##
  txt0 <- xpathApply(
    xmldoc0
    , "/CONTENU/*"
  )
  ##
  addChildren(
    body0
    , txt0
  )
  ##
  saveXML(html0, file)
}