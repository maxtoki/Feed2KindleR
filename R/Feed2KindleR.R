Feed2KindleR <- function(
  feed = "http://www.hardballtimes.com/feed/"
  , fld_html = "html"
  , fld_img = paste(fld_html, "img", sep="/")
  , fld_mobi = "mobi"
){
  options(stringsAsFactors = F)
  require(xml2)
  require(dplyr)
  require(stringr)
  require(XML)
  
  ### read feed ##################################
  page <- read_xml(feed)
  # check if it's rss or atom
  feedtype <- NA
  if(page %>% xml_find_all("/feed") %>% length() > 0) feedtype <- "atom"
  if(page %>% xml_find_all("/rss") %>% length() > 0) feedtype <- "rss"
  if(is.na(feedtype)) stop("feed type not recognized")  
  # page <- read_xml(url)
  
  # parse
  if(feedtype=="rss"){
    # parse rss file
    sitenode <- page %>% xml_find_one("channel")
    
    feedname <- sitenode %>% xml_find_one("title") %>% xml_text
    feeddescr <- sitenode %>% xml_find_one("description") %>% xml_text
    
    articlenodes <- page %>% xml_find_all("//item")
    
    feedarticles <- lapply(articlenodes, function(x){
      arttitle <- x %>% xml_find_one("title") %>% xml_text
      artlink <- x %>% xml_find_one("link") %>% xml_text
      artdate <- x %>% xml_find_one("pubDate") %>% xml_text
      #...description?
      artdata <- data.frame(title=arttitle, link=artlink, pubdate=artdate)
      return(artdata)
    })
    feedarticles <- do.call("rbind", feedarticles)
    
  } else { #if(feedtype=="rss")
    # parse atom file
    sitenode <- page %>% xml_find_one("feed")
    
    feedname <- sitenode %>% xml_find_one("title") %>% xml_text
    feeddescr <- sitenode %>% xml_find_one("subtitle") %>% xml_text
    
    articlenodes <- page %>% xml_find_all("//entry")
    
    feedarticles <- lapply(articlenodes, function(x){
      arttitle <- x %>% xml_find_one("title") %>% xml_text
      artlink <- x %>% xml_find_one("link") %>% xml_attr("href")
      artdate <- x %>% xml_find_one("updated") %>% xml_text
      #...description?
      artdata <- data.frame(title=arttitle, link=artlink, pubdate=artdate)
      return(artdata)
    })
    feedarticles <- do.call("rbind", feedarticles)
  } #if(feedtype=="rss")
  feedData <- list(feed=feedname, description=feeddescr, articles=feedarticles)
  feedData$articles$pubdate <- feedData$articles$pubdate %>% as.POSIXct(format="%a, %d %b %Y %H:%M:%S")
  feedData$articles <- feedData$articles %>% arrange(desc(pubdate))
  
  ### download and process html files ############
  sapply(1:nrow(feedData$articles), function(itemn){
    item <- feedData$articles[itemn,]
    urlparts <- url_parse(item$link)
    pathelements <- urlparts$path %>% str_replace("^/", "") %>% str_replace("/$", "")
    filename <- pathelements[length(pathelements)]
    # TODO: check for any extension
    if(!(filename %>% str_detect("(.html|.htm)$"))) filename <- paste0(filename, ".html")
    
    # if file not in the html folder, download it
    filepath <- paste(fld_html, filename, sep="/")
    if(length(setdiff(filename, list.files(fld_html))) > 0){
      try(download.file(item$link,  filepath, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra")))
      
      # remove script and head tags
      doc <- htmlParse(filepath ,useInternalNode=TRUE)
      r <- xmlRoot(doc)
      removeNodes(xpathSApply(r, "//script"))
      removeNodes(xpathSApply(r, "//head"))
      saveXML(r, filepath)
      
      # save images locally and modify html accordingly
      img <- xpathSApply(r, "//img")
      urls <- sapply(img, function(x) xmlAttrs(x)["src"])
      urlsdf <- url_parse(urls)
      # images names
      imgnames <- sapply(urlsdf$path, function(x){
        spl <- str_split(x, "/")
        nm <- spl[[1]][length(spl[[1]])]
        return(nm)
      })
      # modify the html file to look into the local img folder
      sapply(1:length(img), function(i){
        ond <- img[[i]]
        nnd <- ond
        xmlAttrs(ond)["src"] <- paste(fld_img, imgnames[i], sep="/")
        replaceNodes(ond, nnd)
      })
      saveXML(r, filepath)
      # download images locally (if not already there)
      sapply(1:length(img), function(i){
        if(length(setdiff(tolower(imgnames[i]), tolower(list.files(fld_img))))>0){
          if(urlsdf$server[i] != ""){
            try(download.file(urls[i], paste(fld_img, imgnames[i], sep="/") , quiet = FALSE, mode="wb"), silent = T)
          } else { #if(urlsdf$domain[i] != "")
            try(download.file(paste0(urlparts$scheme[1], "://", urlparts$server[1], "/", urls[i]), paste(fld_img, imgnames[i], sep="/") , quiet = FALSE, mode="wb"), silent = T)
          } #if(urlsdf$domain[i] != "")
        } #if(length(setdiff(imgnames[i], list.files(fld_img))))
      })
      
    } #if(length(setdiff(filename, list.files(fld_html))) > 0)
  }) #sapply(feedData, function(item)
  
  # TODO: clean up old html and image files (like 2 weeks?) using file.info() function
  
  # create TOC html
  n.body <- newXMLNode("body", parent=d.toc)
  n.div <- newXMLNode("div", attrs = c(id = 'toc'), parent=n.body)
  n.h2 <- newXMLNode("h2", "Table of Contents", parent = n.div)
  n.ul <- newXMLNode("ul", parent = n.div)
  sapply(1:nrow(feedData$articles), function(i){
    n.li <- newXMLNode("li", parent = n.ul)
    item <- feedData$articles[i,]
    urlparts <- url_parse(item$link)
    pathelements <- urlparts$path %>% str_replace("^/", "") %>% str_replace("/$", "")
    filename <- pathelements[length(pathelements)]
    # TODO: check for any extension
    if(!(filename %>% str_detect("(.html|.htm)$"))) filename <- paste0(filename, ".html")
    n.a <- newXMLNode("a", feedData$articles[i,"title"], attrs = c(href = filename), parent = n.li)
  })
  saveXML(n.body, paste(fld_html, "toc.html", sep="/"))
  
  # TODO: generate OPF file
  d.opf <- newXMLDoc()
  n.package <- newXMLNode("package", doc = d.opf) #, namespace = c(opf="http://www.idpf.org/2007/opf", asd="http://www.idpf.org/asdfaf")
  n.metadata <- newXMLNode("metadata", parent = n.package) #, namespace = c()
  n.dc.metadata <- newXMLNode("dc-metadata", parent = n.metadata, namespace = c(dc="http://purl.org/metadata/dublin_core", oebpackage="http://openebook.org/namespaces/oeb-package/1.0/"))
  n.title <- newXMLNode("dc:Title", feedData$feed, parent = n.dc.metadata)
  n.language <- newXMLNode("dc:Language", "en", parent = n.dc.metadata)
    #...TODO: language (grab from feed), etc.
  n.manifest <- newXMLNode("manifest", parent = n.package)
  n.spine <- newXMLNode("spine", parent = n.package, attrs = c(toc = "ncx"))
  n.guide <- newXMLNode("guide", parent = n.package)
  n.m.toc <- newXMLNode("item", parent = n.manifest, attrs = c(id = "ncx", `media-type`="application/x-dtbncx+xml", href="toc.ncx"))
  n.m.content <- newXMLNode("item", parent = n.manifest, attrs = c(id = "content", `media-type`="text/x-oeb1-document", href="toc.html"))
  n.g.toc <- newXMLNode("reference", parent = n.guide, attrs = c(type="toc", title="Table of Contents", href="toc.html"))
  n.s.content <- newXMLNode("itemref", parent = n.spine, attrs = c(idref = "content"))
  sapply(1:nrow(feedData$articles), function(i){
    item <- feedData$articles[i,]
    urlparts <- url_parse(item$link)
    pathelements <- urlparts$path %>% str_replace("^/", "") %>% str_replace("/$", "")
    filename <- pathelements[length(pathelements)]
    # TODO: check for any extension
    if(!(filename %>% str_detect("(.html|.htm)$"))) filename <- paste0(filename, ".html")
    n.item <- newXMLNode("item", parent = n.manifest, attrs = c(id=paste0("item", i), `media-type`="text/x-oeb1-document", href=filename))
    n.itemref <- newXMLNode("itemref", parent = n.spine, attrs = c(idref=paste0("item", i)))
    n.reference <- newXMLNode("reference", parent = n.guide, attrs = c(type="text", title=item$title, href=filename))
  })
  saveXML(d.opf, paste0(fld_html, "/", gsub("\\W", "", feedData$feed), ".opf"))
  
  # generate NCX file
  d.ncx <- newXMLDoc()
  n.ncx <- newXMLNode("ncx", doc = d.ncx, namespace = c(ncx="http://www.daisy.org/z3986/2005/ncx/", version="2005-1")) #
  n.head <- newXMLNode("head", parent = n.ncx)
  n.docTitle <- newXMLNode("docTitle", parent = n.head)
  n.title <- newXMLNode("text", feedData$feed, parent = n.docTitle)
  n.navMap <- newXMLNode("navMap", parent = n.ncx)
  n.NavPoint.t <- newXMLNode("navPoint", parent = n.navMap, attrs=c(id="toc", playOrder="1"))
  n.navLabel.t <- newXMLNode("navLabel", parent = n.NavPoint.t)
  n.text.t <- newXMLNode("text", "Table of Contents", parent = n.navLabel.t)
  n.Content.t <- newXMLNode("content", parent = n.NavPoint.t, attrs = c(src="toc.html"))
  sapply(1:nrow(feedData$articles), function(i){
    item <- feedData$articles[i,]
    urlparts <- url_parse(item$link)
    pathelements <- urlparts$path %>% str_replace("^/", "") %>% str_replace("/$", "")
    filename <- pathelements[length(pathelements)]
    # TODO: check for any extension
    if(!(filename %>% str_detect("(.html|.htm)$"))) filename <- paste0(filename, ".html")
    n.NavPoint <- newXMLNode("navPoint", parent = n.navMap, attrs=c(id=paste0("item", i), playOrder=as.character(i+1)))
    n.navLabel <- newXMLNode("navLabel", parent = n.NavPoint)
    n.text <- newXMLNode("text", item$title, parent = n.navLabel)
    n.Content <- newXMLNode("content", parent = n.NavPoint, attrs = c(src=filename))
  })
  saveXML(d.ncx, paste(fld_html, "toc.ncx", sep="/"))
  
  # TODO: launch KindleGen to generate the mobi file
  # TODO: email mobifile to kindle address
  
} #Feed2KindleR

Feed2KindleR()
