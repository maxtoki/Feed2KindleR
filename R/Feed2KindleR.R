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
  
  ### download and process html files ############
  sapply(feedData$feedarticles, function(item){
    urlparts <- url_parse(item$link)
    pathelements <- urlparts$path %>% str_replace("^/", "") %>% str_replace("/$", "")
    filename <- pathelements[length(pathelements)]
    # TODO: check for any extension
    if(!(filename %>% str_detect("(.html|.htm)$"))) filename <- paste0(filename, ".html")
    
    # if file not in the html folder, download it
    filepath <- paste(fld_html, filename, sep="/")
    if(length(setdiff(filename, list.files(fld_html))) > 0)
      download.file(item$link,  filepath, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    
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
      if(length(setdiff(imgnames[i], list.files(fld_img)))){
        if(urlsdf$server[i] != ""){
          try(download.file(urls[i], paste(fld_img, imgnames[i], sep="/") , quiet = FALSE, mode="wb"), silent = T)
        } else { #if(urlsdf$domain[i] != "")
          try(download.file(paste0(urlparts$scheme, "://", urlparts$server, "/", urls[i]), paste(fld_img, imgnames[i], sep="/") , quiet = FALSE, mode="wb"), silent = T)
        } #if(urlsdf$domain[i] != "")
      } #if(length(setdiff(imgnames[i], list.files(fld_img))))
    })
  }) #sapply(feedData, function(item)
  
  # TODO: clean up old html and image files (like 2 weeks?)
  
  # TODO: generate OPF file
  # TODO: generate NCX file
  # TODO: launch KindleGen to generate the mobi file
  # TODO: email mobi file to kindle address
  
} #Feed2KindleR
