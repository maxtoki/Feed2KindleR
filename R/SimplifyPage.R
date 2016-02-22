removescripts <- function(
  url = "destfile.html"
  , dest = "destfile2.html"
){
  require(XML)
  require(xml2)
  doc <- htmlParse(url,useInternalNode=TRUE)
  r <- xmlRoot(doc)
  removeNodes(xpathSApply(r, "//script"))
  removeNodes(xpathSApply(r, "//head"))
  saveXML(r, dest)
}

workimages <- function(
  url = "destfile2.html"
  , dest = "destfile3.html"
  , folder = 'img'
){
  require(XML)
  require(dplyr)
  require(stringr)
  require(urltools)
  doc <- htmlParse(url,useInternalNode=TRUE)
  r <- xmlRoot(doc)
  img <- xpathSApply(r, "//img")
  urls <- sapply(img, function(x) xmlAttrs(x)["src"])
  urlsdf <- url_parse(urls)
  # TODO: there's a url_parse in xml2, slightly different, change to use that one
  maindomain <- urltools::urlsdf %>% group_by(scheme, domain) %>% tally() %>% arrange(desc(n)) %>% filter(domain != "") %>% filter(row_number()==1) %>% mutate(fulldomain = paste0(scheme, "://", domain))
  maindomain <- maindomain$fulldomain

  imgnames <- sapply(urlsdf$path, function(x){
    spl <- str_split(x, "/")
    nm <- spl[[1]][length(spl[[1]])]
    return(nm)
  })
  
  sapply(1:length(img), function(i){
    ond <- img[[i]]
    nnd <- ond
    xmlAttrs(ond)["src"] <- paste(folder, imgnames[i], sep="/")
    replaceNodes(ond, nnd)
  })
  
  sapply(1:length(img), function(i){
      if(urlsdf$domain[i] != ""){
        try(download.file(urls[i], paste(folder, imgnames[i], sep="/") , quiet = FALSE, mode="wb"), silent = T)
      } else { #if(urlsdf$domain[i] != "")
        try(download.file(paste0(maindomain, urls[i]), paste(folder, imgnames[i], sep="/") , quiet = FALSE, mode="wb"), silent = T)
      } #if(urlsdf$domain[i] != "")
    })
  
  saveXML(r, dest)
}
