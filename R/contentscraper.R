#' ContentScraper
#'
#' @param Url character, one url or a vector of urls of web pages to scrape.
#' @param HTmlText character, web page as HTML text to be scraped.use either Url or HtmlText not both.
#' @param XpathPatterns character vector, one or more XPath patterns to extract from the web page.
#' @param CssPatterns character vector, one or more CSS selector patterns to extract from the web page.
#' @param PatternsName character vector, given names for each xpath pattern to extract, just as an indication .
#' @param ManyPerPattern boolean, If False only the first matched element by the pattern is extracted (like in Blogs one page has one article/post and one title). Otherwise if set to True all nodes matching the pattern are extracted (Like in galleries, listing or comments, one page has many elements with the same pattern )
#' @param ExcludeXpathPat character vector, one or more Xpath pattern to exclude from extracted content (like excluding quotes from forum replies or excluding middle ads from Blog post) .
#' @param ExcludeCSSPat character vector, one or more Css pattern to exclude from extracted content.
#' @param astext boolean, default is TRUE, HTML and PHP tags is stripped from the extracted piece.
#' @param encod character, set the weppage character encoding.
#' @return
#' return a named list of extracted content
#' @author salim khalil
#' @examples
#' \dontrun{
#'
#' DATA<-ContentScraper(Url ="http://glofile.com/index.php/2017/06/08/sondage-quel-budget/",
#' CssPatterns = c(".entry-title",".published",".entry-content"), astext = TRUE)
#' #Extract title, publishing date and article from the web page using css selectors
#'
#' txthml<-"<html><title>blah</title><div><p>I m the content</p></div></html>"
#' DATA<-ContentScraper(HTmlText = txthml ,XpathPatterns = "//*/p")
#' #The web page source can be provided also as HTML text (characters)
#'
#' DATA<-ContentScraper(Url ="http://glofile.com/index.php/2017/06/08/athletisme-m-a-rome/",
#' XpathPatterns=c("//head/title","//*/article"),PatternsName=c("title", "article"))
#' #Extract the title and the article from the web page using Xpath patterns,
#' #Patterns Name are provided as an indication.

#' urllist<-c("http://glofile.com/index.php/2017/06/08/sondage-quel-budget/",
#' "http://glofile.com/index.php/2017/06/08/cyril-hanouna-tire-a-boulets-rouges-sur-le-csa/",
#' "http://glofile.com/index.php/2017/06/08/placements-quelles-solutions-pour-doper/")
#'
#' DATA<-ContentScraper(Url =urllist, CssPatterns = c(".entry-title",".entry-content"),
#' PatternsName = c("title","content"))
#' #Extract titles and contents of all 3 given Urls using CSS selectors, As result DATA variable
#' #will handle 6 elements.
#'
#' DATA<-ContentScraper(Url =urllist, CssPatterns = c(".entry-title",".comment-content p"),
#' PatternsName = c("title","comments"), astext = TRUE, ManyPerPattern = TRUE)
#' #Extract titles and comments from a list of blog posts, ManyPerPattern argument enables extracting
#' #multiple similar elements from each page like comments,reviews, quotes and listing.
#'
#' DATA<-ContentScraper(Url = "https://bitcointalk.org/index.php?topic=2334331.0",
#' CssPatterns = c(".post"),
#' ExcludeCSSPat = c(".quote",".quoteheader"),
#' PatternsName = c("posts"), ManyPerPattern = TRUE)
#' # From this Forum post Url we extract the post title and all replies using these CSS selectors
#' # c(".post"), However, we know that each reply contain the the previous Reply as quote so we exclude
#' # all quotes and quotes header from extracted posts using ExcludeCSSPat c(".quote",".quoteheader a")
#' }
#' @import  xml2 selectr
#' @export
#'
#'
ContentScraper <- function(Url, HTmlText, XpathPatterns, CssPatterns, PatternsName, ExcludeXpathPat, ExcludeCSSPat, ManyPerPattern=FALSE, astext=TRUE, encod) {

  if(!missing(Url) && !missing(HTmlText) ){
    stop("Please supply Url or HTmlText, not both !")
  }

  if(!missing(XpathPatterns) && !missing(CssPatterns) ){
    stop("Please supply XpathPatterns or CssPatterns, not both !")
  }

  if(!missing(ExcludeXpathPat) && !missing(ExcludeCSSPat) ){
    stop("Please supply ExcludeXpathPat or ExcludeCSSPat, not both !")
  }
  if(!missing(XpathPatterns) && !missing(PatternsName) ){
    if (length(XpathPatterns)!=length(PatternsName)) stop("PatternsName & XpathPatterns parameters must have the same length ")
  }
  if(!missing(CssPatterns) && !missing(PatternsName) ){
    if (length(CssPatterns)!=length(PatternsName)) stop("PatternsName & CssPatterns parameters must have the same length ")
  }

  if(!missing(ExcludeCSSPat)) {
    if(is.vector(ExcludeCSSPat)){
      ExcludeXpathPat<- unlist(lapply(ExcludeCSSPat, FUN = function(x) { tryCatch(selectr::css_to_xpath(x, prefix = "//") ,error=function(e) stop("Unable to translate supplied css selector, Please check CssPatterns syntax !"))}))
    }
  }
  if(!missing(CssPatterns)) {
    if(is.vector(CssPatterns)){
      XpathPatterns<- unlist(lapply(CssPatterns, FUN = function(x) { tryCatch(selectr::css_to_xpath(x, prefix = "//") ,error=function(e) stop("Unable to translate supplied css selector, Please check CssPatterns syntax !"))}))
    } else {
      stop("CssPatterns parameter must be a vector with at least one element !")
    }
  }

  content<-list()
  if(!missing(Url) && missing(HTmlText)){
    pos<-1
    for(Ur in Url){
      pageinfo<-LinkExtractor(url=Ur, encod=encod)
      HTmlText<-pageinfo[[1]][[10]]
      x<-xml2::read_html(HTmlText, encoding = encod)
      if(ManyPerPattern){
        if (astext && missing(ExcludeXpathPat)){
          invisible(xml_remove(xml_find_all(x, "//script")))
          contentx<-lapply(XpathPatterns,function(n) { tryCatch(xml_text(xml_find_all(x,n)),error=function(e) "NA" ) })

        } else{
          contentx<-lapply(XpathPatterns,function(n) { tryCatch(paste(xml_find_all(x,n)),error=function(e) "" ) })
        }

        if(!missing(ExcludeCSSPat) || !missing(ExcludeXpathPat)){
          #contentx<-unlist(contentx)
          ToExcludeL<-lapply(ExcludeXpathPat,function(n) { tryCatch(paste(xml_find_all(x,n)),error=function(e) NULL ) })
          ToExclude<-unlist(ToExcludeL)
          if(!is.null(ToExclude) && length(ToExclude)>0 ){
            for( i in 1:length(ToExclude)) {
              for (j in 1:length(contentx)) {
                if(length(contentx[[j]])>1){
                  for(k in 1:length(contentx[[j]])){
                    if (grepl(ToExclude[[i]], contentx[[j]][[k]], fixed = TRUE) && nchar(ToExclude[[i]])!=0 ){
                      contentx[[j]][[k]]<-gsub(ToExclude[[i]],"", contentx[[j]][[k]], fixed = TRUE)
                    }
                  }
                } else if(length(contentx[[j]])==1) {
                  if (grepl(ToExclude[[i]], contentx[[j]], fixed = TRUE) && nchar(ToExclude[[i]])!=0 ){
                    contentx[[j]]<-gsub(ToExclude[[i]],"", contentx[[j]], fixed = TRUE)
                  }
                }
              }
            }
          }
          if(astext){
            for (j in 1:length(contentx)) {
              if(length(contentx[[j]])>0){
                for(k in 1:length(contentx[[j]])){
                  contentx[[j]][[k]]<-RemoveTags(contentx[[j]][[k]])
                }
              }
            }
          }
        }
        if  (!missing(PatternsName)){
          for( i in 1:length(contentx)) {
            if(length(contentx[[i]])>0){
              names(contentx)[i]<-PatternsName[[i]]
            }
          }
        }
      } else {
        if (astext && missing(ExcludeXpathPat)){
          invisible(xml_remove(xml_find_all(x, "//script")))
          contentx<-lapply(XpathPatterns,function(n) { tryCatch(xml_text(xml_find_first(x,n)),error=function(e) "" ) })
        } else{
          contentx<-lapply(XpathPatterns,function(n) { tryCatch(paste(xml_find_first(x,n)),error=function(e) "" ) })
        }
        if(!missing(ExcludeCSSPat) || !missing(ExcludeXpathPat)){
          #contentx<-unlist(contentx)
          ToExcludeL<-lapply(ExcludeXpathPat,function(n) { tryCatch(paste(xml_find_all(x,n)),error=function(e) NULL ) })
          ToExclude<-unlist(ToExcludeL)
          if(!is.null(ToExclude) && length(ToExclude)>0 ){
            for( i in 1:length(ToExclude)) {
              for (j in 1:length(contentx)) {
                if(length(contentx[[j]])>1){
                  for(k in 1:length(contentx[[j]])){
                    if (grepl(ToExclude[[i]], contentx[[j]][[k]], fixed = TRUE) && nchar(ToExclude[[i]])!=0 ){
                      contentx[[j]][[k]]<-gsub(ToExclude[[i]],"", contentx[[j]][[k]], fixed = TRUE)
                    }
                  }
                } else if(length(contentx[[j]])==1) {
                  if (grepl(ToExclude[[i]], contentx[[j]], fixed = TRUE) && nchar(ToExclude[[i]])!=0 ){
                    contentx[[j]]<-gsub(ToExclude[[i]],"", contentx[[j]], fixed = TRUE)
                  }
                }
              }
            }
          }
          if(astext){
            for (j in 1:length(contentx)) {
              if(length(contentx[[j]])>0){
                for(k in 1:length(contentx[[j]])){
                  contentx[[j]][[k]]<-RemoveTags(contentx[[j]][[k]])
                }
              }
            }
          }
        }

        if  (!missing(PatternsName)){
          for( i in 1:length(contentx)) {
            names(contentx)[i]<-PatternsName[[i]]
          }
        }
      }
    if(length(Url)>1) content<-c(content,list(contentx))
      else content<-c(content,contentx)

    cat(pos,"..", sep = "")
    pos<-pos+1
    flush.console()
    Sys.sleep(1)
    }
  }
  if(missing(Url) && !missing(HTmlText)){
    x<-xml2::read_html(HTmlText,  encoding= encod)
    if(ManyPerPattern){
      if (astext){
        contentx<-lapply(XpathPatterns,function(n) { tryCatch(xml_text(xml_find_all(x,n)),error=function(e) "" ) })
      } else{
        contentx<-lapply(XpathPatterns,function(n) { tryCatch(paste(xml_find_all(x,n)),error=function(e) "" ) })
      }
      if  (!missing(PatternsName)){
        for( i in 1:length(contentx)) {
          names(contentx)[i]<-PatternsName[[i]]
        }
      }
    }
    else {
      if (astext){
        contentx<-lapply(XpathPatterns,function(n) { tryCatch(xml_text(xml_find_first(x,n)),error=function(e) "" ) })
      } else{
        contentx<-lapply(XpathPatterns,function(n) { tryCatch(paste(xml_find_first(x,n)),error=function(e) "" ) })
      }
      if  (!missing(PatternsName)){
        for( i in 1:length(contentx)) {
          names(contentx)[i]<-PatternsName[[i]]
        }
      }
    }
    content<-c(content,contentx)
  }

  return (content)
}
