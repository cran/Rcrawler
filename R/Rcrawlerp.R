#' Rcrawler
#'
#' The crawler's main function, by providing only the website URL and the Xpath or CSS selector patterns
#' this function can crawl the whole website (traverse all web pages) download webpages, and scrape/extract
#' its contents in an automated manner to produce a structured dataset. The process of a crawling
#' operation is performed by several concurrent processes or nodes in parallel, so it's recommended to
#' use 64bit version of R.
#'
#'
#' @param Website character, the root URL of the website to crawl and scrape.
#' @param no_cores integer, specify the number of clusters (logical cpu) for parallel crawling, by default it's the numbers of available cores.
#' @param no_conn integer, it's the number of concurrent connections per one core, by default it takes the same value of no_cores.
#' @param MaxDepth integer, repsents the max deph level for the crawler, this is not the file depth in a directory structure, but 1+ number of links between this document and root document, default to 10.
#' @param DIR character, correspond to the path of the local repository where all crawled data will be stored ex, "C:/collection" , by default R working directory.
#' @param RequestsDelay integer, The time interval between each round of parallel http requests, in seconds used to avoid overload the website server. default to 0.
#' @param Obeyrobots boolean, if TRUE, the crawler will parse the website\'s robots.txt file and obey its rules allowed and disallowed directories.
#' @param Useragent character, the User-Agent HTTP header that is supplied with any HTTP requests made by this function.it is important to simulate different browser's user-agent to continue crawling without getting banned.
#' @param Timeout integer, the maximum request time, the number of seconds to wait for a response until giving up, in order to prevent wasting time waiting for responses from slow servers or huge pages, default to 5 sec.
#' @param URLlenlimit integer, the maximum URL length limit to crawl, to avoid spider traps; default to 255.
#' @param urlExtfilter character's vector, by default the crawler avoid irrelevant files for data scraping such us xml,js,css,pdf,zip ...etc, it's not recommanded to change the default value until you can provide all the list of filetypes to be escaped.
#' @param urlregexfilter character's vector, filter crawled Urls by regular expression pattern, this is useful when you try to scrape content or index only specific web pages (product pages, post pages).
#' @param ignoreUrlParams character's vector, the list of Url paremeter to be ignored during crawling .
#' @param statslinks boolean, if TRUE, the crawler counts the number of input and output links of each crawled web page.
#' @param KeywordsFilter character vector,  For users who desires to scrape or collect only web pages that contains some keywords one or more. Rcrawler calculate an accuracy score based of the number of founded keywords. This parameter must be a vector with at least one keyword like c("mykeyword").
#' @param KeywordsAccuracy integer value range bewteen 0 and 100, used only with KeywordsFilter parameter to determine the accuracy of web pages to collect. The web page Accuracy value is calculated using the number of matched keywords and their occurence.
#' @param FUNPageFilter function, filter out pages to collect by your custom function (prediction, calssification model). This function should take two arguments URL and Content then test the content arg if is eligible or not following your rules then finally it must returns TRUE or FALSE.
#' @param Encod character, set the website caharacter encoding, by default the crawler will automatically detect the website defined character encoding.
#' @param ExtractXpathPat character's vector, vector of xpath patterns to match for data extraction process.
#' @param ExtractCSSPat character's vector, vector of CSS selector pattern to match for data extraction process.
#' @param PatternsNames character vector, given names for each xpath pattern to extract.
#' @param ExcludeXpathPat character's vector, one or more Xpath pattern to exclude from extracted content ExtractCSSPat or ExtractXpathPat (like excluding quotes from forum replies or excluding middle ads from Blog post) .
#' @param ExcludeCSSPat character's vector, similar to ExcludeXpathPat but using Css selectors.
#' @param ExtractAsText boolean, default is TRUE, HTML and PHP tags is stripped from the extracted piece.
#' @param ManyPerPattern boolean, ManyPerPattern boolean, If False only the first matched element by the pattern is extracted (like in Blogs one page has one article/post and one title). Otherwise if set to True  all nodes matching the pattern are extracted (Like in galleries, listing or comments, one page has many elements with the same pattern )
#' @param NetworkData boolean, If set to TRUE, then the crawler map all the internal hyperlink connections within the given website and return DATA for Network construction using igraph or other tools.(two global variables is returned see details)
#' @param NetwExtLinks boolean, If TRUE external hyperlinks (outlinks) also will be counted on Network edges and nodes.
#'
#' @return
#'
#' The crawling and scraping process may take a long time to finish, therefore, to avoid data loss in the case that a function crashes or stopped in the middle of action, some important data are exported at every iteration to R global environement:
#'
#' - INDEX: A data frame in global environement representing the generic URL index,including the list of fetched URLs and page details
#'   (contenttype,HTTP state, number of out-links and in-links, encoding type, and level).
#'
#' - A repository in workspace that contains all downloaded pages (.html files)
#'
#' If data scraping is enabled by setting ExtractXpathPat or ExtractCSSPat parameter:
#'
#' - DATA: A list of lists in global environement holding scraped contents.
#'
#' - A csv file 'extracted_contents.csv' holding all extracted data.
#'
#' If NetworkData is set to TRUE two additional global variables returned by the function are:
#'
#' - NetwIndex : Vector maps alls hyperlinks (nodes) with a unique integer ID
#'
#' - NetwEdges : data.frame representing edges of the network, with these column : From, To, Weight (the Depth level where the link connection has been discovered) and Type (1 for internal hyperlinks 2 for external hyperlinks).
#'
#' @details
#'
#' To start Rcrawler task you need the provide the root URL of the website you want to scrape, it can be a domain, a subdomain or a website section (eg. http://www.domain.com, http://sub.domain.com or http://www.domain.com/section/). The crawler then will go through all its internal links. The process of a crawling is performed by several concurrent processes or nodes in parallel, So, It is recommended to use R 64-bit version.
#'
#' For more tutorials check https://github.com/salimk/Rcrawler/
#'
#' For scraping complexe character content such as arabic execute Sys.setlocale("LC_CTYPE","Arabic_Saudi Arabia.1256") then set the encoding of the web page in Rcrawler function.
#'
#' If you want to learn more about web scraper/crawler architecture, functional properties and implementation using R language, Follow this link and download the published paper for free .
#'
#' Link: http://www.sciencedirect.com/science/article/pii/S2352711017300110
#'
#' Dont forget to cite Rcrawler paper:
#'
#' Khalil, S., & Fakir, M. (2017). RCrawler: An R package for parallel web crawling and scraping. SoftwareX, 6, 98-106.
#'
#' @examples
#'
#' \dontrun{
#'  Rcrawler(Website ="http://glofile.com/", no_cores = 4, no_conn = 4)
#'
#'  #Crawl, index, and store web pages using 4 cores and 4 parallel requests
#'
#'  Rcrawler(Website = "http://glofile.com/", urlregexfilter =  "/[0-9]{4}/[0-9]{2}/",
#'  ExtractXpathPat = c("//*/article","//*/h1"), PatternsNames = c("content","title"))
#'
#'   # Crawl the website using the default configuration and scrape content matching two XPath
#'   patterns only from post pages matching a specific regular expression "/[0-9]{4}/[0-9]{2}/".
#'   # Note that the user can use the excludepattern  parameter to exclude a node from being extracted,
#'   # e.g., in the case that a desired node includes (is a parent of) an undesired "child" node.
#'
#'   Rcrawler(Website = "http://www.example.com/", no_cores=8, no_conn=8, Obeyrobots = TRUE,
#'   Useragent="Mozilla 3.11")
#'   # Crawl and index the website using 8 cores and 8 parallel requests with respect to
#'   # robot.txt rules.
#'   Rcrawler(Website = "http://www.example.com/", no_cores = 4, no_conn = 4,
#'   urlregexfilter =  "/[0-9]{4}/[0-9]{2}/", DIR = "./myrepo", MaxDepth=3)
#'
#'   # Crawl the website using  4 cores and 4 parallel requests. However, this will only
#'   # index URLs matching the regular expression pattern (/[0-9]{4}/[0-9]{2}/), and stores pages
#'   # in a custom directory "myrepo".
#'   # The crawler stops After reaching the third level of website depth.
#'
#'   Rcrawler(Website = "http://www.example.com/", KeywordsFilter = c("keyword1", "keyword2"))
#'   # Crawl the website and collect only webpages containing keyword1 or keyword2 or both.
#'
#'   Rcrawler(Website = "http://www.example.com/", KeywordsFilter = c("keyword1", "keyword2"),
#'    KeywordsAccuracy = 50)
#'   # Crawl the website and collect only webpages that has an accuracy percentage higher than 50%
#'   # of matching keyword1 and keyword2.
#'
#'   Rcrawler(Website = "http://glofile.com/" , no_cores = 4, no_conn = 4, NetworkData = TRUE)
#'   # Crawl the entire website, and create network edges DATA of internal links.
#'   # Using Igraph for exmaple you can plot the network by the following commands
#'    library(igraph)
#'    network<-graph.data.frame(NetwEdges, directed=T)
#'    plot(network)
#'
#'   Rcrawler(Website = "http://glofile.com/" , no_cores = 4, no_conn = 4, NetworkData = TRUE)
#'   # Crawl the entire website, and create network edges DATA of internal and external links .
#'   #FUNPageFilter parameter usage
#'   # So first you create a function in your computer, it must take two arguments url, content
#'   # then returns true or false.
#'   # Inside the function you should test content variable using your rules
#'   # Finally, you just call it inside Rcrawler function, Then the crawler will evaluate each
#'    page using your function.
#'    Rcrawler(Website = "http://glofile.com", no_cores=2, FUNPageFilter= Mytestfunction )
#'
#'}
#'
#'
#' @author salim khalil
#' @import foreach doParallel parallel data.table selectr
#' @export
#' @importFrom utils write.table
#' @importFrom utils flush.console
#'


Rcrawler <- function(Website, no_cores,no_conn, MaxDepth, DIR, RequestsDelay=0,Obeyrobots=FALSE,
                     Useragent, Encod,Timeout=5, URLlenlimit=255, urlExtfilter,urlregexfilter,
                     ignoreUrlParams, KeywordsFilter,KeywordsAccuracy,FUNPageFilter,
                     ExtractXpathPat, ExtractCSSPat, PatternsNames, ExcludeXpathPat, ExcludeCSSPat,
                     ExtractAsText=TRUE, ManyPerPattern=FALSE, NetworkData=FALSE, NetwExtLinks=FALSE,
                     statslinks=FALSE) {

  if (missing(DIR)) DIR<-getwd()
  if (missing(KeywordsAccuracy)) KeywordsAccuracy<-1
  if (missing(ignoreUrlParams)) ignoreUrlParams<-""
  if (missing(MaxDepth)) MaxDepth<-10
  if (missing(no_cores)) no_cores<-parallel::detectCores()-1
  if (missing(no_conn)) no_conn<-no_cores
  if (missing(Obeyrobots)) Obeyrobots<-FALSE
  if (missing(urlregexfilter)){ urlregexfilter<-".*" }
  else { urlregexfilter<-paste(urlregexfilter,collapse="|")}
  if(missing(Useragent)) {Useragent="Mozilla/5.0 (Windows NT 6.3; WOW64; rv:42.0) Gecko/20100101 Firefox/42.0"}
  if(missing(Encod)) {
    Encod<- Getencoding(Website)
    if (length(Encod)!=0){
      if(Encod=="NULL") Encod="UTF-8" ;
    }
  }
  if (!missing(FUNPageFilter)){
    if (!is.function(FUNPageFilter)) stop("FUNPageFilter parameter must be a function")
  }

  if(!missing(KeywordsFilter)){
    if(!is.vector(KeywordsFilter)){
      stop("KeywordsFilter parameter must be a vector with at least one element !")
    }
  }
  if(!is.numeric(KeywordsAccuracy)){
    stop ("KeywordsAccuracy parameter must be a numeric value between 1 and 100")
  } else {
    if(KeywordsAccuracy<=0 && KeywordsAccuracy>100) {
      stop ("KeywordsAccuracy parameter must be a numeric value between 1 and 100")
    }
  }
  if(!missing(KeywordsFilter) && !missing(FUNPageFilter) ){
    stop("Please supply KeywordsFilter or FUNPageFilter, not both !")
  }

  if(!missing(ExcludeXpathPat) && !missing(ExcludeCSSPat) ){
    stop("Please supply ExcludeXpathPat or ExcludeCSSPat, not both !")
  }
  if ((!missing(ExcludeXpathPat) || !missing(ExcludeCSSPat)) && (missing(ExtractXpathPat) || missing(ExtractCSSPat))){
    stop("ExcludeXpathPat or ExcludeCSSPat should work only if ExtractXpathPat or ExtractCSSPat are used !")
  }
  if(!missing(ExtractXpathPat) && !missing(ExtractCSSPat) ){
    stop("Please supply ExtractXpathPat or ExtractCSSPat, not both !")
  }
  if(!missing(ExtractCSSPat)) {
    if(is.vector(ExtractCSSPat)){
      ExtractXpathPat<- unlist(lapply(ExtractCSSPat, FUN = function(x) { tryCatch(selectr::css_to_xpath(x, prefix = "//") ,error=function(e) stop("Unable to translate supplied css selector, Please check ExtractCSSPat syntax !"))}))
    } else {
      stop("ExtractCSSPat parameter must be a vector with at least one element !")
    }
  }
  if(!missing(ExcludeCSSPat)) {
    if(is.vector(ExcludeCSSPat)){
      ExcludeXpathPat<- unlist(lapply(ExcludeCSSPat, FUN = function(x) { tryCatch(selectr::css_to_xpath(x, prefix = "//") ,error=function(e) stop("Unable to translate supplied css selector, Please check ExcludeCSSPat syntax !"))}))
    }
  }

  if(missing(urlExtfilter)){ urlExtfilter<-c("flv","mov","swf","txt","xml","js","css","zip","gz","rar","7z","tgz","tar","z","gzip","bzip","tar","mp3","mp4","aac","wav","au","wmv","avi","mpg","mpeg","pdf","doc","docx","xls","xlsx","ppt","pptx","jpg","jpeg","png","gif","psd","ico","bmp","odt","ods","odp","odb","odg","odf") }
  keywordCheck<-FALSE
  if(missing(KeywordsFilter) ){
    keywordCheck<-FALSE
  } else {
    if(is.vector(KeywordsFilter)) {
      keywordCheck<-TRUE
    }
  }

  domain<-strsplit(gsub("http://|https://|www\\.", "", Website), "/")[[c(1, 1)]]
  if (Obeyrobots) {
    rules<-RobotParser(Website,Useragent)
    urlbotfiler<-rules[[2]]
    urlbotfiler<-gsub("^\\/", paste("http://www.",domain,"/", sep = ""), urlbotfiler , perl=TRUE)
    urlbotfiler<-gsub("\\*", ".*", urlbotfiler , perl=TRUE)
  } else {urlbotfiler=" "}


  IndexErrPages<-c(200)

  #create repository
  tryCatch(curdate<-format(Sys.time(), "%d%H%M"),error=function(e) curdate<-sample(1000:9999, 1) )
  foldename<-paste(domain,"-",curdate,sep = "")
  path<-paste(DIR,"/", foldename ,sep = "")
  dir.create(path, recursive = TRUE, mode = "0777")
  #if(Backup) {
  #   Fileindex <- file(paste(path,"/","index.csv", sep = ""), "w")
  #  Filefrontier <- file(paste(path,"/","frontier.csv", sep = ""), "w")
  #  Filestat<-file(paste(path,"/","state.txt", sep = ""), "w")
  #  }

  if(!missing(ExtractXpathPat)) {
    Filecontent <- file(paste(path,"/","extracted_contents.csv", sep = ""), "w")
  }
  duplicatedetect<-FALSE
  #create Dataframe
  id<-vector()
  urls<-vector()
  links<-vector()
  status<-vector()
  level<-vector()
  inn<-numeric()
  out<-numeric()
  httpstat<-vector()
  contenttype<-vector()
  encoding<-vector()
  hashcode<-vector()
  Accuracy<-vector()
  allpaquet<-list()
  pkg.env <- new.env()
  if (!missing(ExtractXpathPat)) { pkg.env$Exdata<-list() }
  pkg.env$shema<-data.frame(id,urls,status,level,out,inn,httpstat,contenttype,encoding,Accuracy)
  names(pkg.env$shema) <- c("Id","Url","Stats","Level","OUT","IN","Http Resp","Content Type","Encoding","Accuracy")
  if(NetworkData){
    FromNode<-vector()
    ToNode<-vector()
    Weight<-vector()
    Type<-vector()
    pkg.env$GraphINDEX<-vector()
    pkg.env$GraphINDEX<-c(pkg.env$GraphINDEX,Website)
    pkg.env$GraphEgdes<-data.frame(FromNode,ToNode,Weight,Type)
    names(pkg.env$GraphEgdes) <- c("From","To","Weight","Type")
  }
  #timev<<- vector()
  #timef<<-vector()
  shemav <- vector()
  shemav<-c(shemav,Website)
  Lshemav<-list(shemav)
  M=Listlength(Lshemav)
  lev<-0
  t<-1
  posx<-0
  i<-0
  posenv <- 1
  chunksize<-10000
  envi = as.environment(posenv)
  #cluster initialisation
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  clusterEvalQ(cl, library(xml2))
  clusterEvalQ(cl, library(httr))
  clusterEvalQ(cl, library(data.table))
  clusterExport(cl, c("LinkExtractor","LinkNormalization"))
  clusterExport(cl, c("shema"), envir = pkg.env)
  #tmparallelreq<<-vector()
  #tmparallel<<-vector()
  #tminsertion<<-vector()
  #tminsertionreq<<-vector()
  while (t<=Listlength(Lshemav) && MaxDepth>=lev){
    # extraire les liens sur la page
    rest<-Listlength(Lshemav)-t
    #if(rest==0){ rest<-rest+1 }
    if (no_conn<=rest){
      l<-t+no_conn-1
    } else {
      l<-t+rest
    }
    #cat(t,"to",l,"size:",length(shemav))
    #get links & pageinfo
    if (RequestsDelay!=0) {
      Sys.sleep(RequestsDelay)
    }
    #ptm <- proc.time()
    if(t<chunksize){
      tt<-t
      VposT<-1
    } else {
      VposT<-(t%/%chunksize)+1
      tt<-t%%(chunksize*(t%/%chunksize))+1
    }

    if(l<chunksize){
      ll<-l
      VposL<-1
    }else{
      VposL<-(l%/%chunksize)+1
      ll<-l%%(chunksize*(l%/%chunksize))+1
    }
    tmpshemav<-vector()
    if(VposT!=VposL){
      for(k in tt:(chunksize-1)) {
          #bcat("k:",k)
          tmpshemav<-c(tmpshemav,Lshemav[[VposT]][[k]])
       }
      for(r in 1:ll){
        tmpshemav<-c(tmpshemav,Lshemav[[VposL]][[r]])
        #cat("r:",r)
      }
      #topshemav<<-tmpshemav
      #cat("VposT :",VposT," tt :",tt, " VposL :",VposL, " ll :",ll," tmpshema:",length(tmpshemav))
      allpaquet <- foreach(i=1:length(tmpshemav),  .verbose=FALSE, .inorder=FALSE, .errorhandling='pass')  %dopar%
      {
        LinkExtractor(url = tmpshemav[[i]],id = i,lev = lev, IndexErrPages = IndexErrPages, Useragent = Useragent, Timeout = Timeout, URLlenlimit = URLlenlimit, urlExtfilter=urlExtfilter, encod = Encod, urlbotfiler = urlbotfiler, removeparams=ignoreUrlParams, ExternalLInks=NetwExtLinks)
      }
    } else {

      #cat("VposT :",VposT," tt :",tt, " VposL :",VposL, " ll :",ll)
      j<-0
      allpaquet <- foreach(j=tt:ll,  .verbose=FALSE, .inorder=FALSE, .errorhandling='pass')  %dopar%
      {
        LinkExtractor(url = Lshemav[[VposT]][j],id = i,lev = lev, IndexErrPages = IndexErrPages, Useragent = Useragent, Timeout = Timeout, URLlenlimit = URLlenlimit, urlExtfilter=urlExtfilter, encod = Encod, urlbotfiler = urlbotfiler, removeparams=ignoreUrlParams, ExternalLInks=NetwExtLinks)
      }
    }


    #deb<<-allpaquet
    #for (j in t:l){
    #    cat(shemav[i]);
    #}
    #tmparallelreq<<-c(tmparallelreq,(proc.time() - ptm )[3])
    #tmparallel<<-c(tmparallel,format(Sys.time(), "%M,%S"))
    cat("In process : ")
    #if (no_conn<=rest){
    #  f<-no_conn
    #} else if (no_conn>rest) {
    #  f<-rest
    #}
    #if(0==rest) {
    #  f<-l-t+1
    #}
    #cat("f:",f,"t:",t,"conn",no_conn,"r:",rest)
    #if(f==0){f<-f+1}
    # combine all links.package in one list & insert pageinfo in shema one-by-one
    for (s in 1:length(allpaquet)){
      # pos :  Global positon (regarding to shemav)
      pos<-s+t-1
      cat(pos,"..", sep = "")
      flush.console()
      # timev[pos]<<-Sys.time()
      # timef[pos]<<-format(Sys.time(), "%M,%S")
      # Les page null ne sont pas ajouter au shema
      #debugg<<-allpaquet
      #debugg2<<-shemav
      if(!is.null(allpaquet[[s]][2]) && !("call" %in% names(allpaquet[[s]][2]))) {
        if (NetworkData) {
          tmplinks<-vector()
          tmplinks<-c(tmplinks,unlist(allpaquet[[s]][2]))
          #tmplinks<-c(tmplinks,debugg[[s]][[1]][[2]])
          if(length(tmplinks) > 0){
            pkg.env$GraphINDEX<-c( pkg.env$GraphINDEX , tmplinks[ ! tmplinks %chin% pkg.env$GraphINDEX ] )
            for(NodeElm in tmplinks){
              posNodeFrom<-chmatch(c(allpaquet[[s]][[1]][[2]]),pkg.env$GraphINDEX)
              pkg.env$GraphEgdes[nrow(pkg.env$GraphEgdes) + 1,]<-c(posNodeFrom,chmatch(c(NodeElm),pkg.env$GraphINDEX),lev,1)
            }
          }
          if(NetwExtLinks){
            tmplinks2<-vector()
            tmplinks2<-c(tmplinks2,unlist(allpaquet[[s]][3]))
            if(length(tmplinks2) > 0){
              pkg.env$GraphINDEX<-c( pkg.env$GraphINDEX , tmplinks2[ ! tmplinks2 %chin% pkg.env$GraphINDEX ] )
              for(NodeElm in tmplinks2){
                posNodeFrom<-chmatch(c(allpaquet[[s]][[1]][[2]]),pkg.env$GraphINDEX)
                pkg.env$GraphEgdes[nrow(pkg.env$GraphEgdes) + 1,]<-c(posNodeFrom,chmatch(c(NodeElm),pkg.env$GraphINDEX),lev,2)
              }
            }
          }
        }
        if (statslinks){
          tmplinks<-vector()
          tmplinks<-c(tmplinks,unlist(allpaquet[[s]][[2]]))
          if(length(tmplinks) > 0 && length(pkg.env$shema[[2]])>0){
            for(NodeElm in tmplinks){
              index<-chmatch(c(NodeElm),pkg.env$shema[[2]])
              if(!is.na(index)){
                pkg.env$shema[[6]][index]<-as.numeric(pkg.env$shema[[6]][index])+1
              }
            }
          }
        }
        links<-c(links,allpaquet[[s]][2])
        #debugg2<<-allpaquet[[s]][2]
        #amdebugg3<<-allpaquet[[s]][1]
        if (allpaquet[[s]][[1]][[3]]!="NULL" && allpaquet[[s]][[1]][[10]]!="NULL" ){
          #index URL filter
          if (grepl(urlregexfilter,allpaquet[[s]][[1]][[2]])) {

            if(!missing(FUNPageFilter)){
              contentx<-allpaquet[[s]][[1]][[10]]
              Notagcontentx<-RemoveTags(contentx)
              isContentvalide<-FUNPageFilter(allpaquet[[s]][[1]][[2]],contentx)
              if(!is.logical(isContentvalide)) stop ("FUNPageFilter function must return a logical value TRUE/FALSE")
              if (isContentvalide){
                #if(containtag) {
                #check for duplicate webpage & checksum calculation
                # if (duplicatedetect==TRUE){
                # hash<-getsimHash(contentx,128)
                # Ajouter au shema uniqument les liens non-repete
                # if (!(hash %in% pkg.env$shema$hashcode)){
                # posx, actual position of DF shema
                #  posx<-posx+1
                #  pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],allpaquet[[s]][[1]][[9]],hash)
                #  filename<-paste(posx,".html")
                #  filepath<-paste(path,"/",filename, sep = "")
                #  write(allpaquet[[s]][[1]][[10]],filepath) }
                #  } else {
                if (!missing(ExtractXpathPat)) {
                  excontent<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, ManyPerPattern = ManyPerPattern, PatternsName = PatternsNames, encod=Encod)
                  if(isTarget(excontent)){
                    posx<-posx+1
                    pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],allpaquet[[s]][[1]][[9]],paste0(Accuracy,"%"))
                    filename<-paste0(posx,".html")
                    filepath<-paste(path,"/",filename, sep = "")
                    write(allpaquet[[s]][[1]][[10]],filepath)

                    if (missing(ExcludeXpathPat)){
                      excontent<-c(posx,excontent)
                      excontent2<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, PatternsName = PatternsNames, ManyPerPattern=ManyPerPattern, astext = ExtractAsText, encod=Encod)
                      pkg.env$Exdata<-c(pkg.env$Exdata, list(excontent2))
                      write.table(excontent, file = Filecontent, sep = ";", qmethod="double" ,row.names = FALSE, col.names = FALSE, na = "NA" )
                    }
                    else {
                      #x<<-allpaquet[[s]][[1]][[10]]
                      excontent<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, PatternsName = PatternsNames, ManyPerPattern=ManyPerPattern, ExcludeXpathPat = ExcludeXpathPat ,encod=Encod )
                      excontent<-c(posx,excontent)
                      excontent2<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, PatternsName = PatternsNames, astext = ExtractAsText, ManyPerPattern=ManyPerPattern, ExcludeXpathPat = ExcludeXpathPat, encod=Encod)
                      pkg.env$Exdata<-c(pkg.env$Exdata, list(excontent2))
                      write.table(excontent, file = Filecontent, sep = ";", qmethod="double" ,row.names = FALSE, col.names = FALSE, na = "NA" )
                    }
                    assign("DATA", pkg.env$Exdata, envir = envi )
                  }
                  # }
                } else {
                  posx<-posx+1
                  pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],Encod,paste0(Accuracy,"%"))
                  filename<-paste(posx,".html")
                  filepath<-paste(path,"/",filename, sep = "")
                  write(allpaquet[[s]][[1]][[10]],filepath)
                }
              }

            }
            else if(keywordCheck){
              #check if page content contain some specific keywords
              contentx<-allpaquet[[s]][[1]][[10]]
              Notagcontentx<-tolower(gsub("\\W", " ",RemoveTags(contentx), perl=TRUE))

              AccuracyResult <- foreach(i=1:length(KeywordsFilter),  .verbose=FALSE, .inorder=FALSE, .errorhandling='pass', .combine=c)  %dopar%
              {
                Precifunc(KeywordsFilter[i],length(KeywordsFilter),Notagcontentx)
              }
               Accuracy<-sum(AccuracyResult)
               #Accuracy<-sum(sapply(KeywordsFilter,function(x,y,z) Precifunc(x,length(KeywordsFilter),Notagcontentx) ,simplify = TRUE))
              if (Accuracy>=KeywordsAccuracy){
                #if(containtag) {
                #check for duplicate webpage & checksum calculation
                # if (duplicatedetect==TRUE){
                # hash<-getsimHash(contentx,128)
                # Ajouter au shema uniqument les liens non-repete
                # if (!(hash %in% pkg.env$shema$hashcode)){
                # posx, actual position of DF shema
                #  posx<-posx+1
                #  pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],allpaquet[[s]][[1]][[9]],hash)
                #  filename<-paste(posx,".html")
                #  filepath<-paste(path,"/",filename, sep = "")
                #  write(allpaquet[[s]][[1]][[10]],filepath) }
                #  } else {
                if (!missing(ExtractXpathPat)) {
                  excontent<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, ManyPerPattern = ManyPerPattern, PatternsName = PatternsNames, encod=Encod )
                  if(isTarget(excontent)){
                    posx<-posx+1
                    pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],allpaquet[[s]][[1]][[9]],paste0(Accuracy,"%"))
                    filename<-paste(posx,".html")
                    filepath<-paste(path,"/",filename, sep = "")
                    write(allpaquet[[s]][[1]][[10]],filepath)

                    if (missing(ExcludeXpathPat)){
                      excontent<-c(posx,excontent)
                      excontent2<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat,PatternsName = PatternsNames, ManyPerPattern=ManyPerPattern, astext = ExtractAsText, encod=Encod)
                      pkg.env$Exdata<-c(pkg.env$Exdata, list(excontent2))
                      write.table(excontent, file = Filecontent, sep = ";", qmethod="double" ,row.names = FALSE, col.names = FALSE, na = "NA" )
                    }
                    else {
                      #x<<-allpaquet[[s]][[1]][[10]]
                      excontent<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, PatternsName = PatternsNames, ManyPerPattern=ManyPerPattern, ExcludeXpathPat = ExcludeXpathPat, encod=Encod )
                      excontent<-c(posx,excontent)
                      excontent2<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, PatternsName = PatternsNames, astext = ExtractAsText, ManyPerPattern=ManyPerPattern, ExcludeXpathPat = ExcludeXpathPat, encod=Encod)
                      pkg.env$Exdata<-c(pkg.env$Exdata, list(excontent2))
                      write.table(excontent, file = Filecontent, sep = ";", qmethod="double" ,row.names = FALSE, col.names = FALSE, na = "NA" )
                    }
                    assign("DATA", pkg.env$Exdata, envir = envi )
                  }
                  # }
                } else {
                  posx<-posx+1
                  pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],Encod,paste0(format(round(Accuracy, 2),nsmall = 2),"%"))
                  filename<-paste(posx,".html")
                  filepath<-paste(path,"/",filename, sep = "")
                  write(allpaquet[[s]][[1]][[10]],filepath)
                }
              }
            }
            else {
              if (!missing(ExtractXpathPat)) {
                excontent<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, ManyPerPattern = ManyPerPattern, PatternsName = PatternsNames, encod=Encod )
                if(isTarget(excontent)){
                  posx<-posx+1
                  pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],Encod,'')
                  filename<-paste(posx,".html")
                  filepath<-paste(path,"/",filename, sep = "")
                  write(allpaquet[[s]][[1]][[10]],filepath)
                  if (missing(ExcludeXpathPat)){
                    excontent<-c(posx,excontent)
                    excontent2<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, ManyPerPattern = ManyPerPattern, PatternsName = PatternsNames, astext = ExtractAsText, encod=Encod)
                    pkg.env$Exdata<-c(pkg.env$Exdata, list(excontent2))
                    write.table(excontent, file = Filecontent, sep = ";", qmethod="double" ,row.names = FALSE, col.names = FALSE, na = "NA" )
                  }
                  else {
                    #x<<-allpaquet[[s]][[1]][[10]]
                    excontent<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, PatternsName = PatternsNames,ManyPerPattern = ManyPerPattern, ExcludeXpathPat = ExcludeXpathPat, encod=Encod )
                    excontent<-c(posx,excontent)
                    excontent2<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, PatternsName = PatternsNames, ManyPerPattern = ManyPerPattern, astext = ExtractAsText, ExcludeXpathPat = ExcludeXpathPat, encod=Encod)
                    pkg.env$Exdata<-c(pkg.env$Exdata, list(excontent2))
                    write.table(excontent, file = Filecontent, sep = ";", qmethod="double" ,row.names = FALSE, col.names = FALSE, na = "NA" )
                  }
                  assign("DATA", pkg.env$Exdata, envir = envi )
                }
              } else {
                posx<-posx+1
                pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],1,allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],Encod,'')
                filename<-paste(posx,".html")
                filepath<-paste(path,"/",filename, sep = "")
                write(allpaquet[[s]][[1]][[10]],filepath)
              }
            }

          }
        }
      }

      if(pos==M){
        lev<-lev+1;
        getNewM<-TRUE
      }
    }

    cat("\n")
    links <- unlist(links)
    links <- unique(links)
    # ptm <- proc.time()
    # remplir le shema
    if (length(links)>0){
      #for (i in 1:length(links)){
      # Ajouter au shema uniqument les urls non repete
      # if (!(links[i] %in% shemav) ){
      #  shemav<-c(shemav,links[i])
      #shema[length(shemav),]<<-c(length(shemav),links[i],"waiting","","","1","","","")
      #}}
      #shemav<-c( shemav , links[ ! links %chin% shemav ] )

      for (L in links){
        LshemaSize<-length(Lshemav)
        s<-length(Lshemav[[LshemaSize]])+1
        if (s<chunksize){
          dec<-1
          for (vec in Lshemav){
            if( L %chin% vec) dec<-bitwAnd(0,dec)
          }
          if(dec==1) {
            Lshemav[[LshemaSize]][s]<-L
            s<-s+1
          }
        } else {
          LshemaSize<-LshemaSize+1
          s<-1
          dec<-1
          for (vec in Lshemav){
            if( L %chin% vec) dec<-bitwAnd(0,dec)
          }
          if(dec==1){
            Lshemav<-c(Lshemav,c(L))
            s<-s+1
          }
        }
      }
      #Lshemavv<<-Lshemav
      #cat ("shemav:", length(shemav), " Lshema:",Listlength(Lshemav))

      #calculate level
      if(getNewM){
        M=Listlength(Lshemav)
        getNewM<-FALSE
      }
    }
    #tminsertion<<-c(tminsertion,(proc.time() - ptm )[3])
    #tminsertionreq<<-c(tminsertionreq,format(Sys.time(), "%M,%S"))
    cat("Progress:",format(round(((t/Listlength(Lshemav))*100), 2),nsmall = 2),"%  : ",t, " parssed from ",Listlength(Lshemav)," | Collected pages:",length(pkg.env$shema$Id)," | Level:",lev,"\n")
    # t<-l+1
    t<-t+length(allpaquet)
    if(NetworkData){
      assign("NetwEdges", pkg.env$GraphEgdes, envir = envi )
      assign("NetwIndex", pkg.env$GraphINDEX, envir = envi )
    }
    assign("INDEX", pkg.env$shema, envir = envi )
    #tmp<<-shemav
  }
  if(!missing(ExtractXpathPat)) {
    close(Filecontent)
  }

  #save(shema, file="masterma2.rda")
  stopCluster(cl)
  stopImplicitCluster()
  rm(cl)
  #return(pkg.env$shema)
  cat("+ Check INDEX dataframe variable to see crawling details \n")
  cat("+ Collected web pages are stored in Project folder \n" )
  cat("+ Project folder name :", foldename,"\n")
  cat("+ Project folder path :", path,"\n")
  if(!missing(ExtractXpathPat)){
    cat("+ Scraped data are stored in a variable named : DATA \n")
  }
  if(NetworkData){
    cat("+ Network nodes are stored in a variable named : NetwIndex \n")
    cat("+ Network eadges are stored in a variable named : NetwEdges \n")
  }

}
