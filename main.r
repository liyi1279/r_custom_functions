## origin code copy from website, cannot find link now
## modified by Li
## usage example
# 1. Install public packages form BioManager()
# CheckPackages(c('ggplot2', 'limma'))
# 2. Install downloaded packages from local
# Checkpackages(pkg='pd.hgu133plus2.hs.entrezg',
#               f.path='/home/liyi/Mount_disk1500/CustomCDF/pd.hgu133plus2.hs.entrezg_24.0.0.tar.gz')

CheckPackages <- function(pkg, f.path=NULL){
  # check if biocManager install
  pkg.manager <- ('BiocManager' %in% installed.packages()[,'Package'])
  if(!pkg.manager) install.packages('BioManager')
    
  # check if input package installed
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  
  # install input packages if not installed
  if(length(new.pkg)==1 & !is.null(f.path)){
    install.packages(f.path, repos=NULL, type='source') 
  } else {
    if(length(new.pkg)) BiocManager::install(new.pkg)
  }
  
  # load all packages
  for(i in 1:length(pkg)){
    print(paste('Loading package:', pkg[i]))
    suppressPackageStartupMessages(
      library(pkg[i], character.only = TRUE))
    print('Done')
  }
}


## improved list of objects
## Credit: Taken from:  http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# improved list of objects
## Credit: Taken from:  http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size(Mb)", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out[,2] <- round(out[,2]/1024/1024,2)
  names(out) <- c("Type", "Size(Mb)", "Rows", "Columns")
  out
}
# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size(Mb)", decreasing=TRUE, head=TRUE, n=n)
}


# flatten nested list into one-level list
# code from https://stackoverflow.com/questions/16300344/how-to-flatten-a-list-of-lists
flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}

# Define color with RColorBrewer
transToColors <- function(x){
  CheckPackages('RColorBrewer')
  col.group <- factor(x)
  col.ngroup <- ifelse(nlevels(col.group)>=3,nlevels(col.group),3)
  levels(col.group) <- colorRampPalette(brewer.pal(col.ngroup,'Set1'))(col.ngroup)
  oput <- as.character(col.group)
  names(oput) <- as.character(x)
  return(oput)
}
