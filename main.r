## copy from website
## usage example
#packages <- c('ggplot2', 'afex')
#CheckPackages(packages)
CheckPackages <- function(pkg){
  # check if biocManager install
  pkg.manager <- ('BiocManager' %in% installed.packages()[,'Package'])
  if(!pkg.manager) install.packages('BioManager')
    
  # check if input package installed
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  if(length(new.pkg)) BiocManager::install(new.pkg)
  
  # load all packages
  sapply(pkg, require, character.only=TRUE)
}

## Credit: Taken from:  http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# improved list of objects
## Credit: Taken from:  http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

# improved list of objects
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

