## origin code copy from website, cannot find link now
## modified by Li
## usage example
# 1. Install public packages form BioManager()
# CheckPackages(c('ggplot2', 'limma'))
# 2. Install downloaded packages from local
# Checkpackages(pkg='pd.hgu133plus2.hs.entrezg',
#               f.path='/home/liyi/Mount_disk1500/CustomCDF/pd.hgu133plus2.hs.entrezg_24.0.0.tar.gz')

CheckPackages <- function(pkg, f.path=NULL, message=FALSE){
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
    suppressPackageStartupMessages(library(pkg[i], character.only = TRUE))
    if(message) print(paste('Package: ', pkg[i], ' loaded'))
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

# parsing GO terms with input by GO ID
parseGO <- function(goid, 
                    species=c('Homo sapiens','Mus musculus'), 
                    type=c('protein', 'gene', 'protein_coding_gene', 'miRNA'), keep.code){
  
  CheckPackages(c('stringr', 'RCurl', 'dplyr'))
  species <- 'Homo sapiens'
  type <- 'protein'
  keep.code <- c('EXP','IDA','IPI','IMP','IGI','IEP', # inferred from low-throughput
                 'HTP','HDA','HPI','HMP','HGI','HEP', # inferred from high-throughput 
                 'ISS','ISO', # inferred from structural similairty and Sequence orthology 
                 'TAS', # Traceable author statement 
                 'IC') # Curator
  
  # download 
  url <- str_c('http://golr-aux.geneontology.io/solr/select?defType=edismax&qt=standard&indent=on&wt=csv&rows=100000&start=0&fl=bioentity,bioentity_label,type,taxon_label,annotation_class,annotation_class_label,evidence_type&facet=true&facet.mincount=1&facet.sort=count&json.nl=arrarr&facet.limit=25&hl=true&hl.simple.pre=%3Cem%20class=%22hilite%22%3E&hl.snippets=1000&csv.encapsulator=&csv.separator=%09&csv.header=false&csv.mv.separator=%7C&fq=document_category:%22annotation%22&fq=regulates_closure:%22',goid, '%22&facet.field=aspect&facet.field=taxon_subset_closure_label&facet.field=type&facet.field=evidence_subset_closure_label&facet.field=regulates_closure_label&facet.field=annotation_class_label&facet.field=qualifier&facet.field=annotation_extension_class_closure_label&facet.field=assigned_by&facet.field=panther_family_label&q=*:*')
  f.char <- getURL(url)
  dat0 <- read.csv(textConnection(f.char), sep='\t', header = FALSE) %>% as_tibble
  
  # processing
  colnames(dat0) <- c('ID','Symbol','Type','Source','subTermID','SubTermName','EvidenceCode')
  
  # filter by species, type
  dat1 <- dat0 %>% filter(Source==species & Type==type)
  
  # and labeled by evidence code
  code.sel <- rep(NA,length=nrow(dat1))
  code.sel[dat1$EvidenceCode %in% keep.code] <- 'O'
  code.sel[is.na(code.sel)] <- 'X'
  dat2 <- dat1 %>% mutate(CodeSelected=code.sel)
  
  # extract gene ID 
  dat3 <- dat2 %>% mutate(UniprotID = str_remove(dat2$ID, '[^:]*:'))
  
  # rearrangment with sub-terms
  dat4 <- dat3 %>% dplyr::select(SubTermName, UniprotID, CodeSelected) %>% arrange(SubTermName)
  
  # transform Uniport ID into EntrezID with org.Hs.eg.db
  if(species=='Homo sapiens'){
    CheckPackages('org.Hs.eg.db')
    suppressMessages(
      genes.mapping <- AnnotationDbi::select(org.Hs.eg.db, 
                                             keys=unique(dat4$UniprotID), keytype = 'UNIPROT', 
                                             columns=c('ENTREZID','SYMBOL')))
  } else if(species=='Mus musculus'){
    CheckPackages('org.Mm.eg.db')
    suppressMessages(
      genes.mapping <- AnnotationDbi::select(org.Mm.eg.db, 
                                             keys=unique(dat4$UniprotID), keytype = 'UNIPROT', 
                                             columns=c('ENTREZID','SYMBOL')))
    
  }
  genes.mapping <- genes.mapping %>% na.omit() %>% distinct()
  dat5 <- dat4 %>% inner_join(genes.mapping, by=c('UniprotID' = 'UNIPROT'))
  
  # generage final output
  dat6 <- dat5 %>% dplyr::select(SubTermName, EntrezID=ENTREZID, Symbol=SYMBOL, CodeSelected) %>% distinct()
  
  return(dat6)
}
