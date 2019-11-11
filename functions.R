require(data.table)
require(ggplot2)
require(viridis)
library(scales)


## Rprofile 

## Don't show those silly significanct stars
options(show.signif.stars=FALSE)

## Do you want to automatically convert strings to factor variables in a data.frame?
## WARNING!!! This makes your code less portable/reproducible.
options(stringsAsFactors=FALSE)

## Width of the interpreter region 
options(width=90)


################################################################################
## Create a new invisible environment for all the functions
.env <- new.env()

## Returns a logical vector TRUE for elements of X not in Y
.env$"%nin%" <- function(x, y) !(x %in% y)

## Returns names(df) in single column, numbered matrix format.
.env$n <- function(df) matrix(names(df))

## Single character shortcuts for summary() and head().
.env$s <- base::summary
.env$h <- utils::head

## ht==headtail, i.e., show the first and last 10 items of an object
.env$ht <- function(d) rbind(head(d,10),tail(d,10))

## Show the first 5 rows and first 5 columns of a data frame or matrix
.env$hh <- function(d) if(class(d)=="matrix"|class(d)=="data.frame") d[1:5,1:5]

## Add transparency to a R color 
.env$add.alpha <- function(col, alpha=1){
    if(missing(col))
        stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2,
          function(x)
              rgb(x[1], x[2], x[3], alpha=alpha))
} 

## Strip row names from a data frame (stolen from plyr)
.env$unrowname <- function(x)
    {
        rownames(x) <- NULL
        x
    }

## List objects and classes 
.env$lsa <- function (pos = 1, pattern, order.by="Size")
    {
        napply <- function(names, fn) sapply(names, function(x)
            fn(get(x, pos = pos)))
        names <- ls(pos = pos, pattern = pattern)
        obj.class <- napply(names, function(x) as.character(class(x))[1])
        obj.mode <- napply(names, mode)
        obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
        obj.prettysize <- napply(names, function(x) {
            capture.output(print(object.size(x), units = "auto")) })
        obj.size <- napply(names, object.size)
        obj.dim <- t(napply(names, function(x)
            as.numeric(dim(x))[1:2]))
        vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
        obj.dim[vec, 1] <- napply(names, length)[vec]
        out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
        names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
        out <- out[order(out[[order.by]], decreasing=TRUE), ]
        return(out)    
    }


## List all functions in a package (also from @_inundata)
.env$lsp <-function(package, all.names = FALSE, pattern) {
    package <- deparse(substitute(package))
    ls(
        pos = paste("package", package, sep = ":"),
        all.names = all.names,
        pattern = pattern
    )
}

.env$q <- function (save="no", ...) {
    quit(save=save, ...)
}

## Attach all the variables above
attach(.env)
sshhh <- function(a.package){
    suppressWarnings(suppressPackageStartupMessages(
    library(a.package, character.only=TRUE)))
}

auto.loads <-c("data.table", "ggplot2", "extrafont")

if(interactive()){
    invisible(sapply(auto.loads, sshhh))
    scale_colour_discrete <- function(...) scale_colour_brewer(..., palette="Set2")
    scale_fill_discrete <- function(...) scale_fill_brewer(... , palette="Set2")
#    theme <- function(...) theme(... , panel_grid = element_blank())
    theme_set(theme_bw())
}

#options(datatable.print.topn = 10)


.First <- function(){
    cat("\nWelcome at ", date(), "\n")
}

.Last <- function(){
    cat("\nGoodbye at ", date(), "\n")
}

# R function to read data from plate reader

"
Description
===========
This function uses the data.table package to read and format the data output
from the two platereaders in the lab. In the platereader 1, data should be
saved as list, not table. In platereader 2, data should be saved in txt. 

Arguments
=========

- f        path to the file containing the data (from the current directory)

- reader   which reader does the data come from. 1 for the old and 2 for the
           new one.
" 

read.spec.data <- function(f, reader)    
    {
        require(data.table)
        options(stringsAsFactors = FALSE)
        if (reader == 1) {
            x <- read.delim(f, row.names=NULL)
            x <- data.table('plate' = x$Plate,
                            'well' = x$Well,
                            'row' = match(substr(x$Well, 2, 2), LETTERS),
                            'col' = ordered(as.numeric(substr(x$Well, 3, 4))),
                            'wl' = x$Wavelength,
                            'sample' = x$Sample,
                            't' = round((x$Meas..Time..s.)/60, 0),
                            'abs' = x$Abs)
            x[,well := gsub(' ', '', well)]  
        }

        if (reader == 2) {

            nread <- 1
            aux <- readLines(f, n=nread)
            
            while (!any(grep('avg.time', aux))) {
                nread <- nread+1
                aux <- readLines(f, n=nread)
            }

            # wavelength
            wl <- grep('Wavelength: ', aux)
            wl <- as.numeric(gsub('\\D+','', aux[wl]))

            # plate name, usually in line 2 
            plate <- gsub('\t', '', aux[2])

            # .. col names 
            cn <- unlist(strsplit(aux[nread], '\t'))[-(1:2)]
            wells <- gsub('.*[(]([^.]+)[)].*', '\\1', cn)

            # .. read data            
            x <- read.delim(f, row.names=NULL, skip=nread, header=FALSE)
            names(x) <- c('read', 't', wells)
            x <- x[,-which(is.na(colnames(x)))]
            x <- x[-which(is.na(x$read)),]
            x <- data.table(x)
            
            # .. melt the data to list format 
            x <- melt(x, id.vars = c('read', 't'),
                      variable.name = "well", value.name = "abs",
                      variable.factor = FALSE)
            x[, t := round(t/60)]
            x[, read :=NULL]
            
            # .. update dataframe
            x[, wl := wl]
            x[, plate := plate]
            x[, row := match(substr(well, 1, 1), LETTERS)]
            x[, col := as.numeric(substr(x$well, 2, 3))]
        }
        return(x)
    }


## .. double MM fit model to data 
double.mm <- function(x, f.j, t)
{
    tryCatch({
        k <- nls(f.j ~ 1 - (1 + Vj*x*t) * exp(-Vj*x*t),
                 start = list(Vj = .1))
        list('Vj.fit' = coef(k), 'se' = summary(k)$parameters[2])
    }, error = function(e) {
        list('Vj.fit' = 0, 'se' = 0)
    })
}

## .. langmuir-like null model of amylase activity as a function of N
langmuir.model <- function(V, N)
     {
         r <- nlsLM(V ~ b + vp*(N/(N + K)),
                    start = list(b=0, vp = 1, K=100), 
                    lower = c(b0=0, vp=0, K=0), 
                    upper = c(b0=0, vp=100, K=1e8))

         r
     }
