#' @export
saveDB <- function(tables=c("INSTRUMENT","OHLCV"), path=getOption("DBpath"), envir=.GlobalEnv) {
  for(table.name in tables) {
    file=file.path(path, paste(table.name, ".rds", sep=""))
    saveRDS(get(table.name, envir=envir), file)
    message("Saved ", table.name, " successfully to ", path)
  }
}

#' Upsert
#' 
#' @param new a data.table or character string of a function name that retrieves new data
#' @param always.insert logical. If discrepancies in the non-keyed columns found, still do inserts of new rows?
#' @param always.update logical. If discrepancies in the non-keyed columns found, update the values?
#' @export
upsert <- function(x, new="get.object.source", src, 
                   always.insert=FALSE, always.update=FALSE, 
                   compare.by=key(x), ...) {
  
  name.x <- as.character(substitute(x))
  
  if(is.character(new) | is.function(new)) {
    if(identical(new,"get.object.source"))
      new <- paste(paste("get",as.character(substitute(x)), sep=""), ".",src, sep="")
    new <- do.call(new, args=list(...=...))
  }
  if(!identical(key(x), key(new)))
    stop("Cannot upsert. Keys of 'x' and 'new' must be identical.")

  candidate <- setkeyv(rbindlist(list(x, new)),key(x))

  dupl.by.key <- candidate[duplicated(candidate, by=key(candidate))]
  dupl.by.all <- candidate[duplicated(candidate, by=names(candidate))]
  
  if(identical(dupl.by.all, dupl.by.key)) {
    # No discrepancies found. If duplicated rows, these are duplicit on all columns
    # so no insert or update on these needed. Return only unique rows.
    out <- unique(candidate, by=names(candidate))
    toappend <- new[!x]
    message("Inserted following ", NROW(toappend), " rows:")
    print(toappend)
    return(assign( name.x, out, envir=.GlobalEnv))
  } else {
    message("Discrepancies found:")
        
    # duplicates by key but not by all
    dupl.by.all <- unique(dupl.by.all, by=names(candidate))
    discrepancies <- dupl.by.key[!setkeyv(dupl.by.all, key(candidate))]
    
    # dump for analysis
    print(discrepancies)
    assign(paste(name.x, "discrepancies", sep="."),
           discrepancies, 
           envir=.GlobalEnv)
    assign(paste(name.x, "new", sep="."),
           new, 
           envir=.GlobalEnv)
    
    if(always.update) {
      out <- setkeyv(rbindlist(list(x[!discrepancies], discrepancies)), key(discrepancies))
      message(name.x, " was updated.")
    } else {
      out <- x
      message(name.x, " was not updated.")
    }

    if(always.insert) {
      toappend <- new[!x]
      out <- setkeyv(rbindlist(list(out, toappend)), key(x))
      message("Inserted following ", NROW(toappend), " rows:")
      print(toappend)
    }
    
    if(always.update | always.insert)
      return(assign(as.character(substitute(x)), out, envir=.GlobalEnv))
  }
}

#' Read table from disk, append, save, reload.
#' 
#' @export
updateDB <- function(new, path=getOption("DBpath"), table, envir=.GlobalEnv, reload=T) {
  
  file=file.path(path, paste(table, ".rds", sep=""))
  out <- readRDS(file)
  k <- key(out)
  new <- rbindlist(list(out, new))
  setkeyv(new, k)
  saveRDS(new, file, compress=T)
  if(reload)
    assign(table, new, envir=envir)
}

editDB <- function(x) {
  out <- edit(x)
  out <- data.table(out, key=key(x))
  assign(deparse(substitute(x)), out, envir = .GlobalEnv)
  saveDB(deparse(substitute(x)))
}


