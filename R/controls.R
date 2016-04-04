## controls.R

#' Get a curated list of control samples for a specific array
#' 
#' @param ... passed on to array-specific methods
#' @param db which database to search: must be one of \code{"mm"} (MegaMUGA), \code{"muga"}, (MUGA) or \code{"giga"} (GigaMUGA)
#' 
#' @return a \code{data.frame} of control samples
#' 
#' @export fetch.controls
fetch.controls <- function(..., db = c("mm","mda","giga")) {
	
	if (db == "mm")
		.fetch.controls.mm(...)
	else
		stop("Control samples only defined for MegaMUGA so far.")
	
}

.fetch.controls.mm <- function(type = c("all","classical","wild"), f1 = FALSE, ...) {
	
	tt <- match.arg(type)
	filters <- list(classical = c(A = "A/J%",B = "C57BL/6J%", C = "129S1%", D = "NOD/ShiLtJ%",E = "NZO/HILtJ%"),
					wild = c(F = "CAST/EiJ%", G = "PWK/PhJ%", H = "WSB/EiJ%"))
	ff <- character()
	if (tt == "classical") {
		ff <- c(ff, filters[[1]])
	}
	else if (tt == "wild") {
		ff <- c(ff, filters[[2]])
	}
	else {
		ff <- unlist(unname(filters))
	}
	
	if (f1) {
		pairs <- apply( combn(seq_along(ff), 2), 2, function(i) paste0("%", ff[ i[1] ], "x", ff[ i[2] ]) )
		names(pairs) <- apply( combn(seq_along(ff), 2), 2, function(i) paste0(names(ff)[ i[1] ], names(ff[ i[2] ])) )
		print(pairs)
		ff <- c(ff, pairs)
	}
	
	rez <- plyr::ldply(ff, fetch.samples, by = "name", exact = FALSE, db = "mm", ...)
	colnames(rez)[1] <- "strain"
	rez <- transform(rez, type = ifelse(nchar(strain) == 1, "inbred","F1"))
	return( subset(rez, !grepl("[bB]", flags)) )
	
}