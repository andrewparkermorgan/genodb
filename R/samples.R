## samples.R

#' Retrieve a list of samples by name or ID
#'
#' @param ids a vector of identifiers to search for; "%" is wildcard character
#' @param by search by unique numeric ID (\code{"id"}) or by human-readable sample name (\code{"name"})
#' @param strict.case logical; if \code{TRUE}, perform case-sensitive search
#' @param bad.flags characters in \code{flags} column of database which indicaes a failed sample
#' @param group limit to samples from a defined group
#' @param verbose show progress meter on console
#' @param db which database to search: must be one of \code{"mm"} (MegaMUGA), \code{"muga"}, (MUGA) or \code{"giga"} (GigaMUGA)
#' @param ... ignored
#' 
#' @return a \code{data.frame} with sample names, IDs and other information
#'
#' @export fetch.samples
fetch.samples <- function(ids = NULL, flags = NULL, bad.flags = NULL, group = NULL, by = c("name","id"),
						  exact = TRUE, strict.case = FALSE, verbose = TRUE, db = c("mm","muga","giga"), ...) {
	
	db <- match.arg(db)
	if (db == "mm")
		.fetch.samples.mm(ids = ids, flags = flags, bad.flags = "u", group = group,
						  by = by, exact = exact, strict.case = strict.case, verbose = verbose,
						  db = getOption("genodb.MMDBPATH"))
	else if (db == "muga")
		.fetch.samples.mm(ids = ids, flags = flags, bad.flags = "u", group = group,
						  by = by, exact = exact, strict.case = strict.case, verbose = verbose,
						  db = getOption("genodb.MUGADBPATH"))
	else if (db == "giga")
		.fetch.samples.mm(ids = ids, flags = flags, bad.flags = "u", group = group,
						  by = by, exact = exact, strict.case = strict.case, verbose = verbose, 
						  db = getOption("genodb.GIGADBPATH"))
	else
		.dbnotfound()
	
}

## MegaMUGA-style-specific sample retrieval
.fetch.samples.mm <- function(ids = NULL, flags = NULL, bad.flags = "", group = NULL, db = NULL, by = c("name","id"),
							  exact = TRUE, strict.case = TRUE, verbose = TRUE, ...) {
	
	stopifnot( !all(!is.null(ids), !is.null(group)) )
	
	by <- match.arg(by)
	db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db)
	
	cols <- paste("s", c("id", "name", "well", "batch", "sex", "flags", "timestamp"), sep = ".", collapse = ", ")
	sql <- paste("SELECT", cols)
	if (!is.null(ids)) {
		if (exact) {
			.ids <- na.omit(ids)
			.insert.samples(.ids, db, by = match.arg(by))
			sql <- paste0(sql, " FROM samples s ",
						  "INNER JOIN _mysamples as sg ON s.", by, " = sg.", by)
		}
		else {
			sql <- paste0(sql, " FROM SAMPLES s ",
						  "WHERE s.name LIKE '", ids[1], "'")
		}
	}
	else if (!is.null(group)) {
		sql <- paste0(sql, ", g.name as gname FROM samples s ",
					  "INNER JOIN samples_groups sg ON sg.sid = s.id ",
					  "INNER JOIN groups g ON sg.gid = g.id ",
					  "WHERE g.name LIKE '", group[1], "'")
	}
	else {
		sql <- paste0(sql, " FROM SAMPLES s")
	}
	if (!strict.case)
		sql <- paste0(sql, " COLLATE NOCASE")
	sql <- paste0(sql, ";")
	
	if (verbose)
		cat(sql, "\n")
	
	rez <- .chunk.query(db, sql, -1)
	rez$bad <- grepl(bad.flags[1], rez$flags)
	if (!is.null(flags)) {
		.flags <- flags
		rez <- subset(rez, grepl(.flags, flags))
		message(paste0(nrow(rez), " records with requested flags."))
	}
	return(rez)
	
}

.insert.samples <- function(ids, db, temp.table = "_mysamples", by = c("name","id"), ...) {
	
	stopifnot(!is.null(ids))
	stopifnot(inherits(db, "SQLiteConnection"))
	
	.by <- match.arg(by)
	tnq <- .types.quotes(.by)
	
	sql <- paste("CREATE TEMP TABLE", temp.table, "(", .by, tnq$type,");")
	RSQLite::dbGetQuery(db, sql)
	
	for (s in ids) {
		sql <- paste0("INSERT INTO ", temp.table, " (", .by, ") VALUES (", tnq$quotes, s, tnq$quotes, ");")
		RSQLite::dbGetQuery(db, sql)
	}
	
}

