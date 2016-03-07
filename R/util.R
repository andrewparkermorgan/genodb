## util.R

.dbnotfound <- function() {
	stop("Specify a database in which to search ('muga' = MUGA, 'mm' = MegaMuga, 'giga' = GigaMUGA).")
}

.chunk.query <- function(db, sql, batch.size = -1, one.piece = FALSE, ...) {
	
	stopifnot(inherits(db, "SQLiteConnection"))
	
	i = 0
	if (!one.piece) {
		rez <- RSQLite::dbSendQuery(db, sql)
		df <- data.frame()
		while (!RSQLite::dbHasCompleted(rez)) {
			next.rows <- DBI::fetch(rez, n = batch.size)
			df <- rbind(df, next.rows)
			i <- i + nrow(next.rows)
			cat(i, " records ...\n")
		}
		RSQLite::dbClearResult(rez)
		RSQLite::dbDisconnect(db)
	}
	else {
		df <- RSQLite::dbGetQuery(db, sql)
	}
	
	return(df)
	
}

.types.quotes <- function(col, ...) {
	
	col.type <- "int"
	col.quotes <- ""
	
	if (col == "name") {
		col.type <- "varchar"
		col.quotes <- "'"
	}
	
	return( list(type = col.type, quotes = col.quotes) )
	
}