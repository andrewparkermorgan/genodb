## intensities.R

#' Retrieve genotype calls and hybridization intensities
#' 
#' @param ids vector of sample IDs (names or internal database IDs)
#' @param markers character vector of marker names for which calls and intensities will be returned
#' @param chr limit to markers on these chromosome(s)
#' @param start limit to markers with genomic position >= \code{start} (requires \code{chr})
#' @param end limit to markers with genomic position <= \code{end} (requires \code{chr})
#' @param raw logical; if \code{TRUE}, return raw rather than Illumina-normalized intensities
#' @param by search for samples by unique numeric ID (\code{"id"}) or by human-readable sample name (\code{"name"})
#' @param db which database to search: must be one of \code{"mm"} (MegaMUGA), \code{"muga"}, (MUGA) or \code{"giga"} (GigaMUGA)
#' @param verbose logical; if \code{TRUE}, show progress meter and diagnostic messages to console
#' 
#' @return a \code{data.table} with one row per sample-marker combination
#' 
#' @details Most of our databases are indexed by sample, chromosome, and position but not by marker name: retrieving data
#' 	for specific markers (as opposed to genomic regions) may be very slow.  It is *always* advisable to use numeric sample
#' 	IDs (guaranteed unique within a database) rather than human-readable names (which may be duplicated).
#' 
#' @export fetch.intensities
fetch.intensities <- function(ids, markers = NULL, chr = NULL, start = NULL, end = NULL, raw = FALSE,
							  by = c("id","name"), db = c("mm","muga","mda","giga"), verbose = FALSE) {
	
	by <- match.arg(by)
	db <- match.arg(db)
	if (db == "mm") {
		pos.col <- "position"
		db <- getOption("genodb.MMDBPATH")
	}
	else if (db == "muga") {
		pos.col <- "position"
		db <- getOption("genodb.MUGADBPATH")
	}
	else if (db == "giga") {
		pos.col <- "position38"
		db <- getOption("genodb.GIGADBPATH")
	}
	else
		.dbnotfound()
	
	.fetch.intensities.python(ids = ids, by = by, markers = markers, chr = chr, start = start, end = end,
							  pos.col = pos.col, db = db, raw = raw, verbose = verbose)
	
}

## use Python script to actually run the queries
.fetch.intensities.python <- function(ids, markers = NULL, chr = NULL, start = NULL, end = NULL, pos.col = "position38", raw = FALSE,
									  by = c("id","name"), operation = "intensities", db = NULL, mda = FALSE, verbose = TRUE, ... ) {
	
	if (is.null(db) || !file.exists(db))
		stop("Must supply a non-NULL database path.")
	
	python <- getOption("genodb.pythonexe")
	script <- file.path(system.file(package = "genodb"), "fetch.py")
	
	ff = tempfile()
	ff.id = tempfile()
	write.table(ids, ff.id, row.names = FALSE, col.names = FALSE, quote = FALSE)
	mm.id = tempfile()
	write.table(markers, mm.id, row.names = FALSE, col.names = FALSE, quote = FALSE)
	
	cmd <- paste0(paste(python, script), " -o ", operation," --db ", db, " --samples ", ff.id, " --by ", by[1], " --out ", ff)
	if (mda) {
		cmd <- paste0(cmd, " --mda ")
	}
	if (!is.null(markers))
		cmd <- paste0(cmd, " --markers ", mm.id)
	if (!is.null(chr))
		cmd <- paste0(cmd, " --chr ", paste(chr, collapse = " "))
	if (!is.null(start))
		cmd <- paste0(cmd, " --from-bp ", formatC(start, format = "d"))
	if (!is.null(end))
		cmd <- paste0(cmd, " --to-bp ", formatC(end, format = "d"))
	cmd <- paste0(cmd, " --pos ", pos.col)
	if (raw)
		cmd <- paste(cmd, "--raw")
	
	if (verbose)
		cat(cmd)
	system(cmd, intern = !verbose)
	rez <- data.table::fread(ff)
	if (nrow(rez)) {
		message("Deleting temporary file...")
		file.remove(ff)
	}
	return(rez)
	
}

#' Create an \code{argyle::genotypes} object via a database query
#'
#' @param ids vector of sample IDs (names or internal database IDs)
#' @param snps a code{dataframe} containing marker map in \code{argyle} format: columns chromosome, marker name, cM position, physical position, allele 1, allele 2
#' @param by search for samples by unique numeric ID (\code{"id"}) or by human-readable sample name (\code{"name})
#' @param keep.intensity logical; if \code{TRUE}, return both genotype calls and hybridization intensities
#' @param make.names.markers logical; if \code{TRUE}, sanitize marker names of some non-alphanumeric characters with \code{make.names()}
#' @param ... other options passed to \code{fetch.intensities()}
#'
#' @return a \code{argyle::genotypes} object
#'
#' @export make.genotypes
make.genotypes <- function(ids, snps = NULL, by = c("name","id"), keep.intensity = TRUE, rename = TRUE, make.names.markers = FALSE, ...) {
	
	if (is.null(snps))
		stop("Must provide a marker map as 'snps'.")
	by <- match.arg(by)
	
	ss <- fetch.samples(ids = ids, by = by, ...)
	rownames(ss) <- as.character(ss$id)
	ss$iid <- make.unique(ss$name)
	
	intens <- fetch.intensities(ids = ss$id, by = "id", ...)
	if (make.names.markers) {
		message("Forcing marker names to be R-acceptable...")
		data.table::set(intens, i = NULL, "marker", make.names(intens$marker))
	}
	
	calls <- argyle:::.raw.to.matrix(intens, snps, sample.id.col = "sid", value.col = "call")
	if (keep.intensity) {
		x <- argyle:::.raw.to.matrix(intens, snps, sample.id.col = "sid", value.col = "x")
		y <- argyle:::.raw.to.matrix(intens, snps, sample.id.col = "sid", value.col = "y")
	}
	else {
		intens <- NULL
	}
	
	i <- colnames(calls)
	if (rename)
		nn <- ss[ i,"iid" ]
	else
		nn <- i
	if (keep.intensity) {
		colnames(calls) <- colnames(x) <- colnames(y) <- nn
		intens <- list(x = x, y = y)
	}
	else
		colnames(calls) <- nn
	fam <- argyle:::make.fam(nn, sex = ss[ i,"sex" ], fid = i)
	
	g <- argyle:::genotypes(calls, snps, fam,
							intensity = intens,
							alleles = "native")
	return(g)
	
}