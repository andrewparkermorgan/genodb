## options.R

## set package options: so far, just database paths
## adapted from <http://r-pkgs.had.co.nz/r.html>
.onLoad <- function(libname, pkgname) {
	
	op <- options()
	op.genodb <- list(
		"genodb.MUGADBPATH" = "/db/arrays/muga/geneseek.db",
		"genodb.MMDBPATH" = "/db/arrays/megamuga/megamuga.db",
		"genodb.GIGADBPATH" = "/db/arrays/gigamuga/gigamuga.db",
		"genodb.pythonexe" = "python -u"
	)
	toset <- !(names(op.genodb) %in% names(op))
	if(any(toset)) options(op.genodb[toset])
	
	invisible()
}