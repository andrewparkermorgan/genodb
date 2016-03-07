# genodb

An `R` package for interacting with UNC Systems Genetics genotype databases for the MUGA family of arrays.

## Dependencies
For `R`:

* `DBI`, `RSQLite` -- generic databse machinery
* `data.table` -- efficient representation of in-memory tables

For `python`:

* `python` (>= 2.7)
* `sqlite3`


## Usage

```{r}
library(genodb)

options(genodb.MMDBPATH = "/path/to/my/database")

load("~/db/arrays/megamuga/snps.megamuga.Rdata")
ss <- fetch.samples("DBA/2%", exact = FALSE)
geno <- make.genotypes(ss$id, snps, by = "id", chr = "M")

summary(geno)
```