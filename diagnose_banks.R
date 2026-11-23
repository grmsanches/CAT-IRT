library(arrow)

indir <- file.path(getwd(), "data", "processed")

files <- list.files(
    indir,
    pattern = "^enem_.*item_bank[.]parquet$",
    full.names = TRUE
)

cat("Directory:", indir, "\n")
cat("Banks found:", length(files), "\n")

if (!length(files)) stop("No item banks found.")

for (f in files) {
    x <- as.data.frame(read_parquet(f))
    
    cat("\n============================\n")
    cat("BANK:", basename(f), "\n")
    cat("Rows:", nrow(x), "\n")
    cat("Columns:", paste(names(x), collapse = ", "), "\n")
    
    if (!"item_id" %in% names(x)) {
        cat("No item_id column: validate_bank() generates IDs.\n")
        next
    }
    
    d <- x[
        duplicated(x$item_id) |
            duplicated(x$item_id, fromLast = TRUE),
    ]
    
    cat("Duplicated rows:", nrow(d), "\n")
    cat("Distinct duplicated IDs:", length(unique(d$item_id)), "\n")
    
    if (nrow(d)) {
        cols <- intersect(
            c("item_id", "CO_ITEM", "CO_PROVA", "CO_POSICAO",
              "item_group", "a", "b", "c"),
            names(d)
        )
        
        print(head(d[order(d$item_id), cols, drop = FALSE], 30))
    }
}