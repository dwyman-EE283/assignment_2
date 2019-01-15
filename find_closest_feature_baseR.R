main <- function() {

    load_packages()
    opt <- parse_options()
    infile <- opt$infile
    outfile <- opt$outfile

    # Read the infile
    feature_table <- read_delim(opt$infile, "\t", escape_double = FALSE,
                                  col_names = TRUE, trim_ws = TRUE, na = "NA")

    # Index the table
    feature_table$index <- 1:nrow(feature_table)

    # Proceed from the first to last position in the table, and for each position, 
    # record the index at which a '1' was last seen, and NA if it has never been seen yet. 
    feature_table$last_seen_desc <- record_last_feature_index(feature_table)   

    # Now, perform the same operation, but proceed from the last to first position in the table
    reversed_table <- feature_table[seq(dim(feature_table)[1],1),]
    feature_table$last_seen_asc <- rev(record_last_feature_index(reversed_table))
    
    # For both the ascending and descending index columns, compute the distance (absolute value)
    # between the current index and the feature index
    feature_table$dist_desc <- abs(feature_table$index - feature_table$last_seen_desc)
    feature_table$dist_asc <- abs(feature_table$index - feature_table$last_seen_asc)

    # Replace NAs with something larger than every other value in the dataframe
    larger_than_max <- max(unlist(feature_table), na.rm=TRUE) + 1
    feature_table[is.na(feature_table)] <- larger_than_max

    # Now, use the minimum distance of asc and desc to choose the closest index
    feature_table$closest <- ifelse(feature_table$dist_desc <= feature_table$dist_asc, 
                                                         feature_table$last_seen_desc, 
                                                         feature_table$last_seen_asc)

    # Write the feature value column and the closest column to the provided output file
    write.table(feature_table[,c("value","closest")], outfile, sep="\t", col.names=T, row.names=F, quote=F)
}

record_last_feature_index <- function(feature_table) {
    # Iterate over each position and record the index at which a '1' was last seen.
    # If a '1' has not been seen yet, record 'NA'.

    last_seen <- as.integer(rep(NA, nrow(feature_table)))
    last_index <- NA
    for( i in 1:nrow(feature_table)) {
        # If current row has a 1, the feature is present there, so record current index
        if (feature_table[ i, "value"] == 1) {
            curr_index <- as.integer(feature_table[ i, "index"])
            last_seen[i] <- curr_index
            last_index <- curr_index
        } else {
            last_seen[i] <- last_index
        }
    }
    return(last_seen)
}

parse_options <- function() {

    option_list <- list(
        make_option(c("--f"), action = "store", dest = "infile",
                    default = NULL, help = "Input file consisting of a single column of zeros and ones representing the presence or absence of a feature at that genomic position."),
        make_option(c("--o"), action = "store", dest = "outfile",
                    default = NULL, help = "File to print output")
        )

    opt <- parse_args(OptionParser(option_list=option_list))
    return(opt)
}

load_packages <- function() {
    suppressPackageStartupMessages(library("readr"))
    suppressPackageStartupMessages(library("optparse"))
    return
}


main()
