main <- function() {

    load_packages()
    opt <- parse_options()
    infile <- opt$infile
    outfile <- opt$outfile

    # Read the infile
    feature_table <- read_delim(opt$infile, "\t", escape_double = FALSE,
                                  col_names = TRUE, trim_ws = TRUE, na = "NA")

    # Index the table, but set indices where there is a zero equal to "NA"
    feature_table$index <- 1:nrow(feature_table)
    feature_table$closest <- feature_table$index
    feature_table[feature_table$value == 0, "closest"] <- NA

    # Replace NAs with the nearest good value, working from either top or bottom
    # Uses 'Last Observation Carried Forward' function from zoo library
    # Then compute the distance of each index from the original, and choose the
    # closest value for each.
    feature_table <- mutate( feature_table, index_filled_desc = na.locf(closest, fromLast=F, na.rm = F),
                                            index_filled_asc = na.locf(closest, fromLast=T, na.rm = F)) %>%
                     mutate( dist_desc = abs(index_filled_desc - index), 
                             dist_asc = abs(index_filled_asc - index)) %>%
                     replace_na(list(dist_desc = nrow(feature_table), 
                                     dist_asc = nrow(feature_table))) %>%
                     mutate( closest = if_else(dist_desc <= dist_asc, 
                                               index_filled_desc, index_filled_asc))

    # Write the feature value column and the closest column to the provided output file
    write.table(feature_table[,c("value","closest")], outfile, sep="\t", col.names=T, row.names=F, quote=F)    

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
    suppressPackageStartupMessages(library("tidyverse"))
    suppressPackageStartupMessages(library("zoo"))
    suppressPackageStartupMessages(library("optparse"))
    return
}


main()
