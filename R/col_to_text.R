#' Dataframe Column to Text File
#'
#' @param dat Dataframe to process
#' @param text_col Column to save as text files
#' @param dest_dir Directory to save output
#' @description BTRIS allows queries and downloads of free-text notes in CRIS as a CSV file.
#' This function takes a pre-processed dataframe of this data and saves each note as a separate text file.
#' Filenames are formatted as such: 'mrn,document_date,document_name(note_index).txt'
#' @return Text files for
#' @export
#'
col_to_text <- function(dat, text_col = "document_text", dest_dir) {

    dat <- as.data.frame(dat)

    for (i in 1:nrow(dat)) {
        write.table(
            x = dat[i, text_col],

            file =
                paste0(dest_dir, "/",
                       dat$mrn[i], ",",
                       dat$document_date[i], ",",
                       dat$document_name[i], "(", dat$note_num[i],  ").txt"),

            row.names = FALSE, col.names = FALSE)
    }
}
