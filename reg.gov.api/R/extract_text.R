#' Extract text from a PDF or DOCX to a TXT file
#'
#' @param source Source file
#' @param target Target TXT file; if NULL does not write output
#' @param force  Extract text even if target file already exists
#' @param ... Other arguments, passed down to `extract_text_pdf()` or `extract_text_docx()`
#' @return Extracted text (invisible)
#' @export
extract_text = function(source,
                        target = NULL,
                        force = FALSE,
                        verbose = FALSE,
                        ...) {
    if (verbose) {
        message(source)
    }
    type = tools::file_ext(source)
    valid_types = c('pdf', 'docx')
    assertthat::assert_that(type %in% valid_types,
                            msg = 'Source document must be PDF or DOCX')

    if (is.null(target) || !file.exists(target) || force) {
        if (type == 'pdf') {
            # message('pdf branch')
            text = extract_text_pdf(source, ...)
        } else if (type == 'docx') {
            # message('docx branch')
            text = extract_text_docx(source, ...)
        } else {
            stop('Unhandled document type in extract_text()')
        }

        if (!is.null(target)) {
            readr::write_lines(text, target)
        }
    } else {
        if (verbose) {
            message(target, ' exists')
        }
        text = readr::read_lines(target)
    }
    return(invisible(text))
}

#' Extract text from a PDF file
#'
#' @param source Source file to extract test from
#' @param ocr_threshold If text length in characters is less than this, run it through OCR
#' @return Character vector containing the extracted text
extract_text_pdf = function(source,
                            ocr_threshold = 50) {
    text = pdftools::pdf_text(source) %>%
        stringr::str_flatten(collapse = '\n')

    check_text = text %>%
        stringr::str_flatten() %>%
        stringr::str_squish()

    ## If the text is too short, run it through OCR
    if (stringr::str_length(check_text) < ocr_threshold) {
        image_files = source %>%
            stringr::str_remove('.pdf') %>%
            stringr::str_c('-', 1:pdftools::pdf_info(source)$pages) %>%
            stringr::str_c('.png')
        pdftools::pdf_convert(source,
                              format = 'png',
                              dpi = 150,
                              filenames = image_files)
        text = tesseract::ocr(image_files)
    }
    return(text)
}

#' Extract text from a Word document
#'
#' @param source DOCX file from which to extract text
#' @param ... Used for compatibility with passing options to `extract_text_pdf()`
#' @return Character vector of extracted text
extract_text_docx = function(source, ...) {
    text = source %>%
        officer::read_docx() %>%
        officer::docx_summary() %>%
        dplyr::pull(text) %>%
        stringr::str_flatten(collapse = '\n')
    return(text)
}

# extract_text('../data/01_attachments/EPA-HQ-OA-2018-0259-0019-1.docx',
#              '../data/02_attachment_text/test.txt',
#              'docx')

# extract_text('../data/01_attachments/EPA-HQ-OA-2018-0259-1944-1.pdf',
#              '../data/temp.txt',
#              'pdf',
#              force = TRUE)
