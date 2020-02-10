extract_text = function(source, target, type, 
                        force = FALSE) {
    valid_types = c('pdf', 'docx')
    stopifnot(type %in% valid_types)
    
    if (!file.exists(target) | force) {
        if (type == 'pdf') {
            # message('pdf branch')
            text = extract_text_pdf(source)
        } else if (type == 'docx') {
            # message('docx branch')
            text = extract_text_docx(source)
        } else {
            stop('Unhandled document type in extract_text()')
        }
        
        write_lines(text, target)
    } else {
        text = read_lines(target)
    }
    return(invisible(text))
}

extract_text_pdf = function(source, 
                            ocr_threshold = 50) {
    text = pdf_text(source) %>% 
        str_flatten(collapse = '\n')
    
    check_text = text %>% 
        str_flatten() %>% 
        str_squish()
    
    ## If the text is too short, run it through OCR
    if (str_length(check_text) < ocr_threshold) {
        image_files = source %>% 
            str_remove('.pdf') %>% 
            str_c('-', 1:pdf_info(source)$pages) %>% 
            str_c('.png')
        pdf_convert(source, format = 'png',
                          dpi = 150,
                          filenames = image_files)
        text = tesseract::ocr(image_files)
    }
    return(text)
}

extract_text_docx = function(source, target) {
    text = source %>% 
        read_docx() %>% 
        docx_summary() %>% 
        pull(text) %>% 
        str_flatten(collapse = '\n')
    return(text)
}

# extract_text('../data/01_attachments/EPA-HQ-OA-2018-0259-0019-1.docx', 
#              '../data/02_attachment_text/test.txt', 
#              'docx')

# extract_text('../data/01_attachments/EPA-HQ-OA-2018-0259-1944-1.pdf', 
#              '../data/temp.txt', 
#              'pdf',
#              force = TRUE)
