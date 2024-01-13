library(glue)

build_url = function(comment_id) {
    glue('https://www.regulations.gov/comment/{comment_id}')
}
