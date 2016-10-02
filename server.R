rm(list=ls())
library(shiny); library(png)

imglink <- "http://vignette4.wikia.nocookie.net/powerlisting/images/8/85/Nsmb2_starman_mario.png/revision/latest/scale-to-width-down/602?cb=20130716121859"
#imglink <- "http://www.androidfreeware.net/software_images/landscape-live-wallpapers.thumb.png"
filename <- sub(pattern = "^.+/([!-.0-z]+\\.png).*$", replacement = "\\1", imglink)
if( !file.exists(filename) )
{ download.file(imglink, filename, mode = "wb") }
picture <- png::readPNG(filename)
numCol <- length(picture[ 1, 1, ])

compressPic <- function( picture, n = nrow(picture) ) {
    if( !exists("svds") )
        { svds <- apply(picture, 3, function(x) svd(x)) }

    if( n == 1 )
        { approx <- sapply(svds, function(x) (x$u[,1] * x$d[1]) %*% t(x$v[,1])) }
    else
        { approx <- sapply(svds, function(x) x$u[,1:n] %*% diag(x$d[1:n]) %*% t(x$v[,1:n])) }
    onecorrected <- sapply(approx, function(x) {
        if( x > 1) { x = 1 }
        else if(x < 0) { x = 0 }
        else { x = x }
    })
    return( array(onecorrected,  c(nrow(picture), ncol(picture), length(svds))) )
    #some code from pca analysis in Coursera Exploratory Data Analysis
}


shinyServer(function(input, output) {

    output$origSize <- renderText({
        paste("uncompressed size:", nrow(picture)*ncol(picture)*(numCol), "bytes")
    })
    output$defaultPlot <- renderPlot({
        plot.new()
        rasterImage(picture, 0, 0, 1, 1, interpolate = FALSE)
    })

    output$compressedSize <- renderText({
        paste("compressed size:", input$n*(nrow(picture) + 1 + ncol(picture))*numCol, "bytes")
    })
    output$compressedPlot <- renderPlot({
        picture.compressed <- compressPic( picture, n = input$n )
        plot.new()
        rasterImage(picture.compressed, 0, 0, 1, 1, interpolate = FALSE)
    })
})
