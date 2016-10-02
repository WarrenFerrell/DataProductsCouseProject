
#imglink <- "https://www.clipartsgram.com/image/1791209974-fall-leaves-clip-art-free215177.png"

#imglink <- "https://upload.wikimedia.org/wikipedia/commons/4/47/PNG_transparency_demonstration_1.png"
#imglink <- "http://vignette4.wikia.nocookie.net/powerlisting/images/8/85/Nsmb2_starman_mario.png/revision/latest/scale-to-width-down/602?cb=20130716121859"
#imglink <- "http://pngimg.com/upload/tiger_PNG546.png"
imglink <- "http://blog.worldweatheronline.com/wp-content/uploads/2013/12/skiing-cartwheel.png-small.png"
#imglink <- "https://d2ujflorbtfzji.cloudfront.net/key-image/20d28d6d-456c-4b8f-95c4-aedaabab839d.png"
#imglink <- "http://www.emoticonswallpapers.com/avatar/penguins/penguin%20pingouin%20pinguino%20689.png"
#imglink <- "http://www.avatarist.com/avatars/Logos/Apple-Mac/Crossovers/Super-Mario-Apple.png"

#imglink <- "http://www.androidfreeware.net/software_images/landscape-live-wallpapers.thumb.png"

filename <- sub(pattern = "^.+/([!-.0-z]+\\.png).*$", replacement = "\\1", imglink)


library(shiny); library(png)
download <- function(link){
    download.file(link, paste0("www/",filename), mode = "wb")
    picture <<- png::readPNG(paste0("www/",filename))
}

compressPic <- function( picture, n = nrow(picture) ) {
    if( !exists("svds") )
        { svds <<- apply(picture, 3, function(x) svd(x)) }

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
    output$defaultPlot <- renderPlot({
        if( !file.exists(paste0("www/",filename)) )
            download( imglink )
        picture <<- png::readPNG(paste0("www/",filename))
        plot.new()
        rasterImage(picture, 0, 0, 1, 1, interpolate = FALSE)
    })

    output$compressedPlot <- renderPlot({
        file.compressed <- paste0("n", input$n, filename)
        if( file.exists(paste0("www/", file.compressed)) ){
            picture.compressed <- png::readPNG(paste0("www/", file.compressed))
            plot.new()
            rasterImage(picture.compressed, 0, 0, 1, 1, interpolate = FALSE)
        }
        else{
            picture.compressed <- compressPic( picture, n = input$n )
            plot.new()
            rasterImage(picture.compressed, 0, 0, 1, 1, interpolate = FALSE)
            png::writePNG( picture.compressed, paste0("www/", file.compressed) )
        }
  })
  # output$compressedPlot <- renderUI({
  #     file.compressed <- paste0("n", input$n, filename)
  #     if( file.exists(paste0("www/", file.compressed)) ){
  #         img(src = file.compressed, align = "right")
  #     }
  #     else{
  #         picture.compressed <- compressPic( picture, n = input$n )
  #         writePNG( picture.compressed, paste0("www/", file.compressed) )
  #         img(src = file.compressed, align = "right")
  #     }
  # })
})
