#' Create a combination of colours for plotting bivariate maps using {colorspace}
#'
#' @param og_pal1 A string indicated one of the available sequential palettes from
#' {colorspace}, the first value of this palette is used as the lowest in the combination.
#' @param og_pal2 A string indicated one of the available sequential palettes from
#' {colorspace}
#' @param num1 The number of colours to include in {og_pal1}, default is 5.
#' @param num2 The number of colours to include in {og_pal2}, note that this does
#' not need to be the same as num1, but by default is.
#'
#' @importFrom colorspace sequential_hcl hex2RGB hex mixcolor
#' @importFrom grDevices colorRampPalette palette
#'
#' @export




## define function for bivariate colours
make_colours <- function(og_pal1 = "OrRd" ,
                         og_pal2 = "Blues",
                         num1 = 5,
                         num2 = 5) {

   ## get colour palettes
   sequence1 <- colorspace::sequential_hcl(num1, palette = og_pal1)
   sequence2 <- colorspace::sequential_hcl(num2, palette = og_pal2)

   ## convert darkest colours to RGB
   deepest <- hex2RGB(c(sequence1[1], sequence2[1]))

   ## mix two colours to get the top right colour
   mix_deep <- hex(
     mixcolor(0.5,
              deepest[1], deepest[2]
     )
   )

   ## create an empty list for sequences
   output <- list()

   ## fill initial two sequences
   output[[1]] <- sequence1
   output[[num2]] <- sequence2

   ## get sequences for either side of square outside
   ## from darkest sequence1 to new darkest
   palette <- colorRampPalette(colors = c(mix_deep, sequence1[1]))
   output[[num2 + 1]] <- palette(num2)
   ## from darkest sequence2 to new darkest (as many colors as numbers in sequence1)
   palette <- colorRampPalette(colors = c(mix_deep, sequence2[1]))
   output[[num2 + 2]] <- palette(num1)

   ## get the inbetweens going upwards between sequence 2 and newly created top outter line
   for (i in 2:(num2 - 1)) {
     palette <- colorRampPalette(colors = c(output[[num2 + 1]][i], output[[num2]][i]))
     output[[i]] <- palette(num1)
   }


   ## create an easier to understand grid with rows=seq1 and cols=seq1
   simpler <- matrix(NA, nrow = num1, ncol = num2)
   ## make the first column seq1
   simpler[ , 1] <- rev(output[[1]])
   ## make the first row seq2
   simpler[1, 2:num2] <- rev(output[[num2]][1:(num2 - 1)])
   ## make the last column the outer column
   simpler[2:num1, num2] <- rev(output[[num2 + 2]][1:(num1 - 1)])

   ## fill in the columns between
   inbetweeners <- c(2:(num2 - 1))

   for (i in 1:length(inbetweeners)) {
      ## define which is the currrent number
      mat_num <- inbetweeners[i]
      ## the other end
      list_num <- rev(inbetweeners)[i]
      ## fill in appropriate column
      simpler[2:num1 , mat_num] <- rev(output[[list_num]][1:(num1 - 1)])
   }



   ## find which num is bigger
   max_num <- max(num1, num2)

   ## create all combinations
   structured <- expand.grid(
      ## create a data frame with two vars
     data.frame(
        ## rows is sequence2 (plus x number of NAs to fill in)
        rws = c(1:num2, rep.int(NA, max_num - num2)),
        ## cols is seq1 plus extra NAs
        cls = c(1:num1, rep.int(NA, max_num - num1))
                )
   )

   ## drop structured where rws or columns are NA
   structured <- structured[complete.cases(structured), ]

   ## create an empty colours variable
   structured$clrs <- NA

   for (i in 1:nrow(structured)) {
      structured[i, "clrs"] <- simpler[structured$cls[i], structured$rws[i]]
   }

   ## return list with colours
   # output

   ## return dataframe with colours
   structured

}
