# Define grattan colours =======================================================
base_yellow <<- dplyr::if_else(options("grattan_palette") == "old", "#FFE07F", "#3E0E39")
base_darkyellow <- dplyr::if_else(options("grattan_palette") == "old", "#FFC35A", "#F5B50C")
base_orange <-  dplyr::if_else(options("grattan_palette") == "old", "#F68B33", "#EF7900")
base_darkorange <- "#D4582A" #unchanged
base_red <- dplyr::if_else(options("grattan_palette") == "old", "#A02226", "#A1253E")
base_darkred <- dplyr::if_else(options("grattan_palette") == "old", "#621214", "#611633")
base_blue <- dplyr::if_else(options("grattan_palette") == "old", "#A3C7DF", "#6A99C4")
base_darkblue <- dplyr::if_else(options("grattan_palette") == "old", "#3E81CE", "#4371A1")

# Yellow light ----
yellow_palette <- grDevices::colorRampPalette(c(base_yellow, "white"))(10)

#' grattan_lightyellow
#'
#' @export
#'
grattan_lightyellow <- yellow_palette[1]

#' grattan_lightyellow1
#'
#' @export
#'
grattan_lightyellow1 <- yellow_palette[2]

#' grattan_lightyellow2
#'
#' @export
#'
grattan_lightyellow2 <- yellow_palette[3]

#' grattan_lightyellow3
#'
#' @export
#'
grattan_lightyellow3 <- yellow_palette[4]

#' grattan_lightyellow4
#'
#' @export
#'
grattan_lightyellow4 <- yellow_palette[5]

#' grattan_lightyellow5
#'
#' @export
#'
grattan_lightyellow5 <- yellow_palette[6]
#' grattan_lightyellow6
#'
#' @export
#'
grattan_lightyellow6 <- yellow_palette[7]

#' grattan_lightyellow7
#'
#' @export
#'
grattan_lightyellow7 <- yellow_palette[8]

#' grattan_lightyellow8
#'
#' @export
#'
grattan_lightyellow8 <- yellow_palette[9]

# Yellow dark ----
darkyellow_palette <- grDevices::colorRampPalette(c(base_darkyellow, "white"))(10)

#' '
#'
#' @export
#'
grattan_darkyellow <- darkyellow_palette[1]

#' '
#'
#' @export
#'
grattan_darkyellow1 <- darkyellow_palette[2]

#' '
#'
#' @export
#'
grattan_darkyellow2 <- darkyellow_palette[3]

#' '
#'
#' @export
#'
grattan_darkyellow3 <- darkyellow_palette[4]

#' '
#'
#' @export
#'
grattan_darkyellow4 <- darkyellow_palette[5]

#' '
#'
#' @export
#'
grattan_darkyellow5 <- darkyellow_palette[6]

#' '
#'
#' @export
#'
grattan_darkyellow6 <- darkyellow_palette[7]

#' '
#'
#' @export
#'
grattan_darkyellow7 <- darkyellow_palette[8]

#' '
#'
#' @export
#'
grattan_darkyellow8 <- darkyellow_palette[9]



#' '
#'
#' @export
#'
grattan_yellow <- grattan_darkyellow


#' '
#'
#' @export
#'
grattan_yellow1 <- grattan_darkyellow1


#' '
#'
#' @export
#'
grattan_yellow2 <- grattan_darkyellow2


#' '
#'
#' @export
#'
grattan_yellow3 <- grattan_darkyellow3


#' '
#'
#' @export
#'
grattan_yellow4 <- grattan_darkyellow4


#' '
#'
#' @export
#'
grattan_yellow5 <- grattan_darkyellow5


#' '
#'
#' @export
#'
grattan_yellow6 <- grattan_darkyellow6


#' '
#'
#' @export
#'
grattan_yellow7 <- grattan_darkyellow7


#' '
#'
#' @export
#'
grattan_yellow8 <- grattan_darkyellow8


# Orange light ----------
orange_palette <- grDevices::colorRampPalette(c(base_orange, "white"))(10)

#' '
#'
#' @export
#'
grattan_lightorange <- orange_palette[1]

#' '
#'
#' @export
#'
grattan_lightorange1 <- orange_palette[2]

#' '
#'
#' @export
#'
grattan_lightorange2 <- orange_palette[3]

#' '
#'
#' @export
#'
grattan_lightorange3 <- orange_palette[4]

#' '
#'
#' @export
#'
grattan_lightorange4 <- orange_palette[5]

#' '
#'
#' @export
#'
grattan_lightorange5 <- orange_palette[6]

#' '
#'
#' @export
#'
grattan_lightorange6 <- orange_palette[7]

#' '
#'
#' @export
#'
grattan_lightorange7 <- orange_palette[8]

#' '
#'
#' @export
#'
grattan_lightorange8 <- orange_palette[9]


#' '
#'
#' @export
#'
grattan_orange <- grattan_lightorange

#' '
#'
#' @export
#'
grattan_orange1 <- grattan_lightorange1

#' '
#'
#' @export
#'
grattan_orange2 <- grattan_lightorange2

#' '
#'
#' @export
#'
grattan_orange3 <- grattan_lightorange3

#' '
#'
#' @export
#'
grattan_orange4 <- grattan_lightorange4

#' '
#'
#' @export
#'
grattan_orange5 <- grattan_lightorange5

#' '
#'
#' @export
#'
grattan_orange6 <- grattan_lightorange6

#' '
#'
#' @export
#'
grattan_orange7 <- grattan_lightorange7

#' '
#'
#' @export
#'
grattan_orange8 <- grattan_lightorange8


# dark:
darkorange_palette <- grDevices::colorRampPalette(c(base_darkorange, "white"))(10)


#' '
#'
#' @export
#'
grattan_darkorange <- darkorange_palette[1]


#' '
#'
#' @export
#'
grattan_darkorange1 <- darkorange_palette[2]


#' '
#'
#' @export
#'
grattan_darkorange2 <- darkorange_palette[3]


#' '
#'
#' @export
#'
grattan_darkorange3 <- darkorange_palette[4]


#' '
#'
#' @export
#'
grattan_darkorange4 <- darkorange_palette[5]


#' '
#'
#' @export
#'
grattan_darkorange5 <- darkorange_palette[6]


#' '
#'
#' @export
#'
grattan_darkorange6 <- darkorange_palette[7]


#' '
#'
#' @export
#'
grattan_darkorange7 <- darkorange_palette[8]


#' '
#'
#' @export
#'
grattan_darkorange8 <- darkorange_palette[9]



# Red --------------
# light:
red_palette <- grDevices::colorRampPalette(c(base_red, "white"))(10)


#' '
#'
#' @export
#'
grattan_lightred <- red_palette[1]


#' '
#'
#' @export
#'
grattan_lightred1 <- red_palette[2]


#' '
#'
#' @export
#'
grattan_lightred2 <- red_palette[3]


#' '
#'
#' @export
#'
grattan_lightred3 <- red_palette[4]


#' '
#'
#' @export
#'
grattan_lightred4 <- red_palette[5]


#' '
#'
#' @export
#'
grattan_lightred5 <- red_palette[6]


#' '
#'
#' @export
#'
grattan_lightred6 <- red_palette[7]


#' '
#'
#' @export
#'
grattan_lightred7 <- red_palette[8]


#' '
#'
#' @export
#'
grattan_lightred8 <- red_palette[9]



#' '
#'
#' @export
#'
grattan_red <- grattan_lightred


#' '
#'
#' @export
#'
grattan_red1 <- grattan_lightred1


#' '
#'
#' @export
#'
grattan_red2 <- grattan_lightred2


#' '
#'
#' @export
#'
grattan_red3 <- grattan_lightred3


#' '
#'
#' @export
#'
grattan_red4 <- grattan_lightred4


#' '
#'
#' @export
#'
grattan_red5 <- grattan_lightred5


#' '
#'
#' @export
#'
grattan_red6 <- grattan_lightred6


#' '
#'
#' @export
#'
grattan_red7 <- grattan_lightred7


#' '
#'
#' @export
#'
grattan_red8 <- grattan_lightred8



# dark:
darkred_palette <- grDevices::colorRampPalette(c(base_darkred, "white"))(10)


#' '
#'
#' @export
#'
grattan_darkred <- darkred_palette[1]


#' '
#'
#' @export
#'
grattan_darkred1 <- darkred_palette[2]


#' '
#'
#' @export
#'
grattan_darkred2 <- darkred_palette[3]


#' '
#'
#' @export
#'
grattan_darkred3 <- darkred_palette[4]


#' '
#'
#' @export
#'
grattan_darkred4 <- darkred_palette[5]


#' '
#'
#' @export
#'
grattan_darkred5 <- darkred_palette[6]


#' '
#'
#' @export
#'
grattan_darkred6 <- darkred_palette[7]

#' '
#'
#' @export
#'
grattan_darkred7 <- darkred_palette[8]

#' '
#'
#' @export
#'
grattan_darkred8 <- darkred_palette[9]

# Blue -------------------------------------------------------------------------
# light:
blue_palette <- grDevices::colorRampPalette(c(base_blue, "white"))(10)


#' '
#'
#' @export
#'
grattan_lightblue <- blue_palette[1]


#' '
#'
#' @export
#'
grattan_lightblue1 <- blue_palette[2]


#' '
#'
#' @export
#'
grattan_lightblue2 <- blue_palette[3]


#' '
#'
#' @export
#'
grattan_lightblue3 <- blue_palette[4]


#' '
#'
#' @export
#'
grattan_lightblue4 <- blue_palette[5]


#' '
#'
#' @export
#'
grattan_lightblue5 <- blue_palette[6]


#' '
#'
#' @export
#'
grattan_lightblue6 <- blue_palette[7]


#' '
#'
#' @export
#'
grattan_lightblue7 <- blue_palette[8]


#' '
#'
#' @export
#'
grattan_lightblue8 <- blue_palette[9]


#' '
#'
#' @export
#'
grattan_blue <- grattan_lightblue


#' '
#'
#' @export
#'
grattan_blue1 <- grattan_lightblue1


#' '
#'
#' @export
#'
grattan_blue2 <- grattan_lightblue2


#' '
#'
#' @export
#'
grattan_blue3 <- grattan_lightblue3


#' '
#'
#' @export
#'
grattan_blue4 <- grattan_lightblue4


#' '
#'
#' @export
#'
grattan_blue5 <- grattan_lightblue5


#' '
#'
#' @export
#'
grattan_blue6 <- grattan_lightblue6


#' '
#'
#' @export
#'
grattan_blue7 <- grattan_lightblue7


#' '
#'
#' @export
#'
grattan_blue8 <- grattan_lightblue8



# dark:
darkblue_palette <- grDevices::colorRampPalette(c(base_darkblue, "white"))(10)

#' '
#'
#' @export
#'
grattan_darkblue <- darkblue_palette[1]

#' '
#'
#' @export
#'
grattan_darkblue1 <- darkblue_palette[2]

#' '
#'
#' @export
#'
grattan_darkblue2 <- darkblue_palette[3]

#' '
#'
#' @export
#'
grattan_darkblue3 <- darkblue_palette[4]

#' '
#'
#' @export
#'
grattan_darkblue4 <- darkblue_palette[5]

#' '
#'
#' @export
#'
grattan_darkblue5 <- darkblue_palette[6]

#' '
#'
#' @export
#'
grattan_darkblue6 <- darkblue_palette[7]
#' '
#'
#' @export
#'
grattan_darkblue7 <- darkblue_palette[8]

#' '
#'
#' @export
#'
grattan_darkblue8 <- darkblue_palette[9]


# black ----

#' '
#'
#' @export
#'
grattan_black <- "#000000"

# grey ------

# light

#' '
#'
#' @export
#'
grattan_lightgrey <- "#828282"

#' '
#'
#' @export
#'
grattan_lightgrey1 <- "#8F8F8F"

#' '
#'
#' @export
#'
grattan_lightgrey2 <- "#9B9B9B"

#' '
#'
#' @export
#'
grattan_lightgrey3 <- "#A8A8A8"

#' '
#'
#' @export
#'
grattan_lightgrey4 <- "#B4B4B4"

#' '
#'
#' @export
#'
grattan_lightgrey5 <- "#C1C1C1"

#' '
#'
#' @export
#'
grattan_lightgrey6 <- "#CDCDCD"



#' '
#'
#' @export
#'
grattan_lightgrey7 <- "#DADADA"

#' '
#'
#' @export
#'
grattan_lightgrey8 <- "#E6E6E6"

# dark
#' '
#'
#' @export
#'
grattan_darkgrey <- "#575757"

#' '
#'
#' @export
#'
grattan_darkgrey1 <- "#686868"

#' '
#'
#' @export
#'
grattan_darkgrey2 <- "#797979"

#' '
#'
#' @export
#'
grattan_darkgrey3 <- "#898989"

#' '
#'
#' @export
#'
grattan_darkgrey4 <- "#9A9A9A"

#' '
#'
#' @export
#'
grattan_darkgrey5 <- "#ABABAB"

#' '
#'
#' @export
#'
grattan_darkgrey6 <- "#BCBCBC"

#' '
#'
#' @export
#'
grattan_darkgrey7 <- "#CDCDCD"

#' '
#'
#' @export
#'
grattan_darkgrey8 <- "#DDDDDD"

# Add other specific Grattan colours
#' '
#'
#' @export
#'
grattan_grey1 <- "#D9D9D9"

#' '
#'
#' @export
#'
grattan_grey2 <- "#AEAEAE"

#' '
#'
#' @export
#'
grattan_grey3 <- "#828282"

#' '
#'
#' @export
#'
grattan_grey4 <- "#575757"

#' '
#'
#' @export
#'
grattan_grey5 <- "#2B2B2B"

#' '
#'
#' @export
#'
grattan_grey_alpha <- "#E1E3E5"

#' grattan_grey_title
#'
#' @export
#'
grattan_grey_title <- "#6A737B"

#' grattan_gridlinegrey
#'
#' @export
#'
grattan_gridlinegrey <- "#C3C7CB"

#' '
#'
#' @export
#'
grattan_orange_alpha <- "#FEF0DE"

#' '
#'
#' @export
#'
grattan_box <- "#FEF0DE"

# Add legacy items
grattan_lightyellow_f <- "#FFE79F"
grattan_yellow_f <- "#FFD283"
grattan_lightorange_f <- "#F8A866"
grattan_orange_f <- "#F8A866"
grattan_darkorange_f <- "#DE815F"
grattan_red_f <- "#B7595C"
grattan_darkred_f <- "#894D4E"


#' A list of Grattan colour sets
#'
#' "full": red, dark orange, light orange, yellow, light yellow
#' "full_f": faded version of "full"
#' "light": light orange, yellow, light yellow
#' "dark": red, dark orange, light orange
#' "diverging": red, faded red, white, faded light orange, light orange
#' "grey": grey 1, grey 2, grey 3, grey 4, grey 5
"grattan_palette_set"
