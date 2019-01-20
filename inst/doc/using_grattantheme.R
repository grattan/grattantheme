## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", fig.align='center')
library(ggplot2)
library(grattantheme)

## ------------------------------------------------------------------------
plot <- ggplot(Orange,
               aes(x = age,
                   y = circumference,
                   fill = Tree)) +
        geom_bar(stat = "identity") +
        labs(x = "Age of tree",
             y = "",
             colour = "Tree")

## ---- echo = FALSE-------------------------------------------------------
plot

## ------------------------------------------------------------------------
plot + 
  theme_grattan()

## ------------------------------------------------------------------------
plot + 
  theme_grattan(legend = "top")

## ------------------------------------------------------------------------
plot + 
  theme_grattan() +
  grattan_y_continuous()

## ------------------------------------------------------------------------
plot + 
  theme_grattan(background = "box") +
  grattan_y_continuous()

## ------------------------------------------------------------------------
plot + 
  coord_flip() +
  theme_grattan(flipped = TRUE) +
  grattan_y_continuous()

## ------------------------------------------------------------------------

plot + 
  theme_grattan(base_size = 8, base_family = "serif") +
  grattan_y_continuous()


## ---- out.width = "250px", echo = FALSE----------------------------------
knitr::include_graphics("grattan_pal.pdf")

## ------------------------------------------------------------------------
plot + 
  geom_vline(xintercept = 850, colour = grattan_grey3) +
  theme_grattan() +
  grattan_y_continuous()


## ------------------------------------------------------------------------
plot + 
  theme_grattan() +
  grattan_y_continuous() +
  grattan_fill_manual(n = 5)


## ------------------------------------------------------------------------
plot + 
  theme_grattan() +
  grattan_y_continuous() +
  grattan_fill_manual(n = 5, reverse = TRUE)


## ---- eval = TRUE, error= TRUE, fig.show='hide'--------------------------
plot + 
  theme_grattan() +
  grattan_y_continuous() + 
  grattan_fill_manual(n = 3)

## ------------------------------------------------------------------------
# Create and store the plot:
plot_final <- plot +
              theme_grattan() +
              grattan_y_continuous() +
              grattan_fill_manual(n = 5, reverse = TRUE) +
              labs(title = "Intergenerational inequality in trees",
                 subtitle = "Circumference of trees by age",
                 x = "Age", 
                 y = "",
                 caption = "Notes: orange trees only. Source: inbuilt base R datasets.")

# Save the plot
grattan_save(filename = "tree_plot.pdf",
             object = plot_final,
             type = "normal")


## ---- out.width = "300px", echo = FALSE----------------------------------
knitr::include_graphics("tree_plot.pdf")

## ---- eval = FALSE-------------------------------------------------------
#  # Save the plot
#  grattan_save(filename = "tree_plot_fullslide.pdf",
#               object = plot_final,
#               type = "fullslide")
#  

## ---- include=FALSE------------------------------------------------------
# Actually save the plot to call, but don't include output
grattan_save(filename = "tree_plot_fullslide.pdf",
             object = plot_final,
             type = "fullslide")


## ---- out.width = "400px", echo = FALSE----------------------------------
knitr::include_graphics("tree_plot_fullslide.pdf")

