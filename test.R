library(tidyverse)

p1 <- mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point() +
  theme_grattan() +
  labs(title = "My title",
       subtitle = "My subtitle",
       caption = "Notes: extremely long AP-style notes go in here in which you try to hedge and describe all the limitations of the analysis in the hope that this chart can fully stand on its own despite being the end point of a pretty complicated analytical funnel. Source: GRIP.")

p2 <- mtcars %>%
  rownames_to_column("car") %>%
  ggplot(aes(x = reorder(car, mpg),
             y = mpg)) +
  geom_col() +
  theme_grattan() +
  labs(title = "title",
       subtitle = "subtitle",
       caption = "Notes: notes. Source: source")

p <- list(p1, p2)




create_pptx_shell(p,
                    "test_new.pptx",
                    "fullslide")

add_graph_to_pptx(p,
                  "test_new.pptx")


grattan_save_pptx_2s(p,
                     "list_2s.pptx",
                     "fullslide")


microbenchmark::microbenchmark(grattan_save_pptx(p1,
                                                    "list.pptx",
                                                    "fullslide"),
                               grattan_save_pptx_2s(p1,
                                                    "list_2s.pptx",
                                                    "fullslide"),
                               unit = "s",
                               times = 5)
