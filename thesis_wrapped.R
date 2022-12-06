# spotify wrapped but for R code! Largely based on this blog post: https://nrennie.rbind.io/blog/2022-12-03-how-to-make-your-own-rstats-wrapped/, but adjusted for total number of times a function was used and made to look pretty...


library(janitor)
library(tidyverse)
library(showtext) # fonts
library(png)

### yes yes this is bad practice but this is to more easily separate scripts from unpublished data//work in progress...only judge so much!!
setwd('C:/Users/klohm/Documents/Research/Ch_1_mtDNA_Structure/ToSearch/')

## k now give it the directory to sum thru
fpath <- glue::glue(here::here(), "/Research/Ch_1_mtDNA_Structure/ToSearch")  ## takes working directory + glues the ch dir to end
all_files <- list.files(path = fpath, recursive = TRUE) ## search thr all ch1 subfolders to get list o' files

## just the R scripts now
to_search <- all_files %>% 
  as_tibble() %>% 
  mutate(filetype = str_extract(value, pattern = "\\..*")) %>% 
  filter(filetype == ".R" ) %>% 
  pull(value)

count.function.names <- function(file) {
  data <- getParseData(parse(file=file))
  function_names <- data$text[which(data$token=="SYMBOL_FUNCTION_CALL")]
  occurrences <- data.frame(table(function_names))
  result <- occurrences$Freq
  names(result) <- occurrences$function_names
  result
}

# bad files - 5,9,17

## now to pull out the most used fns...let's see!
all_functions <- map(.x = to_search, .f = ~ count.function.names(.x)) %>%  # map puts out a list of scripts
  bind_rows() %>% # unnest list by binding rows
  rowid_to_column(., var = "rowid") %>% 
  adorn_totals("row") %>% #add a column sum
  slice_tail() # only want the total row

function_count <- all_functions %>% 
  pivot_longer(!rowid, names_to = "Function", values_to = "Count") %>% 
  slice_max(Count, n = 5, with_ties = FALSE)  # pull top 5 favorite functions

# element_blank    89 ggplot
# c                84 base...
# filter           73 dplry
# aes              43 ggplot
# group_by         33 dplyr

# download the r hex tiles
ggplot <- download.file("https://ggplot2.tidyverse.org/logo.png", destfile = 'ggplot.png', mode = 'wb')
dplyr <- download.file("https://dplyr.tidyverse.org/logo.png", destfile = 'dplyr.png', mode = 'wb')
baser <- download.file("https://github.com/rstudio/hex-stickers/blob/main/thumbs/RStudio.png?raw=true", destfile = 'baser.png', mode = 'wb')
# read in png hex tiles
ggplotpng <- readPNG('ggplot.png')
dplyrpng <- readPNG('dplyr.png')
baserpng <- readPNG('baser.png')

# load wrapped design (made in canva real quick)
wrappedpng <- readPNG('spotifywrapped.png')
wrapped2png <- readPNG('spotifywrapped2.png')

# add a font that looks like spotify wraped..
font_add_google("Raleway", "raleway")
showtext_auto()

# tiny df for label placement
tinydf <- data.frame(x= 2 ,
                     y= c(1:5))

tinydf2 <- data.frame(x = rep(1, 5),
                      y = 1:5,
                      label = paste0("#", rev(1:5)))

# and to make the plot!

p <- ggplot() +
  # add pretty image to make it look like spotify, plot first so lyrs on top
  annotation_raster(wrapped2png, ymin = .5,ymax= 5.5,xmin = -.25,xmax = .5) +
  annotation_raster(wrappedpng, ymin = .5,ymax= 5.5,xmin = 3.75,xmax = 4.5) +
  # add text with the numbers 1 to 5
  geom_text(data = tinydf2,
            mapping = aes(x = x,
                          y = y,
                          label = label),
            colour = "black",
            size = 20,
            fontface = "bold",
            family = "raleway") +
  # add text with the names of the functions
  geom_text(data = function_count,
            mapping = aes(x = rep(2.25, 5),
                          y = rev(1.25:5.25),
                          label = paste0(Function, "() ")),
            colour = "black",
            hjust = 0,
            size = 11,
            fontface = "bold",
            family = "raleway") +
  # add number of times used as
  geom_text(data = function_count,
            mapping = aes(x = rep(2.25, 5),
                          y = rev(1:5),
                          label = paste0(Count, " times")),
            colour = "black",
            hjust = 0,
            size = 9,
            family = "raleway") +
  # add title using geom_text() instead of labs()
  geom_text(data = data.frame(),
            aes(x = 2., y = 6.25, label = "My Top Functions"),
            colour = "black",
            fontface = "bold",
            hjust = 0.5,
            size = 14,
            family = "raleway") +
  # add caption using geom_text() instead of labs() to get btr placement
  geom_text(data = data.frame(),
            aes(x = 4.2, y = 0, label = "#MScTHESISWRAPPED"),
            colour = "black",
            fontface = "bold",
            hjust = 0.95,
            size = 10,
            family = "raleway") +
  #add the pkg images
  annotation_raster(ggplotpng, ymin = 4.5,ymax= 5.5,xmin = 1.25,xmax = 1.75)+
  annotation_raster(baserpng, ymin = 3.5,ymax= 4.5,xmin = 1.25,xmax = 1.75)+
  annotation_raster(dplyrpng, ymin = 2.5,ymax= 3.5,xmin = 1.25,xmax = 1.75)+
  annotation_raster(ggplotpng, ymin = 1.5,ymax= 2.5,xmin = 1.25,xmax = 1.75)+
  annotation_raster(dplyrpng, ymin = .5,ymax= 1.5,xmin = 1.25,xmax = 1.75) +
  # set axis limits
  scale_x_continuous(limits = c(-0, 4.25)) +
  scale_y_continuous(limits = c(-0.5,6.5)) +
  # set the theme to take out most of the plot junk
  theme_void() +
  theme(plot.background = element_rect(fill = "yellow", colour = "yellow"), # make bkgrnd yellow
        plot.margin = margin(10, 0, 10, 0)) # add some blank space to top/btm of plot



show(p)


ggsave("thesis_wrapped.png", width = 4.5, height = 6.5, units = "in")
