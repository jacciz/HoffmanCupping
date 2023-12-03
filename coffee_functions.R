flavor_colors = tibble::tribble(~ids, ~color,
                                "Fruity",  "#ee3087",
                                "Sour/Acid", "#e6550d",         #
                                "Alcohol/Fermented",  "#9ecae1",
                                "Green/Vegetative", "#3fa746", #
                                "Stale/Papery", "#fee6ce",  #
                                "Earthy", "#ff3d19",   ##
                                "Chemical",  "#9ecae1",  ###
                                "Roasted" , "#747e7d",
                                "Cereal", "#eedaac",
                                "Spices", "#ff6a6d",
                                "Nutty", "#d3a778",
                                "Cocoa" , "#3b281e", # chocolate?
                                "Sweet" , "#f2f4f4",
                                "Floral",  "#abb9dc")

find_two_words <- function(str, two_word){
  c = stringi::stri_count_regex(str, pattern = two_word)
  rep(two_word, c)
}

count_cupping_notes <-
  function(cup_list,
           count_min = 20) {
    
    remove_words = c("nice", "and", "aftertaste", "better", "bit", "but", "drink", "favorite", "feel", "felt",
                     "finish", "first", "for", "good", "got", "too", "out", "that", "than", "the", "thin", "this",
                     "very", "one", "had", "when", "more", "high", "little", "was", "would", "some", "not", "would",
                     "with", "really", "flavor", "mouth", "notes", "taste", "less", "most", "well", "diner", "after",
                     "much", "tasted", "like", "almost", "tastes", "cup", "flavors", "slightly", "slight", "liked",
                     "just", "tasting", "note", "notes", "also", "what", "maybe", "from", "similar", "its", "didnt",
                     "could", "all", "coffee", "coffees", "which", "probably", "something", "way", "dont", "did",
                     "lot", "over", "have", "process", "time", "down", "brown")
    
    # The first part of each regex will be used to count later on (i.e. black tea)
    two_words = c("brown sugar", "black tea|tea like|tea-like|tealike", "olive oil|olives", "dark chocolate|dark choc|dark chocolatey")

    # find_two_words(word, "brown sugar")
    # two_words_counts = purrr::map(two_words, find_two_words, str = word) |> unlist()
    # two_words_counts = sub("\\|.*", "", two_words_counts)
    
    # Put all words in a single long string
    cup_words = cup_list |> stringr::str_to_lower() |>
      paste0(collapse = " ")
    
    two_words_counts = purrr::map(two_words, find_two_words, str = cup_words) |> unlist()
    two_words_counts = sub("\\|.*", "", two_words_counts)
    
    # Also remove dark chocolate bc we will count chocolate
    cup_words = stringi::stri_replace_all(str = cup_words, regex = "dark chocolate", replacement = "")
    
    cup_words = cup_words |> stringr::str_split(pattern = " |,|\n|/") |> unlist()
    
    cup_words = stringi::stri_replace_all_regex(
      cup_words,
      pattern = c(
        "[[:punct:]]|[0-9]+",
        "raspberries",
        "strawberries",
        "blueberries",
        "^berries$",
        "^cherries",
        "^fruit$|^fruit$|fruitful|fruitiest|fruitiness|fruitier|fruityness|fruits$|fruite$|fruty$",
        "chocolatey",
        "acidic|acidity",
        "bitterness",
        "citrusy",
        "^smokey$|^smoke$",
        "^wood$",
        "^balance$",
        "^roast$|roasty",
        "^toast$|toasty",
        "^nuts$|^nut$",
        "^darker$",
        "sweetness",
        "^cool$|^cools$",
        "fermenty",
        "^funk$",
        "^love$",
        "juice$",
        "tealike",
        "^apples$",
        "^stone",
        "^wine$",
        "^cloves$",
        "paper",
        "herby|herbed|herb|herbs" # replace dark chocolate with nothing
      ),
      replacement = c(
        "",
        "raspberry",
        "strawberry",
        "blueberry",
        "berry",
        "cherry",
        "fruity",
        "chocolate",
        "acid",
        "bitter",
        "citrus",
        "smoky",
        "woody",
        "balanced",
        "roasted",
        "toasted",
        "nutty",
        "dark",
        "sweet",
        "cooled",
        "fermented",
        "funky",
        "loved",
        "juicy",
        "tea",
        "apple",
        "stone fruit",
        "winey",
        "clove",
        "papery",
        "herb-like"
      ),
      vectorize_all = FALSE
    )
    
    cup_words = cup_words[!cup_words %in% remove_words]
    cup_words = cup_words[nchar(cup_words) > 2] # Above removes punctuation
    
    # Add the two words counts
    cup_words = c(cup_words, two_words_counts)
    
    cup_words_table = cup_words |> table()
    
    more_than_x = cup_words_table[cup_words_table > count_min]
    
    return(more_than_x[order(more_than_x, decreasing = TRUE)])
  }

prepare_words <- function(coffee_words, color = "#000") {
  coffee_df = data.frame(coffee_words) |> mutate(cup_words = stringr::str_to_title(cup_words),
                                                 Freq = color)
  coffee_df = coffee_df |> mutate(
    cup_words = recode(
      cup_words,
      "Acid" = "Sour/Acid",
      "Caramel" = "Caramelized",
      "Citrus" = "Citrus Fruit",
      Vegetal = "Vegetative",
      "Papery" = "Stale/Papery"
    )
  )
  return(coffee_df)
}

coffee_sunburst <- function(coffee_cup_df) {
  # coffee_cupping_tasting = coffee_cupping_tasting |> mutate(parent_name = sub("\\-.*", "", parents))
  # coffee_cupping_tasting = left_join(coffee_cupping_tasting, flavor_colors, by = c("ids" = "ids"))
  coffee_cup_df <-
    coffee_cup_df |> mutate(new_label = paste0(
      "<b>",
      end_name,
      ": ",
      "</b>",
      ifelse(is.na(labels), "", stringr::str_wrap(labels, width = 30))
    ))

  coffee_cup_df |> plot_ly() |> add_trace(
    marker = list(colors = ~ color),
    type = 'sunburst',
    ids = ~ ids,
    labels = ~ end_name,
    parents = ~ parents,
    text = ~ new_label,
    textinfo = 'label',
    # What shows on the chart
    hovertemplate = '%{text}<extra></extra>',
    domain = list(column = 1),
    maxdepth = 3,
    insidetextorientation = 'radial'
  ) |>
    layout(margin = list(
      l = 0,
      r = 0,
      b = 0,
      t = 0
    )) |>
    as_widget()
}

coffee_words_join <-
  function(coffee_words,
           coffee_cup_df,
           color = "#fee6ce") {
    coffee_join = left_join(coffee_cup_df, coffee_words, by = c("end_name" = "cup_words"))
    
    coffee_join = coffee_join |> mutate(color = ifelse(is.na(Freq), color, Freq))
    return(coffee_join)
  }