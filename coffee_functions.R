count_cupping_notes <-
  function(cup_list = df$coffee_d_notes,
           count_min = 20) {
    
    remove_words = c("nice", "and", "aftertaste", "better", "bit", "but", "drink", "favorite", "feel", "felt", "finish", "first", "for", "good", "got", "too", "out", "that", "than", "the", "thin", "this", "very", "one", "had", "when", "more", "high", "little", "was", "would", "some", "not", "would", "with", "really", "flavor", "mouth", "notes", "taste", "less", "most", "well", "diner", "after", "much", "tasted", "like", "almost", "tastes", "cup", "flavors", "slightly", "slight", "liked", "just", "tasting", "note", "notes", "also", "what", "maybe", "from", "similar", "its", "didnt", "could", "all", "coffee", "coffees", "which", "probably", "something", "way", "dont", "did", "lot", "over", "have", "process", "time", "down")
    
    cup_words = cup_list |> stringr::str_to_lower() |> paste0(collapse = " ") |> stringr::str_split(pattern = " |,|\n|/") |> unlist()
    
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
        "^smoky$|^smoke$",
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
        "^apples$"
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
        "smokey",
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
        "apple"
      ),
      vectorize_all = FALSE
    )
    
    cup_words = cup_words[!cup_words %in% remove_words]
    cup_words = cup_words[nchar(cup_words) > 2] # Above removes punctuation
    cup_words_table = cup_words |> table()
    
    more_than_x = cup_words_table[cup_words_table > count_min]
    
    return(more_than_x[order(more_than_x, decreasing = TRUE)])
  }