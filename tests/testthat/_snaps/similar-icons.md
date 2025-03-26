# get_similar_icons returns relevant suggestions

    Code
      get_similar_icons("chart-bar")
    Output
      [1] "bar-chart"           "bar-chart-fill"      "bar-chart-line"     
      [4] "bar-chart-steps"     "bar-chart-line-fill"

---

    Code
      get_similar_icons("person-add")
    Output
      [1] "person-add"      "person-fill-add" "person-badge"    "person-dash"    
      [5] "person-up"      

---

    Code
      get_similar_icons("person-+")
    Output
      [1] "person-x"    "person-up"   "person-add"  "person-dash" "person-down"

---

    Code
      get_similar_icons("person", max_suggestions = 5)
    Output
      [1] "person"      "person-x"    "person-up"   "person-add"  "file-person"

# get_similar_icons handles edge cases

    Code
      get_similar_icons("")
    Output
      [1] "x"  "at" "hr" "pc" "tv"

