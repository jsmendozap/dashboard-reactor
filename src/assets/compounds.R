compounds <- tibble::tribble(
  ~name,              ~formula,             ~weight,       ~carbon,        ~hydrogen,      ~oxygen,
  "Hydrogen",            "H2",                 2,              0L,              2L,            0L,
  "Carbon monoxide",     "CO",                 28.01,          1L,              0L,            1L,
  "Carbon dioxide",      "CO2",                44.01,          1L,              0L,            2L,
  "Methane",             "CH4",                16.04,          1L,              4L,            0L,
  "Ethane",              "C2H6",               30.07,          2L,              2L,            0L,
  "Ethylene",            "C2H4",               28.05,          2L,              2L,            0L,
  "Propane",             "C3H8",               44.097,         3L,              3L,            0L,
  "Propylene",           "C3H6",               42.081,         3L,              3L,            0L,
  "n-butane",            "n-C4H10",            58.012,         4L,              4L,            0L,
  "i-butane",            "iso-C4H10",          58.012,         4L,              4L,            0L,
  "Cis-2-butene",        "c-C4H8",             56.1,           4L,              4L,            0L,
  "t-2-butene",          "t-C4H8",             56.1,           4L,              4L,            0L,
  "Nitrogen",            "N2",                 28.0134,        0L,              0L,            0L,
  "Argon",               "Ar",                 39.948,         0L,              0L,            0L,
  "Water",               "H2O",                18.015,         0L,              0L,            1L
)