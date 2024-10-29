industry_dat <- data.frame(
  stringsAsFactors = FALSE,
  anzsic_division = c("Agriculture, Forestry and Fishing","Mining","Manufacturing",
                      "Electricity, Gas, Water and Waste Services","Construction",
                      "Wholesale Trade","Retail Trade",
                      "Accommodation and Food Services","Transport, Postal and Warehousing",
                      "Information Media and Telecommunications",
                      "Financial and Insurance Services",
                      "Rental, Hiring and Real Estate Services",
                      "Professional, Scientific and Technical Services","Administrative and Support Services",
                      "Public Administration and Safety",
                      "Education and Training","Health Care and Social Assistance",
                      "Arts and Recreation Services","Other Services"),
  anzsic_division_code = c("A","B","C","D","E",
                           "F","G","H","I","J","K","L","M","N","O",
                           "P","Q","R","S")
)

paulified_classifications <-  c("Agriculture, forestry, and fishing","Mining","Manufacturing",
                                "Electricity, gas, water, and waste services","Construction",
                                "Wholesale trade","Retail trade",
                                "Accommodation and food services","Transport, postal, and warehousing",
                                "Information media and telecommunications",
                                "Financial and insurance services",
                                "Rental, hiring, and real estate services",
                                "Professional, scientific, and technical services",
                                "Administrative and support services","Public administration and safety",
                                "Education and training","Health care and social assistance",
                                "Arts and recreation services","Other services")
dat <- industry_dat %>%
  dplyr::mutate(industry = paulify_classifications(anzsic_division))

test_that("paulify_classifications works", {

 testthat::expect_equal(dat$industry, paulified_classifications)

})
