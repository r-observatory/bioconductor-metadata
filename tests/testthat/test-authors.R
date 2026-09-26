test_that("parse_authors_at_r extracts people, roles, and ORCID", {
  ar <- 'c(person("Dirk", "Eddelbuettel", role = c("aut","cre"),
             comment = c(ORCID = "0000-0001-6419-907X")),
           person("Romain", "Francois", role = "aut"))'
  out <- parse_authors_at_r(ar, "Rcpp")
  expect_equal(nrow(out), 2)
  expect_equal(out$family, c("Eddelbuettel", "Francois"))
  expect_equal(out$role[1], "aut, cre")
  expect_equal(out$orcid[1], "0000-0001-6419-907X")
  expect_equal(out$orcid[2], NA_character_)
  expect_true(all(out$package == "Rcpp"))
})

test_that("parse_authors_at_r returns empty on unparseable input", {
  out <- parse_authors_at_r("Hadley Wickham (free text, not Authors@R)", "x")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("package","given","family","email","role","orcid",
                             "ror_id","comment"))
})

one_person <- function(comment_code) {
  parse_authors_at_r(sprintf('person("Ada", "Lovelace", role = "aut", comment = %s)',
                             comment_code), "pkgA")
}

test_that("parse_authors_at_r reads a named ROR and drops its URL prefix", {
  out <- one_person('c(ROR = "https://ror.org/02nr0ka47")')
  expect_equal(out$ror_id, "02nr0ka47")
  expect_identical(out$comment, NA_character_)
})

test_that("parse_authors_at_r leaves ror_id NA for a malformed ROR", {
  out <- one_person('c(ROR = "https://ror.org/not-a-ror")')
  expect_identical(out$ror_id, NA_character_)
})

test_that("parse_authors_at_r joins the unnamed comment parts with a comma", {
  out <- one_person('c(ORCID = "0000-0002-1825-0097", "University X", "Department   Y")')
  expect_equal(out$orcid, "0000-0002-1825-0097")
  expect_equal(out$comment, "University X, Department Y")
})

test_that("parse_authors_at_r stores an ORCID URL as the bare iD", {
  out <- one_person('c(ORCID = "http://orcid.org/0000-0003-3199-3722")')
  expect_equal(out$orcid, "0000-0003-3199-3722")
  out <- one_person('c(ORCID = "https://orcid.org/0000-0002-1825-0097")')
  expect_equal(out$orcid, "0000-0002-1825-0097")
})

test_that("an ORCID iD with a valid check digit moves from the free comment", {
  out <- one_person('"ORCID: 0000-0002-1825-0097"')
  expect_equal(out$orcid, "0000-0002-1825-0097")
  expect_identical(out$comment, NA_character_)

  out <- one_person('"<https://orcid.org/0000-0002-1825-0097>"')
  expect_equal(out$orcid, "0000-0002-1825-0097")
  expect_identical(out$comment, NA_character_)
})

test_that("a moved ORCID iD leaves other comment text verbatim", {
  out <- one_person('"Senior author, 0000-0002-1825-0097"')
  expect_equal(out$orcid, "0000-0002-1825-0097")
  expect_equal(out$comment, "Senior author, 0000-0002-1825-0097")
})

test_that("an ORCID iD with a wrong check digit stays in the comment", {
  out <- one_person('"0000-0002-1825-0098"')
  expect_identical(out$orcid, NA_character_)
  expect_equal(out$comment, "0000-0002-1825-0098")
})

test_that("two ORCID iDs in one comment move nowhere", {
  out <- one_person('"0000-0002-1825-0097 and 0000-0001-6419-907X"')
  expect_identical(out$orcid, NA_character_)
  expect_equal(out$comment, "0000-0002-1825-0097 and 0000-0001-6419-907X")
})

test_that("a named ORCID wins and the comment is left as written", {
  out <- one_person('c(ORCID = "0000-0001-6419-907X", "see 0000-0002-1825-0097")')
  expect_equal(out$orcid, "0000-0001-6419-907X")
  expect_equal(out$comment, "see 0000-0002-1825-0097")
})

test_that("a ROR id moves from the free comment", {
  out <- one_person('"ROR: https://ror.org/02nr0ka47"')
  expect_equal(out$ror_id, "02nr0ka47")
  expect_identical(out$comment, NA_character_)

  out <- one_person('"Funded by https://ror.org/02nr0ka47"')
  expect_equal(out$ror_id, "02nr0ka47")
  expect_equal(out$comment, "Funded by https://ror.org/02nr0ka47")
})

test_that("the stored comment collapses whitespace and keeps a long link whole", {
  link <- paste0("https://github.com/ropensci/software-review/issues/",
                 strrep("9", 150))
  out <- one_person(sprintf('"Reviewed  at\\n   %s"', link))
  expect_equal(out$comment, paste("Reviewed at", link))
  expect_gt(nchar(out$comment), 120)
})

test_that("control characters and tabs in a comment are cleaned and the row is kept", {
  out <- parse_authors_at_r(
    'person("Ada", "Lovelace", role = "aut", comment = "Dept\\tX\\001 Lab")', "pkgA")
  expect_equal(nrow(out), 1L)
  expect_equal(out$comment, "Dept X Lab")
})

test_that("a non-ASCII affiliation in a comment is stored unchanged", {
  txt <- "Universität Zürich, Département de biologie"
  out <- one_person(sprintf('"%s"', txt))
  expect_identical(enc2utf8(out$comment), txt)
  expect_equal(nchar(out$comment), nchar(txt))
})

test_that("an error while reading the free comment keeps the identifiers", {
  orig <- normalize_author_comments
  assign("normalize_author_comments", function(...) stop("boom"), envir = globalenv())
  on.exit(assign("normalize_author_comments", orig, envir = globalenv()), add = TRUE)
  expect_message(
    out <- one_person('c(ORCID = "0000-0002-1825-0097", ROR = "02nr0ka47", "text")'),
    "pkgA")
  expect_equal(nrow(out), 1L)
  expect_equal(out$orcid, "0000-0002-1825-0097")
  expect_equal(out$ror_id, "02nr0ka47")
  expect_identical(out$comment, NA_character_)
})

test_that("an error while cleaning the free comment keeps the identifiers", {
  orig <- sanitize_comment_text
  assign("sanitize_comment_text", function(...) stop("bad bytes"), envir = globalenv())
  on.exit(assign("sanitize_comment_text", orig, envir = globalenv()), add = TRUE)
  expect_message(
    out <- one_person('c(ORCID = "0000-0002-1825-0097", ROR = "02nr0ka47", "text")'),
    "bad bytes")
  expect_equal(nrow(out), 1L)
  expect_equal(out$orcid, "0000-0002-1825-0097")
  expect_equal(out$ror_id, "02nr0ka47")
  expect_identical(out$comment, NA_character_)
})

test_that("carry_forward_authors fills columns a prior catalog predates with NA", {
  prior <- data.frame(package = c("A", "B"), given = c("Ann", "Bo"),
                      family = c("X", "Y"), email = NA_character_,
                      role = "aut", orcid = NA_character_,
                      stringsAsFactors = FALSE)
  out <- carry_forward_authors(prior, keep = "B")
  expect_equal(names(out), BIOC_AUTHOR_COLS)
  expect_equal(out$package, "B")
  expect_identical(out$ror_id, NA_character_)
  expect_identical(out$comment, NA_character_)
})

test_that("carry_forward_authors keeps ror_id and comment a prior catalog holds", {
  prior <- data.frame(package = "A", given = "Ann", family = "X",
                      email = NA_character_, role = "aut", orcid = NA_character_,
                      ror_id = "02nr0ka47", comment = "University X",
                      stringsAsFactors = FALSE)
  out <- carry_forward_authors(prior, keep = "A")
  expect_equal(out$ror_id, "02nr0ka47")
  expect_equal(out$comment, "University X")
  expect_equal(nrow(carry_forward_authors(prior, keep = character(0))), 0L)
})
