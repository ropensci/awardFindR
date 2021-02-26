# Contributing

## New sources

New sources can be entirely self-contained in a single `.R` file.

Sources need to have a minimum of two functions:

1.  a `_get` function that scrapes raw data from the source and returns it as-is,
2.  and a `_standardize` function that harmonizes the data to fit the data.frame output from `awardFindR`.

### `_get`

The `_get` routine is exported, so end users can call it directly if they have interest in a specific data source. It should be as faithful a reproduction of the original data source as possible, though variables should still be R-friendly (for example, award amounts as strings like "\$300,000" aren't useful. An integer of 300000 is much more useful.)

These routines will usually use HTML or JSON-based HTTP resources. `utils.R` in this package has a `request()` function that handles HTTP POST and GET requests with `httr` and returns xml2 objects (which can be used with `rvest`) or json as a base R `list()` object automatically. In order to centralize HTTP error handling and standardize message output to the user, please employ this function as much as possible for HTTP requests.

If the source does not use HTML, XML or JSON, it may be necessary to 

### `_standardize`

The `awardFindR` only calls the `*_standardize` function, with the wildcard defined as each string in the `sources` argument. This function should accept the similar input as the `awardFindR` function itself, namely 1. A vector of keywords to search 2. A beginning and end date object to delineate the search (e.g. "2015-01-01") These should be translated into whatever the source-specific requirements are for search terms. If an API can only delineate searches by year, get the year. If an API can only handle one keyword at a time, loop the function through multiple keywords.

This function needs to return a `data.frame` with the following columns:

1.  *institution* Institution name
2.  *pi* Principal investigator name
3.  *year* Award year
4.  *start* Award start date
5.  *end* Award end date
6.  *program* Program or department name
7.  *amount* Award amount (in US dollars)
8.  *id* Award ID number (or any way of uniquely identifying duplicates from one source)
9.  *title* Project title or description
10. *keyword* The searched keyword that returned this result
11. *source* The source the result came from
