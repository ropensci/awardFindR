# Contributing
`awardFindR` is built to be easily extendable to include additional data sources and we welcome contributions adding support for additional databases of research funding. 

Adding a new source involves three steps:
1. Adding a `sourcename.R` file that parse the database using its API or webscraping as discussed
2. Adding the source to default `sources` argument of the `awardFindR()` function in `main.R`
3. Adding relevant tests

## Adding a new source 

New sources are entirely self-contained in a single `.R` file.

Sources need to have a minimum of two functions:

1.  a `*_get` function that scrapes raw data from the source and returns it as-is,
2.  and a `*_standardize` function that harmonizes the data to fit the data.frame output from `awardFindR`.

The file containing the source should have an easily identifiable filename, which should be reflected in the naming of the `*_get` and `*_standardize` functions. For example, import from the "Megafunds Foundation" should be in `megafunds.R`, containing `megafunds_get()` and `megafunds_standardize()`.

### `*_get`

The `*_get` routine is exported, so end users can call it directly if they have interest in a specific data source. It should be as faithful a reproduction of the original data source as possible, though variables should still be R-friendly (for example, award amounts as strings like "\$300,000" aren't useful. An integer of 300000 is much more useful.)

These routines will usually use HTML or JSON-based HTTP resources. `utils.R` in this package has a `request()` function that handles HTTP POST and GET requests with `httr` and returns xml2 objects (which can be used with `rvest` or `xml2`) or json as a base R `list()` object automatically. In order to centralize HTTP error handling and standardize message output to the user, please employ this function as much as possible for HTTP requests.

### Style and dependencies

This function can be largely tailored to the needs of an API, but should follow some basic style guidelines. Three arguments are required:

1. Keyword
2. From date
3. To date

The function should accept at least one keyword and two date terms as arguments. _keyword_ is a string, or, if the source can handle more than one keyword at a time, _keywords_ is a vector of strings. Dates are typically exact days or years, depending on the capabilities of the search function available from the source. Exact dates should be date objects named _from_date_ and _to_date_, while years should be integers named _from_year_ and _to_year_. 

Additional source-specific variables can be added to the function, but should have default values specified. `*_get` routines should function as expected with these three variables alone.

If at all possible, searching should be done server-side to reduce HTTP traffic. Downloading the whole grant database and searching with `grep()` or a similar function should be a last resort. This also minimizes CPU load.

JSON sources shouldn't need any additional dependencies, since the `request()` function handles encoding and decoding from/to lists automatically. HTML/XML sources, however, should use `rvest` or `xml2`, respectively. 

If the source does not use HTML, XML or JSON, it may be necessary to employ additional dependencies as necessary. This is an extreme case and should be done with care.

### `*_standardize`

The `awardFindR` calls the `*_standardize` function, which should in turn call the `*_get` function described above. All `*_standardize` functions need to accept the exact same input. This includes:

1. *keywords* - A vector of keywords to search 
2. *from_date* - A beginning date object (e.g. "2015-01-01")
3. *end_date* - And ending date object

The date objects should be translated into whatever the source-specific requirements are for search terms. If an API can only delineate searches by year, get the year. If an API can only handle one keyword at a time, loop the function through multiple keywords with `lapply()`.

This function needs to return a `data.frame` with the following columns:

1.  *institution* - Institution name
2.  *pi* -  Principal investigator name
3.  *year* - Award year
4.  *start* - Award start date
5.  *end* - Award end date
6.  *program* - Program or department name
7.  *amount* - Award amount (in US dollars)
8.  *id* - Award ID number (or any way of uniquely identifying duplicates from one source)
9.  *title* - Project title or description
10. *keyword* - The searched keyword that returned this result
11. *source* - The source the result came from

## Including the Source in `awardFindR()`

Source routines are included in the `awardFindR()` function through the _sources_ argument. Include the name of your new source (the section before "_standardize") in the default value of the _sources_ for `awardFindR()` in `main.R`. This will include your new source routine in the default functionality, and expose the name in the documentation where users are likely to discover it.

## Adding Tests

The main tests in `test-full.R` attempt to exercise both the successful and no results code branches. The latter always works. Unfortunately, the default search for the former (the terms "qualitative analysis" and "ethnography" in 2018) does not actually return results for some smaller sources. For this and other reasons, it's a good idea to create a source-specific test for the `_standardize` function.

Tests should actually return results, but should also minimally tax the API. Less than 10 results would be ideal. There is one exception to this: when a source needs to loop through multiple pages. In this case, the smallest number of results that triggers the looping is ideal, in order to ensure maximum test coverage.

One test should verify reproducibility; i.e. we get the same results again for a query in a specified date range. When used with a real-world HTTP resource, this should alert us if there is a change in how the resource is provided.

Finally, HTTP requests for tests should be cached using the ROpenSci package [`vcr`](https://docs.ropensci.org/vcr/). Individual sources should have their own cassettes. This limits stress on the APIs from frequent testing of internal logic, especially by the continuous integration system. Failure to do this could potentially result in service-level bans if we're not careful.
