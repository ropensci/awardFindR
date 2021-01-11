# QDR awardsBot

The QDR awardsBot scrapes a variety of grant databases for specific keywords. 

## How the bot works

The `awardsBot()` routine is meant to be the simplest way to reproduce the data collection. This routine uses they keywords listed in a .csv file (one term per line) and scrapes all supported databases. These queries are date-limited to the previous two years. **NOTE:** even though this only covers two years, this can be very heavy on API queries! Ideally, this shouldn't be run during business hours, or even during weekdays.

`awardsBot()` has parameters to change sources, keywords and dates as search criteria. See included help and vignettes.

## Dependencies

This package depends on `xml2` and `httr`.

## Contributor guidelines

### Adding support for new sources

The best way to add support for new sources would be to contribute any routine that generates a data.frame from an internet-based source. The basic logical structure of the program is that the `apis.R` routines handle the raw variable names from the sources, so simply returning the data as available is the first step. A query with no results should return a null value. 

Next, integrating the routine into the `apis.R` glue code involves including an additional section as appropriate renaming the variables so that we can `rbind.data.frame()` everything together.

This package so far has preferred handling with APIs through xml rather than json, though this is totally discretionary within each source routine.
