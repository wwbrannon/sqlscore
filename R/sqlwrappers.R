# Several simple wrappers around functions originally in dplyr that have
# moved to dbplyr with dplyr 0.6.0. Each takes one argument and conditionally
# calls the corresponding function in the appropriate d(b)plyr package.
# See http://dplyr.tidyverse.org/articles/compatibility.html

get_sql <-
function()
{
    if (utils::packageVersion("dplyr") > "0.5.0")
    {
        dplyr::check_dbplyr()
        fn <- dbplyr::sql
    }
    else
    {
        fn <- dplyr::sql
    }

    fn
}


get_ident <-
function()
{
    if (utils::packageVersion("dplyr") > "0.5.0")
    {
        dplyr::check_dbplyr()
        fn <- dbplyr::ident
    }
    else
    {
        fn <- dplyr::ident
    }

    fn
}

get_build_sql <-
function()
{
    if (utils::packageVersion("dplyr") > "0.5.0")
    {
        dplyr::check_dbplyr()
        fn <- dbplyr::build_sql
    }
    else
    {
        fn <- dplyr::build_sql
    }

    fn
}

get_translate_sql <-
function()
{
    if (utils::packageVersion("dplyr") > "0.5.0")
    {
        dplyr::check_dbplyr()
        fn <- dbplyr::translate_sql
    }
    else
    {
        fn <- dplyr::translate_sql
    }

    fn
}
