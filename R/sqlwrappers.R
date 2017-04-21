# Several simple wrappers around functions originally in dplyr that have
# moved to dbplyr with dplyr 0.6.0. Each takes one argument and conditionally
# calls the corresponding function in the appropriate d(b)plyr package.
# See http://dplyr.tidyverse.org/articles/compatibility.html

ident <-
function(obj_name)
{
    if (utils::packageVersion("dplyr") > "0.5.0") {
        dplyr::check_dbplyr()
        dbplyr::ident(obj_name = obj_name)
    }
    else {
        dplyr::ident(obj_name = obj_name)
    }
}

build_sql <-
function(obj_name)
{
    if (utils::packageVersion("dplyr") > "0.5.0") {
        dplyr::check_dbplyr()
        dbplyr::build_sql(obj_name = obj_name)
    }
    else {
        dplyr::build_sql(obj_name = obj_name)
    }
}

translate_sql <-
function(obj_name)
{
    if (utils::packageVersion("dplyr") > "0.5.0") {
        dplyr::check_dbplyr()
        dbplyr::translate_sql(obj_name = obj_name)
    }
    else {
        dplyr::translate_sql(obj_name = obj_name)
    }
}
