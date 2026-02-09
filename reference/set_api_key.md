# Set or change the Planet API key

Prompts the user to enter a Planet API key and saves it in a hidden
`.env` file in the working directory.

## Usage

``` r
set_api_key(change_key = F)
```

## Arguments

- change_key:

  Logical. If `TRUE`, prompts for a new API key even if one already
  exists (default: `FALSE`).

## Value

Invisibly returns the API key.

## Note

You will need an active Planet account and an API key to access the
PlanetScope API. You can sign up for an account on the [Planet
website](https://www.planet.com/get-started/). Once you have an account,
you can copy your API key from your account settings.

## Examples

``` r
if (FALSE) { # \dontrun{
set_api_key() # Set the API key for the first time
set_api_key(change_key = TRUE) # Change the API key
} # }
```
