# Default UI dropdown values

A list of default values for the various dropdown menus in the sidebar
and on the graph tab.

## Usage

``` r
varlist
```

## Format

A list with 13 columns:

- state:

  List of states

- town:

  List of towns

- watershed:

  List of watersheds

- site_id:

  List of site IDs

- site_name:

  List of site names

- loc_choices:

  List of toggle options. Possible values: "blank", "By Town" = "town",
  "By State" = "town", "By Watershed" = "watershed"

- loc_tab:

  Name of hidden tab for importwqd:::mod_sidebar_location. Possible
  values: "notoggle", "toggle", "blank"

- param:

  List of parameters

- param_score:

  List of parameters that have been assigned a score

- param_cat:

  List of download only/categorical parameters

- depth:

  List of depths. Possible values: Surface, Midwater, Near Bottom,
  Bottom

- year:

  List of years

- month:

  List of months
