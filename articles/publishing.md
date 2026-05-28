# Publishing

## Overview

There are several different ways to share to share your custom dashboard
online, including shinyapps.io, posit connect, and shiny server. For
smaller organizations, I recommend
[shinyapps.io](https://www.shinyapps.io/) because it is straightforward
to use and the free tier includes 25 hours of active use per month.

## Uploading to Shinyapps.io

1.  Make an account on [shinyapps.io](https://www.shinyapps.io/)
2.  Run `data-raw/08_launch.R`
3.  Shinyapps will host the dashboard as a standalone website. You can
    find the link to the dashboard under “Recent Applications” on the
    main page, or under Applications \> All.

## Embedding

WQdashboard can be embedded in any other website with an
[iframe](https://www.w3schools.com/tags/tag_iframe.asp). Simply copy the
code below and replace “LINK” with the link to your dashboard. The
height, width, and title can be adjusted as needed.

``` html
<iframe src="LINK" title="Water Quality Dashboard" width="100%" height = "800"></iframe>
```

WQdashboard will load whenever the page it is embedded on is opened. If
you are concerned about using too many active hours, embed WQdashboard
on its own page instead of the homepage.
