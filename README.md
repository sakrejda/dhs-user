# Demographic Health Survey (DHS) comfort code.

Some code to make Demographic Health Survey data easier to use so you can look
up what data they have on, e.g.-Kenya, without remembering the country code.  
The R package is 'pdhs' and there's an example in 'inst/examples'.  There's 
also some fragile code for downloading recode manuals and such.

Ideally some of this might work against their API at some point but that's 
not an immediate goal.

Install for the moment using devtools:

```
 devtools::install_github(repo='sakrejda/dhs-user', subdir='pdhs')
```



