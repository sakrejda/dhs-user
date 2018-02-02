# Demographic Health Survey (DHS) comfort code.

Some code to make Demographic Health Survey data easier to use so you can look up what data they have on, e.g.-Kenya, without remembering the country code. 
 
The R package is 'pdhs' and there's an example in 'inst/examples'.  There's also some fragile code for downloading recode manuals and such.  Ideally some of this might work against their API at some point but that's not an immediate goal.

Install for the moment using devtools:

```
 devtools::install_github(repo='sakrejda/dhs-user', subdir='pdhs')
```

The package expects to be able to write some data in it's install directory (yes that's sub-optimal, no I'm not fixing it now) so install it in a local library rather than a system-wide location.

There are also examples in `pdhs/inst/examples` in the repo and after you install
you can find them by doing:

```
dir(pdhs:::examples(), full.names=TRUE)
```

They're pretty simple and decently commented. The `extract.R` example has a basic workflow and the other examples go through some use of the helper functions.

