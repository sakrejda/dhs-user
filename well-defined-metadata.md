# ReRecode

Metadata for recoding DHS-recode columns.  The .yaml file is much 
easier to read if you open it in an editor like 'RStudio' or 'atom'
that will properly highlight text.

## Short descriptions:

1. name: "name of the column we generate, avoid acronyms in stage 1, any
         acronyms must be obvious from the 'definition' field."
2. stage: 
    1. columns generated from coded DHS columns, potentially defined
       differently in different surveys.
    2. columns generated only from stage 1 columns or DHS columns that
       are constant across all surveys.
3. definition: definition of the generated column, this should capture
               be verbose enough to capture different treatment of different
               surveys"
4. values:
    * boolean: TRUE/FALSE in R, 0/1 in .csv, etc...
    * real: "numeric" in R, decimal in .csv, etc..
    * integer: "numeric" in R, integers in .csv, etc...
5. units: "for real/integer values the unit being tracked" 



