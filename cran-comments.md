## Resubmission
This is a resubmission. In this version I have:

- Added \value to all exported functions where this has been missing before
- Changed all \dontrun{} to \donttest{} as all of these can be run but just take longer than 5 seconds
- Removed all par() in the test files in order not to change a users' options()

## R CMD Checks

There were no ERRORs or WARNINGs.

There was two NOTEs: 

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Moritz Schwarz <moritz.schwarz@scmo.eu>'

This is expected as this is the first submission of this package.

*Possibly misspelled words in DESCRIPTION:
     Pretis (15:292)
     Schwarz (15:303)
     operationalising (15:224)
     
The first two are names of the authors. Both are correct. The final one was confirmed to be correct using the Oxford Dictionary and using British English.
