## Resubmission

Submission for version 0.2.1

### General changes in version 0.2.1: 

This is an update to extend the package with a new method to enable further analysis tools. 

- Added a new method to `isatpanel()`
- This necessitated a few changes throughout the package, including all plotting functions
- I also enabled more specific control of subsetting my methods by id and time in `isatpanel()`. For this, I added quite extensive testing. 
- I also implemented codecov to gain a better understanding of code testing coverage in the future. 

## R CMD Checks

devtools::check_rhub() is currently throwing an error which I don't believe is related to this package: "SSL peer certificate or SSH remote key was not OK: [builder.r-hub.io] schannel: SEC_E_UNTRUSTED_ROOT (0x80090325) - The certificate chain was issued by an authority that is not trusted."
