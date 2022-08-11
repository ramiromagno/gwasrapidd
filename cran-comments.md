## R CMD check results

0 errors | 0 warnings | 0 note

## R-hub check results

All okay except this NOTE in _Fedora Linux, R-devel, clang, gfortran_:

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

I believe this is an issue related to the testing environment and not so much
with the package itself.

## Examples taking > 5s

I believe that all issues related with the NOTEs
_Examples with CPU (user + system) or elapsed time > 5s_
should be fixed by now.
