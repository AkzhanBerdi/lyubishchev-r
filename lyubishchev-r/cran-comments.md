## Test environments
* local R 4.3.3, R CMD check --as-cran: 0 errors | 0 warnings | 3 notes

## R CMD check results

There were no ERRORs or WARNINGs.

Notes:

* This is a new release, so "CRAN incoming feasibility" flags a new
  maintainer (Akzhan Berdeyev <akzhan.berdeyev@gmail.com>). This is expected.

* The same note reports the URL
  http://www.zin.ru/animalia/coleoptera/rus/lyubis05.htm as "403 Forbidden".
  The URL is correct and valid in a browser; the host (ZIN RAS Coleoptera
  Laboratory archive) rejects automated HEAD/GET requests. It is the canonical
  primary source for Lyubishchev's 1943 manuscript and is cited deliberately.

* "future file timestamps" and the HTML-manual note (no 'tidy'/'V8' command)
  are artifacts of the local check environment, not the package.

## Downstream dependencies
There are currently no downstream dependencies.
