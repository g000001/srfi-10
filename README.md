# SRFI-10 for CL: #, external form

* https://srfi.schemers.org/srfi-10/
* License: Unlicense

```cl
(enable-read-time-application)
```

```cl
(disable-read-time-application)
```

Example:

```cl
(define-reader-ctor '+ #'cl:+)

#,(+ 3 #,(+ #,(+ 3 3) 3))
;=> 12
```




