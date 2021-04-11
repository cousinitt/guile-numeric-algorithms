(use-modules (srfi srfi-1))

(define install-debug #t)

(define (split-path fn)
  "Split a unix path into a list of strings. Ignores '.' and repeated '/'."
  (filter (lambda (x)
            (not (or (string=? x ".")
                     (string=? x ""))))
          (string-split fn #\/)))


(define (ensure-directory dir mode)
  "Make sure a directory named DIR exists.

If directories need to be created for that to be true, use MODE as the access
mode for these new directories. Should any part of the directory be an existing
file that is not a directory, the procedure errors out."
  ; The desired directory path is broken up into a list by 'split-path'.
  ; The fold function operates on the list from the left, concatenating the items together with '/'.
  ; A check is made if the file exists, and if not, a directory is made there.
  ; If the path exists, and is a file, the function returns with an error.
  ; There is an else ... I am not sure what winds up there.
  ;
  ; The accumulator for the fold is the concatenated elements so far.
  (fold (lambda (x acc)
          (format #t "acc: ~s~%" acc) ; FIXME
          (let ([cur (string-concatenate (list acc "/" x))])
            (cond [(not (file-exists? cur))
                   (if install-debug
                       (format #t "  ..mkdir ~o ~s~%" mode cur)
                       (mkdir cur mode))]
                  [(not (is-directory? cur))
                   (error "File exists, but is not a directory: ~s~%" cur)]
                  [else #f])
            cur))
        ""
        (split-path dir)))

;------------------------------------------------------------------------------;
; The logic in that file did:
; Constants:
;   +) debug install flag

;   +) man page location and where in man it should be

;   +) documentation files
;   +) documentation install flag
;   +) documentation install prefix, logic, which defaulted to '/usr/local'

;   +) DESTDIR was an environment variable
;   +) ? makefile destdir

; Functions:
;   +) install a file:
;   The $DESTDIR environment variable.
;   The *.go files are put in (%site-ccache-dir).
;   The *.scm files go in (%site-dir)

;   +) install manual file. These go in:
;   (string-concatenate "/usr/local" "/share/man/")

;   +) install documentation file. These go in:
;   (string-concatenate "/usr/local" "/share/doc/guile-yourproject")
; 




















