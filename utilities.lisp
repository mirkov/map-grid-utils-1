(in-package :map-grid-utils)

(defun affis-congruent-p (&rest affis)
  "Return t if all (rest AFFIS) are congruent with (first AFFIS)

Return NIL and INDEX when it finds the first non-congruent affi.  It
does not check the remaining AFFIS.  INDEX is the index of the first non-conforming AFFI.

INDEX=0 refers to (first AFFIS)"
  (loop for first = (first affis)
     for this-affi in (rest affis)
     for index = 1
     unless (affi:check-conformability (first affis) this-affi)
     do (return-from affis-congruent-p (values nil index)))
  t)