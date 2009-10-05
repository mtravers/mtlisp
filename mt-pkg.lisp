(in-package :cl-user)

#| ######################################################################

 Package definition for Mike Travers' utilities.

Copyright © 1994-97 Michael Travers

Permission is given to use and modify this code
as long as the copyright notice is preserved.

Send questions, comments, and fixes to mt@media.mit.edu.

-------------------------------------------------------------------------

 Defines the MT package, which some of the other files in this collection
 use.

 Probably not everything is exported that should be.
 11/6/07 -- update for ABCL, don't support some symbols that are used by AlanR stuff

###################################################################### |#

(defpackage :mt
  (:use  #+MCL :ccl :cl)
  (:export ; from mt-utils.lisp
           "DOSEQUENCE" "EXTREME" "EXTREMES" "MAXIMIZE" "MINIMIZE" "MAXIMUMS" "MINIMUMS" "RANDOM-ELEMENT"
	   "NAMED-LAMBDA" "SYMBOLIZE"
           "DO-FOR-ARRAY-ELEMENTS" "CIRCULAR-LIST" "PUSH-END" "PUSHNEW-END" "FRINGE"
           "MAPAPPEND" "MAPUNION" "MAPCROSS" "MAPSUM" "SPLIT-LIST" "FILTER" "FILTER-OUT"
           "COLLECTING" #-:ABCL "COLLECT" "COLLECT-NEW"
           "STRING+" "STRING-REPLACE" "PARSE-SUBSTRINGS" "TRUNCATE-STRING" "FAST-STRING" "FAST-TEMP-STRING" "STRING-TRUNCATE" "STRING-PREFIX-EQUALS" "PUSH-STRING" "STRING-REMOVE-CHARS"
           "DELETEF" "ONCE-ONLY"
           "MV-LET*" #-CCL "LET-GLOBALLY"
           "DATE-TIME-STRING" "SHORT-DATE-TIME-STRING" "FORMAT-TIME"
           "DEF-CACHED-FUNCTION"
           #-:ABCL "DEFSUBST"
           "TEST-DEFUN"
           "^" "SIGN" "ABS-MAX" "INTEGERS" "ARAND" "ABSMIN" "SINGLE-PI" "DEGREES-TO-RADIANS" "RADIANS-TO-DEGREES" "D2R" "D2RI" "R2D"
           "+&" "-&" "INCF&" "DECF&" "1+&" "1-&" "*&" "/&"
           "=&" ">&" "<&" ">=&" "<=&" "MAX&" "MIN&" "ZEROP&" "PLUSP&" "MINUSP&"
           "LOGAND&" "LOGIOR&" "LOGNOT&" "LOGANDC1&" "LOGXOR&"
           "CAR&" "CDR&" "SVREF&" "AREF&" "SCHAR&"
           "LOG2" "NUMBER-OF-BITS" "AVERAGE" "STD-DEV" "GEO-MEAN" "NTH-ROOT"
           "PI/2" "PI/4" "2PI"
	   "COERCE-NUMBER"
           #-:ABCL "AIF" "AWHEN" "AAND" "ACOND" "ALAMBDA" #-:ABCL "IT" "SELF"
;          "AIF" "AWHEN" "AAND" "ACOND" "ALAMBDA"  "IT" "SELF"
           "SYMBOL-CONC" "SYMBOL-CONC*" "KEYWORDIZE"
           "EXTERNALP" "ADD-NICKNAME"
           "STREAM-COPY" "FILE-COPY"
           #-:ABCL "SUBCLASSES" #-:ABCL "SUPERCLASSES"
           "PLIST-MIXIN" "OGET" "OPUT" "CALL-IF"
           "SET-EQUAL" "CURRY" "RCURRY"
           "DUMP-HT" "HT-CONTENTS"
           "DUMP-VARS-TO-FILE" "DUMP-VAR-TO-FILE"
           "DEFBIT"
           "FIRSTN" "REPORT-AND-IGNORE-ERRORS"
	   "TRANSITIVE-CLOSURE" "NOW"
           "READ-UNTIL" "READ-UNTIL-STRING" "NEW-STRING"
           "STRING-TRIM-WHITESPACE" "FAST-WHITESPACEP"
           "STRING-SPLIT"
	   "ONE-STRING" "DOC"
           *WHITESPACE*
           ;; from mcl-hacks.lisp -- I don't even have that file around anymore.
#|
           "EDIT-ANY" "DISASM" "SAVE-ALL-FILES"
           "AT-LISTENER-LEVEL" "IN-OWN-PROCESS" 
           "ERRCHECK" "CHECK-RESERR" "WITH-HANDLE"
           "WITH-RECT" "WITH-VIEW-RECT" "MAKE-GRAY" "MAKE-COLOR*" "MAKE-POLY"
           "WINDOW-EXPOSE" "BRING-TO-TOP"
           "SCALE-SUBVIEWS-MIXIN" "VIEW-SCALE" "VIEW-SHRINK-TO-FIT-SUBVIEWS"
           "VIEW-OVERLAPS?" "VIEWS-OVERLAP?" "POINT-MAX" "POINT-MIN" "POINT>"
           "VIEW-ADJUST-FOR-SUBVIEWS"
           "VIEW-CONTAINED-BY?" "VIEW-POSITION-RELATIVE"
           "VIEW-REAL-POSITION" "VIEW-REAL-POSITION-RELATIVE"
           "VIEW-HIERARCHY" "WINDOW-UNDER-POINT"
           "WARP-MOUSE" "RAW-MOUSE" "HIDE-MOUSE"
           "REALNAME" "REREQUIRE" "WILDCARD-COPY"
           "RREF*" "RESOURCEP" "RESOURCE-INFO" "ALL-RESOURCES-OF-TYPE"
           "WITH-TIMEOUT" "SET-TIMEOUT"
           "HERE" "REL-PATHNAME"
           "ADD-MENU-ITEM-AT"
           ;; Pixmap utils
           "COPYBITS"
           "PIXMAP-DIALOG-ITEM" "PICT-DIALOG-ITEM"
           "MAKE-GWORLD*" "WITH-GWORLD"
           "WINDOW-SNAPSHOT"
           "PICT->PIXMAP" "ARRAY->PIXMAP" "PIXMAP->ARRAY" "BITMAP->ARRAY"
           "ROTATE-PIXMAP" "PIXMAP-MASK"
           "NEW-PIXMAP" "MAKE-PIXMAP" "MAKE-BITMAP*" "KILL-BITMAP"
|#
           ;; Slogging
           "SLOG" "*SLOGGING*" "*SLOG-OUTPUT*"
#|  Moved to cl-user
           ;; CTRACE
           "CTRACE" "WITH-CTRACE" "CTRACE-FUNCTION" "UNCTRACE-FUNCTION" "CTRACE-ALL" "CT"
           "WITH-CTRACING" "START-CTRACE" "STOP-CTRACE" "VIEW-CTRACE" "*LAST-CTRACE*"
           "CALLING" "RETURNED"    ; words used by ctrace-function - sigh
|#
           ))


