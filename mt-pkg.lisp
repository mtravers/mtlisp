(in-package :cl-user)

#| ######################################################################

 Package definition for Mike Travers' utilities.

Copyright (c) 1994-97 Michael Travers

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
  (:use :cl)				
  (:export ; from mt-utils.lisp
           "DOSEQUENCE" "EXTREME" "EXTREMES" "MAXIMIZE" "MINIMIZE" "MAXIMUMS" "MINIMUMS" "RANDOM-ELEMENT"
	   "NAMED-LAMBDA" "SYMBOLIZE"
           "DO-FOR-ARRAY-ELEMENTS" "CIRCULAR-LIST" "PUSH-END" "POP-END" "PUSHNEW-END" "FRINGE"
           "MAPAPPEND" "MAPUNION" "MAPCROSS" "MAPSUBSETS" "MAPSUM" "MAPTREE" "FLATTEN"
	   "UNION*" "INTERSECTION*"
	   "SPLIT-LIST" "FILTER" "FILTER-OUT" "FIND-ALL" "GROUP"
           "COLLECTING" #-:ABCL "COLLECT" "COLLECT-NEW" "COLLECT-IF"
	   "SUMMING" "SUM"
           "STRING+" "STRING-JOIN" "STRING-REPLACE" "PARSE-SUBSTRINGS" "FAST-STRING" "FAST-TEMP-STRING" "STRING-PREFIX-EQUALS" "PUSH-STRING" "STRING-REMOVE-CHARS" "STRING-UPPER-CASE-P"
	   "STRING-TRUNCATE" "STRING-TRUNCATE-TO-WORD-BOUNDARY"
	   "TRANSLATE-STRING" "NTRANSLATE-STRING" "VECTOR->STRING"
           "DELETEF" "REMOVEF" "REPLACEF" "ONCE-ONLY"
           "MV-LET*" #-CCL "LET-GLOBALLY"
           "DATE-TIME-STRING" "SHORT-DATE-TIME-STRING" "FORMAT-TIME"
           "DEF-CACHED-FUNCTION" "DEF-CACHED-FUNCTION-1"
           #-:ABCL "DEFSUBST"
           "TEST-DEFUN"
           "^" "SIGN" "ABS-MAX" "INTEGERS" "ARAND" "ABSMIN" "SINGLE-PI" "DEGREES-TO-RADIANS" "RADIANS-TO-DEGREES" "D2R" "D2RI" "R2D"
           "+&" "-&" "INCF&" "DECF&" "1+&" "1-&" "*&" "/&"
           "=&" ">&" "<&" ">=&" "<=&" "MAX&" "MIN&" "ZEROP&" "PLUSP&" "MIN)SP&"
           "LOGAND&" "LOGIOR&" "LOGNOT&" "LOGANDC1&" "LOGXOR&"
           "CAR&" "CDR&" "SVREF&" "AREF&" "SCHAR&"
           "LOG2" "NUMBER-OF-BITS" "AVERAGE" "STD-DEV" "GEO-MEAN" "NTH-ROOT"
           "PI/2" "PI/4" "2PI"
	   "COERCE-NUMBER" "COERCE-INTEGER"
           #-:ABCL "AIF" "AWHEN" "AAND" "ACOND" "ALAMBDA" #-:ABCL "IT" "SELF"
;          "AIF" "AWHEN" "AAND" "ACOND" "ALAMBDA"  "IT" "SELF"
           "SYMBOL-CONC" "SYMBOL-CONC*" "KEYWORDIZE" "UP-KEYWORDIZE"
           "EXTERNALP" "ADD-NICKNAME"
           "STREAM-COPY" "DOLINES" "STREAM-COPY-BY-LINES" "FILE-COPY" "FILE-TO-STRING" "RELATIVE-PATHNAME" "DUMB-RELATIVE-PATHNAME" "THIS-PATHNAME"
           #-:ABCL "SUBCLASSES" #-:ABCL "SUPERCLASSES"
           "PLIST-MIXIN" "OGET" "OPUT" "CALL-IF"
	   "DOTREE" "DOTREE-ALL"
           "SET-EQUAL" "STABLE-NSET-DIFFERENCE" "CURRY" "RCURRY" "TRANSITIVE-CLOSURE" "TRANSITIVE-CLOSURE-PROCEDURE"
	   "LISTIFY" "UNLISTIFY"
	   "FINDPROP" "ASSOCDR" "ASSOCADR"
           "DUMP-HT" "HT-CONTENTS" "HASH-KEYS"
           "DUMP-VARS-TO-FILE" "DUMP-VAR-TO-FILE"
           "DEFBIT"
           "FIRSTN" "SUBSEQ-SAFE" "BREAK-LIST"
	   "ITRACE" "REPORT-AND-IGNORE-ERRORS"
	   "NOW" 
	   "WITHOUT-INTERRUPTS" "IN-BACKGROUND"
           "READ-UNTIL" "READ-UNTIL-STRING" "NEW-STRING"
           "STRING-TRIM-WHITESPACE" "STRING-REMOVE-WHITESPACE" "FAST-WHITESPACEP"
           "STRING-SPLIT"
	   "ONE-STRING" "DOC"
           *WHITESPACE*
	   "DELETE-KEYWORD-ARGS" "DELETE-KEYWORD-ARG"
	   "PLET" "PLET*" "PCOND"
           ;; CTRACE
           "CTRACE" "WITH-CTRACE" "CTRACE-FUNCTION" "UNCTRACE-FUNCTION" "CTRACE-ALL" "CT"
           "WITH-CTRACING" "START-CTRACE" "STOP-CTRACE" "VIEW-CTRACE" "*LAST-CTRACE*"
           "CALLING" "RETURNED"    ; words used by ctrace-function - sigh
	   "UNIX-CURRENT-TIME"
	   "MAPSEQUENCE"
           )
  )


