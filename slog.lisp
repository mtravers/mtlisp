(in-package :mt)

(defvar *slogging* nil)

(defvar *slog-output* *standard-output*)

(defmacro slog (condition format &rest args)
  `(when *slogging*
     (format *slog-output* "~%slog> ")
     (format *slog-output* ,format ,@args)))
