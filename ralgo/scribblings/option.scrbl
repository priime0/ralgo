#lang scribble/manual

@(require ralgo/option (for-label racket))

@title[#:style 'toc]{Options}

@author[(author+email "Lucas Sta Maria" "lucas@priime.dev")]

@(define (short-version) (car (regexp-match #px"^\\d+.\\d+" (version))))

@defmodule[ralgo/option]

An @deftech{option} is an explicit representation of an optional value: a value that can either be something or nothing. In particular, an option is either a @racket[some] struct containing anything, or a @racket[none] struct containing nothing.

@section{Options Constructors and Selectors}

@defproc[(none) none?]{

Returns a @racket[none] struct representing a None value. The @racket[none] struct does not receive any arguments.}

@defproc[(none? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @racket[none] struct, @racket[#f] otherwise.}

@defproc[(some [v any/c]) some?]{

Returns a @racket[some] struct representing a Some value containing @racket[v].}

@defproc[(some? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @racket[some] struct, @racket[#f] otherwise.}
