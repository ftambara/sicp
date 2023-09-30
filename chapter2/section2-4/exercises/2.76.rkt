#lang racket


;; Changes that must be made to add types and operations:

;; Generic operations with explicit dispatch
;;  For each new type, all generic operations that will be used on it
;;  need to be modified to dispatch to the new operations.
;;  For each new generic operation, all existing types that the
;;  procedure is intended to be used with must be considered.

;; Data-directed programming
;;  Each new type must define its own versions of the generic
;;  operations and register them.
;;  New operations must be added to each type operation definition
;;  and registration procedures.

;; Message passing
;;  New types result in a new constructor, with corresponding versions
;;  of existing operations.
;;  To add a new operation, all existing dispatches must be modified
;;  to understand the new message.


;; Most appropriate for:

;; Adding types
;;  Adding types is easy with either data-directed or message passing
;;  styles, since new code is clumped together and existing code is
;;  not affected.

;; Adding operations
;;  Paradoxically, the seemingly unconfortable method of delegating
;;  dispatch responsibility to operations seems the most flexible for
;;  adding new operations, since existing code suffers no
;;  modifications.
;;  At second place, though, data-directed programming may be more
;;  explicit than the message passing style about where the new
;;  procedure code should go.
