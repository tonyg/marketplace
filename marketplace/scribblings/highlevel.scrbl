#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title{High-level interface}

@section{#lang marketplace}
**** ground-vm, ground-vm:
@section{Constructing topics and roles}
**** ?
**** Role
**** Orientation
**** InterestType
@section{Constructing transitions}
**** transition, transition:, transition/no-state
**** cons-trees of actions; null, false, void; use of (when)
**** sequence-actions
@section{Actions}
**** Communication-related
***** endpoint, endpoint:
***** delete-endpoint
***** send-message
***** send-feedback
**** Process- and scheduling-related
***** spawn, spawn:
***** quit
***** yield, yield:
***** nested-vm, nested-vm:
**** Cross-layer
***** at-meta-level, at-meta-level:
