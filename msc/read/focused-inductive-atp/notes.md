---
title: "Notes: Focused Inductive Theorem Proving"
pdf: "focused-inductive-atp.pdf"
---

# Focused Inductive Theorem Proving

Discusses proof search using the _intuitionistic sequent calculus_, system
mu-LJ.

To make proof search tractable, connectives are organized into two
_polarities_:

* asynchronous connectives: rules with invertible right-introduction forms.

  * $\forall$, $\supset$, $\nu$.

* synchronous connectives: rules with invertible left-introduction forms.

  * $\land$, $\lor$, $\exists$, $=$, $\mu$.

A fixed-point formula can be _frozen_, which turns it into an indivisible
_atom_ that is neither asynchronous nor synchronous.

Synchronous connectives are actually synchronous _on the right_. When a
synchronous connective appears on the left, it is treated as _asynchronous_.
The converse also applies: an asynchronous formula is treated as _synchronous_
when it appears on the left.

Hence, the _asynchronous_ phase contains _unfocused_ sequents and introduces
both _asynchronous_ connectives on the _right_ and _synchronous_ connectives on
the _left_.
Similarly, the _synchronous_ phase contains _focused_ sequents and introduces
both _synchronous_ connectives on the _right_ and _asynchronous_ connectives on
the _left_.

When the focus is on the _right_, only the toplevel _synchronous_ connectives
of the focused formula can be introduced.
When the focus is on the _left_, only the toplevel _asynchronous_ connectives
of the focused formula can be introduced.

A phase change occurs only when no rule appropriate to the current phase is
allowed.

The asynchronous phase ends when:

* no synchronous formula remains on the left.

The synchronous phase ends when:

* the focus is on the left on a synchronous formula, or
* the focus is on the right on an asynchronous formula.

Each fixed-point has two rules per phase:

* one treats the fixed-point transparently, as a structured formula; and
* the other treats the fixed-point opaquely as an indivisible atom.

The unfolding and initial rules for fixed-points are _synchronous_.
The (co)induction and freezing rules are _asynchronous_.

A constraint is placed on the asynchronous phase: any LFP hypothesis (or GFP
conclusion) must be used _immediately_ for (co)induction or be frozed, so it
can never be used for induction again.
After freezing a (co)inductive hypothesis, it can be used only for the init
rule at a later time.

Remark: if we focus on a full synchronous fixed-point, then the focus can never
be released. Hence, the proof must be completed in this phase.

A least fixed-point instance $\mu B \mathbf{t}$ has a _progressing unfolding_
if at most one pattern in the body $B$ is matched by $\mathbf{t}$.
This is important because performing a progressing unfolding of an LFP is an
invertible operation, and so it can be performed eagerly in the asynchronous
phase.

The asynchronous phase proceeds as follows:

1. apply non-backtracking asynchronous rules.
2. for all remaining asynchronous fixed-poitns, try to:

  * freeze,
  * (co)induct, or
  * unfold (non-progressing)

  and return to step 1. These choices are tried in order, and backtracked out
  of when they fail.
