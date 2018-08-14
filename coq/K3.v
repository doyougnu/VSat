(* Kleenes strong logic in Coq *)

Require Import Bool.
Require Import Strings.String.
Require Import Relations.Relation_Definitions.
Require Import Classes.Morphisms.
Require Import Setoids.Setoid.

Module k3.

Inductive k3: Set :=
  | true
  | both
  | false.

Definition andk (a b : k3) :=
  match a, b with
    | true, true => true
    | true, both => both
    | true, false => false
    | both, true => both
    | both, both => both
    | both, false => false
    | false, true => false
    | false, both => false
    | false, false => false
  end.

Definition ork (a b : k3) :=
  match a, b with
    | true, true => true
    | true, both => true
    | true, false => true
    | both, true => true
    | both, both => both
    | both, false => both
    | false, true => true
    | false, both => both
    | false, false => false
  end.

Definition negk (a : k3) :=
  match a with
    | true => false
    | both => both
    | false => true
  end.

Definition imp (a b : k3) :=
  match a, b with
    | true, true => true
    | true, both => both
    | true, false => false
    | both, true => true
    | both, both => both
    | both, false => both
    | false, true => true
    | false, both => true
    | false, false => true
  end.

Theorem andk_commut : forall p q : k3,
  andk p q = andk q p.
  Proof.
    intros. destruct p as [].
    - destruct q as [].
      + trivial.
      + trivial.
      + simpl. reflexivity.
    - destruct q as [].
      + trivial.
      + trivial.
      + reflexivity.
    - destruct q as [].
      + simpl. reflexivity.
      + reflexivity.
      + simpl. reflexivity. Qed.

End k3.