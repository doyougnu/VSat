(* Kleenes strong logic in Coq *)

Require Import Bool.
Require Import Strings.String.
Require Import Relations.Relation_Definitions.
Require Import Classes.Morphisms.
Require Import Setoids.Setoid.

Inductive k3: Set :=
  | true3
  | both3
  | false3.

Definition andk3 (a b : k3) :=
  match a, b with
    | true3, true3 => true3
    | true3, both3 => both3
    | true3, false3 => false3
    | both3, true3 => both3
    | both3, both3 => both3
    | both3, false3 => false3
    | false3, true3 => false3
    | false3, both3 => false3
    | false3, false3 => false3
  end.

Definition ork3 (a b : k3) :=
  match a, b with
    | true3, true3 => true3
    | true3, both3 => true3
    | true3, false3 => true3
    | both3, true3 => true3
    | both3, both3 => both3
    | both3, false3 => both3
    | false3, true3 => true3
    | false3, both3 => both3
    | false3, false3 => false3
  end.

Definition negk (a : k3) :=
  match a with
    | true3 => false3
    | both3 => both3
    | false3 => true3
  end.

Definition imp (a b : k3) :=
  match a, b with
    | true3, true3 => true3
    | true3, both3 => both3
    | true3, false3 => false3
    | both3, true3 => true3
    | both3, both3 => both3
    | both3, false3 => both3
    | false3, true3 => true3
    | false3, both3 => true3
    | false3, false3 => true3
  end.
