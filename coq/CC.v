(* Simple Choice Calculus in Coq *)

Require Import Bool.
Require Import Strings.String.
Require Import Relations.Relation_Definitions.
Require Import Classes.Morphisms.
Require Import Setoids.Setoid.

Inductive vprop : Type :=
  | lit : bool -> vprop
  | chc : string -> vprop -> vprop -> vprop.

(* Negation of a vprop is identical for C2 except for choices it inverts the
choice *)
Fixpoint negv (p : vprop) : vprop :=
  match p with
  | lit b => lit (negb b)
  | chc d l r => chc d r l
  end.

Fixpoint andv (l r :vprop) :=
  match l with
    | lit false => lit false
    | lit true => match r with
                | lit false => lit false
                | lit true => lit true
                | chc d l'' r' => chc d l'' r'

               end
    | chc d l' r' => match r with
                      | lit false => lit false
                      | lit true => chc d l' r'
                      | chc d' l'' r'' => chc d
                                             (chc d' (andv l' l'') (andv l' r''))
                                             (chc d' (andv r' l'') (andv r' r''))
                    end
  end.

Fixpoint orv (l r :vprop) :=
  match l with
    | lit true => lit true
    | lit false => match r with
                | lit false => lit false
                | lit true => lit true
                | chc d l'' r' => chc d l'' r'

               end
    | chc d l' r' => match r with
                      | lit true => lit true
                      | lit false => chc d l' r'
                      | chc d' l'' r'' => chc d
                                             (chc d' (orv l' l'') (orv l' r''))
                                             (chc d' (orv r' l'') (orv r' r''))
                    end
  end.


Fixpoint impv (l r :vprop) := negv (orv l r).
