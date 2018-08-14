(* Simple Choice Calculus in Coq *)

Require Import Bool.
Require Import Strings.String.
Require Import Relations.Relation_Definitions.
Require Import Classes.Morphisms.
Require Import Setoids.Setoid.

Module CC.

Inductive vprop : Type :=
  | true : vprop
  | false : vprop
  | chc : string -> vprop -> vprop -> vprop.

(* Negation of a vprop is identical for C2 except for choices it inverts the
choice *)
Fixpoint negv (p : vprop) : vprop :=
  match p with
  | true => false
  | false => true
  | chc d l r => chc d (negv l) (negv r)
  end.

Fixpoint andv (l r :vprop) :=
  match l with
    | false => false
    | true => match r with
             | false => false
             | true => true
             | chc d l'' r' => chc d l'' r'
             end
    | chc d l' r' => match r with
                      | false => false
                      | true => chc d l' r'
                      | chc d' l'' r'' => chc d
                                             (chc d' (andv l' l'') (andv l' r''))
                                             (chc d' (andv r' l'') (andv r' r''))
                    end
  end.

Fixpoint orv (l r :vprop) :=
  match l with
    | true => true
    | false => match r with
                | false => false
                | true => true
                | chc d l'' r' => chc d l'' r'

               end
    | chc d l' r' => match r with
                      | true => true
                      | false => chc d l' r'
                      | chc d' l'' r'' => chc d
                                             (chc d' (orv l' l'') (orv l' r''))
                                             (chc d' (orv r' l'') (orv r' r''))
                    end
  end.


Definition impv (l r :vprop) := negv (orv l r).

(* a literal false in an and expression makes the whole expression false *)
Theorem andv_f_f : forall p q : vprop,
    andv false q = false.
  Proof.
    intros. simpl. reflexivity. Qed.

(* and elimination for choices, if we have a lit true in the and we can just
return the other side*)
Theorem andv_elminination : forall p : vprop,
      andv true p = p.
  Proof.
    intros. induction p as [].
    - trivial.
    - trivial.
    - simpl. reflexivity. Qed.

Axiom chc_commut : forall d d' : string,
      forall l r l' r' : vprop,
      forall f : vprop -> vprop -> vprop,
 chc d (chc d' (f l l') (f l r')) (chc d' (f r l') (f r r'))
 = chc d' (chc d (f l' l) (f l' r)) (chc d (f r' l) (f r' r)).

Theorem andv_commut : forall p q : vprop,
  andv p q = andv q p.
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
      + simpl. rewrite chc_commut. reflexivity. Qed.

Theorem orv_commut : forall p q : vprop,
  orv p q = orv q p.
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
      + simpl. rewrite chc_commut. reflexivity. Qed.

End CC.