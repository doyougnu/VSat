Require Export CC.
Require Export K3.

Definition toK3 (l : CC.vprop) : k3.k3 :=
  match l with
  | CC.true => k3.true
  | CC.false => k3.false
  | CC.chc d l r => k3.both
  end.

Theorem and_commut_diag : forall p q : CC.vprop,
    toK3 (CC.andv p q) = k3.andk (toK3 p) (toK3 q).
  Proof.
    intros. destruct p.
    - destruct q.
      + reflexivity
      + reflexivity.
      + reflexivity.
      + reflexivity.
    - destruct q.
      + reflexivity.
      + reflexivity.
      + reflexivity.
    - destruct q.
      + reflexivity.
      + reflexivity.
      + reflexivity.
  Qed.

Theorem or_commut_diag : forall p q : CC.vprop,
    toK3 (CC.orv p q) = k3.ork (toK3 p) (toK3 q).
  Proof.
    intros. destruct p.
    - destruct q.
      + reflexivity
      + reflexivity.
      + reflexivity.
      + reflexivity.
    - destruct q.
      + reflexivity.
      + reflexivity.
      + reflexivity.
    - destruct q.
      + reflexivity.
      + reflexivity.
      + reflexivity.
  Qed.

Theorem neg_commut_diag : forall p : CC.vprop,
    toK3 (CC.negv p) = k3.negk (toK3 p).
  Proof.
    intros. destruct p.
    - reflexivity.
    - reflexivity.
    - reflexivity.
  Qed.