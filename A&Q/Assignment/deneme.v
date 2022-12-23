Require Import ZArith.



Definition mirror {A: Type} (R: A -> A -> Prop) := 
  forall x y, x = y <-> exists z: A, R x z /\ R z y.

Definition RZ: (Z -> Z -> Prop) := fun x y => y = Z.mul (-1) x.

Lemma two_a: mirror RZ.
Proof.
  unfold mirror, RZ.
  intros x y .
  split.
  - intros. subst. exists y.
    split.
    + destruct y.
      * simpl. reflexivity.
      *simpl. reflexivity.
       rewrite <- Z.mul_opp_r.
        rewrite <- Z.mul_1_l.
        reflexivity.
      * simpl.
        rewrite <- Z.mul_assoc.
        rewrite <- Z.mul_1_l.
        reflexivity.
  - intros [z [H1 H2]].
    destruct z.
    + inversion H1.
      inversion H2.
      reflexivity.
    + inversion H1.
      inversion H2.
      reflexivity.
    + inversion H1.
      inversion H2.
      reflexivity.
Qed.