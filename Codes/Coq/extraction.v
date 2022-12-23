Require Import PeanoNat Bool.

Lemma double: forall(a:nat) , a+a = 2*a.
Proof.
      intros.
      case_eq a.
      -intro Ha.
      simpl. reflexivity.
      -intros n Hn. simpl.
      rewrite <- plus_n_0. reflexivity.