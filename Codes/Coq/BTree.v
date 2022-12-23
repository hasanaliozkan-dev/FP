
Require Import Nat Arith Bool.

Inductive BTree (a: Type): Type :=
 | Empty: BTree a
 | Node : a -> BTree a -> BTree a -> BTree a.

Arguments Empty {_}.
Arguments Node {_} _ _ _.

Fixpoint height {a: Type} (t: BTree a): nat :=
  match t with
    | Empty      => 0
    | Node n l r => max (height l) (height r) + 1
  end.

Fixpoint perfect {a: Type} (t: BTree a): Prop :=
  match t with
    | Empty      => True
    | Node n l r => (height l = height r) /\ (perfect l) /\ (perfect r)
  end.

Fixpoint size {a: Type} (t: BTree a): nat :=
  match t with
    | Empty      => 0
    | Node n l r => size l + size r + 1
  end.


Lemma helper1: forall (a: nat), pow 2 a + pow 2 a = pow 2 (a+1).
Proof. intro a.
       induction a.
       - simpl. reflexivity.
       - simpl.
         rewrite <- !plus_n_O.
         rewrite IHa.
         reflexivity.
Qed.

Lemma helper2: forall (a: nat),
  1 <= pow 2 a.
Proof. intro a.
       induction a.
       - simpl. 
         apply Nat.le_refl.
       - simpl.
         rewrite <- plus_n_O.
         specialize (le_plus_trans 1 (pow 2 a) (pow 2 a)).
         intro H.
         apply H.
         exact IHa.
Qed.

Theorem perfectness: forall {a: Type} (t: BTree a),
  perfect t -> size t = pow 2 (height t) - 1.
Proof. intros a t.
       induction t.
       - intro Ha.
         simpl.
         reflexivity.
       - intro Ha.
         simpl in Ha.
         destruct Ha as (Ha, (Hb, Hc)).
         specialize (IHt1 Hb).
         specialize (IHt2 Hc).
         simpl.
         rewrite IHt1.
         rewrite IHt2.
         rewrite Ha.
         rewrite Nat.max_id.
         rewrite <- plus_assoc.
         rewrite Nat.sub_add.
         rewrite <- Nat.add_sub_swap.
         rewrite helper1.
         reflexivity.
         apply helper2.
         apply helper2.
Qed.
         

  
  