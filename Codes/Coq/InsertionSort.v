Require Import PeanoNat Bool List.



Fixpoint foldr {A B : Type} (f: A -> B -> B ) (b:B) (l: list A): B :=
  match l with
    |nil => b
    |cons x xs => f x (foldr f b xs)
end.



Fixpoint insert (a:nat) (l: list nat): list nat :=
  match l with
     | nil => cons a nil
     | cons x xs => if a <=? x
                    then cons a (cons x xs)
                    else cons x (insert a xs)
end.


Definition insertionSort := foldr insert nil.

Compute insertionSort (cons 3 (cons 5 (cons 1 (cons 2 nil)))).



Fixpoint isSorted (l: list nat): bool :=
   match l with
    | nil => true
    | cons x xs => 
        match xs with
          | nil => true
          |cons y ys => (x<=?y) && isSorted xs
        end
end.

Fixpoint smaller (x: nat) (l: list nat) : bool := 
  match l with
    |nil => true
    | cons y ys => (x <=? y) && (smaller x ys)

end.

Lemma nat_leb_flip:
 forall (a b: nat), (a<=?b) = false -> (b <=? a) = true.
Proof.

Admitted.


Lemma nat_leb_trans:
 forall (a b c: nat), (a<=?b) = true -> (b <=? c) = true -> (a <=?c) = true.
Proof.

Admitted.

Lemma is_sorted_cons_l: forall(x: nat) (xs: list nat) , 
    isSorted (cons x xs) = true -> isSorted xs = true /\ smaller x xs = true.
Proof.
Admitted.
Lemma is_sorted_cons_r: forall(x: nat) (xs: list nat) , 
    isSorted xs = true /\ smaller x xs = true -> isSorted (cons x xs) = true.
Proof.
      intros x xs (Ha ,Hb).
      induction xs.
      - simpl. reflexivity.
      - simpl in *.
        rewrite andb_true_iff in Hb.
        destruct Hb as (Hb,Hc).
        rewrite Hb. simpl.
        exact Ha.
Qed.


Lemma insert_keeps_smaller: forall(x y: nat) (ys: list nat),
    y <=? x = true ->  smaller y ys = true -> smaller y (insert x ys) = true.
Proof.
      intros x y ys Ha Hb.
      induction ys.
      -simpl. rewrite Ha. simpl. reflexivity.
      -simpl in Hb. simpl.
      case_eq(x <=? a).
        +intro Hc. simpl.
          rewrite Ha. simpl.
          exact Hb.
        + intro Hc. simpl.
          rewrite andb_true_iff in Hb.
          destruct Hb as (Hb,Hd).
          rewrite Hb. simpl. apply IHys. exact Hd.

Qed.

Lemma insert_keeps_sorted: forall(x: nat) (xs: list nat),
      isSorted xs = true -> isSorted (insert x xs) = true.
Proof.
      intros x xs Ha.
      induction xs.
      - simpl. reflexivity.
      - simpl. case_eq (x<=? a).
        + intro Hb. simpl.
          simpl in Ha.
          rewrite Hb. simpl.
          exact Ha.
        +intro Hb.
        apply is_sorted_cons_l in Ha.
      destruct Ha as (Ha,Hc).
      apply is_sorted_cons_r.
      split.
      ++ apply IHxs.
      exact Ha.
      ++ apply insert_keeps_smaller.
      apply nat_leb_flip.
      exact Hb. exact Hc.
Qed.


Theorem insertionSort_sorts: forall (l: list nat),
isSorted(insertionSort l) = true.
  Proof.
        intros l.
        induction l.
        -simpl. reflexivity.
        - unfold insertionSort in *.
        simpl.
        apply insert_keeps_sorted.
        exact IHl.
Qed.




