
Inductive List (a: Type): Type :=
  | Nil : List a
  | Cons: a -> List a -> List a.

Print List_ind.

Check @Nil.
Check @Cons.

Arguments Nil {_}.
Arguments Cons {_} _ _.

Fixpoint append {a: Type} (l1 l2: List a): List a :=
  match l1 with
    | Nil       => l2
    | Cons x xs => Cons x (append xs l2)
  end.

Lemma append_r_nil: 
  forall {a: Type} (l: List a),
  append l Nil = l.
Proof. intros.
       induction l.
       - simpl. reflexivity.
       - simpl. rewrite IHl. reflexivity.
Qed.

Print append_r_nil.

Lemma append_assoc: forall {a: Type} (l1 l2 l3: List a),
  append l1 (append l2 l3) = append (append l1 l2) l3.
Proof. intros a l1.
       induction l1.
       - intros l2 l3.
         simpl. reflexivity.
       - intros l2 l3.
         simpl.
         rewrite IHl1.
         reflexivity.
Qed.

Print nat.
Print Nat.add.

Lemma helper1: forall a: nat, a + 0 = a.
Proof. intro a.
       induction a.
       - simpl. reflexivity.
       - simpl. rewrite IHa. reflexivity.
Qed.

Lemma helper2:
  forall (a b: nat),
  S (a + b) = a + S b.
Proof. intro a.
       induction a.
       - intro b. simpl. reflexivity.
       - intro b. simpl. rewrite IHa.
         reflexivity.
Qed.

Lemma add_comm: forall (a b: nat),
  a + b = b + a.
Proof. intro a.
       induction a.
       - intro b.
         simpl.
         rewrite helper1.
         reflexivity.
       - intro b.
         simpl.
         rewrite IHa.
         rewrite helper2.
         reflexivity.
Qed.





