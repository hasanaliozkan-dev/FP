
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





