

Lemma ex0 : forall (A B : Prop) , A \/ B -> B \/ A.
Proof. intros A B H.
          destruct  H as [ H | H ].
          -apply or_intror.
          exact H.
          -apply or_introl.
          exact H.
Qed.

Lemma ex1_v2 : forall (A B : Prop) , A \/ B -> B \/ A.
Proof. intro A.
       intro B.
       intro H.
       destruct H.
       - right. exact H.
       - left. apply H.
Qed.




Lemma ex2 : forall (A B : Prop) , A /\ B -> B \/ A.
Proof. intros A B H.
       destruct H as (Ha, Hb).
       right. exact Ha.
Qed.



