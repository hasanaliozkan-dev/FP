

Lemma ex1_v1: forall (A B: Prop), A \/ B -> B \/ A.
Proof. intros A B H.
       destruct H as [ H | H ].
       - apply or_intror.
         exact H.
       - apply or_introl.
         exact H.
Qed.

Lemma ex1_v2: forall (A B: Prop), A \/ B -> B \/ A.
Proof. intro A.
       intro B.
       intro H.
       destruct H.
       - right. exact H.
       - left. apply H.
Qed.

Lemma ex2: forall (A B: Prop), A /\ B -> B \/ A.
Proof. intros A B H.
       destruct H as (Ha, Hb).
       right. exact Ha.
       (* left. exact Hb. *)
Qed.

Lemma ex3: forall (X: Type) (P: X -> Prop),
  ~ (exists (x: X), P x) -> (forall (x: X), ~P x).
Proof. intros X P H x.
       unfold not in *.
       intro px.
       apply H.
       exists x.
       exact px.
Qed.

Lemma ex4: forall (X: Type) (P: X -> Prop),
  (forall (x: X), ~ P x) -> ~ (exists (x: X), P x).
Proof. intros X P H.
       unfold not in *.
       intro Hx.
       destruct Hx as (x, Hx).
       specialize (H x).
       apply H.
       exact Hx.
Qed.

Axiom LEM: forall (P: Prop), P \/ ~P.

Lemma dne: forall (P: Prop), ~~P -> P.
Proof. intros.
       specialize (LEM P).
       intro HP.
       destruct HP as [ HP | HP ].
       - exact HP.
       - unfold not in *.
         specialize (H HP).
         contradiction.
Qed.

Lemma ex5: forall (X: Type) (P: X -> Prop),
  ~ (forall (x: X), ~ P x) -> (exists (x: X), P x).
Proof. intros X P H.
       specialize (LEM (exists (x: X), P x)).
       intro HE.
       destruct HE as [ HE | HE ].
       - exact HE.
       - specialize (ex3 X P HE); intro HF.
         unfold not in H.
         specialize (H HF).
         contradiction.
Qed.













