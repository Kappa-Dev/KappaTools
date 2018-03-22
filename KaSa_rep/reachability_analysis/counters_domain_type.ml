type comparison_op =
    LEQT | LT | GT | GTEQ | EQ

type restriction =
  {
    test: Occu1.trans * comparison_op * Ckappa_sig.c_state_index list ;
    assign : Ckappa_sig.c_site_name * Ckappa_sig.c_state_index list ;
    delta : Occu1.trans * int list ;
  }

type static =
  {
    packs:
      Ckappa_sig.Site_map_and_set.Set.t
        Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif ;
    backward_pointers:
      Ckappa_sig.Site_map_and_set.Set.t
        Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif ;
    rule_restrictions:
      restriction
        Ckappa_sig.Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
        Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif
  }
