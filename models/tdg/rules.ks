##### TDG DNA interactions

'TDG.DNA' T(dbd,cat),BP(n) -> T(dbd!1,cat),BP(n!1) @ 'tdg_k_on'
'TDG..DNA' T(dbd!1,cat),BP(n!1) -> T(dbd,cat),BP(n) @ 'tdg_k_off'

# TDG sliding on DNA
'-->' T(dbd!1,cat),BP(n!1,end5!2),BP(end3!2,n) -> T(dbd!1,cat),BP(n,end5!2),BP(end3!2,n!1) @ 'k_slide'
'<--' T(dbd!1,cat),BP(n!1,end3!2),BP(end5!2,n) -> T(dbd!1,cat),BP(n,end3!2),BP(end5!2,n!1) @ 'k_slide'

# TDG anchoring into the mismatch
'TDG anchoring' T(dbd!1,cat),BP(n~T!1,state~d) -> T(dbd,cat!1),BP(n~T!1,state~d) @ 'k_reco'
'TDG disconnect' T(cat!1),BP(n!1) -> T(cat),BP(n) @ 'k_disco'


# Context dependent Glycosylase activity
'TDG excision [TpG]' T(cat!1),BP(n~T!1,state~d,end5!2),BP(end3!2,n~G?) -> T(cat!1),BP(n~AP!1,state~d,end5!2),BP(end3!2,n~G?) @ 'k_rm_TpG'
'TDG excision [TpA]' T(cat!1),BP(n~T!1,state~d,end5!2),BP(end3!2,n~A?) -> T(cat!1),BP(n~AP!1,state~d,end5!2),BP(end3!2,n~A?) @ 'k_rm_TpA'
'TDG excision [TpT]' T(cat!1),BP(n~T!1,state~d,end5!2),BP(end3!2,n~T?) -> T(cat!1),BP(n~AP!1,state~d,end5!2),BP(end3!2,n~T?) @ 'k_rm_TpT'
'TDG excision [TpC]' T(cat!1),BP(n~T!1,state~d,end5!2),BP(end3!2,n~C?) -> T(cat!1),BP(n~AP!1,state~d,end5!2),BP(end3!2,n~C?) @ 'k_rm_TpC'

##### TDG BER interactions

'TDG.BER' T(cat!1,ber),BP(n~AP!1),BER(t) -> T(cat!1,ber!2),BP(n~AP!1),BER(t!2) @ 'tdg.ber_k_on'
'TDG..BER' T(ber!1),BER(t!1) -> T(ber),BER(t)  @ 'tdg.ber_k_off'

'base excision repair [w/o dnmt]' T(cat!1,ber!2,mt),BP(n~AP!1,state~d),BER(t!2) -> T(cat,ber,mt),BP(n~C,state~u),BER(t) @ 'k_ber' 
'base excision repair [w dnmt]' T(cat!1,ber!2,mt!3),BP(n~AP!1,state~d),BER(t!2),DNMT3(t!3) -> T(cat,ber,mt),BP(n~C,state~m),BER(t),DNMT3(t) @ 'k_ber' 

#### TDG DNMT3

'TDG.DNMT' T(mt),DNMT3(t) -> T(mt!1),DNMT3(t!1) @ 'TDG.DNMT_k_on'
'TDG..DNMT' T(mt!1),DNMT3(t!1) -> T(mt),DNMT3(t) @ 'TDG.DNMT_k_off'


