exception Time_out

let hashnumber=1;;
let hashnumber_graph= 1;;

let tra=false;;
let time_out=200;;

let tab=8;;

let kirchoff=true;;

let wonder= ref false;;

let string_of_color  s = 
  match s with 
     "vert"   -> "#00FF00"
  |  "rouge"  -> "#FF0000"
  |  "bleu"   -> "#0000FF"
  |  "noir"   -> "#FFFFFF"
  |  "gris"   -> "#DDDDDD"
  |  "blanc"  -> "#000000"
  |  "orange" -> "#FF7F50" 
  |  _ -> "#0000FF";;

let backcolor="noir";;
let dead_color="gris";;
let inf_color="orange";;
let can_color="vert";;
let pro_color="rouge";;
let link_color="bleu";;


let st_inf=(*"&#8734;"*)"+oo";;

let doc="http://www.di.ens.fr/~feret/prototypes/help_pisa000.html";;
let server="http://localhost/cgi-bin/pisa";;
let back="http://www.di.ens.fr/~feret/prototypes/prototypes.html.en";;
let ambients="http://localhost/cgi-bin/amb";;
let home="http://www.di.ens.fr/~feret/";;
let script=server;;

let title="Pi.s.a III : a Pi-calculus Static Analyzer";;
let shorttitle="Pi.s.a. III";;
let proto="/usr/lib/cgi-bin/pisa_html ";;


let menu = ["<LI><A href=\"#log\">log</A></LI>\n";
            "<LI><A href=\""^doc^"\">help</A></LI>\n";
            "<LI><A href=\""^server^"\">new analysis</A></LI>\n";
            "<LI><A href=\""^home^"\">home page</A></LI>\n"];;


let version="3.24";;
let derniere_mise_a_jour = "Fri November 19 2004";;

let entete="";;


let foot=("<SMALL>Pi-s.a. Version "^version^", last Modified "^derniere_mise_a_jour^"<Br> Pi-s.a. is an experimental prototype for academic use only.</SMALL><Hr><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Hr><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br><Br>\n");;


let parse_error=("Parse Error<Br>
                  Please ask any question on <A HREF=\"mailto:feret@ens.fr\">feret@ens.fr</A><Br>\n<A href=\"pisa\">other analysis session</A><Br>\n"^foot);;

let affiche_time_out () = 
    print_string "Time_out <Br>\n";
    print_string ("Due to security policy, analysis time is limited to "^(string_of_int time_out)^"s.<Br>\n");
    print_string ("<A href=\"pisa\">other analysis session</A><Br>\n");
    print_string foot;
    raise Time_out;;


let sigma = ref "high" ;;
let simplify_graph= ref true ;;
let uni=ref false;;
let occurrence=ref true;;
let occur=ref 1 ;;
(*"high" -> pair ;
  "low"  -> second component*)
let occ_content=ref "mi";;

let npro=ref 0 ;;
let n_pro=npro;;
