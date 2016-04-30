let log s = Firebug.console##log(Js.string s)

let get_element_by_id id =
  let () = log "get_element_by_id" in
  Js.Opt.get (Dom_html.document##getElementById(Js.string id))
             (fun () -> assert false)

type t = unit
type rs = t React.signal
type rf = ?step:React.step -> t -> unit
type rp = rs * rf
open Tyxml_js.Html5

let half_col_div ?other_cls:(ol=[]) x =
  div ~a:[a_class ("col-sm-6" :: ol)] x

let half_col_div_section x =
  half_col_div
    ~other_cls:["section"] x

let hyperlink link name =
  a ~a:[a_href link] [pcdata name]

let quarter_col_div x = div ~a:[a_class ["col-sm-3"]] x
let full_col_div x = div ~a:[a_class ["col-sm-12"]] x

let compute_header () =
  full_col_div
      [h2 ~a:[a_class ["name"]]
           [strong [pcdata "Ion Alberdi, PhD"]];
       h2 ~a:[a_class ["position"]]
           [strong [pcdata "Software Engineer"]]]

let compute_information () =

  full_col_div
    [half_col_div
       [pcdata "Tel: +33 6 75 86 27 27";
        br ();
        pcdata "Mail: ";
        hyperlink "mailto:nolaridegi@gmail.com"
                  "nolaridebi@gmail.com";
        br ();
        pcdata "Github: ";
        hyperlink "https://github.com/yetanotherion"
                  "yetanotherion";
        br ();
        pcdata "Researchgate: ";
        a ~a:[a_href "https://www.researchgate.net/profile/Ion_Alberdi"]
          [pcdata "Ion Alberdi"];
        br ();
        pcdata "Open source: ";
        ul [li [strong [pcdata "Python: "];
                hyperlink "https://github.com/buildbot/buildbot"
                          "buildbot";
               ];
            li [strong [pcdata "OCaml"];
                pcdata ": ";
                ul
                  [
                    li [pcdata "Facebook events' audience analysis: ";
                        hyperlink "https://vimeo.com/121533725"
                                  "demo";
                        pcdata " ";
                        hyperlink
                          "https://github.com/yetanotherion/eliom_and_facebook"
                          "(src)";
                       ];
                    li [pcdata "Games to learn languages: ";
                        hyperlink "http://www.languagames.com/hiztegia"
                                  "collaborative dictionary";
                        pcdata ", ";
                        hyperlink "http://www.languagames.com/eus_taulak"
                                  "animated verbs";
                        pcdata " ";
                        hyperlink "https://github.com/yetanotherion/hizkuntzak"
                                  "(src)";
                       ]]];

            li [strong [pcdata "C"];
                pcdata ": Linux rootkit ";
                hyperlink "https://www.sstic.org/2005/\
                           presentation/UberLogger_un_\
                           observatoire_niveau_noyau_pour_\
                           la_lutte_informative_defensive/"
                          "(fr)";
               ]]
       ];

     half_col_div
       [img ~a:[a_class ["picture"]]
            ~src:(uri_of_string "photo.jpg")
            ~alt:"Picture not available"
            ()]]

let compute_intel () =
  [half_col_div_section
     [p [strong [pcdata "2012 - nowadays"];
         br ();
         strong [hyperlink "https://01.org/"
                           "INTEL / SSG / OTC"]]];
   half_col_div_section
       [p [pcdata "Leading Test Scheduling Efficiency \
                   in the Continuous \
                   Integration engine of Intel inside Android software
                   (python/angularjs/coffeescript/dc/d3/java)."]]]
let compute_celad () =
  [half_col_div_section
     [p [strong [pcdata "2011 - Short missions";
                 br ();
                 hyperlink "http://www.celad.com/"
                           "CELAD"]]];
   half_col_div_section
     [p [hyperlink "http://www.quotium.com/"
                   "Quotium";
         pcdata ": Validation of ";
         hyperlink "http://www.quotium.com/seeker/security/"
                   "Seeker";
         pcdata " (a web application pentesting software).";
         br ();
         hyperlink "http://www.ebtp.fr/"
                   "EBTP";
         pcdata ": Audit after finding and exploiting \
                 security holes in their software."]]]

let compute_laas () =
  [half_col_div_section
     [p [strong [pcdata "2006 - 2010"];
         br ();
         strong [hyperlink "https://www.laas.fr/public/"
                           "LAAS-CNRS"];
         br ();
         strong [pcdata "(";
                 hyperlink "http://www.cyberlycee.fr/barthou/site/"
                           "MPSI-MP";
                 pcdata "/";
                 hyperlink "http://www.enseirb-matmeca.fr/"
                           "ENSEIRB";
                 pcdata ")"]]];
   half_col_div_section
     [p [pcdata "Luth: a non-monolithic \
                   firewall/IDS/IPS (";
         hyperlink "https://www.researchgate.net/\
                    publication/221204865_Luth_\
                    Composing_and_Parallelizing_\
                    Midpoint_Inspection_Devices"
                   "paper";
         pcdata ", ";
         hyperlink "https://www.researchgate.net/\
                    publication/50193691_Malicious_\
                    traffic_observation_using_a_\
                    framework_to_parallelize_and_\
                    compose_midpoint_inspection_devices"
                   "PhD";
         pcdata ").";
         br ();
         pcdata "5 weeks in ";
         hyperlink "http://www.sfc.wide.ad.jp/IRL/"
                   "Jun Murai Lab.";
         br ();
         pcdata "Managed 3 interns: French, \
                 Bulgarian and Japanese master's \
                 degree students.";
         br ();]
       ]
  ]


let compute_profesional_experience () =
  let content = (List.map
                   (fun x -> full_col_div x)
                   [compute_intel ();
                    compute_celad ();
                    compute_laas ()]) in
  let header = full_col_div [half_col_div
                               [h2 ~a:[a_class ["big-border-bottom"]]
                                   [pcdata "PROFESSIONAL EXPERIENCES"]]] in
  div ([header; hr ()] @ content)

let compute_languages () =
  half_col_div
    [h3 ~a:[a_class ["big-border-bottom"]]
        [pcdata "LANGUAGES"];
     strong [pcdata "English, French, Spanish"];
     pcdata ": fluent.";
     br ();
     strong [pcdata "Basque"];
     pcdata ": mother language.";
     br ();
     strong [pcdata "Russian"];
     pcdata ": begginer."]


let compute_extra_curricular_activities () =
  half_col_div
    [h3 ~a:[a_class ["big-border-bottom"]]
        [pcdata "EXTRACURRICULAR ACTIVITIES"];
     strong [pcdata "Musician"];
     pcdata ": Released an ";
     hyperlink "http://evilness.bandcamp.com/\
                album/unreachable-clarity-instrumental-edition"
               "EP";
     pcdata ". ";
     hyperlink "https://www.youtube.com/watch?v=PnqLYlcRwwc"
               "Collaboration";
     pcdata " with a professional \
             contemporary dance company.";
     br ();
     strong [pcdata "Trips"];
     pcdata ": England, Italy, Spain, \
            Israel-Palestine, Cuba, \
            Japan, Russia.";
     br ();
     strong [pcdata "Teaching"];
     pcdata ": Basque language."]


let compute_bottom () =
  full_col_div [compute_languages ();
                compute_extra_curricular_activities ()]

let compute_view f model =
  div [compute_header ();
       compute_information ();
       compute_profesional_experience ();
       compute_bottom ();
      ]

let view ((r, f): rp) =
  let new_elt = React.S.map (compute_view f) r in
  Tyxml_js.R.Html5.(div (ReactiveData.RList.singleton_s
                           new_elt))

let _ =
  Lwt.bind
    (Lwt_js_events.onload ())
    (fun _ ->
     let r, f = React.S.create () in
     let content = get_element_by_id "content" in
     let under_content = view (r, f) in
     let () =
       Dom.appendChild
         content
         (Tyxml_js.To_dom.of_div under_content) in
    Lwt.return_unit)
