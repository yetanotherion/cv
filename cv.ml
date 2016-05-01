let log s = Firebug.console##log(Js.string s)

let get_element_by_id id =
  let () = log "get_element_by_id" in
  Js.Opt.get (Dom_html.document##getElementById(Js.string id))
             (fun () -> assert false)


type t = {more_about_test_efficiency: bool;
          more_about_cda: bool}

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
let third_col_div x = div ~a:[a_class ["col-sm-4"]] x
let full_col_div x = div ~a:[a_class ["col-sm-12"]] x

let compute_header () =
  full_col_div
      [h2 ~a:[a_class ["name"]]
           [strong [pcdata "Ion Alberdi, PhD"]];
       h2 ~a:[a_class ["position"]]
           [strong [pcdata "Software Engineer"]]]

let compute_open_source () =
  [pcdata "Open source contributions: ";
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
          ]
      ]
  ]

let compute_contact () =
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
  ]
let compute_information () =
  full_col_div
    [quarter_col_div (compute_contact ());
     div ~a:[a_class ["col-sm-6"]] (compute_open_source ());
     quarter_col_div [img ~a:[a_class ["picture"; "center-block"]]
                          ~src:(uri_of_string "photo.jpg")
                          ~alt:"Picture not available"
                          ()]]

let setup_handler f model div update_model =
  let onclick () =
    let () = f (update_model model) in
    Lwt.return_unit
  in
  Lwt_js_events.(async (fun () -> clicks
                                    (Tyxml_js.To_dom.of_div div)
                                    (fun _ _ -> onclick ())))

let create_div collapsed =
  let curr_class =
    if collapsed then
      "arrow-down"
    else "arrow-right"
  in
  div ~a:[a_class [curr_class]] []

let compute_test_efficiency_content f model =
  let new_div = create_div model.more_about_test_efficiency in
  let () = setup_handler f model new_div
                         (fun model ->
                          let new_state =
                            not model.more_about_test_efficiency in
                          {model with more_about_test_efficiency=new_state}) in
  let other =
    if model.more_about_test_efficiency then
      [ul [
           li [strong [pcdata "Handling unreliable tests"];
               pcdata ":";
               ul [li [pcdata "Algorithm to \
                               measure and detect unreliable verdicts."];
                   li [pcdata "Design and implement an automated test \
                               quarantine system (+50% on reliability)."]
                  ]
              ];
           li [strong [pcdata "Dynamic scheduling"];
               pcdata ":";
               ul [li [pcdata "Automate the identification and \
                               integration of reverts, fixing \
                               regressions by processing \
                               unreliable test results (referred as \
                               'fast_revert')."];
                   li [pcdata "Algorithm to decide when \
                               'fast_revert' should be used \
                               over the 'wait for the test result \
                               before integration' strategy ";
                       pcdata "(+25% on integration speed)."]]];
           li [strong [pcdata "Implementation"];
               pcdata ": ";
               hyperlink "https://www.python.org/"
                         "Python";
               pcdata " (";
               hyperlink "http://flask.pocoo.org/"
                         "Flask";
               pcdata ", ";
               hyperlink "https://twistedmatrix.com/"
                         "Twisted";
               pcdata "), ";
               hyperlink "https://www.rabbitmq.com/"
                         "RabbitMQ";
               pcdata ", ";
               hyperlink "https://www.docker.com/"
                         "Docker";
               pcdata ", ";
               hyperlink "http://buildbot.net/"
                         "Buildbot";
               pcdata ", ";
               hyperlink "https://www.cloudfoundry.org/"
                         "Cloudfoundry";
               pcdata ", ";
               hyperlink "http://microservices.io/patterns/microservices.html"
                         "Micro service architecture";
               pcdata ", ";
               hyperlink "https://www.mysql.com/"
                         "MySQL";
               pcdata ", ";
               hyperlink "https://github.com/d3"
                         "d3";
               pcdata ", ";
               hyperlink "https://angularjs.org/"
                         "angularjs";
               pcdata "."]
         ]
      ]
    else []
  in
  new_div, other


let compute_test_efficiency f model =
  let before, after = compute_test_efficiency_content f model in
  let content =
    [pcdata "Leading Test Scheduling Efficiency \
                   in the Continuous \
                   Integration engine of Intel inside Android software "]
    @ (before :: after) in
  [half_col_div_section
     [p [strong [pcdata "2015 - nowadays"];
         br ();
         strong [hyperlink "https://01.org/"
                           "INTEL / SSG / OTC"]]];
   half_col_div_section content
  ]

let compute_cda_content f model =
let new_div = create_div model.more_about_cda in
  let () = setup_handler f model new_div
                         (fun model ->
                          let new_state =
                            not model.more_about_cda in
                          {model with more_about_cda=new_state}) in
  let other =
    if model.more_about_cda then
      [ul [
           li [strong [pcdata "Upstream-compliance"];
               pcdata ": ";
               pcdata "Automate the generation, validation and integration of ";
               pcdata "software to be used in both upstream and local \
                       branches. "];
           li [strong [pcdata "CI interconnexion"];
               pcdata ": ";
               pcdata "Automate the generation, validation and integration of \
                       git ";
               hyperlink "https://git-scm.com/docs/git-merge"
                         "merge-commit";
               pcdata "-s in different ";
               hyperlink "https://www.gerritcodereview.com/"
                         "gerrit";
               pcdata " server/branches."];
           li [strong [pcdata "Scaling"];
               pcdata ": ";
               pcdata "Parallelize Android build scheduling. \
                       (Scaled up to 20 ";
               hyperlink "https://source.android.com/source/\
                          building.html"
                         "Android targets";
               pcdata " in parallel per up to 10 supported branches."
              ];
           li [strong [pcdata "Implementation"];
               pcdata ": ";
               hyperlink "https://www.python.org/"
                         "Python";
               pcdata ", ";
               hyperlink "http://buildbot.net/"
                         "Buildbot";
               pcdata "."
              ]
         ]
      ]
    else []
  in
  new_div, other

let compute_cda f model =
  let before, after = compute_cda_content f model in
  let content =
    [pcdata "Senior Software Developer in the Scrum team of the \
             Continuous Integration engine of Intel inside Android software "]
    @ (before :: after) in
  [half_col_div_section
     [p [strong [pcdata "2012 - 2015"];
         br ();
         strong [hyperlink "https://www.intel.com/"
                           "INTEL / MCG"]]];
   half_col_div_section content
  ]


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

let compute_profesional_experience f model =
  let content = (List.map
                   (fun x -> full_col_div x)
                   [compute_test_efficiency f model;
                    compute_cda f model;
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
     pcdata ": beginner."]


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
             contemporary dance ";
     hyperlink "http://www.elirale.org/fr/home/"
               "company";
     pcdata ".";
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
       compute_profesional_experience f model;
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
     let more_about_test_efficiency, more_about_cda = false, false in
     let r, f = React.S.create {more_about_test_efficiency; more_about_cda} in
     let content = get_element_by_id "content" in
     let under_content = view (r, f) in
     let () =
       Dom.appendChild
         content
         (Tyxml_js.To_dom.of_div under_content) in
    Lwt.return_unit)
