let log s = Firebug.console##log(Js.string s)

let get_element_by_id id =
  let () = log "get_element_by_id" in
  Js.Opt.get (Dom_html.document##getElementById(Js.string id))
             (fun () -> assert false)

module Animation = struct
    type dims = { width: int; height: int }
    type margin = {top: int; right: int;
                   bottom: int; left: int}

    type state = [`None | `Begin | `End]
    type coord = {x: float;
                  y: float}
    let coord x y = {x; y}
    type text = {content: string;
                 pos: coord}
    let create_text x y content =
      let pos = coord x y in
      {pos; content}

    type year_text = {year: text;
                      label: text}
    type draw = {line_position: coord;
                 uberlogger: year_text option;
                 github: year_text option;
                 msg: text option}
    type t = {state: state;
              draw: draw list;
              sleep: float;
              dims: dims;
              margin: margin}

    let shift_draw t =
      match t.draw with
      | [] | _ :: []-> {t with state=`End}
      (*| [] -> {t with state=`End}
      | hd :: [] -> {t with draw=[hd]}*)
      | _ :: tl -> {t with draw=tl}

    let curr_draw t = List.hd t.draw
    let start_animation t = {t with state=`Begin}
    let create dims margin sleep to_draw =
      {state=`None;
       draw=to_draw;
       sleep=sleep;
       dims=dims;
       margin=margin}
end

type t = {more_about_test_efficiency: bool;
          more_about_cda: bool;
          animation: Animation.t}

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

let create_svg f model =
  let open Animation in
  let animation = model.animation in
  let dims, margin = animation.dims,
                     animation.margin in
  let curr_draw = curr_draw animation in
  let create_line x1 x2 y1 y2 =
    Tyxml_js.Svg.(line ~a:[a_x1 (x1, None);
                           a_x2 (x2, None);
                           a_y1 (y1, None);
                           a_y2 (y2, None);
                           a_style "opacity: 1;"]
                       [])
  in
  let lines =
    let line_x = curr_draw.line_position.x in
    let line_y = curr_draw.line_position.y in
    let arrow_delta = 7.0 in
    [create_line 0.0 line_x line_y line_y;
     create_line (line_x -. arrow_delta)
                 (line_x)
                 (line_y +. arrow_delta) line_y;
     create_line (line_x -. arrow_delta)
                 line_x
                 (line_y -. arrow_delta) line_y]
  in
  let create_text_svg ?anchor:(anchor=`Middle) txt =
    Tyxml_js.Svg.(text ~a:[a_text_anchor anchor;
                           a_x_list [(txt.pos.x, None)];
                           a_y_list [(txt.pos.y, None)]]
                       [pcdata txt.content])
  in
  let create_milestone mil =
    match mil with
    | None -> []
    | Some x -> [create_text_svg x.year;
                 create_text_svg x.label;
                 create_line x.year.pos.x x.year.pos.x
                             (curr_draw.line_position.y -. 4.0)
                             (curr_draw.line_position.y +. 4.0)]
  in
  let uberlogger = create_milestone curr_draw.uberlogger in
  let github = create_milestone curr_draw.github in
  let msg = match curr_draw.msg with
    | None -> []
    | Some x -> [create_text_svg ~anchor:`Start x] in
  let under_g = lines @ uberlogger @ github @ msg in
  let inside_g = Tyxml_js.Svg.(g ~a:[a_transform
                                       (Svg_types.Translate
                                          (float_of_int margin.left,
                                           Some (float_of_int margin.top)))]
                                 under_g) in

  let my_svg = Tyxml_js.Html5.(svg ~a:[Tyxml_js.Svg.a_width
                                         (float_of_int dims.width, None);
                                       Tyxml_js.Svg.a_height
                                         (float_of_int dims.height, None)]
                                   [inside_g]) in
  let () = Lwt_js_events.(async
                            (fun () ->
                             let%lwt () = Lwt_js.sleep animation.sleep in
                             let () = f ({model with
                                           animation=shift_draw animation}) in
                             Lwt.return_unit)) in

  my_svg

let compute_rootkit_animation f model =
  match Animation.(model.animation.state) with
  | `End -> [hyperlink "https://www.sstic.org/2005/\
                        presentation/UberLogger_un_\
                        observatoire_niveau_noyau_pour_\
                        la_lutte_informative_defensive/"
                       "(fr)"]
  | `None ->
     let link = hyperlink "" "(src)" in
     let onmouseover () =
       let () = f ({model with animation=Animation.(start_animation
                                                      model.animation)}) in
       Lwt.return_unit
     in
     let () = Lwt_js_events.(async (fun () ->
                                    mouseovers
                                      (Tyxml_js.To_dom.of_a link)
                                      (fun _ _ -> onmouseover ()))) in
     [link]
  | `Begin -> [div [create_svg f model]]


let compute_open_source f model =
  [strong [pcdata "Open source"];
   pcdata " contributions: ";
   ul [li [strong [pcdata "Python: "];
           hyperlink "https://github.com/buildbot/buildbot"
                     "Buildbot";
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
       li ([strong [pcdata "C"];
            pcdata ": Uberlogger, backdoor for Linux 2.4 "] @
             (compute_rootkit_animation f model))
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

let compute_information f model =
  full_col_div
    [quarter_col_div (compute_contact ());
     div ~a:[a_class ["col-sm-6"]] (compute_open_source f model);
     quarter_col_div [img ~a:[a_class ["picture"; "center-block"]]
                          ~src:(uri_of_string "photo.jpg")
                          ~alt:"Picture not available"
                          ()]]

let setup_collapse_handler f model a update_model =
  let onclick () =
    let () = f (update_model model) in
    Lwt.return_unit
  in
  Lwt_js_events.(async (fun () -> clicks
                                    (Tyxml_js.To_dom.of_a a)
                                    (fun _ _ -> onclick ())))

let create_collapse_link collapsed =
  let curr_class =
    if collapsed then
      "(less)"
    else "(more)"
  in
  hyperlink "#" curr_class

let compute_test_efficiency_content f model =
  let new_collapse_link = create_collapse_link
                            model.more_about_test_efficiency in
  let () = setup_collapse_handler
             f model
             new_collapse_link
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
                       pcdata "(+13% on integration speed)."]]];
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
  new_collapse_link, other


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
let new_collapse_link = create_collapse_link model.more_about_cda in
  let () = setup_collapse_handler f model new_collapse_link
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
               pcdata "the software to be used in both upstream and local \
                       branches. "];
           li [strong [pcdata "CI interconnexion"];
               pcdata ": ";
               pcdata "Automate the generation, validation and integration of \
                       git ";
               hyperlink "https://git-scm.com/docs/git-merge"
                         "merge-commit";
               pcdata "-s in different ";
               hyperlink "https://www.gerritcodereview.com/"
                         "Gerrit";
               pcdata " server/branches."];
           li [strong [pcdata "Scaling"];
               pcdata ": ";
               pcdata "Parallelize Android build scheduling. \
                       Scaled up to 20 ";
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
               pcdata ", ";
               hyperlink "https://www.jfrog.com/artifactory/"
                         "Artifactory";
               pcdata "."
              ]
         ]
      ]
    else []
  in
  new_collapse_link, other

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
       compute_information f model;
       compute_profesional_experience f model;
       compute_bottom ();
      ]

let view ((r, f): rp) =
  let new_elt = React.S.map (compute_view f) r in
  Tyxml_js.R.Html5.(div (ReactiveData.RList.singleton_s
                           new_elt))
let range start_idx end_idx =
  let rec _range accum start_idx end_idx =
    if start_idx == end_idx then List.rev accum
    else _range (start_idx :: accum) (start_idx + 1) end_idx
  in
  _range [] start_idx end_idx

let create_animation animation_duration time_per_letter sleep dims margin =
  let open Animation in
  let line_length = dims.width - margin.left in
  let date_origin = 2004 in
  let date_end = 2010 in
  let date_length = float_of_int (date_end - date_origin) in
  let compute_date_x date =
    let delta  = date - date_origin in
    (float_of_int (delta * line_length)) /. date_length
  in
  let number_points = animation_duration /. sleep in
  let shift_at_each = (float_of_int line_length) /. number_points in
  let height = dims.height - margin.bottom in
  let height_f = float_of_int height in
  let line_height = height_f /. 2.0 in
  let create_milestone ?ymax:(ymax=height_f -. 5.0) year name =
    let x = compute_date_x year in
    let g = 9.91 in
    let y0 = line_height +. 6.0 in
    let translate_in_y y = height_f -. y in
    let translated_y0 = translate_in_y y0 in
    let v0 = sqrt ((ymax -. y0) *. (g *. 2.0)) in
    let _y t = translate_in_y (y0 +. v0 *. t -. 0.5 *. g *. t *. t) in
    let y t =
      let res = _y (t /. 3.0) in (* slow-down a little *)
      if res > translated_y0 then translated_y0
      else res
    in
    fun position ->
    let delta = position -. x in
    if delta < 0.0 then None
    else
      let t = delta /. shift_at_each in
      let ps = Printf.sprintf in
      Some {year=create_text
                   x
                   (line_height +. 18.0) (ps "%d" year);
            label=create_text
                    x
                    (y t) name}
  in
  let uberlogger = create_milestone 2005 "Uberlogger" in
  let github = create_milestone 2008 "Github" in
  let animation = List.fold_left (fun accum idx ->
                                  let new_position = (float_of_int idx) *.
                                                       shift_at_each in
                                  let new_elt =
                                      {line_position=coord new_position
                                                           line_height;
                                       uberlogger=uberlogger new_position;
                                       github=github new_position;
                                       msg=None;
                                      } in
                                  new_elt :: accum)
                                 [] (range 0 (int_of_float number_points)) in
  let last = List.hd animation in
  let clocks_per_letter = int_of_float (time_per_letter /. sleep) in
  let create_msg_text msg =
    let num_letters = String.length msg in
    fun idx ->
    let msg_len = min (idx / clocks_per_letter) num_letters in
    create_text 0.0 0.0 (String.sub msg 0 msg_len)
  in
  let messages =
    ["The source code was lost somewhere in the net.";
     "(Github didn't exist yet :S)";
     "Long story short, Uberlogger is 'open lost source' :).";
     "Please find above a link to an article (in french).";
     "Have a good day !"
    ]
  in
  let make_msg_animation msg =
    let f = create_msg_text msg in
    let number_points = clocks_per_letter * ((String.length msg) + 13) in
    List.rev (List.fold_left (fun accum idx ->
                              let new_elt =
                                {last with msg=Some (f idx)}
                              in
                              new_elt :: accum)
                             [] (range 0 number_points))
  in
  let flatten ll = List.fold_left (fun accum l -> accum @ l) [] ll in
  Animation.create dims margin sleep ((List.rev animation) @
                                        (flatten
                                           (List.map
                                              make_msg_animation
                                              messages)))
let _ =
  let dims = Animation.({width=450; height=100}) in
  let margin = Animation.({top=30; right=20;
                           bottom=30; left=50}) in
  Lwt.bind
    (Lwt_js_events.onload ())
    (fun _ ->
     let more_about_test_efficiency,
         more_about_cda,
         animation = false, false, create_animation
                                     1.0
                                     0.02
                                     0.01 dims margin in
     let r, f = React.S.create {more_about_test_efficiency; more_about_cda;
                                animation} in
     let content = get_element_by_id "content" in
     let under_content = view (r, f) in
     let () =
       Dom.appendChild
         content
         (Tyxml_js.To_dom.of_div under_content) in
    Lwt.return_unit)
