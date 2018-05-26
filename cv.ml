let log s = Firebug.console##log(Js.string s)

let get_element_by_id id =
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
    type drawing = {time_arrow_position: coord;
                    uberlogger: year_text option;
                    github: year_text option;
                    msg: text option}
    type t = {state: state;
              to_draw: drawing list;
              sleep: float;
              dims: dims;
              margin: margin}

    let stopped_animation =
      {state=`End;
       to_draw=[];
       sleep=0.0;
       dims={width=0; height=0};
       margin={top=0; bottom=0;
               right=0; left=0}}


    let shift_to_draw t =
      match t.to_draw with
      | [] | _ :: []-> {t with state=`End}
      | _ :: tl -> {t with to_draw=tl}

    let curr_drawing t = List.hd t.to_draw

    let drawing_in_progress t =
      match t.state with
      | `Begin -> true
      | `End | `None -> false

    let start_animation t = {t with state=`Begin}
    let create dims margin sleep to_draw =
      {state=`None;
       to_draw;
       sleep;
       dims;
       margin}
end

type t_below = {more_about_criteo: bool;
                more_about_test_efficiency: bool;
                more_about_cda: bool}
type t_above = {animation: Animation.t;
                image_uri: string}


module RTypes (T: sig type t end) = struct
    type rs = T.t React.signal
    type rf = ?step:React.step -> T.t -> unit
    type rp = rs * rf
  end

module RBelow = RTypes(struct type t = t_below end)
module RAbove = RTypes(struct type t = t_above end)

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
  let curr_drawing = curr_drawing animation in
  let create_line x1 x2 y1 y2 =
    Tyxml_js.Svg.(line ~a:[a_x1 (x1, None);
                           a_x2 (x2, None);
                           a_y1 (y1, None);
                           a_y2 (y2, None);
                           a_style "opacity: 1;"]
                       [])
  in
  let lines =
    let time_arrow_x = curr_drawing.time_arrow_position.x in
    let time_arrow_y = curr_drawing.time_arrow_position.y in
    let arrow_delta = 7.0 in
    [create_line 0.0 time_arrow_x time_arrow_y time_arrow_y;
     create_line (time_arrow_x -. arrow_delta)
                 (time_arrow_x)
                 (time_arrow_y +. arrow_delta) time_arrow_y;
     create_line (time_arrow_x -. arrow_delta)
                 time_arrow_x
                 (time_arrow_y -. arrow_delta) time_arrow_y]
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
                             (curr_drawing.time_arrow_position.y -. 4.0)
                             (curr_drawing.time_arrow_position.y +. 4.0)]
  in
  let uberlogger = create_milestone curr_drawing.uberlogger in
  let github = create_milestone curr_drawing.github in
  let msg = match curr_drawing.msg with
    | None -> []
    | Some x -> [create_text_svg ~anchor:`Start x] in
  let under_g = lines @ uberlogger @ github @ msg in
  let transform = `Translate (float_of_int margin.left, Some (float_of_int margin.top)) in
  let inside_g = Tyxml_js.Svg.(g ~a:[a_transform [transform]] under_g) in
  let my_svg = Tyxml_js.Html5.(svg ~a:[Tyxml_js.Svg.a_width
                                         (float_of_int dims.width, None);
                                       Tyxml_js.Svg.a_height
                                         (float_of_int dims.height, None)]
                                   [inside_g]) in
  let () = Lwt_js_events.(async
                            (fun () ->
                             let get_time () =
                               let date = new%js Js.date_now in
                               date##getTime
                             in
                             let start_time = get_time () in
                             let rec wait_until_done () =
                               let %lwt () = request_animation_frame () in
                               if (get_time () -. start_time > animation.sleep) then Lwt.return_unit
                               else wait_until_done ()
                             in
                             let%lwt () = wait_until_done() in
                             let () = f ({model with
                                           animation=shift_to_draw animation}) in
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
  [strong [pcdata "Tel: "];
   pcdata "+33 6 75 86 27 27";
   br ();
   strong [pcdata "Mail: "];
   hyperlink "mailto:nolaridegi@gmail.com"
             "nolaridebi@gmail.com";
   br ();
   strong [pcdata "Github: "];
   hyperlink "https://github.com/yetanotherion"
             "yetanotherion";
   br ();
   strong [pcdata "Researchgate: "];
   a ~a:[a_href "https://www.researchgate.net/profile/Ion_Alberdi"]
     [pcdata "Ion Alberdi"];
   br ();
   strong [pcdata "Certifications: "];
   ul [
        li [a ~a:[a_href "https://www.coursera.org/account/accomplishments/records/YPU5YECL2ELE"]
              [pcdata "Machine Learning by Stanford University on Coursera."]]
     ];
  ]


let compute_information f model =
  full_col_div
    [quarter_col_div (compute_contact ());
     div ~a:[a_class ["col-sm-6"]] (compute_open_source f model);
     quarter_col_div [img ~a:[a_class ["picture"; "center-block"]]
                          ~src:(uri_of_string model.image_uri)
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

let compute_criteo_content f model =
  let new_collapse_link = create_collapse_link
                            model.more_about_criteo in
  let () = setup_collapse_handler
             f model
             new_collapse_link
             (fun model ->
              let new_state =
                not model.more_about_criteo in
              {model with more_about_criteo=new_state}) in
  let other =
    if model.more_about_criteo then
      [ul [
           li [strong [pcdata "Online migration of a 1000+ maven multi module mono repo to gradle"];
               pcdata ":";
               ul [li [pcdata "Designer and main contributor of a maven to gradle converter (Java)."];
                  ]
             ];
           li [strong [pcdata "Maintain / improve the software factory"];
               pcdata ":";
               ul [li [pcdata "Jenkins mono master (~ 150 slaves, ~ 5K jobs)"];
                   li [pcdata "Gerrit"];
                   li [pcdata "In house .NET end2end test sandboxes"];
                   li [pcdata "In house .NET build tool (C#)"]
                 ]
             ];
           li [strong [pcdata "Implementation "];
               hyperlink "https://www.java.com/en/" "Java";
               pcdata " ";
               hyperlink "http://groovy-lang.org/" "Groovy";
               pcdata " ";
               hyperlink "https://jenkins.io/" "Jenkins";
               pcdata " ";
               hyperlink "https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/" "C#";
               pcdata " ";
               hyperlink "https://gradle.org/" "Gradle";
               pcdata " ";
               hyperlink "http://maven.apache.org/" "Maven";
               pcdata " ";
               hyperlink "https://www.gerritcodereview.com/" "Gerrit";
               pcdata " ";
               hyperlink "https://msdn.microsoft.com/en-us/library/dd393574.aspx" "MSBuild";
               pcdata " ";
               hyperlink "https://www.chef.io/" "Chef";
               pcdata "."]
         ]
      ]
    else []
  in
  new_collapse_link, other

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
               hyperlink "https://www.docker.com/"
                         "Docker";
               pcdata ", ";
               hyperlink "http://spark.apache.org/"
                         "Spark";
               pcdata ", ";
               hyperlink "http://www.scala-lang.org/"
                         "Scala";
               pcdata ", ";
               hyperlink "http://buildbot.net/"
                         "Buildbot";
               pcdata ", ";
               hyperlink "https://www.cloudfoundry.org/"
                         "Cloudfoundry";
               pcdata ", ";
               hyperlink "http://www.splunk.com/"
                         "Splunk";
               pcdata ", ";
               hyperlink "http://microservices.io/patterns/microservices.html"
                         "Micro service architecture";
               pcdata ", ";
               hyperlink "https://www.mysql.com/"
                         "MySQL";
               pcdata ", ";
               hyperlink "https://www.mongodb.com/"
                         "MongoDB";
               pcdata ", ";
               hyperlink "http://cassandra.apache.org/"
                         "Cassandra";
               pcdata ", ";
               hyperlink "https://www.rabbitmq.com/"
                         "RabbitMQ";
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

let compute_criteo f model =
  let before, after = compute_criteo_content f model in
  let content =
    [strong [pcdata "Senior Software Engineer"];
     pcdata "  in R&D/Devtools, owners of .NET/JVM software factory "
    ]
    @ (before :: after) in
  [half_col_div_section
     [p [strong [pcdata "2016 (nov) - now"];
         br ();
         strong [hyperlink "https://www.criteo.com/"
                           "Criteo / R&D"]]];
   half_col_div_section content
  ]

let compute_test_efficiency f model =
  let before, after = compute_test_efficiency_content f model in
  let content =
    [strong [pcdata "Leading Test Scheduling Efficiency "];
     pcdata "in the Continuous \
             Integration engine of Intel inside Android software "]
    @ (before :: after) in
  [half_col_div_section
     [p [strong [pcdata "2015 - 2016 (nov)"];
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
    [strong [pcdata "Senior Software Developer"];
     pcdata " in the Scrum team of the \
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
     [p [strong [pcdata "Luth: "];
         pcdata "a non-monolithic \
                 firewall/IDS/IPS for Linux (";
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
                   [compute_criteo f model;
                    compute_test_efficiency f model;
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

let view ((r_below, f_below): RBelow.rp) ((r_above, f_above): RAbove.rp) =
  let information = React.S.map (compute_information f_above) r_above in
  let prof_experience = React.S.map (compute_profesional_experience f_below)
                                    r_below in
  let react_div_with_one_child c =
    Tyxml_js.R.Html5.div (ReactiveData.RList.singleton_s c)
  in
  let div_above =  react_div_with_one_child information in
  let div_below = react_div_with_one_child prof_experience in
  div [compute_header (); div_above; div_below; compute_bottom ()]

let range start_idx end_idx =
  let rec _range accum start_idx end_idx =
    if start_idx == end_idx then List.rev accum
    else _range (start_idx :: accum) (start_idx + 1) end_idx
  in
  _range [] start_idx end_idx

let create_animation animation_duration time_per_letter sleep dims margin =
  let open Animation in
  let time_arrow_length = dims.width - margin.left in
  let date_origin = 2004 in
  let date_end = 2010 in
  let date_length = float_of_int (date_end - date_origin) in
  let compute_date_x date =
    let delta  = date - date_origin in
    (* scaling dates in x *)
    (float_of_int (delta * time_arrow_length)) /. date_length
  in
  let number_points = animation_duration /. sleep in
  let shift_x_per_iteration = (float_of_int time_arrow_length) /. number_points in
  let height = dims.height - margin.bottom in
  let height_f = float_of_int height in
  let time_arrow_y = height_f /. 2.0 in

  let create_milestone ?ymax:(ymax=height_f -. 5.0) year name =
    let x = compute_date_x year in
    let g = 9.91 in
    let g = g *. 0.3 in (* to slow down the fall *)
    let y0 = time_arrow_y +. 6.0 in
    let translate_in_y y = height_f -. y in
    let translated_y0 = translate_in_y y0 in
    let v0 = sqrt ((ymax -. y0) *. (g *. 2.0)) in
    let _y t = translate_in_y (y0 +. v0 *. t -. 0.5 *. g *. t *. t) in
    let y t =
      let res = _y t in
      (* we don't go lower than y0 *)
      if res > translated_y0 then translated_y0
      else res
    in
    let t_end = 2.0 *. v0 /. g in
    (* need to have enough points to reach t_end + x *)
    let number_points_to_fall_down = (t_end +.
                                        x /. shift_x_per_iteration) in
    (* + 1.0 to get the ceil (and not stopping just before t_end) when
       converting to int *)
    let number_points_to_fall_down = number_points_to_fall_down +. 1.0 in
    (* + 1.0 (again) as that number will be given to
       range a b = [a, ..., b - 1] (see above) *)
    let max_number_points_to_fall_down = number_points_to_fall_down +. 1.0 in
    let f position =
      let delta = position -. x in
      if delta < 0.0 then None
      else
        let t = delta /. shift_x_per_iteration in
        let ps = Printf.sprintf in
        Some {year=create_text
                     x
                     (time_arrow_y +. 18.0) (ps "%d" year);
              label=create_text
                      x
                      (y t) name}
    in
    (f, max_number_points_to_fall_down)
  in
  let uberlogger, _ = create_milestone 2005 "Uberlogger" in
  let github, github_end = create_milestone 2008 "Github" in
  let number_points_to_put_milestones_down = int_of_float
                                               (max github_end number_points) in
  let time_arrow_end = int_of_float number_points in
  let animation = List.fold_left (fun accum idx ->
                                  (* The time_arrow shouldn't go further
                                     than time_arrow_end. However
                                     the two milestones should observe idx-es
                                     that go further, so that both text "fall"
                                     until they reach their initia point.
                                     We compute two different 'x'-es for
                                     this reason *)
                                  let new_arrow_idx = float_of_int
                                                        (min idx
                                                             time_arrow_end)
                                  in
                                  let new_arrow_x = new_arrow_idx *.
                                                      shift_x_per_iteration in
                                  let new_milestone_x = (float_of_int idx) *.
                                                          shift_x_per_iteration
                                  in
                                  let new_elt =
                                      {time_arrow_position=coord new_arrow_x
                                                                 time_arrow_y;
                                       uberlogger=uberlogger new_milestone_x;
                                       github=github new_milestone_x;
                                       msg=None;
                                      } in
                                  new_elt :: accum)
                                 [] (range
                                       0
                                       (number_points_to_put_milestones_down))
  in
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
    let wait_before_stopping = 13 in
    let number_points = clocks_per_letter * ((String.length msg) +
                                               wait_before_stopping) in
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
     let image_uri = "photo.jpg" in
     let animation = create_animation 1.3
                       0.02
                       0.01 dims margin in
     let more_about_test_efficiency,
         more_about_cda,
         more_about_criteo = false, false, false in
     let r_above, f_above = React.S.create {more_about_test_efficiency;
                                            more_about_cda;
                                            more_about_criteo} in
     let r_below, f_below = React.S.create {animation; image_uri} in
     let content = get_element_by_id "content" in
     let under_content = view (r_above, f_above) (r_below, f_below) in
     let () =
       Dom.appendChild
         content
         (Tyxml_js.To_dom.of_div under_content) in
    Lwt.return_unit)
