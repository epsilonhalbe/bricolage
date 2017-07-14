port module Main exposing (main)

import Material
import Material.Button as Button exposing (..)
import Material.Card as Card
import Material.Chip as Chip exposing (..)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Helpers exposing (map1st, map2nd)
import Material.Icon as Icon
import Material.List as MList
import Material.Options as Options exposing (Style, css)
import Material.Slider as Slider
import Material.Snackbar as Snackbar
import Material.Textfield as Textfield

import Html exposing (Html, span, div, programWithFlags)
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (map)
import Platform.Sub exposing (none)
import Set exposing (Set, size, insert, empty)
import String
import String exposing (join)
import Svg exposing (svg, text, circle, line, path)
import Svg.Attributes exposing (..)
import Task exposing (Task)
import Time exposing (Time, every, second, inSeconds)

main = programWithFlags
     { init = initialize
     , view = view
     , update = update
     , subscriptions = subscriptions
     }

-- MODEL

type alias Team = Set String

type alias Model = { sec         : Int
                   , maxMinutes  : Float
                   , running     : Bool
                   , currentTeam : Set String
                   , savedTeams  : List Team
                   , name        : String
                   , mdl         : Material.Model
                   , cacheError  : Snackbar.Model ()
                   }

initialize : Maybe (List (List String)) -> (Model, Cmd Msg)
initialize teams =
    ({init | savedTeams = List.map Set.fromList
                       <| Maybe.withDefault [] teams}
    , Cmd.none)

init : Model
init = { sec = 0
       , maxMinutes = 15
       , running = False
       , currentTeam = Set.empty
       , savedTeams = []
       , name = ""
       , cacheError = Snackbar.model
       , mdl = Material.model }

-- UPDATE

type Msg = Add
         | Clear
         | Delete String
         | KeyUp Int
         | Mdl (Material.Msg Msg)
         | Name String
         | RemoveTeam (Set String)
         | SaveTeam
         | SetMinutes Float
         | SetTeam (Set String)
         | Start
         | Stop
         | Tick Int
         | None



update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Add -> ({model | currentTeam = Set.union (Set.fromList <| List.filter (not << String.isEmpty) <| List.map String.trim
                                                       <| String.split ";" model.name) model.currentTeam
                   , name = "" }, Cmd.none)
    Clear -> ({model | currentTeam = Set.empty}, Cmd.none)
    Delete member -> ({model | currentTeam = Set.remove member model.currentTeam}, Cmd.none)
    KeyUp keycode -> if keycode == 13 then update Add model
                                      else update None model
    Mdl msg -> Material.update Mdl msg model
    Name name -> ({model | name = name }, Cmd.none)
    RemoveTeam xs -> let newTeams = List.filter ((/=) xs) model.savedTeams
                      in ( {model | savedTeams = newTeams}
                         , saveTeams <| List.map Set.toList newTeams )
    SaveTeam -> let newTeams = model.currentTeam :: model.savedTeams
                 in ( {model | savedTeams = newTeams
                             , currentTeam = Set.empty}
                    , saveTeams <| List.map Set.toList newTeams )
    SetMinutes maxMinutes -> ({model | maxMinutes = maxMinutes }, Cmd.none)
    SetTeam xs -> ({model | currentTeam = xs}, Cmd.none)
    Start -> ({model | running = True }, Cmd.none)
    Stop  -> ({model | running = False, sec = 0 }, Cmd.none)
    Tick sec -> ({model | sec = sec}, Cmd.none)
    None -> (model, Cmd.none)

-- PORTS

port saveTeams : List (List String) -> Cmd msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = every second <| \t ->
    if model.running && model.sec < round (60 * model.maxMinutes)
        then Tick (model.sec + 1)
        else Stop

-- VIEW

view : Model -> Html Msg
view model = let sec_ = (toFloat (model.sec * (Set.size model.currentTeam))) / (60 * model.maxMinutes)
              in div [style "margin:auto;width:90%;text-align:center;"]
                [ Html.h1 [] [Html.text <| if model.running then timeFormat (round (model.maxMinutes * 60) - model.sec)
                                                            else "Scrum clock"]
                , Grid.grid []
                   [ Grid.cell [ Grid.size Grid.Tablet 12, Grid.size Grid.Desktop 6, Grid.size Grid.Phone 12
                               , Grid.order Grid.Desktop 1 ]
                     [ Html.span []
                          [svg [ style "border:1px white solid"
                               , viewBox "-260 -260 520 520", width "100%"]
                               (List.concat (List.map2 (circlet sec_ (Set.size model.currentTeam)) (List.range 1 (Set.size model.currentTeam)) (Set.toList model.currentTeam)))
                          ]
                     ]
                   , Grid.cell [ Grid.size Grid.Tablet 12, Grid.size Grid.Desktop 6, Grid.size Grid.Phone 12]
                     [ Button.render Mdl [0] model.mdl
                           ([ Button.fab
                            , Button.colored
                            , Options.onClick <| if model.running then Stop else Start ]
                             ++ if Set.isEmpty model.currentTeam then [Button.disabled] else []
                            )
                           [ Icon.i <| if model.running then "stop" else "play_arrow"]
                     , Slider.view <|
                           [ Slider.onChange SetMinutes
                           , Slider.value <| model.maxMinutes
                           , Slider.max 30
                           , Slider.min 1
                           , Slider.step 1
                           ] ++ if model.running then [Slider.disabled] else []
                     , Html.p [] [Html.text <| timeFormat (round (model.maxMinutes * 60) - model.sec)]
                     , Html.p [] [Html.text "Start by adding new members to your scrum team by entering some names (separated by semicolons)."]
                     , Button.render Mdl [0] model.mdl
                         ([ Button.fab
                          , Button.colored
                          , Options.onClick Add ]
                              ++ if model.running || String.isEmpty model.name || Set.member model.name model.currentTeam
                                    then [Button.disabled] else [])
                         [ Icon.i "add"]
                     , Html.span [style "margin-left: 1em"] [Textfield.render Mdl [2] model.mdl
                         ([ Textfield.label "new member"
                          , Textfield.floatingLabel
                          , Textfield.value model.name
                          , Options.onInput Name
                          , Options.on "keyup" (Decode.map KeyUp <| Decode.at ["keyCode"] Decode.int)
                          ] ++ if model.running then [Textfield.disabled] else [])
                         []]
                     , div [style "text-align:left;"] <| List.map (memberChip model.running) <| Set.toList model.currentTeam
                     , div [style "padding-top: 1em"]
                        [ Button.render Mdl [0] model.mdl
                           ([ Button.raised
                            , Button.colored
                            , Options.onClick Clear ]
                                ++ if model.running || Set.isEmpty model.currentTeam
                                      then [Button.disabled] else [])
                           [ Html.text "Clear" ]
                        ]
                     , div [style "padding-top: 1em"]
                        [ Button.render Mdl [0] model.mdl
                           ([ Button.raised
                            , Button.colored
                            , Options.onClick SaveTeam ]
                                ++ if model.running || Set.isEmpty model.currentTeam
                                                    || List.member model.currentTeam model.savedTeams
                                      then [Button.disabled] else [])
                           [ Html.text "Save new Team" ]
                        ]
                     , div [style "padding-top: 1em"]
                        [ Grid.grid [] <| List.map (teamCard model) model.savedTeams
                        ]
                     ]
                   ]
                ]

timeFormat : Int -> String
timeFormat secs = let mins_ = String.padLeft 2 '0' <| toString <| secs // 60
                      secs_ = String.padLeft 2 '0' <| toString <| secs % 60
                   in mins_ ++ ":" ++ secs_

initials : String -> String
initials = String.toUpper << String.concat << List.map (String.left 1) << String.split " "

memberChip : Bool -> String -> Html Msg
memberChip running str =
  Chip.span
    (if running
       then [ Chip.deleteIcon "lens"]
       else [ Chip.deleteClick (Delete str)
            , Chip.deleteIcon "cancel"
            ])
    [ Chip.contact Html.span
        [ Color.background Color.primary
        , Color.text Color.white
        , Options.onClick None
        ]
        [ Html.text <| initials str ]
    , Chip.content []
        [ Html.text str ]
    ]

circlet : Float -> Int -> Int -> String ->  List (Svg.Svg msg)
circlet sec_ n k str =
    let maxR = 250
        minR = 50
        area = (maxR^2 - minR^2) * pi
        n_ = toFloat (n+1)
        k_ = toFloat (k-1)
        inneR =sqrt <| ((n_-k_-1)*area)/(n_*pi) - minR^2
        outeR =sqrt <| ((n_-k_)*area)/(n_*pi) - minR^2
    in [ Svg.path [ d <| arc inneR outeR (clamp k_ (k_+0.999) sec_) (k_+0.999)
                  , stroke "#FF4081"
                  , strokeWidth "2"
                  , fill "#3F51B5"
                  , fillOpacity <| toString <| 1.00 - k_/(3*n_)
                  ] []
       , Svg.text_ [ x <|toString <| (-(outeR + inneR))/2
                   , y "0", fill "#FFFFFF"
                   , textAnchor "middle"
                   , writingMode "vertical-rl"
                   , style "text-orientation: upright"
                   ] [Svg.text <| initials str]
       ]

arc : Float -> Float -> Float -> Float -> String
arc inneR outeR fromDeg toDeg =
  let startInner = join " " <| map toString [xCoord 0 inneR fromDeg, yCoord 0 inneR fromDeg]
      startOuter = join " " <| map toString [xCoord 0 outeR fromDeg, yCoord 0 outeR fromDeg]
      endInner   = join " " <| map toString [xCoord 0 inneR toDeg  , yCoord 0 inneR toDeg]
      endOuter   = join " " <| map toString [xCoord 0 outeR toDeg  , yCoord 0 outeR toDeg]
      largeArcFlag = if abs (toDeg - fromDeg) <= 0.5 then "0" else "1"
  in join " " <| List.map (join " ")
                    [["M", startOuter]
                    ,["A", toString outeR, toString outeR, "0", largeArcFlag, "0", endOuter ]
                    ,["L", endInner]
                    ,["A", toString inneR, toString inneR, "0", largeArcFlag, "1", startInner ]
                    ,["L", startOuter, "Z"]
                    ]

xCoord : Float -> Float -> Float -> Float
xCoord mx r phi = mx + r * cos (turns phi)

yCoord : Float -> Float -> Float -> Float
yCoord my r phi = my - r * sin (turns phi)

clock : Float -> String -> Html Msg
clock currTime color =
  let
    angle = turns currTime - pi/2
    handX = toString <| (50 + 40 * cos angle)
    handY = toString <| (50 + 40 * sin angle)
  in Html.span [] [ svg [ viewBox "0 0 100 100", width "300px" ]
             [ circle [ cx "50", cy "50", r "45", fill color ] []
             , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
             ]
    ]

teamCard : Model -> Set String -> Grid.Cell Msg
teamCard model xs =
           Grid.cell [ Grid.size Grid.Desktop 6]
            [ Card.view
               [ css "width" "100%"
               , css "text-align" "left"
               , Elevation.e8
               , Color.background (Color.color Color.Pink Color.S300)
               , Options.onClick (SetTeam xs)
               ]
               [ Card.title [ ]
                   [ Card.head [ Color.text Color.white ] [ Html.text "Team" ]
                   , Card.subhead [ Color.text Color.white ] [ Html.text "click to load team" ]
                   ]
               , Card.menu []
                    [ Button.render Mdl [0,0] model.mdl
                        [ Button.icon
                        , Button.ripple
                        , Color.text Color.white
                        , Options.onClick (RemoveTeam xs)]
                        [ Icon.i "close" ]
                    ]
               , Card.text []
                  [MList.ul [] <| List.map (\x -> MList.li [] [Html.text x]) <| Set.toList xs ]
               ]
            ]

