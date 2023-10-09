port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, h2, span, small, input, label)
import Html.Attributes exposing (style, class, classList, type_,  value, title)
import Html.Events exposing (onClick, onMouseDown, onMouseUp, onMouseOver, onMouseOut, onMouseLeave, onInput)
import Json.Encode
import WebAudio
import WebAudio.Property exposing (..)
import WebAudio.Context exposing (AudioContext)

port toWebAudio : Json.Encode.Value -> Cmd msg

port wakeWebAudio : () -> Cmd msg

type alias Model =
    { contextEncoded : Json.Encode.Value
    , context : AudioContext
    , isPlaying : Bool
    , previousIsPlaying : Bool
    , playingText : String
    , playingNotes : List Freq
    , rootNote : Freq
    , volume : Float
    }

type alias Freq = Float

type alias Vol = Float

initialModel : Json.Encode.Value -> Model
initialModel contextEncoded =
    let
        contextOrCrash =
            case WebAudio.Context.from contextEncoded of
                Just  context ->
                    context
                    
                Nothing ->
                    Debug.todo "No Audio Context"
    in
    { contextEncoded = contextEncoded
    , context = contextOrCrash
    , playingText = "Click once to activate" 
    , playingNotes = []
    , previousIsPlaying = False
    , isPlaying = False
    , rootNote = a4
    , volume = 0.5
    }


type Msg
    = SetChord Chord
    | Stop
    | Play
    | ChangeVol Float

    
    
a4 : Freq
a4 =
    440
    
    
type alias Chord =
    { smallLabel : String
    , bigLabel : String
    , header : String
    , notes : List Freq
    , beats : Freq
    }

    
justFifth rootNote =
    [rootNote, (applyInterval rootNote (3, 2))]
  
  
fifths : Freq -> List Chord
fifths rootNote =
    [ Chord "Single Note" "Root" "🔊 A₄ by itself (440Hz)" [rootNote] 0
    , makeInterval "Kite Perfect" "Fifth" "🔊 Kite 5th (41-tone)"
        rootNote (applyCents rootNote 702.44) (applyInterval rootNote (3, 2))
    , Chord "Just Perfect" "Fifth" "🔊 Just 5th (Pure)" [rootNote, (applyInterval rootNote (3, 2))] 0
    , makeInterval "Standard Perfect" "Fifth" "🔊 Standard 5th (12-tone)"
        rootNote (applyCents rootNote 700) (applyInterval rootNote (3, 2))
    ]


majorThirds : Freq -> List Chord
majorThirds rootNote =
    [ Chord "Single Note" "Root" "🔊 A₄ by itself (440Hz)" [rootNote] 0
    , makeInterval "Kite DownMajor" "Third" "🔊 Kite ⌄M3 (41-tone)"
        rootNote (applyCents rootNote 380.49) (applyInterval rootNote (5, 4))
    , Chord "Just Major" "Third" "🔊 Just 5th (Pure)" [rootNote, (applyInterval rootNote (5, 4))] 0
    , makeInterval "Standard Major" "Third" "🔊 Standard M3 (12-tone)"
        rootNote (applyCents rootNote 400) (applyInterval rootNote (5, 4))
    ]

fourths : Freq -> List Chord
fourths rootNote =
    [ Chord "Single Note" "Root" "🔊 A₄ by itself (440Hz)" [rootNote] 0
    , makeInterval "Kite Perfect" "Fourth" "🔊 Kite P4 (41-tone)"
        rootNote (applyCents rootNote 497.56) (applyInterval rootNote (4, 3))
    , Chord "Just Perfect" "Fourth" "🔊 Just 4th (Pure)" [rootNote, (applyInterval rootNote (4, 3))] 0
    , makeInterval "Standard Perfect" "Fourth" "🔊 Standard P4 (12-tone)"
        rootNote (applyCents rootNote 500) (applyInterval rootNote (4, 3))
    ]


makeInterval smallLabel bigLabel header note1 note2 ideal =
    Chord smallLabel bigLabel header [note1, note2] (abs (note2 - ideal))
    
applyInterval : Float -> (Int, Int) -> Float
applyInterval note (numerator, denominator) =
    note * (toFloat numerator) / (toFloat denominator)
    
centsToFrequencyRatio : Float -> Float
centsToFrequencyRatio cents =
    2^(cents/100/12)

applyCents : Float -> Float -> Float
applyCents note cents =
    note * (centsToFrequencyRatio cents)
    
    

update : Msg -> Model -> Model
update msg model =
    case msg of
        Play ->
            { model | isPlaying = True }
            
        Stop ->
            { model | isPlaying = False, playingText = "Press and hold an interval to play" }

        SetChord chord ->
            { model | playingText = chord.header
            , playingNotes = chord.notes
            }
            
        ChangeVol newVol ->
            { model | volume = newVol }




view : Model -> Html Msg
view model =
    div [classList [("playing", model.isPlaying)] , onMouseLeave Stop] 
        ((h2 [style "text-align" "center"] [ text model.playingText ])
            :: (List.map buttonRow 
            [ majorThirds model.rootNote
            , fourths model.rootNote
            , fifths model.rootNote
            ])
            ++ (volumeSlider model)
        )

volumeSlider model =
    let
        pausedAttr =
            case WebAudio.Context.state model.context of
                WebAudio.Context.Suspended ->
                    Html.Attributes.attribute "suspended" "true"

                WebAudio.Context.Closed ->
                    Html.Attributes.disabled True

                WebAudio.Context.Running ->
                    Html.Attributes.disabled False

    in
    [ text "🔊"
    , input 
        [Html.Attributes.type_ "range"
        , pausedAttr
        , value (String.fromFloat model.volume)
        , onInput (\s -> ChangeVol (Maybe.withDefault model.volume (String.toFloat s)))
        , title "Distortion (0ver 50%) makes the interference more obvious"
        , Html.Attributes.min "0.001"
        , Html.Attributes.max "1"
        , Html.Attributes.step "0.001"
        ] []
    ]
  

buttonRow chords =
    div [onMouseDown Play
        , onMouseUp Stop
        , style "display" "flex"
        , style "justify-content" "space-between"
        , style "gap" "10px"
        ]
            (List.map chordButton chords)

chordButton : Chord -> Html Msg
chordButton chord =
    let
        beatSpeedAttr =
            Html.Attributes.attribute "style" ("--beat-speed: " ++ String.fromFloat beatSpeed ++ "s") 
        
        beatSpeed =
            1 / chord.beats / 4
    in
    div [ class "noteButton", onMouseOver (SetChord chord), beatSpeedAttr ] 
        [ small [] [ text chord.smallLabel]
        , div [] [ text chord.bigLabel]
        ]


audio : Model -> List WebAudio.Node
audio model =
    let
        playNote freq =
            WebAudio.oscillator [ (WebAudio.Property.frequency freq)]
                [ fade model]
    in
    List.map playNote model.playingNotes
    
    
fade model =
    case (model.previousIsPlaying, model.isPlaying) of
        (False, True) ->
            WebAudio.gain
                [ (WebAudio.Property.gain 0.000000000001)
                , exponentialRampToValueAtTime ((WebAudio.Context.currentTime model.context)+0.1) 
                 (WebAudio.Property.gain model.volume)
                ]     
                [ WebAudio.audioDestination ]   
        
        (True, False) ->
             WebAudio.gain 
                [ (WebAudio.Property.gain  model.volume)
                , exponentialRampToValueAtTime ((WebAudio.Context.currentTime model.context)+0.1) 
                 (WebAudio.Property.gain 0.000000000001)
                ]     
                [ WebAudio.audioDestination ]       
        
        (False, False) ->
            WebAudio.gain 
                [ (WebAudio.Property.gain 0.000000000001) ]     
                [ WebAudio.audioDestination ]

        (True, True) ->
            WebAudio.gain 
                [ (WebAudio.Property.gain model.volume) ]     
                [ WebAudio.audioDestination ]

main : Program Json.Encode.Value Model Msg
main =
    let
        overrideUpdate msg model =
            let 
                newContext =
                    Maybe.withDefault model.context (WebAudio.Context.from model.contextEncoded)

                afterUpdate =
                    update msg 
                        {model | context = newContext}

                newModel =
                    {afterUpdate | previousIsPlaying = model.isPlaying}
            in
            ( newModel
                ,  
                audio newModel
                            |> Json.Encode.list WebAudio.encode
                            |> toWebAudio
                )
    in
    Browser.element
        { init = \contextEncoded -> (initialModel contextEncoded, Cmd.none)
        , view = view
        , subscriptions = \model -> Sub.none
        , update = overrideUpdate
        }
