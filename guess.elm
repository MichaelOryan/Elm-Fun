module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


-- Define HTML Program


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Game State


type alias GameState =
    { number : Int
    , guess : Int
    , feedbackText : String
    , feedbackColour : String
    }



-- Messages passed to update


type Msg
    = Number Int
    | UpdateGuess String
    | MakeGuess
    | GenerateNumber



-- Starting game state


init : ( GameState, Cmd Msg )
init =
    ( defaultGameState, cmdGenerateNewNumber )



-- Gamestate helpers
-- Default


defaultGameState : GameState
defaultGameState =
    GameState defaultNumber invalidGuess noFeedBackText noFeedBackColour



-- Default with a specified number to guess


defaultGameStateWithNumber : Int -> GameState
defaultGameStateWithNumber number =
    GameState number invalidGuess noFeedBackText noFeedBackColour



-- Generate a new number to guess


cmdGenerateNewNumber : Cmd Msg
cmdGenerateNewNumber =
    Random.generate Number pickNumber



-- Update


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg gameState =
    case msg of
        -- When asked to make a new random number
        GenerateNumber ->
            ( gameState, cmdGenerateNewNumber )

        -- New gamestate with new number
        Number number ->
            ( defaultGameStateWithNumber number, Cmd.none )

        -- ! [] same as having Cmd.none
        -- Store the players current inputed guess
        UpdateGuess guess ->
            { gameState | guess = Result.withDefault invalidGuess (String.toInt guess) } ! []

        -- Player wishes to pick their current number as a guess
        MakeGuess ->
            updateFeedBack gameState (answer gameState)



-- Update feedback in gamestate to display for the player


updateFeedBack : GameState -> ( String, String ) -> ( GameState, Cmd Msg )
updateFeedBack gameState ( colour, feedback ) =
    { gameState
        | feedbackText = feedback
        , feedbackColour = colour
    }
        ! []



-- Pick a random number between the lowest possible and the highest possible


pickNumber : Random.Generator Int
pickNumber =
    Random.int lowestNumber highestNumber



-- Subscriptions


subscriptions : GameState -> Sub Msg
subscriptions gameState =
    Sub.none



-- Header for the page. Title


htmlHeader : Html Msg
htmlHeader =
    h1 [] [ text ("I am thinking of number between " ++ toString lowestNumber ++ " and " ++ toString highestNumber ++ ".") ]



-- Form input for players guess


htmlInputGuess : Html Msg
htmlInputGuess =
    input [ type_ "text", placeholder "Guess", onInput UpdateGuess ] []



-- Button player presses to make their guess with the current input in the input field


htmlGuessButton : Html Msg
htmlGuessButton =
    button [ onClick MakeGuess ] [ text "Guess" ]



-- Have computer pick a new random number to guess


htmlNewNumberButton : Html Msg
htmlNewNumberButton =
    button [ onClick GenerateNumber ] [ text "New Number" ]



-- Feedback on how the players guess went


htmlGuessFeedbackText : GameState -> Html Msg
htmlGuessFeedbackText gameState =
    div [ style [ ( "color", gameState.feedbackColour ) ] ] [ text gameState.feedbackText ]



-- HTML view


view : GameState -> Html Msg
view gameState =
    div []
        [ htmlHeader
        , htmlInputGuess
        , htmlGuessButton
        , htmlGuessFeedbackText gameState
        , htmlNewNumberButton
        ]



-- Constant for an invalid guess


invalidGuess : Int
invalidGuess =
    -1



-- Constant for the default number the player is trying to guess


defaultNumber : Int
defaultNumber =
    1



-- Lowest possible number in the range the computer can pick


lowestNumber : Int
lowestNumber =
    1



-- Highest possible number in the range the computer can pick


highestNumber : Int
highestNumber =
    10



-- For no feedback text to display


noFeedBackText : String
noFeedBackText =
    ""



-- For no feedback text colour


noFeedBackColour : String
noFeedBackColour =
    ""



-- Colour for when the player guesses the right number


correctColour : String
correctColour =
    "green"



-- Colour for when the player guesses too high


highGuessColour : String
highGuessColour =
    "grey"



-- Colour for when the player guesses too low


lowGuessColour : String
lowGuessColour =
    "blue"



-- Colour for when the player guesses an invalid number or not a number at all


invalidGuessColour : String
invalidGuessColour =
    "red"



-- Check for bad guess/invalid guess


badGuess : GameState -> Bool
badGuess gameState =
    gameState.guess == invalidGuess



-- Check for player guess is higher than number


highGuess : GameState -> Bool
highGuess gameState =
    gameState.guess > gameState.number



-- Check for player guess is lower than number


lowGuess : GameState -> Bool
lowGuess gameState =
    gameState.guess < gameState.number



-- Check answer and give feedback


answer : GameState -> ( String, String )
answer gameState =
    if badGuess gameState then
        ( invalidGuessColour, "Invalid guess. Please enter a positive integer." )
    else if highGuess gameState then
        ( highGuessColour, "The number I am thinking of is lower than your guess." )
    else if lowGuess gameState then
        ( lowGuessColour, "The number I am thinking of is higher than your guess." )
    else
        ( correctColour, "Correct! You guessed my Number!" )
