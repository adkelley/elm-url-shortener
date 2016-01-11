module ShortUrl where

import Html exposing ( .. )
import Html.Attributes exposing ( .. )
--import Graphics.Element exposing ( .. )

type alias UrlId = Int

type alias UrlItem =
  { shortUrl : String
  , longUrl : String
  , uid : UrlId
  }


-- inbound ports
port dispatchReset : Signal ()
port dispatchShorten : Signal String


-- outbound ports
port count : Signal Int
port count =
  Signal.map .ids  modelChanges


port short : Signal String
port short =
  let
    getShortUrl : List UrlItem -> String
    getShortUrl urls =
      case ( List.head urls ) of
        Nothing ->
          ""
        Just urlItem ->
          urlItem.shortUrl
  in
    Signal.map ( getShortUrl << .urls ) modelChanges


-- Model

type alias Model = 
  { urls : List UrlItem
  , ids : Int
  }
  
initialModel : Model
initialModel = 
  { urls = []
  , ids = 0
  }


modelChanges : Signal Model
modelChanges =
  Signal.foldp update initialModel actions


-- Update

type Action
  = Shorten String
  | Reset 




actions : Signal Action
actions =
  Signal.merge 
        ( Signal.map Shorten dispatchShorten )
        ( Signal.map ( always Reset ) dispatchReset  )



update : Action -> Model -> Model
update action model =
  case action of
    Shorten url  ->
      let
        item = 
          UrlItem url url (model.ids + 1)
      in
        { model
        | urls = item :: model.urls
        , ids = model.ids + 1
        }

    Reset ->
      initialModel

-- View

viewUrl :  UrlItem -> Html
viewUrl item =
   li
   [  ]
   [ text item.longUrl ]

  
view : Model -> Html
view model =
  ul
   [  ]
   ( List.map viewUrl model.urls )


main : Signal Html
main =
  Signal.map view modelChanges

-- view : Model -> Element
-- view model =
--   show model

-- main : Signal Element
-- main =
--   Signal.map view modelChanges

