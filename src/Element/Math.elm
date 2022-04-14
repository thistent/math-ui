module Element.Math exposing (..)

-- import Html.Attributes exposing (id, style)

import Element as El exposing (Attribute, Element, el)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



--import Element.Math.Attribute as MA
--import Element.Math.Internal.Model as Internal


type MathExpr
    = Const String
    | Var String
    | TyVar String
    | KVar String
    | ExprList (List MathExpr)
    | Op String
    | Exp MathExpr MathExpr
    | Sub MathExpr MathExpr
    | ExpSub MathExpr MathExpr MathExpr
    | Frac MathExpr MathExpr
    | OfType MathExpr MathExpr
    | OfKind MathExpr MathExpr
    | Pars MathExpr
    | Lam MathExpr MathExpr
    | ReductionRule String MathExpr MathExpr
    | ApplyTo MathExpr MathExpr
    | SpacedExprs (List MathExpr)
    | ContainedBy MathExpr MathExpr


type alias ColorScheme =
    { const : El.Color
    , var : El.Color
    , tyVar : El.Color
    , kVar : El.Color
    , op : El.Color
    , exp : El.Color
    , frac : El.Color
    , ofType : El.Color
    , ofKind : El.Color
    , pars : El.Color
    , lam : El.Color
    , reductionRule : El.Color
    , replace : El.Color
    }



-- Ast Generating Functions --


kindNamed : String -> MathExpr
kindNamed =
    KVar


ofKind : MathExpr -> MathExpr -> MathExpr
ofKind =
    OfKind


tyVar : String -> MathExpr
tyVar =
    TyVar


elementOf : MathExpr -> MathExpr -> MathExpr
elementOf b a =
    ExprList [ a, Op isIn, b ]


reductionRule : String -> MathExpr -> MathExpr -> MathExpr
reductionRule =
    ReductionRule


kindStar : MathExpr
kindStar =
    Op star


typeNamed : String -> MathExpr
typeNamed =
    TyVar


pars : MathExpr -> MathExpr
pars =
    Pars


containedBy : MathExpr -> MathExpr -> MathExpr
containedBy =
    ContainedBy


ifix : MathExpr -> MathExpr -> MathExpr
ifix a b =
    Pars <| ExprList [ Op "ifix", a, b ]


fun : MathExpr -> MathExpr -> MathExpr
fun a b =
    Pars <| ExprList [ a, Op arrow, b ]


var : String -> MathExpr
var =
    Var


exprList : List MathExpr -> MathExpr
exprList =
    SpacedExprs


impliedBy : MathExpr -> MathExpr -> MathExpr
impliedBy a b =
    ExprList [ a, Op turnStile, b ]



-- Reduce and Render --


reduce : MathExpr -> MathExpr
reduce expr =
    case expr of
        Sub (Exp b e) s ->
            ExpSub (reduce b) (reduce e) (reduce s)

        Exp (Sub b s) e ->
            ExpSub (reduce b) (reduce e) (reduce s)

        _ ->
            expr


render : Float -> ColorScheme -> MathExpr -> Element msg
render size color expr =
    el
        [ Font.size <| round size
        , Font.regular
        , Font.italic
        , El.centerX
        ]
    <|
        case reduce expr of
            Const s ->
                el
                    [ El.centerX
                    , El.centerY
                    , El.padding <| round <| size * 0.1
                    , Font.color color.const --<| El.rgb 1 0.8 0.6
                    ]
                <|
                    El.text s

            Var s ->
                el
                    [ El.centerX
                    , El.centerY
                    , El.padding <| round <| size * 0.1
                    , Font.color color.var --<| El.rgb 0.6 0.8 1

                    --, El.moveUp <| size * 0.1
                    ]
                <|
                    El.text s

            TyVar s ->
                el
                    [ El.centerX
                    , El.centerY
                    , El.padding <| round <| size * 0.1
                    , Font.color color.tyVar --<| El.rgb 0.6 1 0.8
                    , Font.bold
                    ]
                <|
                    El.text s

            KVar s ->
                el
                    [ El.centerX
                    , El.centerY
                    , El.padding <| round <| size * 0.1
                    , Font.color color.kVar --<| El.rgb 0.8 1 0.6
                    , Font.bold
                    ]
                <|
                    El.text s

            ExprList ls ->
                El.row
                    [ El.centerX
                    , El.centerY

                    --, El.padding <| round <| size * 0.1
                    , El.spacing <| round <| size * 0.5
                    ]
                <|
                    List.map (render size color) ls

            Op o ->
                el [ Font.color color.op ] <| El.text o

            Exp e b ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.2
                    , El.padding <| round <| size * 0.1
                    ]
                    [ render size color b
                    , el
                        [ El.alignTop
                        , El.moveUp <| size * 0.2
                        , El.moveLeft 2.0
                        ]
                      <|
                        render (size * 0.75) color e
                    ]

            Sub s b ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.2
                    , El.padding <| round <| size * 0.1
                    ]
                    [ render size color b
                    , el
                        [ El.alignBottom
                        , El.moveLeft 2.0
                        ]
                      <|
                        render (size * 0.75) color s
                    ]

            ExpSub e s b ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.2
                    , El.padding <| round <| size * 0.1
                    ]
                    [ render size color b
                    , El.column
                        [ El.moveLeft 2.0 ]
                        [ el
                            [ El.alignTop
                            , El.moveRight 1.0
                            ]
                          <|
                            render (size * 0.75) color e
                        , el
                            [ El.alignBottom
                            , El.moveLeft 2.0
                            ]
                          <|
                            render (size * 0.75) color s
                        ]
                    ]

            Frac n d ->
                render size color <| ReductionRule "" n d

            OfType t v ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.3
                    , El.padding <| round <| size * 0.1
                    ]
                    [ el [ El.centerX, El.centerY ] <| render size color v
                    , el
                        [ El.centerX
                        , El.centerY
                        , Font.size <| round <| size * 0.8
                        , Font.color color.ofType
                        ]
                      <|
                        El.text "∶"
                    , el [ El.centerX, El.centerY ] <| render size color t
                    ]

            OfKind k v ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.25
                    , El.padding <| round <| size * 0.1
                    ]
                    [ el [ El.centerX, El.centerY ] <| render size color v
                    , el
                        [ El.centerX
                        , El.centerY
                        , Font.size <| round <| size * 0.8
                        , Font.color color.ofKind --<| El.rgb 0.5 0.5 0.5
                        ]
                      <|
                        --"⦂⦂", "::"
                        El.text "∷"
                    , el
                        [ El.centerX
                        , El.centerY
                        ]
                      <|
                        render size color k
                    ]

            Pars exp ->
                el [ El.paddingXY (round <| size * 0.15) 0 ] <|
                    el
                        [ El.paddingEach
                            { edges
                                | left = round <| size * 0.3
                                , right = round <| size * 0.35
                                , top = round <| size * 0.25
                                , bottom = round <| size * 0.3
                            }
                        , Border.widthEach
                            { edges
                                | left = round <| size * 0.1
                                , right = round <| size * 0.1
                            }
                        , Border.roundEach
                            { topLeft = round <| size * 1.5
                            , topRight = round <| size * 1.15
                            , bottomLeft = round <| size * 1.15
                            , bottomRight = round <| size * 1.5
                            }
                        , color.pars |> changeAlpha 0.3 |> Border.color -- El.rgba 0.5 0.5 0.5 0.25
                        , color.pars |> changeAlpha 0.05 |> Bg.color -- El.rgba 0.5 0.5 0.5 0.1
                        ]
                    <|
                        --el [ El.moveUp <| size * 0.075 ] <|
                        render (size * 0.98) color exp

            Lam v b ->
                El.row [ Font.color color.lam ]
                    [ el [ El.padding <| round <| size * 0.2 ] <| El.text lambda
                    , render size color v
                    , el [ El.padding <| round <| size * 0.2, El.moveRight <| size * 0.1 ] <| El.text "⟶"
                    , render size color b
                    ]

            ReductionRule name n d ->
                let
                    marginSize =
                        round <| size * 0.4
                in
                El.row []
                    [ El.column
                        [ El.centerX
                        , El.centerY
                        , El.spacing <| round <| size * 0.1
                        , El.padding <| round <| size * 0.1
                        , El.moveRight <| size * 0.1
                        ]
                        [ el
                            [ El.centerX
                            , El.centerY
                            , El.paddingEach
                                { edges
                                    | left = marginSize
                                    , right = marginSize
                                }
                            , El.moveLeft <| size * 0.15
                            , El.moveUp <| size * 0.05
                            ]
                          <|
                            render (size * 0.95) color n
                        , el
                            [ El.width El.fill
                            , El.height <| El.px 2
                            , El.centerX
                            , El.centerY
                            , El.moveLeft <| size * 0.2
                            , Bg.color color.frac --<| El.rgb 0.5 0.5 0.5
                            , El.onRight <|
                                el
                                    [ Font.unitalicized
                                    , Font.bold
                                    , El.moveUp <| size * 0.6
                                    , El.moveRight <| size * 0.25
                                    ]
                                <|
                                    El.text name
                            ]
                            El.none
                        , el
                            [ El.centerX
                            , El.centerY
                            , El.paddingEach
                                { edges
                                    | left = marginSize
                                    , right = marginSize
                                }
                            , El.moveLeft <| size * 0.3
                            ]
                          <|
                            render (size * 0.95) color d
                        ]

                    --, el [ color.frac |> changeAlpha 0 |> Font.color ] <| El.text name
                    ]

            ApplyTo a b ->
                El.row []
                    [ el
                        [ El.width <| El.px <| round <| size * 0.35
                        , El.height El.fill
                        , Border.widthEach { edges | top = 2, bottom = 2, left = 2 }
                        , color.pars |> changeAlpha 0.3 |> Border.color -- El.rgba 0.5 0.5 0.5 0.25
                        , color.pars |> changeAlpha 0.05 |> Bg.color -- El.rgba 0.5 0.5 0.5 0.1
                        ]
                        El.none
                    , el
                        [ El.paddingEach
                            { edges
                                | top = round <| size * 0.15
                                , bottom = round <| size * 0.15
                                , left = round <| size * 0.2
                                , right = round <| size * 0.2
                            }
                        , color.pars |> changeAlpha 0.3 |> Border.color -- El.rgba 0.5 0.5 0.5 0.25
                        , color.pars |> changeAlpha 0.05 |> Bg.color -- El.rgba 0.5 0.5 0.5 0.1
                        ]
                      <|
                        El.row [ El.spacing <| round <| size * 0.1 ] [ render (size * 0.98) color a, render (size * 0.98) color b ]
                    , el
                        [ El.width <| El.px <| round <| size * 0.35
                        , El.height El.fill
                        , Border.widthEach { edges | top = 2, bottom = 2, right = 2 }
                        , color.pars |> changeAlpha 0.3 |> Border.color -- El.rgba 0.5 0.5 0.5 0.25
                        , color.pars |> changeAlpha 0.05 |> Bg.color -- El.rgba 0.5 0.5 0.5 0.1
                        ]
                        El.none
                    ]

            SpacedExprs ls ->
                List.map (render size color) ls
                    |> List.intersperse (el [ El.width <| El.px <| round <| size * 1.8 ] El.none)
                    |> El.row []

            ContainedBy a b ->
                El.row [] [ render size color a, El.text ",", render size color b ]



-- ∋ -- contains
--
-- Math Characters --


{-| The Natural Numbers
-}
nat : String
nat =
    "ℕ"


forAll : MathExpr -> MathExpr -> MathExpr -> MathExpr
forAll v k e =
    ExprList
        [ Op all
        , v |> ofKind k
        , Op "⸳"
        , e
        ]


all : String
all =
    "∀"


thereExists : String
thereExists =
    "∃"


rangeRestriction : String
rangeRestriction =
    "▷"


domRestriction : String
domRestriction =
    "◁"


bigLambda : String
bigLambda =
    "Λ"


lambda : String
lambda =
    "λ"


context : MathExpr
context =
    Op "Γ"


dot : String
dot =
    "⦁"


star : String
star =
    --"∗"
    "★"


isIn : String
isIn =
    "∈"


arrow : String
arrow =
    "⟶"


turnStile : String
turnStile =
    "⊢"


equiv : String
equiv =
    "≡"



-- Helper functions --


edges =
    { top = 0, bottom = 0, left = 0, right = 0 }


corners =
    { topLeft = 0, bottomLeft = 0, bottomRight = 0, topRight = 0 }


changeAlpha : Float -> El.Color -> El.Color
changeAlpha a color =
    let
        rgbColor =
            color |> El.toRgb
    in
    { rgbColor | alpha = a } |> El.fromRgb


alpha : String
alpha =
    "α"


beta : String
beta =
    "β"


gamma : String
gamma =
    "γ"


delta : String
delta =
    "δ"
