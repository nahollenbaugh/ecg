{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Zquot
import Functions
import ECGroup
import ECGroupUtil
import ZquotUtil
import Control.Applicative
import Control.Monad.Trans.RWS.Lazy
import Control.Monad

import           Yesod
import           Data.Text           (Text)
data ArithmeticSite = ArithmeticSite

mkYesod "ArithmeticSite" [parseRoutes|
/ecgdescription       EcgResultR POST
/sqrtmodpresult       SqrtModpResultR POST
/cornacchiaresult     CornacchiaResultR POST
/cornacchiainfo       CornacchiaInfoR GET
/sqrtmodpinfo         SqrtModpInfoR GET
/zquotoperationresult ZquotOperationResultR POST
/                     HomeR GET
|]


instance Yesod ArithmeticSite
instance RenderMessage ArithmeticSite FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR = do
  (ecgWidget,            ecgEnctype)            <- generateFormPost ecgForm
  (zquotOperationWidget, zquotOperationEnctype) <- generateFormPost $ zquotOperationForm Nothing Nothing Nothing Nothing Nothing
  (sqrtModpWidget,       sqrtModpEnctype)       <- generateFormPost $ sqrtModpForm Nothing Nothing Nothing
  (cornacchiaWidget,     cornacchiaEnctype)     <- generateFormPost $ cornacchiaForm Nothing Nothing
  defaultLayout $ do
    projectDescription
    formLucius
    formJulius
    [whamlet|
<table>
  <tr>
    <th scope="row" rowspan="2">Z/n (n possibly zero)
    <th scope="row"> +-*/
    <td>
      <form action=@{ZquotOperationResultR} enctype=#{ecgEnctype} method="post">
        ^{zquotOperationWidget}
  <tr>
    <th scope="row">
      Square roots in prime fields&nbsp
      <a href=@{SqrtModpInfoR}>?
    <td>
      <form method="post" action=@{SqrtModpResultR} enctype=#{sqrtModpEnctype}>
        ^{sqrtModpWidget}
  <tr>
    <th scope="row" rowspan="2">Elliptic curve
    <th scope="row">Describe
    <td>
      <form action=@{EcgResultR} enctype=#{zquotOperationEnctype} method="post">
        ^{ecgWidget}
  <tr>
    <th scope="row">Count
    <td>
  <tr>
    <th scope="row">Diophantine
    <th scope="row">
      Cornacchia&nbsp
      <a href=@{CornacchiaInfoR}>?
    <td>
      <form action=@{CornacchiaResultR} enctype=#{cornacchiaEnctype} method="post">
        ^{cornacchiaWidget}
|]
    toWidget [cassius|
table
  border-collapse: collapse
td, th
  border: 1px solid black
  text-align: center
  padding: 0.5em
  font-weight: normal
|]
projectDescription = [whamlet|
<p>
  This is a site I made using the
  <a href="http://yesodweb.com">Yesod
  web framework.  It does some silly things mostly with elliptic curve groups
  over (for now) finite prime fields.  For elliptic curve groups, see
  <a href="https://en.wikipedia.org/wiki/Elliptic_curve#The_group_law">Wikipedia
  though the details are worked out in characteristic not 2 or 3 because the
  equations are much simplified when we are allowed to divide by 2 and 3.  For
  this project I have followed the standard notation in 
  <a href="https://link.springer.com/book/10.1007/978-0-387-09494-6">Silverman
  (though note that the first edition contains some typoes).  
|]



cornacchiaForm d p extra = do
  (dRes, dView) <- mreq intField (inputFieldSettings "d") d
  (pRes, pView) <- mreq intField (inputFieldSettings "p") p
  let widget = do
        [whamlet|
#{extra}
x^2+
^{fvInput dView}
y^2
<input type="submit" value="=">
  ^{fvInput pView}
|]
  let res = case (dRes, pRes) of
              (FormSuccess dValue, FormSuccess pValue) -> FormSuccess (dValue, pValue)
              _ -> failureFrom [dRes, pRes]
  return (res, widget)::RWST
                        (Maybe (Env, FileEnv), ArithmeticSite, [Lang])
                        Enctype
                        Ints
                        (HandlerFor ArithmeticSite)
                        (FormResult (Integer, Integer), WidgetFor ArithmeticSite ())

getSqrtModpInfoR
  = defaultLayout [whamlet|
<p>
  Use the Tonelli Shanks algorithm (algorithm 1.5.1 in
  <a href="https://link.springer.com/book/10.1007/978-3-662-02945-9">Cohen#
  \. the following
  description of the algorithm summarizes the discussion there) to compute square
  roots mod p in expected time O(ln(p)^4).  There are formulas of the form
<p> \sqrt a = a^\{(p+1)/4} when p\equiv 3 mod 4
<p> and \sqrt a is either a^\{(p+3)/8} or 2a(4a)^\{(p-5)/8} when p\equiv 5 mod 8
  \ that solve the problem for increasingly many p, and this algorithm allows
  us to solve the problem for general p by expressing a square root as the product
  of an easily computed power of a (the (p-5)/4's and (p+1)/4 above) and an
  element of the maximal subgroup of
  (Z/p)* of order a power of two.  The second factor is computed by first
  determining a generator.  It is easy to check that a number is generates, and
  in fact to obtain the claimed average case bound we ought to select candidates
  randomly, but in practice it doesn't (seem to) matter so we don't
|]
getCornacchiaInfoR 
  = defaultLayout [whamlet|
<p>
  For a prime p and a d with 0\<d\<p, compute a solution to x^2+dy^2=p or determine
  that no such exists in tim
|]
postCornacchiaResultR = do
  ((result, _), _) <- runFormPost $ cornacchiaForm Nothing Nothing
  let resultWidget = case result of
                       FormSuccess (d,p) -> let cornacchiaResult = cornacchia d p
                                                ysqterm = if d == 0
                                                          then ""
                                                          else if d == 1
                                                               then "+y^2"
                                                               else "+"++(show d)++"y^2"
                                            in case cornacchiaResult of
                                                 Just (x,y)
                                                   -> [whamlet|
The equation x^2#{ysqterm}=#{p} has a solution (x,y)=(#{x},#{y})|]
                                                 Nothing
                                                   -> [whamlet|
The equation x^2#{ysqterm}=#{p} has no solution in Z|]
                       _                 -> [whamlet|b|]
  (widget, enctype) <- case result of
                         FormSuccess (d,p) -> generateFormPost $ cornacchiaForm (Just d) (Just p)
                         _  -> generateFormPost $ cornacchiaForm Nothing Nothing
  defaultLayout $ do
    resultWidget
    [whamlet|
<form enctype=#{enctype} method="post" action=@{CornacchiaResultR}>
  ^{widget}
|]
    formJulius
    formLucius

sqrtModpFields square mod result = do
  (squareRes, squareView) <- (mreq intField (inputFieldSettings "") square)
                             :: MForm (HandlerFor ArithmeticSite) (FormResult Integer, FieldView ArithmeticSite)
  (modRes,    modView)    <- mreq intField (inputFieldSettings "") mod
  (resultRes, resultView) <- mreq intField (resultFieldSettings "") $ v result
  return (squareRes,  modRes,  resultRes,
          squareView, modView, resultView)
  where
    v x = case x of Just Undefined -> Nothing
                    Nothing        -> Nothing
                    Just s         -> Just $ toLift s
sqrtModpForm square mod result extra = do 
  (squareRes,  modRes,  resultRes,
   squareView, modView, resultView) <- sqrtModpFields square mod result
  let widget = do
        [whamlet|
                #{extra}
                sqrt
                ^{fvInput squareView}
                <input type="submit" value="=">
                ^{fvInput resultView}
                &nbsp in Z/
                ^{fvInput modView}
                |]
  let res = case (squareRes, modRes) of
              (FormSuccess square, FormSuccess mod) -> FormSuccess (square, mod)
              _ -> failureFrom [squareRes, modRes]
  return (res, widget)

postSqrtModpResultR = do
  ((result, _), _) <- runFormPost $ sqrtModpForm Nothing Nothing Nothing
  case result of
    FormSuccess (square, mod) -> do
      let result        =  sqrtModp $ Value mod square
      let top = show $ dropWhile ((<=) 1000) $ iterate (flip quot 10) $ toLift result
      (widget, enctype) <- generateFormPost $ sqrtModpForm (Just square) (Just mod) (Just result)
      defaultLayout $ do
        [whamlet|
$case result
  $of Undefined
    #{square} is not a square mod #{mod}
  $of _
    #{square} is a square mod #{mod}.  It's square root is #{toLift result}
<form method="post" action=@{SqrtModpResultR} enctype=#{enctype}>
  ^{widget}
|]
        when ((not $ isUndefined result) && (toLift result > 1000))
          (toWidget
            [julius|document.getElementsByClassName("result)[0].setAttribute("placeholder","#{top}...");|])
        formLucius
        formJulius
    _ -> defaultLayout [whamlet|b|]
      
failureFrom (r:rs) = case r of
                       -- the left sides and right sides have different types
                       FormMissing   -> FormMissing
                       FormFailure v -> FormFailure v
                       _             -> failureFrom rs

zquotOperationFields :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Op -> Maybe Integer
                     -> RWST
                     (Maybe (Env, FileEnv), ArithmeticSite, [Lang])
                     Enctype
                     Ints
                     (HandlerFor ArithmeticSite)
                     ( FormResult Integer
                     , FormResult Integer
                     , FormResult Integer
                     , FormResult Op
                     , FormResult Integer
                     , FieldView ArithmeticSite
                     , FieldView ArithmeticSite
                     , FieldView ArithmeticSite
                     , FieldView ArithmeticSite
                     , FieldView ArithmeticSite
                     )
resultFieldSettings placeholder = FieldSettings
                                  ""
                                  Nothing
                                  Nothing
                                  Nothing
                                  [("class","result"),("placeholder",placeholder)]
inputFieldSettings placeholder  = FieldSettings
                                  ""
                                  Nothing
                                  Nothing
                                  Nothing
                                  [("class","input"),("placeholder",placeholder)]
formLucius = toWidget [lucius|
input[type='number'] {width: 3em}
.result {text-align: center}
.input {text-align: center}
|]
formJulius = toWidget [julius|
results = document.getElementsByClassName("result");
for (var i = 0; i < results.length; i++){
  results[i].setAttribute("disabled","disabled");
}
|]
zquotOperationFields fst snd mod operator result = do
  (fstRes, fstView)           <- mreq intField (inputFieldSettings "") fst
  (sndRes, sndView)           <- mreq intField (inputFieldSettings "") snd
  (modRes, modView)           <- mreq intField (inputFieldSettings "") mod
  (operatorRes, operatorView) <- mreq (selectFieldList ops) "" operator
  (resultRes, resultView)     <- mreq intField (resultFieldSettings "") result
  return (fstRes,  sndRes,  modRes,  operatorRes,  resultRes,
          fstView, sndView, modView, operatorView, resultView)
zquotOperationForm fst snd mod operator result extra = do 
  (fstRes,  sndRes,  modRes,  operatorRes,  resultRes,
   fstView, sndView, modView, operatorView, resultView) <- zquotOperationFields fst snd mod operator result
  let widget = do
        [whamlet|
#{extra}
^{fvInput fstView}
^{fvInput operatorView}
^{fvInput sndView}
<input type=submit value="=">
^{fvInput resultView}
&nbsp in Z/
^{fvInput modView}
|]
  case (fstRes, sndRes, modRes, operatorRes) of
    (FormSuccess fst, FormSuccess snd, FormSuccess mod, FormSuccess operator)
      -> return (FormSuccess $ Just (fst, snd, mod, operator), widget)
    _ -> return (FormSuccess Nothing, widget)

-- getZquotOperationR :: Handler Html
-- getZquotOperationR = do
--   (widget, enctype) <- generateFormPost $ zquotOperationForm Nothing Nothing Nothing (Just Plus) Nothing
--   defaultLayout [whamlet|
-- Hello World!  hackage.haskell.org/package/HUnit introduction to hunit
-- <form method=post action=@{ZquotOperationResultR} enctype=#{enctype}>
--   ^{widget}
-- |]
postZquotOperationResultR = do
  ((result, _), _)  <- runFormPost $ zquotOperationForm Nothing Nothing Nothing (Just Plus) Nothing
  case result of
    (FormSuccess (Just (fst, snd, mod, operator))) -> do
      (widget, enctype) <- generateFormPost $ zquotOperationForm (Just fst)  (Just snd)  (Just mod) (Just operator)
                           (Just $ toLift $ on2 (op operator) (Value mod) fst snd)
      defaultLayout $ do
        [whamlet|
<form method=post action=@{ZquotOperationResultR} enctype=#{enctype}>
  ^{widget}
|]
        formLucius
        formJulius
    _             -> defaultLayout [whamlet|b|]

ecgForm :: Html -> MForm Handler (FormResult Curve, Widget)
ecgForm extra = do 
  (nRes, nView) <- mreq intField (inputFieldSettings "")  Nothing
  (aRes, aView) <- mreq intField (inputFieldSettings "a_1") Nothing
  (bRes, bView) <- mreq intField (inputFieldSettings "a_2") Nothing
  (cRes, cView) <- mreq intField (inputFieldSettings "a_3") Nothing
  (dRes, dView) <- mreq intField (inputFieldSettings "a_4") Nothing
  (fRes, fView) <- mreq intField (inputFieldSettings "a_6") Nothing
  let widget = do
        [whamlet|
#{extra}
{y^2+
^{fvInput aView}
xy+
^{fvInput cView}
y=x^3+
^{fvInput bView}
x^2+
^{fvInput dView}
x+
^{fvInput fView}
} over Z/
^{fvInput nView}
&nbsp
<input type="submit" value="go">
|]
  let curveRes = curve <$> nRes <*> aRes <*> bRes <*> cRes <*> dRes <*> fRes
  return (curveRes, widget)
postEcgResultR = do
  ((result, widget), enctype) <- runFormPost ecgForm
  case result of
    FormSuccess curve -> defaultLayout $ displayCurve curve
    _  -> defaultLayout [whamlet|b|]

operationForm :: Html -> MForm Handler (FormResult (Operation Zquot), Widget)
operationForm = renderDivs $ flip operation
  <$> (parseZquot <$> areq textField "" (Just "0"))
  <*> (parseOperator <$> areq textField "" (Just "+"))
  <*> (parseZquot <$> areq textField "" (Just "0"))

displayCurve c = do
  let ps = map show $ enumerate c
  let pointsString = foldl ((++) . (flip (++) ", ")) (head ps) (tail ps)
  [whamlet|
<p>Curve #{show c}
<p>with points: #{pointsString}
|]
  when (singular c) [whamlet|is singular|]
  unless (singular c) $ multiplicationTable c
multiplicationTable c = do
  [whamlet|
<table>
  <head>
    <tr>
    <th>+
    $forall p <- map show $ enumerate c
      <th scope="col">#{p}
  <tbody>
    $forall p <- enumerate c
      <tr>
        <th scope="row">#{show p}
        $forall q <- map (show . ep c p) $ enumerate c
          <td>#{q}
|]
  toWidget [cassius|
table
  border-collapse: collapse
td
  border: 1px solid black
  text-align: center
th
  border: 3px solid black
|]
  

data Op = Plus | Minus | Times | Div deriving Eq
ops = [("+"::Text,Plus), ("-",Minus), ("*",Times), ("/",Div)]
op :: Op -> Zquot -> Zquot -> Zquot
op Plus = (+)
op Minus = (-)
op Times = (*)
op Div = (/)
parseZquot t = Value 0 $ if (head $ show t) == '0' then 1 else 2
parseOperator _ = (+)

main :: IO ()
main = warp 3000 ArithmeticSite
