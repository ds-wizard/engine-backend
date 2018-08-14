module Service.DataManagementPlan.Templates.Css
  ( dmpCSS
  ) where

import Clay
import Prelude hiding (div, span)

-- | CSS style for generate HTML DMP
dmpCSS =
  renderWith pretty [] $ do
    body ? margin (em 0.2) (em 0.2) (em 0.2) (em 0.2)
    headerCSS
    footerCSS
    mainCSS

headerCSS :: Css
headerCSS = do
  header ? borderBottom solid (px 3) (rgb 170 170 170)
  header |> h1 ? do
    fontSize (pct 200)
    margin (em 0.2) (em 0) (em 0.2) (em 0)
  header |> h1 |> div # ".km-name" ? do
    color (rgb 170 170 170)
    fontSize (pct 70)
    float floatRight
  header ? after & do
    content (stringContent "")
    display block
    clear both

footerCSS :: Css
footerCSS = do
  footer ? do
    padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    textAlign center
    fontSize (pct 90)
  footer |> span # ".dsw-link" ? before & content (stringContent "⟨")
  footer |> span # ".dsw-link" ? after & content (stringContent "⟩")

mainCSS :: Css
mainCSS = do
  section # ".chapter" ? borderBottom solid (px 2) (rgb 204 204 204)
  div # ".question" ? do
    borderLeft solid (px 2) orange
    padding (em 0) (em 0.2) (em 0) (em 0.2)
    margin (em 0.1) (em 0) (em 0.5) (em 0.2)
  li # ".expert" |> span # ".email" ? before & content (stringContent "[")
  li # ".expert" |> span # ".email" ? after & content (stringContent "]")
  li # ".reference-resourcepage" |> a # ".resourcepage-link" ? before & content (stringContent "Resource page: ")
  li # ".reference-url" |> a # ".url-link" ? before & content (stringContent "URL: ")
  li # ".reference-xref" |> span # ".xref-uuid" ? before & content (stringContent "Related question: ")
  div # ".answer-block" |> star # ".answer" ? do
    fontWeight bold
    fontStyle italic
  div # ".answer-block" |> star # ".answer" ? before & content (stringContent " ✔  ")
  div # ".answer-option" |> p # ".advice" ? before & content (stringContent " ⓘ  ")
  div # ".answer-items" |> div # ".answer-item" |> span # ".title" ? display none
  div # ".question.required" ? p # ".no-answer" ? do
    fontWeight bold
    color red
  div # ".question.optional" ? p # ".no-answer" ? do
    fontWeight bold
    color lightslategray
  p # ".no-answer" ? before & content (stringContent " ✘  ")
  div # ".indications" |> table |> tbody |> tr |> th ? textAlign (alignSide sideLeft)
  div # ".indications" |> table |> tbody |> tr |> th ? after & content (stringContent ": ")
  div # ".metrics" |> table |> tbody |> tr |> td # lastOfType ? textAlign (alignSide sideRight)
