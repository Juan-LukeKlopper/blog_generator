main :: IO ()
main = putStrLn (render myhtml)

type Title = String

newtype HTML = HTML String

getHTMLString :: HTML -> String
getHTMLString (HTML str) = str

render :: HTML -> String
render (HTML str) = str

newtype Structure = Structure String

getStructureString :: Structure -> String
getStructureString (Structure str) = str

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

customWrap_ :: String -> String -> String
customWrap_ tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Title -> Structure -> HTML
html_ title content =
  HTML
    ( customWrap_ "html"
      ( customWrap_ "head" (customWrap_ "title" title)
        <> customWrap_  "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . customWrap_ "p"

h1_ :: String -> Structure
h1_ = Structure . customWrap_ "h1"

myhtml :: HTML
myhtml = html_ "Programming adventures" ( append_ (h1_ "Hello, my name is Juan-luke Klopper")( append_(p_ "i'm a software developer that was born in South Africa")(p_ "And this is my first ever blog")))
