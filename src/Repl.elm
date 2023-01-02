module Repl exposing (..)

import Edn
import Edn.Parser
import Parser


type alias Edn =
    Edn.Edn


run : Parser.Parser a -> String -> Result (List Parser.DeadEnd) a
run =
    Parser.run


edn : Parser.Parser Edn.Edn
edn =
    Edn.Parser.edn
