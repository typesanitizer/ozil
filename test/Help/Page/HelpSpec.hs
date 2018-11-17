module Help.Page.HelpSpec where

import Commons
import Help.Page.Help
import Help.Page.Lenses

import Test.Hspec

import Data.Vector.Generic as V

spec_HelpParsing :: Spec
spec_HelpParsing = it "Help parsing indent" $
  (parseHelpPage t1 ^. indents)
    `shouldBe` mkIndentGuess (Just (1, 16)) (Just 5) Nothing
  where
    t1 = "Foo bar baz\n\
         \\n\
         \ -h, --help     Text\n\
         \     --info     Text\n\
         \\n"
    mkIndentGuess i j k = IndentGuess
      { _flagIndent = (mkItemIndent i, j)
      , _subcommandIndent = mkItemIndent k
      , _argIndent = Nothing
      }
    mkItemIndent ii = uncurry ItemIndent <$> ii

spec_HelpParsingTable :: Spec
spec_HelpParsingTable = it "Help Parsing Table" $ do
  checkTable t2 t2FlagTable
  checkTable t3 t3FlagTable
  checkTable t4 t4FlagTable
  where
    checkTable t tbl = (parseHelpPage t ^. body & V.toList) `shouldContain` [tbl]
    mkEntries = V.fromList . fmap (uncurry TableEntry)
    mkFlagTable xs = Tabular Flag (mkEntries xs) (ItemIndent 1 16)
    t2 = "t2\n\
         \ -f --foo       beep\n\
         \    --qux       doot\n\
         \"
    t2FlagTable = mkFlagTable [("-f --foo", "beep"), ("--qux", "doot")]
    t3 = "t3\n\
         \ -f --foo       beep\n\
         \ -b --baz       boop\n\
         \    --qux       doot\n\
         \                doot"
    t3FlagTable = mkFlagTable
      [("-f --foo", "beep"), ("-b --baz", "boop"), ("--qux", "doot doot")]
    t4 = "t4\n\
         \ARGS:\n\
         \ <foo>...       foo"
    t4FlagTable = Tabular Arg (mkEntries [("<foo>...", "foo")]) (ItemIndent 1 16)
