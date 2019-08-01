module Html where

import Data.List (find)
import Text.HTML.TagSoup (Tag(..))
import Text.HTML.TagSoup.Tree (TagTree(..), parseTree)
import Text.StringLike (StringLike)
import Data.Functor((<&>))
import Data.Maybe (catMaybes, listToMaybe)
import Numeric.Natural

data Selector str = 
    TagName str |
    AttrValue str str

data Locator = Anywhere | ExactPosition Natural deriving Eq


tree :: StringLike str => str -> [TagTree str]
tree = parseTree

mainBranch :: StringLike str => [TagTree str] -> Maybe (TagTree str)
mainBranch = find p where
    p (TagBranch "html" _ _) = True
    p _                     = False

selectorMatches :: Eq str => Selector str -> TagTree str -> Bool
selectorMatches  (TagName wantedName) (TagBranch actualName _ _)
    | actualName == wantedName = True
    | otherwise = False

selectorMatches (AttrValue wantedAttr wantedName) (TagBranch _ attrs _)
    | (wantedAttr, wantedName) `elem` attrs = True
    | otherwise = False

selectorMatches _ _ = False    

select :: Eq str => Locator -> Selector str -> TagTree str  -> [TagTree str]
select l s t = selectWithPos 0 t
    where
        selectWithPos pos tt
            | (l == Anywhere || l == ExactPosition pos) && selectorMatches s tt = [tt]
            | otherwise = let
                children = case tt of 
                    TagBranch _ _ ch -> ch
                    TagLeaf _ -> []
                childrenWithIndex = [0 ..] `zip` children
                in concat $ fmap (uncurry selectWithPos) childrenWithIndex


leafText :: StringLike s => TagTree s -> Maybe s
leafText (TagLeaf (TagText s)) = Just s
leafText _ = Nothing

extractTextFromChildren :: StringLike s => TagTree s -> [s]
extractTextFromChildren (TagBranch _ _ children) = catMaybes $ fmap leafText children
extractTextFromChildren _ = []

extractEntriesFromRoot :: StringLike str => TagTree str -> [[str]]
extractEntriesFromRoot = (fmap extractTextFromCodeBlock) . extractCodes 
    where
        extractTextFromCodeBlock t = concat $ extractParagraphsFromCode t <&> extractTextFromChildren
        
        extractCodes = select Anywhere (TagName "code")

        extractParagraphsFromCode = select Anywhere (TagName "p")

extractNameParagraphFromRoot :: (StringLike str, Show str) => TagTree str -> Maybe str
extractNameParagraphFromRoot r = firstP >>= (listToMaybe . extractTextFromChildren)
        where
            allPs = select (ExactPosition 1) (TagName "p") r
            firstP = listToMaybe allPs        