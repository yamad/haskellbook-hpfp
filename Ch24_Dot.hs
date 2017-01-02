{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Chapter 24, Parser Combinators
--
-- DOT language (http://www.graphviz.org/content/dot-language) for
-- graph specification in graphviz
module Ch24_Dot where

import Control.Applicative ((<|>))
import Text.Trifecta
import Data.Char (toUpper, toLower)

-- Grammar
--
-- graph      : [ *strict* ] (*graph* | *digraph*) [ ID ] '{' stmt_list '}'
-- stmt_list  : [ stmt [ ';' ] stmt_list ]
-- stmt	      : node_stmt
--            | edge_stmt
--            | attr_stmt
--            | ID '=' ID
--            | subgraph
-- attr_stmt  :	(*graph* | *node* | *edge*) attr_list
-- attr_list  :	'[' [ a_list ] ']' [ attr_list ]
-- a_list     :	ID '=' ID [ (';' | ',') ] [ a_list ]
-- edge_stmt  :	(node_id | subgraph) edgeRHS [ attr_list ]
-- edgeRHS    :	edgeop (node_id | subgraph) [ edgeRHS ]
-- node_stmt  :	node_id [ attr_list ]
-- node_id    :	ID [ port ]
-- port	      :	':' ID [ ':' compass_pt ]
--            |	':' compass_pt
-- subgraph   :	[ *subgraph* [ ID ] ] '{' stmt_list '}'
-- compass_pt :	(*n* | *ne* | *e* | *se* | *s* | *sw* | *w* | *nw* | *c* | *_*)


-- datatypes taken from haphviz, github.com/NorfairKing/haphviz
type GraphName = String

data GraphType
  = UndirectedGraph
  | DirectedGraph
  deriving (Eq, Show)

type AttributeName = String
type AttributeValue = String
type Attribute = (AttributeName, AttributeValue)

data NodeId
  = UserId String
  | Nameless Int
  deriving (Eq, Show)

data DecType
  = DecGraph
  | DecNode
  | DecEdge
  deriving (Eq, Show)

data DotGraph =
  Graph GraphType
        GraphName
        Dot
  deriving (Eq, Show)

data RankdirType
  = LR
  | RL
  | TB
  | BT
  deriving (Eq, Show)

data Dot
  = Node NodeId
         [Attribute]
  | Edge NodeId
         NodeId
         [Attribute]
  | Declaration DecType
                [Attribute]
  | Ranksame Dot
  | Subgraph String
             Dot
  | RawDot String
  | Label String
  | Rankdir RankdirType
  | DotSeq Dot
           Dot
  | DotEmpty
  deriving (Eq, Show)

instance Monoid Dot where
  mempty = DotEmpty
  mappend DotEmpty d = d
  mappend d DotEmpty = d
  mappend d (DotSeq d1 d2) = DotSeq (mappend d d1) d2
  mappend d1 d2 = DotSeq d1 d2



-- Parser ----

-- graph      : [ *strict* ] (*graph* | *digraph*) [ ID ] '{' stmt_list '}'
-- stmt_list  : [ stmt [ ';' ] stmt_list ]
-- stmt	      : node_stmt
--            | edge_stmt
--            | attr_stmt
--            | ID '=' ID
--            | subgraph
-- attr_stmt  :	(*graph* | *node* | *edge*) attr_list
-- attr_list  :	'[' [ a_list ] ']' [ attr_list ]
-- a_list     :	ID '=' ID [ (';' | ',') ] [ a_list ]
-- edge_stmt  :	(node_id | subgraph) edgeRHS [ attr_list ]
-- edgeRHS    :	edgeop (node_id | subgraph) [ edgeRHS ]
-- node_stmt  :	node_id [ attr_list ]
-- node_id    :	ID [ port ]
-- port	      :	':' ID [ ':' compass_pt ]
--            |	':' compass_pt
-- subgraph   :	[ *subgraph* [ ID ] ] '{' stmt_list '}'
-- compass_pt :	(*n* | *ne* | *e* | *se* | *s* | *sw* | *w* | *nw* | *c* | *_*)

graph :: Parser DotGraph
graph = do
  commentSpace
  skipOptional tSTRICT
  t <- graphType <?> "type of graph"
  name <- option "" $ tokenC iD <?> "name"
  stmts <- braces $ tokenC (statementList t) <?> "statements"
  return $ Graph t name stmts
  where
    graphType = do
      t <- tGRAPH <|> tDIGRAPH
      case map toLower t of
        "graph" -> return UndirectedGraph
        "digraph" -> return DirectedGraph

statementList
  :: (Monad m, TokenParsing m)
  => GraphType -> m Dot
statementList gt =
  mconcat <$>
  some
    (commentSpace *> tokenC (statement gt) <* skipOptional (symbol ";"))

statement
  :: (Monad m, TokenParsing m)
  => GraphType -> m Dot
statement gt =
  choice
    [ try $ edgeStatement gt
    , try bareAttribute
    , try nodeStatement
    , try attributeStatement
    , subgraph gt
    ]

bareAttribute :: (Monad m, TokenParsing m) => m Dot
bareAttribute = Declaration DecGraph . (:[]) <$> attribute

attributeStatement
  :: (Monad m, TokenParsing m)
  => m Dot
attributeStatement = do
  t <- tGRAPH <|> tNODE <|> tEDGE <?> "attribute type"
  let decT =
        case map toLower t of
          "graph" -> DecGraph
          "node" -> DecNode
          "edge" -> DecEdge
  Declaration decT <$> attributeList <?> "attribute list"

attributeList
  :: (Monad m, TokenParsing m)
  => m [Attribute]
attributeList = concat <$> some (brackets $ many (attribute <* maybeSep))
  where
    maybeSep =
      commentSpace *>
      (skipOptional (choice . map symbol $ [";", ","]) <?> "attribute separator") <*
      commentSpace

attribute
  :: (Monad m, TokenParsing m)
  => m (String, String)
attribute = do
  name <- token iD <?> "attribute name identifier"
  value <- symbol "=" *> tokenC iD <?> "attribute value (= ID)"
  return (name, value)

edgeStatement
  :: (Monad m, TokenParsing m)
  => GraphType -> m Dot
edgeStatement gt = do
  let edgeOp =
        case gt of
          DirectedGraph -> "->"
          UndirectedGraph -> "--"
      target = tokenC (try (subgraph gt) <|> (flip Node [] <$> nodeId))
  first <- target <* symbol edgeOp
  targets <- target `sepBy1` symbol edgeOp
  attrs <- option [] attributeList <?> "attributes"
  return $ makeEdges attrs (first:targets)

nodeStatement
  :: (Monad m, TokenParsing m)
  => m Dot
nodeStatement = Node <$> nodeId <*> option [] attributeList

nodeId
  :: (Monad m, TokenParsing m)
  => m NodeId
nodeId = UserId <$> tokenC iD <* skipOptional port <?> "node id"

port :: (Monad m, TokenParsing m) => m ()
port =
  char ':' *> (try compassPt <|> (iD <* option "" (char ':' *> compassPt))) *>
  return () <?> "port"

subgraph
  :: (Monad m, TokenParsing m)
  => GraphType -> m Dot
subgraph gt = do
  let name = option "" (tSUBGRAPH *> tokenC iD)
  Subgraph <$> name <*> braces (statementList gt)

compassPt
  :: TokenParsing m
  => m String
compassPt =
  (choice . map symbol) ["ne", "nw", "n", "se", "sw", "s", "e", "w", "c", "_"]

-- | identifier
iD
  :: (Monad m, TokenParsing m)
  => m String
iD = choice [htmlString, quoted, numeralID, alphaNumID]
-- "Any string of alphabetic ([a-zA-Z\200-\377]) characters,
-- underscores ('_') or digits ([0-9]), not beginning with a
-- digit"
  where
    alphaNumID
      :: (Monad m, TokenParsing m)
      => m String
    alphaNumID = do
      first <- alpha <|> char '_'
      rest <- many (alpha <|> digit <|> char '_')
      return $ first : rest
      where
        alpha = letter <|> satisfyRange '\200' '\377'
    -- "a numeral [-]?(.[0-9]+ | [0-9]+(.[0-9]*)? )"
    numeralID
      :: TokenParsing m
      => m String
    numeralID = (++) <$> neg <*> (decOnly <|> fullNum)
      where
        neg = option "" (string "-") -- optional leading sign
        decOnly = (:) <$> char '.' <*> some digit -- .[0-9]+
        decPart = (:) <$> char '.' <*> many digit -- .[0-9]*
        fullNum = (++) <$> some digit <*> option [] decPart -- [0-9]+(.[0-9]*)?
    -- "any double-quoted string ("...") possibly containing escaped quotes (\")"
    quoted
      :: TokenParsing m
      => m String
    quoted = quotes (concat <$> many (try escapedQuote <|> nonQuote))
      where
        quotes = between (char '"') (char '"')
        nonQuote = (: []) <$> notChar '"'
        escapedQuote = string "\\\""
    -- "an HTML string (<...>)"
    htmlString
      :: TokenParsing m
      => m String
    htmlString = angles (many $ notChar '>')


slashComment
  :: (Monad m, TokenParsing m)
  => m String
slashComment = symbol "//" *> many (notChar '\n') <?> "comment"

starComment
  :: (Monad m, TokenParsing m)
  => m String
starComment =
  symbol "/*" *> manyTill anyChar (try (string "*/")) <?> "wrapped comment"

cppDecl
  :: (Monad m, TokenParsing m)
  => m String
cppDecl = symbol "#" *> many (notChar '\n') <?> "preprocessor declaration"

comment
  :: (Monad m, TokenParsing m)
  => m String
comment = try slashComment <|> starComment <|> cppDecl


-- Terminals ----
--
-- "The keywords node, edge, graph, digraph, subgraph, and strict are
-- case-independent."
tSTRICT   :: (Monad m, TokenParsing m) => m String
tSTRICT   = caseInsensitiveSymbol "strict"
tNODE     :: (Monad m, TokenParsing m) => m String
tNODE     = caseInsensitiveSymbol "node"
tGRAPH    :: (Monad m, TokenParsing m) => m String
tGRAPH    = caseInsensitiveSymbol "graph"
tDIGRAPH  :: (Monad m, TokenParsing m) => m String
tDIGRAPH  = caseInsensitiveSymbol "digraph"
tEDGE     :: (Monad m, TokenParsing m) => m String
tEDGE     = caseInsensitiveSymbol "edge"
tSUBGRAPH :: (Monad m, TokenParsing m) => m String
tSUBGRAPH = caseInsensitiveSymbol "subgraph"


-- Helpers ----
caseInsensitiveChar :: CharParsing m => Char -> m Char
caseInsensitiveChar c = char (toUpper c) <|> char (toLower c)

caseInsensitiveString :: (CharParsing m, Monad m) => String -> m String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

caseInsensitiveSymbol :: (Monad m, TokenParsing m) => String -> m String
caseInsensitiveSymbol s = tokenC $ caseInsensitiveString s

commentSpace :: (Monad m, TokenParsing m) => m ()
commentSpace =
  skipMany (token comment <|> some space) <?> "comments and whitespace"

tokenC :: (Monad m, TokenParsing m) => m a -> m a
tokenC p = p <* (commentSpace <|> pure ())

makeEdges :: [Attribute] -> [Dot] -> Dot
makeEdges _ [] = DotEmpty
makeEdges _ [_] = DotEmpty
makeEdges atts (x:y:xs) = mappend (makeEdge atts x y) (makeEdges atts (y:xs))

makeEdge :: [Attribute] -> Dot -> Dot -> Dot
makeEdge atts (Node nid1 _) (Node nid2 _) = Edge nid1 nid2 atts
makeEdge atts (Subgraph _ d) n2@(Node _ _) =
  mconcat $ map (flip (makeEdge atts) n2) $ collectNodes d
makeEdge atts n1@(Node _ _) (Subgraph _ d) =
  mconcat $ map ((makeEdge atts) n1) $ collectNodes d
makeEdge atts (Subgraph _ d1) (Subgraph _ d2) =
  mconcat $ makeEdge atts <$> collectNodes d1 <*> collectNodes d2
makeEdge _ _ _ = DotEmpty

collectNodes :: Dot -> [Dot]
collectNodes n@(Node _ _) = [n]
collectNodes (DotSeq d1 d2) = collectNodes d1 ++ collectNodes d2
collectNodes _ = []
