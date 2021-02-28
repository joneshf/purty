{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Format
  ( format,
  )
where

import qualified "this" Annotation
import qualified "purescript-cst" Language.PureScript.CST.Print
import qualified "purescript-cst" Language.PureScript.CST.Types
import qualified "purs-tool-log" Log
import "rio" RIO hiding (log, span)
import qualified "rio" RIO.NonEmpty
import qualified "purs-tool-cst" SourceRange
import qualified "this" Span

type Indent =
  Utf8Builder

type Indentation =
  Utf8Builder

type Prefix =
  Utf8Builder

type Suffix =
  Utf8Builder

adoBlock ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.AdoBlock Span.Span ->
  IO Utf8Builder
adoBlock log span indentation indent' adoBlock' = case adoBlock' of
  Language.PureScript.CST.Types.AdoBlock ado doStatements in' expr' -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "AdoBlock" adoBlock' span
    sourceToken log indent' blank ado
      <> foldMap
        ( \doStatement' ->
            pure prefix
              <> doStatement log indentation indent doStatement'
        )
        doStatements
      <> pure prefix
      <> sourceToken log indent' blank in'
      <> pure space
      <> expr log indentation indent expr'

array ::
  (Show a) =>
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.Delimited a ->
  IO Utf8Builder
array log indent f g array'' = case array'' of
  Language.PureScript.CST.Types.Wrapped open Nothing close -> do
    debug log "Delimited" array'' (Span.wrapped array'')
    sourceToken log indent blank open
      <> sourceToken log indent blank close
  Language.PureScript.CST.Types.Wrapped open (Just array') close ->
    arrayNonEmpty
      log
      indent
      f
      g
      (Language.PureScript.CST.Types.Wrapped open array' close)

arrayNonEmpty ::
  (Show a) =>
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.DelimitedNonEmpty a ->
  IO Utf8Builder
arrayNonEmpty log indent f g array' = do
  let (before, after) = case span of
        Span.MultipleLines ->
          (blank, blank)
        Span.SingleLine ->
          (space, space)
      span = Span.wrapped array'
  debug log "DelimitedNonEmpty" array' span
  wrapped
    log
    indent
    ( \separated' ->
        pure before
          <> separated log (Span.separated f separated') indent space g separated'
          <> pure after
    )
    array'

blank :: Utf8Builder
blank = ""

binder ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Binder Span.Span ->
  IO Utf8Builder
binder log indentation indent' binder'' = case binder'' of
  Language.PureScript.CST.Types.BinderArray span delimited' -> do
    debug log "BinderArray" binder'' span
    array
      log
      indent'
      SourceRange.binder
      (binder log indentation indent')
      delimited'
  Language.PureScript.CST.Types.BinderBoolean span boolean _ -> do
    debug log "BinderBoolean" binder'' span
    sourceToken log indent' blank boolean
  Language.PureScript.CST.Types.BinderChar span char _ -> do
    debug log "BinderChar" binder'' span
    sourceToken log indent' blank char
  Language.PureScript.CST.Types.BinderConstructor span name' binders -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "BinderConstructor" binder'' span
    qualifiedName log indent' blank name'
      <> foldMap
        ( \binder' ->
            pure prefix
              <> binder log indentation indent' binder'
        )
        binders
  Language.PureScript.CST.Types.BinderNamed span name' at binder' -> do
    debug log "BinderNamed" binder'' span
    name log indent' blank name'
      <> sourceToken log indent' blank at
      <> binder log indentation indent' binder'
  Language.PureScript.CST.Types.BinderNumber span negative number _ -> do
    debug log "BinderNumber" binder'' span
    foldMap (sourceToken log indent' blank) negative
      <> sourceToken log indent' blank number
  Language.PureScript.CST.Types.BinderOp span binder1 name' binder2 -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "BinderOp" binder'' span
    binder log indentation indent' binder1
      <> qualifiedName log indent' prefix name'
      <> pure prefix
      <> binder log indentation indent binder2
  Language.PureScript.CST.Types.BinderParens span wrapped' -> do
    debug log "BinderParens" binder'' span
    parens log span indentation indent' (binder log indentation) wrapped'
  Language.PureScript.CST.Types.BinderRecord span delimited' -> do
    debug log "BinderRecord" binder'' span
    record
      log
      indent'
      (SourceRange.recordLabeled SourceRange.binder)
      ( recordLabeled
          log
          indentation
          indent'
          SourceRange.binder
          (binder log indentation)
      )
      delimited'
  Language.PureScript.CST.Types.BinderString span string _ -> do
    debug log "BinderString" binder'' span
    sourceToken log indent' blank string
  Language.PureScript.CST.Types.BinderTyped span binder' colons type'' -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "BinderTyped" binder'' span
    binder log indentation indent' binder'
      <> sourceToken log indent' space colons
      <> pure prefix
      <> type' log indentation indent type''
  Language.PureScript.CST.Types.BinderVar span name' -> do
    debug log "BinderVar" binder'' span
    name log indent' blank name'
  Language.PureScript.CST.Types.BinderWildcard span wildcard -> do
    debug log "BinderWildcard" binder'' span
    sourceToken log indent' blank wildcard

caseOf ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.CaseOf Span.Span ->
  IO Utf8Builder
caseOf log span indentation indent' caseOf' = case caseOf' of
  Language.PureScript.CST.Types.CaseOf case' head of' branches -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "CaseOf" caseOf' span
    sourceToken log indent' blank case'
      <> pure space
      <> separated
        log
        (Span.separated SourceRange.expr head)
        indent
        space
        (expr log indentation indent)
        head
      <> pure space
      <> sourceToken log indent' blank of'
      <> foldMap
        ( \(binders, guarded') ->
            pure prefix
              <> separated
                log
                (Span.separated SourceRange.binder binders)
                indent
                space
                (binder log indentation indent)
                binders
              <> guarded log indentation indent guarded'
        )
        branches

classFundep ::
  Log.Handle ->
  Indent ->
  Language.PureScript.CST.Types.ClassFundep ->
  IO Utf8Builder
classFundep log indent classFundep' = case classFundep' of
  Language.PureScript.CST.Types.FundepDetermined arrow names -> do
    debug log "FundepDetermined" classFundep' Span.SingleLine
    sourceToken log indent blank arrow
      <> foldMap (name log indent space) names
  Language.PureScript.CST.Types.FundepDetermines names arrow names' -> do
    debug log "FundepDetermines" classFundep' Span.SingleLine
    foldMap
      ( \name' ->
          name log indent blank name'
            <> pure space
      )
      names
      <> sourceToken log indent blank arrow
      <> foldMap (name log indent space) names'

classHead ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.ClassHead Span.Span ->
  IO Utf8Builder
classHead log span indentation indent' classHead' = case classHead' of
  Language.PureScript.CST.Types.ClassHead class' super name' typeVarBindings fundeps -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "ClassHead" classHead' span
    sourceToken log indent' blank class'
      <> foldMap
        ( \(constraints, arrow) ->
            pure prefix
              <> oneOrDelimited
                log
                indent
                SourceRange.constraint
                (constraint log indentation indent)
                constraints
              <> sourceToken log indent space arrow
        )
        super
      <> name log indent space name'
      <> foldMap
        ( \typeVarBinding' ->
            pure space
              <> typeVarBinding log indentation indent typeVarBinding'
        )
        typeVarBindings
      <> foldMap
        ( \(bar, classFundeps) ->
            sourceToken log indent space bar
              <> pure space
              <> separated
                log
                (Span.separated SourceRange.classFundep classFundeps)
                indent
                space
                (classFundep log indent)
                classFundeps
        )
        fundeps

comment ::
  Log.Handle ->
  Language.PureScript.CST.Types.Comment Language.PureScript.CST.Types.LineFeed ->
  IO (Maybe Utf8Builder)
comment log comment'' = case comment'' of
  Language.PureScript.CST.Types.Comment comment' -> do
    debug log "Comment" comment'' (Span.comment Span.lineFeed comment'')
    pure (Just (display comment'))
  Language.PureScript.CST.Types.Line _ -> do
    Log.debug log "Not formatting `Line`"
    pure Nothing
  Language.PureScript.CST.Types.Space _ -> do
    Log.debug log "Not formatting `Space`"
    pure Nothing

commentLeading ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Types.Comment Language.PureScript.CST.Types.LineFeed ->
  IO Utf8Builder
commentLeading log indent prefix comment'' = case comment'' of
  Language.PureScript.CST.Types.Comment comment' -> do
    debug log "Comment" comment'' (Span.comment Span.lineFeed comment'')
    pure (prefix <> display comment' <> newline <> indent)
  Language.PureScript.CST.Types.Line _ -> do
    Log.debug log "Not formatting `Line`"
    pure blank
  Language.PureScript.CST.Types.Space _ -> do
    Log.debug log "Not formatting `Space`"
    pure blank

commentTrailing ::
  (Show a) =>
  Log.Handle ->
  (a -> Span.Span) ->
  Prefix ->
  Language.PureScript.CST.Types.Comment a ->
  IO Utf8Builder
commentTrailing log f prefix comment'' = case comment'' of
  Language.PureScript.CST.Types.Comment comment' -> do
    debug log "Comment" comment'' (Span.comment f comment'')
    pure (prefix <> space <> display comment')
  Language.PureScript.CST.Types.Line _ -> do
    Log.debug log "Not formatting `Line`"
    pure blank
  Language.PureScript.CST.Types.Space _ -> do
    Log.debug log "Not formatting `Space`"
    pure blank

commentsLeading ::
  Log.Handle ->
  Indent ->
  Prefix ->
  [Language.PureScript.CST.Types.Comment Language.PureScript.CST.Types.LineFeed] ->
  IO Utf8Builder
commentsLeading log indent prefix commentsLeading' = case commentsLeading' of
  [] -> do
    Log.debug log "No leading comments to format"
    pure blank
  _ -> do
    debug log "leading comments" commentsLeading' Span.MultipleLines
    foldMap (commentLeading log indent prefix) commentsLeading'

commentsTrailing ::
  (Show a) =>
  Log.Handle ->
  (a -> Span.Span) ->
  Prefix ->
  [Language.PureScript.CST.Types.Comment a] ->
  IO Utf8Builder
commentsTrailing log f prefix commentsTrailing' = case commentsTrailing' of
  [] -> do
    Log.debug log "No trailing comments to format"
    pure blank
  _ -> do
    debug log "trailing comments" commentsTrailing' Span.MultipleLines
    foldMap (commentTrailing log f prefix) commentsTrailing'

commentsTrailingModule ::
  Log.Handle ->
  [Language.PureScript.CST.Types.Comment Language.PureScript.CST.Types.LineFeed] ->
  IO Utf8Builder
commentsTrailingModule log commentsTrailing' = do
  comments' <- traverse (comment log) commentsTrailing'
  case RIO.NonEmpty.nonEmpty (catMaybes comments') of
    Nothing -> do
      Log.debug log "No trailing comments to format"
      pure blank
    Just comments -> do
      Log.debug log ("Formatting trailing comments" <> fold (RIO.NonEmpty.intersperse newline comments))
      pure (newline <> fold (RIO.NonEmpty.intersperse newline comments) <> newline)

constraint ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Constraint Span.Span ->
  IO Utf8Builder
constraint log indentation indent' constraint' = case constraint' of
  Language.PureScript.CST.Types.Constraint span name' types -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "Constraint" constraint' span
    qualifiedName log indent' blank name'
      <> foldMap
        ( \type'' ->
            pure prefix
              <> type' log indentation indent type''
        )
        types
  Language.PureScript.CST.Types.ConstraintParens span wrapped' -> do
    debug log "ConstraintParens" constraint' span
    parens log span indentation indent' (constraint log indentation) wrapped'

dataCtor ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.DataCtor Span.Span ->
  IO Utf8Builder
dataCtor log indentation indent' dataCtor' = case dataCtor' of
  Language.PureScript.CST.Types.DataCtor span name' types -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "DataCtor" dataCtor' span
    pure space
      <> name log indent' blank name'
      <> foldMap
        ( \type'' ->
            pure prefix
              <> type' log indentation indent type''
        )
        types

dataHead ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.DataHead Span.Span ->
  IO Utf8Builder
dataHead log indentation indent dataHead' = case dataHead' of
  Language.PureScript.CST.Types.DataHead data' name' typeVarBindings -> do
    debug log "DataHead" dataHead' Span.SingleLine
    sourceToken log indent blank data'
      <> name log indent space name'
      <> foldMap
        ( \typeVarBinding' ->
            pure space
              <> typeVarBinding log indentation indent typeVarBinding'
        )
        typeVarBindings

dataMembers ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.DataMembers Span.Span ->
  IO Utf8Builder
dataMembers log indentation indent' dataMembers' = case dataMembers' of
  Language.PureScript.CST.Types.DataAll span sourceToken' -> do
    debug log "DataAll" dataMembers' span
    sourceToken log indent' blank sourceToken'
  Language.PureScript.CST.Types.DataEnumerated span delimited' -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent')
          Span.SingleLine ->
            (indent', blank)
    debug log "DataEnumerated" dataMembers' span
    pure prefix
      <> delimited
        log
        indent'
        SourceRange.name
        (name log indent blank)
        delimited'

debug :: (Show a) => Log.Handle -> Utf8Builder -> a -> Span.Span -> IO ()
debug log x y z =
  Log.debug
    log
    ( "Formatting `"
        <> x
        <> "`: "
        <> displayShow y
        <> " as `"
        <> displayShow z
        <> "`"
    )

declaration ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Declaration Span.Span ->
  IO Utf8Builder
declaration log indentation indent'' declaration' = case declaration' of
  Language.PureScript.CST.Types.DeclClass span classHead' members -> do
    let indent' = indent'' <> indentation
    debug log "DeclClass" declaration' span
    classHead log span indentation indent'' classHead'
      <> foldMap
        ( \(where'', labeleds) ->
            sourceToken log indent'' space where''
              <> foldMap
                ( \labeled' ->
                    pure (newline <> indent')
                      <> labeledNameType log indentation indent' labeled'
                )
                labeleds
        )
        members
      <> pure newline
  Language.PureScript.CST.Types.DeclData span dataHead' dataCtors' -> do
    let indent' = indent'' <> indentation
    debug log "DeclData" declaration' span
    dataHead log indentation indent'' dataHead'
      <> foldMap
        ( \(equals, dataCtors) ->
            pure (newline <> indent')
              <> sourceToken log indent' blank equals
              <> separated
                log
                Span.MultipleLines
                indent'
                blank
                (dataCtor log indentation indent')
                dataCtors
        )
        dataCtors'
      <> pure newline
  Language.PureScript.CST.Types.DeclDerive span derive newtype' instanceHead' -> do
    debug log "DeclDerive" declaration' span
    sourceToken log indent'' blank derive
      <> foldMap (sourceToken log indent'' space) newtype'
      <> pure space
      <> instanceHead log indentation indent'' instanceHead'
      <> pure newline
  Language.PureScript.CST.Types.DeclFixity span fixityFields' -> do
    debug log "DeclFixity" declaration' span
    fixityFields log indent'' fixityFields'
      <> pure newline
  Language.PureScript.CST.Types.DeclForeign span foreign'' import'' foreign''' -> do
    debug log "DeclForeign" declaration' span
    sourceToken log indent'' blank foreign''
      <> sourceToken log indent'' space import''
      <> pure space
      <> foreign' log span indentation indent'' foreign'''
      <> pure newline
  Language.PureScript.CST.Types.DeclInstanceChain span instances -> do
    debug log "DeclInstanceChain" declaration' span
    separated
      log
      (Span.separated SourceRange.instance' instances)
      indent''
      space
      (instance' log indentation indent'')
      instances
      <> pure newline
  Language.PureScript.CST.Types.DeclKindSignature span kindOfDeclaration labeled' -> do
    debug log "DeclKindSignature" declaration' span
    sourceToken log indentation indent'' kindOfDeclaration
      <> pure space
      <> labeledNameType log indentation indent'' labeled'
  Language.PureScript.CST.Types.DeclNewtype span dataHead' equals name' type'' -> do
    let constructorSpan =
          Span.betweenSourceRanges
            (SourceRange.name name')
            (SourceRange.type' type'')
        indent' = indent'' <> indentation
        prefix = case constructorSpan of
          Span.MultipleLines ->
            newline <> indent'
          Span.SingleLine ->
            space
    debug log "DeclNewtype" declaration' span
    dataHead log indentation indent'' dataHead'
      <> pure (newline <> indent')
      <> sourceToken log indent' blank equals
      <> pure space
      <> name log indent' blank name'
      <> pure prefix
      <> type' log indentation indent' type''
      <> pure newline
  Language.PureScript.CST.Types.DeclRole span type'' role'' name' roles -> do
    debug log "DeclRole" declaration' span
    sourceToken log indentation indent'' type''
      <> pure space
      <> sourceToken log indentation indent'' role''
      <> pure space
      <> name log indent'' blank name'
      <> foldMap
        ( \role' ->
            pure space
              <> role log indentation indent'' role'
        )
        roles
      <> pure newline
  Language.PureScript.CST.Types.DeclSignature span labeled' -> do
    debug log "DeclSignature" declaration' span
    labeledNameType log indentation indent'' labeled'
  Language.PureScript.CST.Types.DeclType span dataHead' equals type'' -> do
    let indent' = indent'' <> indentation
        indent = indent' <> indentation
    debug log "DeclType" declaration' span
    dataHead log indentation indent'' dataHead'
      <> pure (newline <> indent')
      <> sourceToken log indent' blank equals
      <> pure space
      <> type' log indentation indent type''
      <> pure newline
  Language.PureScript.CST.Types.DeclValue span valueBindingFields' -> do
    debug log "DeclValue" declaration' span
    valueBindingFields log indentation indent'' valueBindingFields'
      <> pure newline

declarations ::
  Log.Handle ->
  Indentation ->
  [Language.PureScript.CST.Types.Declaration Span.Span] ->
  IO Utf8Builder
declarations log indentation declarations' = case declarations' of
  [] -> do
    Log.debug log "No declarations to format"
    pure blank
  _ -> do
    let indent = blank
    debug log "declarations" declarations' Span.MultipleLines
    foldMap
      ( \declaration' ->
          pure newline
            <> declaration log indentation indent declaration'
      )
      declarations'

delimited ::
  (Show a) =>
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.Delimited a ->
  IO Utf8Builder
delimited log indent f g delimited' = do
  debug log "Delimited" delimited' (Span.wrapped delimited')
  wrapped
    log
    indent
    (foldMap (\as -> separated log (Span.separated f as) indent space g as))
    delimited'

delimitedNonEmpty ::
  (Show a) =>
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.DelimitedNonEmpty a ->
  IO Utf8Builder
delimitedNonEmpty log indent f g delimitedNonEmpty' = do
  debug log "DelimitedNonEmpty" delimitedNonEmpty' (Span.wrapped delimitedNonEmpty')
  wrapped
    log
    indent
    (\as -> separated log (Span.separated f as) indent space g as)
    delimitedNonEmpty'

doBlock ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.DoBlock Span.Span ->
  IO Utf8Builder
doBlock log span indentation indent' doBlock' = case doBlock' of
  Language.PureScript.CST.Types.DoBlock do' doStatements -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "DoBlock" doBlock' span
    sourceToken log indent' blank do'
      <> foldMap
        ( \doStatement' ->
            pure prefix
              <> doStatement log indentation indent doStatement'
        )
        doStatements

doStatement ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.DoStatement Span.Span ->
  IO Utf8Builder
doStatement log indentation indent' doStatement' = case doStatement' of
  Language.PureScript.CST.Types.DoBind binder' arrow expr' -> do
    let span = Span.doStatement doStatement'
    debug log "DoBind" doStatement' span
    binder log indentation indent' binder'
      <> sourceToken log indent' space arrow
      <> exprPrefix log span indentation indent' expr'
  Language.PureScript.CST.Types.DoDiscard expr' -> do
    debug log "DoDiscard" doStatement' (Span.doStatement doStatement')
    expr log indentation indent' expr'
  Language.PureScript.CST.Types.DoLet let' letBindings -> do
    let indent = indent' <> indentation
    debug log "DoLet" doStatement' (Span.doStatement doStatement')
    sourceToken log indent' blank let'
      <> foldMap
        (letBinding log indentation indent (newline <> indent) newline)
        (RIO.NonEmpty.init letBindings)
      <> letBinding
        log
        indentation
        indent
        (newline <> indent)
        blank
        (RIO.NonEmpty.last letBindings)

export ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Export Span.Span ->
  IO Utf8Builder
export log indentation indent' export' = case export' of
  Language.PureScript.CST.Types.ExportClass span class' name' -> do
    debug log "ExportClass" export' span
    sourceToken log indent' blank class'
      <> name log indent' space name'
  Language.PureScript.CST.Types.ExportKind span kind' name' -> do
    debug log "ExportKind" export' span
    sourceToken log indent' blank kind'
      <> name log indent' space name'
  Language.PureScript.CST.Types.ExportModule span module'' name' -> do
    debug log "ExportModule" export' span
    sourceToken log indent' blank module''
      <> name log indent' space name'
  Language.PureScript.CST.Types.ExportOp span name' -> do
    debug log "ExportOp" export' span
    name log indent' blank name'
  Language.PureScript.CST.Types.ExportType span name' dataMembers' -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', blank)
    debug log "ExportType" export' span
    name log indent' blank name'
      <> pure prefix
      <> foldMap (dataMembers log indentation indent) dataMembers'
  Language.PureScript.CST.Types.ExportTypeOp span type'' name' -> do
    debug log "ExportTypeOp" export' span
    sourceToken log indent' blank type''
      <> name log indent' space name'
  Language.PureScript.CST.Types.ExportValue span name' -> do
    debug log "ExportValue" export' span
    name log indent' blank name'

exports ::
  Log.Handle ->
  Indentation ->
  Maybe
    ( Language.PureScript.CST.Types.DelimitedNonEmpty
        (Language.PureScript.CST.Types.Export Span.Span)
    ) ->
  IO Utf8Builder
exports log indentation exports'' = case exports'' of
  Nothing -> do
    Log.debug log "No exports to format"
    pure blank
  Just exports' -> do
    let indent = indentation
        prefix = case span of
          Span.MultipleLines ->
            newline <> indent
          Span.SingleLine ->
            space
        span = Span.delimitedNonEmpty exports'
    debug log "exports" exports' span
    pure prefix
      <> delimitedNonEmpty
        log
        indent
        SourceRange.export
        (export log indentation indent)
        exports'

expr ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Expr Span.Span ->
  IO Utf8Builder
expr log indentation indent'' expr'' = case expr'' of
  Language.PureScript.CST.Types.ExprAdo span adoBlock' -> do
    debug log "ExprAdo" expr'' span
    adoBlock log span indentation indent'' adoBlock'
  Language.PureScript.CST.Types.ExprApp span expr1 expr2 -> do
    debug log "ExprApp" expr'' span
    expr log indentation indent'' expr1
      <> exprPrefix log span indentation indent'' expr2
  Language.PureScript.CST.Types.ExprArray span delimited' -> do
    let indent' = case span of
          Span.MultipleLines ->
            indent'' <> indentation
          Span.SingleLine ->
            indent''
    debug log "ExprArray" expr'' span
    array
      log
      indent''
      SourceRange.expr
      (expr log indentation indent')
      delimited'
  Language.PureScript.CST.Types.ExprBoolean span boolean _ -> do
    debug log "ExprBoolean" expr'' span
    sourceToken log indent'' blank boolean
  Language.PureScript.CST.Types.ExprCase span caseOf' -> do
    debug log "ExprCase" expr'' span
    caseOf log span indentation indent'' caseOf'
  Language.PureScript.CST.Types.ExprChar span char _ -> do
    debug log "ExprChar" expr'' span
    sourceToken log indent'' blank char
  Language.PureScript.CST.Types.ExprConstructor span name' -> do
    debug log "ExprConstructor" expr'' span
    qualifiedName log indent'' blank name'
  Language.PureScript.CST.Types.ExprDo span doBlock' -> do
    debug log "ExprDo" expr'' span
    doBlock log span indentation indent'' doBlock'
  Language.PureScript.CST.Types.ExprHole span hole -> do
    debug log "ExprHole" expr'' span
    name log indent'' blank hole
  Language.PureScript.CST.Types.ExprIdent span name' -> do
    debug log "ExprIdent" expr'' span
    qualifiedName log indent'' blank name'
  Language.PureScript.CST.Types.ExprIf span ifThenElse' -> do
    debug log "ExprIf" expr'' span
    ifThenElse log span indentation indent'' ifThenElse'
  Language.PureScript.CST.Types.ExprInfix span expr1 wrapped' expr2 -> do
    let (indent, indent', prefix, prefix') = case span of
          Span.MultipleLines ->
            ( indent' <> indentation,
              indent'' <> indentation,
              newline <> indent,
              newline <> indent'
            )
          Span.SingleLine ->
            (indent', indent'', space, space)
    debug log "ExprInfix" expr'' span
    expr log indentation indent'' expr1
      <> pure prefix'
      <> wrapped log indent (expr log indentation indent') wrapped'
      <> pure prefix
      <> expr log indentation indent expr2
  Language.PureScript.CST.Types.ExprLambda span lambda' -> do
    debug log "ExprLambda" expr'' span
    lambda log span indentation indent'' lambda'
  Language.PureScript.CST.Types.ExprLet span letIn' -> do
    debug log "ExprLet" expr'' span
    letIn log span indentation indent'' letIn'
  Language.PureScript.CST.Types.ExprNegate span negative expr' -> do
    debug log "ExprNegate" expr'' span
    sourceToken log indent'' blank negative
      <> expr log indentation indent'' expr'
  Language.PureScript.CST.Types.ExprNumber span number _ -> do
    debug log "ExprNumber" expr'' span
    sourceToken log indent'' blank number
  Language.PureScript.CST.Types.ExprOp span expr1 op expr2 -> do
    let (indent, indent', prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, indent'' <> indentation, newline <> indent')
          Span.SingleLine ->
            (indent', indent'', space)
    debug log "ExprOp" expr'' span
    expr log indentation indent'' expr1
      <> qualifiedName log indent' prefix op
      <> pure space
      <> expr log indentation indent expr2
  Language.PureScript.CST.Types.ExprOpName span name' -> do
    debug log "ExprOpName" expr'' span
    qualifiedName log indent'' blank name'
  Language.PureScript.CST.Types.ExprParens span wrapped' -> do
    debug log "ExprParens" expr'' span
    parens log span indentation indent'' (expr log indentation) wrapped'
  Language.PureScript.CST.Types.ExprRecord span delimited' -> do
    debug log "ExprRecord" expr'' span
    record
      log
      indent''
      (SourceRange.recordLabeled SourceRange.expr)
      ( recordLabeled
          log
          indentation
          indent''
          SourceRange.expr
          (expr log indentation)
      )
      delimited'
  Language.PureScript.CST.Types.ExprRecordAccessor span recordAccessor' -> do
    debug log "ExprRecordAccessor" expr'' span
    recordAccessor log span indentation indent'' recordAccessor'
  Language.PureScript.CST.Types.ExprRecordUpdate span expr' delimitedNonEmpty' -> do
    let (indent', prefix) = case span of
          Span.MultipleLines ->
            (indent'' <> indentation, newline <> indent')
          Span.SingleLine ->
            (indent'', space)
    debug log "ExprRecordUpdate" expr'' span
    expr log indentation indent'' expr'
      <> pure prefix
      <> recordNonEmpty
        log
        indent'
        SourceRange.recordUpdate
        (recordUpdate log indentation indent')
        delimitedNonEmpty'
  Language.PureScript.CST.Types.ExprSection span section -> do
    debug log "ExprSection" expr'' span
    sourceToken log indent'' blank section
  Language.PureScript.CST.Types.ExprString span string _ -> do
    debug log "ExprString" expr'' span
    sourceToken log indent'' blank string
  Language.PureScript.CST.Types.ExprTyped span expr' colons type'' -> do
    let (indent', prefix) = case span of
          Span.MultipleLines ->
            (indent'' <> indentation, newline <> indent')
          Span.SingleLine ->
            (indent'', space)
    debug log "ExprTyped" expr'' span
    expr log indentation indent'' expr'
      <> sourceToken log indent'' space colons
      <> pure prefix
      <> type' log indentation indent' type''

exprPrefix ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Expr Span.Span ->
  IO Utf8Builder
exprPrefix log span indentation indent' expr' =
  pure prefix
    <> expr log indentation indent expr'
  where
    indent :: Utf8Builder
    indent = case expr' of
      Language.PureScript.CST.Types.ExprAdo {} -> indent'
      Language.PureScript.CST.Types.ExprApp {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprArray {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprBoolean {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprCase {} -> indent'
      Language.PureScript.CST.Types.ExprChar {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprConstructor {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprDo {} -> indent'
      Language.PureScript.CST.Types.ExprHole {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprIdent {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprIf {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprInfix {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprLambda {} -> indent'
      Language.PureScript.CST.Types.ExprLet {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprNegate {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprNumber {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprOp {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprOpName {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprParens {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprRecord {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprRecordAccessor {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprRecordUpdate {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprSection {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprString {} -> indent' <> indentation
      Language.PureScript.CST.Types.ExprTyped {} -> indent' <> indentation
    multiLine :: Utf8Builder
    multiLine = case expr' of
      Language.PureScript.CST.Types.ExprAdo {} -> space
      Language.PureScript.CST.Types.ExprApp {} -> newline <> indent
      Language.PureScript.CST.Types.ExprArray {} -> newline <> indent
      Language.PureScript.CST.Types.ExprBoolean {} -> newline <> indent
      Language.PureScript.CST.Types.ExprCase {} -> space
      Language.PureScript.CST.Types.ExprChar {} -> newline <> indent
      Language.PureScript.CST.Types.ExprConstructor {} -> newline <> indent
      Language.PureScript.CST.Types.ExprDo {} -> space
      Language.PureScript.CST.Types.ExprHole {} -> newline <> indent
      Language.PureScript.CST.Types.ExprIdent {} -> newline <> indent
      Language.PureScript.CST.Types.ExprIf {} -> newline <> indent
      Language.PureScript.CST.Types.ExprInfix {} -> newline <> indent
      Language.PureScript.CST.Types.ExprLambda {} -> space
      Language.PureScript.CST.Types.ExprLet {} -> newline <> indent
      Language.PureScript.CST.Types.ExprNegate {} -> newline <> indent
      Language.PureScript.CST.Types.ExprNumber {} -> newline <> indent
      Language.PureScript.CST.Types.ExprOp {} -> newline <> indent
      Language.PureScript.CST.Types.ExprOpName {} -> newline <> indent
      Language.PureScript.CST.Types.ExprParens {} -> newline <> indent
      Language.PureScript.CST.Types.ExprRecord {} -> newline <> indent
      Language.PureScript.CST.Types.ExprRecordAccessor {} -> newline <> indent
      Language.PureScript.CST.Types.ExprRecordUpdate {} -> newline <> indent
      Language.PureScript.CST.Types.ExprSection {} -> newline <> indent
      Language.PureScript.CST.Types.ExprString {} -> newline <> indent
      Language.PureScript.CST.Types.ExprTyped {} -> newline <> indent
    prefix :: Utf8Builder
    prefix = case span of
      Span.MultipleLines ->
        multiLine
      Span.SingleLine ->
        space

exprPrefixElseIf ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Expr Span.Span ->
  IO Utf8Builder
exprPrefixElseIf log span indentation indent expr' =
  case expr' of
    Language.PureScript.CST.Types.ExprAdo {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprApp {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprArray {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprBoolean {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprCase {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprChar {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprConstructor {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprDo {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprHole {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprIdent {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprIf {} ->
      pure space
        <> expr log indentation indent expr'
    Language.PureScript.CST.Types.ExprInfix {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprLambda {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprLet {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprNegate {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprNumber {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprOp {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprOpName {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprParens {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprRecord {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprRecordAccessor {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprRecordUpdate {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprSection {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprString {} -> exprPrefix log span indentation indent expr'
    Language.PureScript.CST.Types.ExprTyped {} -> exprPrefix log span indentation indent expr'

fixityFields ::
  Log.Handle ->
  Indent ->
  Language.PureScript.CST.Types.FixityFields ->
  IO Utf8Builder
fixityFields log indent fixityFields' = case fixityFields' of
  Language.PureScript.CST.Types.FixityFields (infix', _) (precedence, _) fixityOp' -> do
    debug log "FixityFields" fixityFields' Span.SingleLine
    sourceToken log indent blank infix'
      <> sourceToken log indent space precedence
      <> pure space
      <> fixityOp log indent fixityOp'

fixityOp ::
  Log.Handle ->
  Indent ->
  Language.PureScript.CST.Types.FixityOp ->
  IO Utf8Builder
fixityOp log indent fixityOp' = case fixityOp' of
  Language.PureScript.CST.Types.FixityType type'' name' as op -> do
    debug log "FixityType" fixityOp' Span.SingleLine
    sourceToken log indent blank type''
      <> qualifiedName log indent space name'
      <> sourceToken log indent space as
      <> name log indent space op
  Language.PureScript.CST.Types.FixityValue name' as op -> do
    debug log "FixityValue" fixityOp' Span.SingleLine
    qualifiedName log indent blank name'
      <> sourceToken log indent space as
      <> name log indent space op

foreign' ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Foreign Span.Span ->
  IO Utf8Builder
foreign' log span indentation indent' foreign'' = case foreign'' of
  Language.PureScript.CST.Types.ForeignData data' labeled' -> do
    debug log "ForeignData" foreign'' span
    sourceToken log indent' blank data'
      <> pure space
      <> labeledNameKind log indentation indent' labeled'
  Language.PureScript.CST.Types.ForeignKind kind' name' -> do
    debug log "ForeignKind" foreign'' span
    sourceToken log indent' blank kind'
      <> name log indent' space name'
  Language.PureScript.CST.Types.ForeignValue labeled' -> do
    debug log "ForeignValue" foreign'' span
    labeledNameType log indentation indent' labeled'

format ::
  (Show a) =>
  Log.Handle ->
  Indentation ->
  Language.PureScript.CST.Types.Module a ->
  IO Utf8Builder
format log indentation module'' = do
  Log.debug log "Annotating module"
  annotated <- Annotation.module' log module''
  Log.debug log ("Annotated module" <> displayShow annotated)
  Log.debug log "Formatting module"
  formatted <- module' log indentation annotated
  Log.debug log ("Formatted module" <> display formatted)
  pure formatted

guarded ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Guarded Span.Span ->
  IO Utf8Builder
guarded log indentation indent' guarded' = case guarded' of
  Language.PureScript.CST.Types.Guarded guardedExprs -> do
    let indent = indent' <> indentation
    debug log "Guarded" guarded' (Span.guarded guarded')
    foldMap
      ( \guardedExpr' ->
          pure (newline <> indent)
            <> guardedExpr log indentation indent guardedExpr'
      )
      guardedExprs
  Language.PureScript.CST.Types.Unconditional separator where'' -> do
    debug log "Unconditional" guarded' (Span.guarded guarded')
    sourceToken log indent' space separator
      <> where' log indentation indent' where''

guardedExpr ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.GuardedExpr Span.Span ->
  IO Utf8Builder
guardedExpr log indentation indent' guardedExpr' = case guardedExpr' of
  Language.PureScript.CST.Types.GuardedExpr bar patternGuards separator where'' -> do
    let indent = indent' <> indentation
    debug log "GuardedExpr" guardedExpr' (Span.guardedExpr guardedExpr')
    sourceToken log indent' blank bar
      <> pure space
      <> separated
        log
        (Span.separated SourceRange.patternGuard patternGuards)
        indent'
        space
        (patternGuard log indentation indent)
        patternGuards
      <> pure space
      <> sourceToken log indent' blank separator
      <> where' log indentation indent' where''

ifThenElse ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.IfThenElse Span.Span ->
  IO Utf8Builder
ifThenElse log span indentation indent ifThenElse' = case ifThenElse' of
  Language.PureScript.CST.Types.IfThenElse if' cond then' true else' false -> do
    let prefix = case span of
          Span.MultipleLines ->
            newline <> indent
          Span.SingleLine ->
            space
    debug log "IfThenElse" ifThenElse' span
    sourceToken log indent blank if'
      <> pure space
      <> expr log indentation indent cond
      <> pure space
      <> sourceToken log indent blank then'
      <> exprPrefix log span indentation indent true
      <> pure prefix
      <> sourceToken log indent blank else'
      <> exprPrefixElseIf log span indentation indent false

import' ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Import Span.Span ->
  IO Utf8Builder
import' log indentation indent' import'' = case import'' of
  Language.PureScript.CST.Types.ImportClass span class' name' -> do
    debug log "ImportClass" import'' span
    sourceToken log indent' blank class'
      <> name log indent' space name'
  Language.PureScript.CST.Types.ImportKind span kind' name' -> do
    debug log "ImportKind" import'' span
    sourceToken log indent' blank kind'
      <> name log indent' space name'
  Language.PureScript.CST.Types.ImportOp span name' -> do
    debug log "ImportOp" import'' span
    name log indent' blank name'
  Language.PureScript.CST.Types.ImportType span name' dataMembers' -> do
    let indent = case span of
          Span.MultipleLines ->
            indent' <> indentation
          Span.SingleLine ->
            indent'
    debug log "ImportType" import'' span
    name log indent' blank name'
      <> foldMap (dataMembers log indentation indent) dataMembers'
  Language.PureScript.CST.Types.ImportTypeOp span type'' name' -> do
    debug log "ImportTypeOp" import'' span
    sourceToken log indent' blank type''
      <> name log indent' space name'
  Language.PureScript.CST.Types.ImportValue span name' -> do
    debug log "ImportValue" import'' span
    name log indent' blank name'

importDecl ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.ImportDecl Span.Span ->
  IO Utf8Builder
importDecl log indentation indent'' importDecl' = case importDecl' of
  Language.PureScript.CST.Types.ImportDecl span import'' name'' imports'' rename -> do
    let indent' = indent'' <> indentation
    debug log "ImportDecl" importDecl' span
    sourceToken log indent'' blank import''
      <> name log indent'' space name''
      <> foldMap
        ( \(hiding', imports') ->
            case hiding' of
              Just hiding -> do
                let hidingPrefix = case span of
                      Span.MultipleLines ->
                        newline <> indent'
                      Span.SingleLine ->
                        space
                    importPrefix = case span of
                      Span.MultipleLines ->
                        newline <> indent
                      Span.SingleLine ->
                        space
                    indent = indent' <> indentation
                pure hidingPrefix
                  <> sourceToken log indent' blank hiding
                  <> pure importPrefix
                  <> delimitedNonEmpty
                    log
                    indent
                    SourceRange.import'
                    (import' log indentation indent')
                    imports'
              Nothing -> do
                let importPrefix = case span of
                      Span.MultipleLines ->
                        newline <> indent'
                      Span.SingleLine ->
                        space
                pure importPrefix
                  <> delimitedNonEmpty
                    log
                    indent'
                    SourceRange.import'
                    (import' log indentation indent')
                    imports'
        )
        imports''
      <> foldMap
        ( \(as, name') -> do
            let prefix = case span of
                  Span.MultipleLines ->
                    newline <> indent'
                  Span.SingleLine ->
                    space
            pure prefix
              <> sourceToken log indent' blank as
              <> name log indent' space name'
        )
        rename

imports ::
  Log.Handle ->
  Indentation ->
  [Language.PureScript.CST.Types.ImportDecl Span.Span] ->
  IO Utf8Builder
imports log indentation imports' = case imports' of
  [] -> do
    Log.debug log "No imports to format"
    pure blank
  _ -> do
    let indent = blank
    debug log "imports" imports' Span.MultipleLines
    foldMap
      ( \importDecl' ->
          pure newline
            <> importDecl log indentation indent importDecl'
      )
      imports'
      <> pure newline

instance' ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Instance Span.Span ->
  IO Utf8Builder
instance' log indentation indent' instance'' = case instance'' of
  Language.PureScript.CST.Types.Instance instanceHead' body -> do
    let indent = indent' <> indentation
        span = Span.instance' instance''
    debug log "Instance" instance'' span
    instanceHead log indentation indent' instanceHead'
      <> foldMap
        ( \(where'', instanceBindings) ->
            sourceToken log indent' space where''
              <> foldMap
                ( \instanceBinding' ->
                    pure (newline <> indent)
                      <> instanceBinding log indentation indent instanceBinding'
                )
                instanceBindings
        )
        body

instanceBinding ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.InstanceBinding Span.Span ->
  IO Utf8Builder
instanceBinding log indentation indent instanceBinding' = case instanceBinding' of
  Language.PureScript.CST.Types.InstanceBindingName span valueBindingFields' -> do
    debug log "InstanceBindingName" instanceBinding' span
    valueBindingFields log indentation indent valueBindingFields'
  Language.PureScript.CST.Types.InstanceBindingSignature span labeled' -> do
    debug log "InstanceBindingSignature" instanceBinding' span
    labeledNameType log indentation indent labeled'

instanceHead ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.InstanceHead Span.Span ->
  IO Utf8Builder
instanceHead log indentation indent'' instanceHead' = case instanceHead' of
  Language.PureScript.CST.Types.InstanceHead instance'' name' colons constraints' className types -> do
    let (indent, indent', prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, indent'' <> indentation, newline <> indent')
          Span.SingleLine ->
            (indent'', indent'', space)
        span = Span.instanceHead instanceHead'
        typePrefix = case listToMaybe types of
          Just type'' -> case Span.betweenSourceRanges (SourceRange.qualifiedName className) (SourceRange.type' type'') of
            Span.MultipleLines -> newline <> indent
            Span.SingleLine -> space
          Nothing -> space
    debug log "InstanceHead" instanceHead' span
    sourceToken log indent'' blank instance''
      <> name log indent'' space name'
      <> sourceToken log indent'' space colons
      <> foldMap
        ( \(constraints, arrow) ->
            pure prefix
              <> oneOrDelimited
                log
                indent'
                SourceRange.constraint
                (constraint log indentation indent)
                constraints
              <> sourceToken log indent' space arrow
        )
        constraints'
      <> qualifiedName log indent' prefix className
      <> foldMap
        ( \type'' ->
            pure typePrefix
              <> type' log indentation indent type''
        )
        types

label ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Types.Label ->
  IO Utf8Builder
label log indent prefix label'' = case label'' of
  Language.PureScript.CST.Types.Label label' _ -> do
    debug log "Label" label' (Span.label label'')
    sourceToken log indent prefix label'

labeled ::
  (Show a, Show b) =>
  Log.Handle ->
  Indentation ->
  Indent ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (a -> IO Utf8Builder) ->
  (b -> Language.PureScript.CST.Types.SourceRange) ->
  (Indent -> b -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.Labeled a b ->
  IO Utf8Builder
labeled log indentation indent' f g h i labeled' = case labeled' of
  Language.PureScript.CST.Types.Labeled label' separator value -> do
    let span = Span.labeled f h labeled'
        (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "Labeled" labeled' span
    g label'
      <> sourceToken log indent space separator
      <> pure prefix
      <> i indent value

labeledLabelType ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Labeled
    Language.PureScript.CST.Types.Label
    (Language.PureScript.CST.Types.Type Span.Span) ->
  IO Utf8Builder
labeledLabelType log indentation indent =
  labeled
    log
    indentation
    indent
    SourceRange.label
    (label log indent blank)
    SourceRange.type'
    (type' log indentation)

labeledNameKind ::
  (Show a) =>
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Labeled
    (Language.PureScript.CST.Types.Name a)
    (Language.PureScript.CST.Types.Type Span.Span) ->
  IO Utf8Builder
labeledNameKind log indentation indent =
  labeled
    log
    indentation
    indent
    SourceRange.name
    (name log indent blank)
    SourceRange.type'
    (type' log indentation)

labeledNameType ::
  (Show a) =>
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Labeled
    (Language.PureScript.CST.Types.Name a)
    (Language.PureScript.CST.Types.Type Span.Span) ->
  IO Utf8Builder
labeledNameType log indentation indent =
  labeled
    log
    indentation
    indent
    SourceRange.name
    (name log indent blank)
    SourceRange.type'
    (type' log indentation)

lambda ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Lambda Span.Span ->
  IO Utf8Builder
lambda log span indentation indent' lambda' = case lambda' of
  Language.PureScript.CST.Types.Lambda reverseSolidus binders arrow expr' -> do
    debug log "Lambda" lambda' span
    sourceToken log indent' blank reverseSolidus
      <> foldMap
        ( \binder' ->
            binder log indentation indent' binder'
              <> pure space
        )
        binders
      <> sourceToken log indent' blank arrow
      <> exprPrefix log span indentation indent' expr'

letBinding ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Prefix ->
  Suffix ->
  Language.PureScript.CST.Types.LetBinding Span.Span ->
  IO Utf8Builder
letBinding log indentation indent' prefix suffix letBinding' = case letBinding' of
  Language.PureScript.CST.Types.LetBindingName span valueBindingFields' -> do
    debug log "LetBindingName" letBinding' span
    pure prefix
      <> valueBindingFields log indentation indent' valueBindingFields'
      <> pure suffix
  Language.PureScript.CST.Types.LetBindingPattern span binder' equals where'' -> do
    debug log "LetBindingPattern" letBinding' span
    pure prefix
      <> binder log indentation indent' binder'
      <> sourceToken log indent' space equals
      <> where' log indentation indent' where''
      <> pure suffix
  Language.PureScript.CST.Types.LetBindingSignature span labeled' -> do
    debug log "LetBindingSignature" letBinding' span
    pure prefix
      <> labeledNameType log indentation indent' labeled'

letIn ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.LetIn Span.Span ->
  IO Utf8Builder
letIn log span indentation indent' letIn' = case letIn' of
  Language.PureScript.CST.Types.LetIn let' letBindings in' expr'' -> do
    let (inPrefix, indent, prefix) = case span of
          Span.MultipleLines ->
            (newline <> indent', indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (space, indent', space)
    debug log "LetIn" letIn' span
    sourceToken log indent' blank let'
      <> foldMap
        (letBinding log indentation indent prefix newline)
        (RIO.NonEmpty.init letBindings)
      <> letBinding
        log
        indentation
        indent
        prefix
        blank
        (RIO.NonEmpty.last letBindings)
      <> pure inPrefix
      <> sourceToken log indent' blank in'
      <> pure prefix
      <> expr log indentation indent expr''

module' ::
  Log.Handle ->
  Indentation ->
  Language.PureScript.CST.Types.Module Span.Span ->
  IO Utf8Builder
module' log indentation module''' = case module''' of
  Language.PureScript.CST.Types.Module span module'' name' exports' where'' imports' declarations' trailing -> do
    debug log "Module" module''' span
    sourceToken log blank blank module''
      <> name log blank space name'
      <> exports log indentation exports'
      <> sourceToken log blank space where''
      <> pure newline
      <> imports log indentation imports'
      <> declarations log indentation declarations'
      <> commentsTrailingModule log trailing

name ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Types.Name a ->
  IO Utf8Builder
name log indent prefix name'' = case name'' of
  Language.PureScript.CST.Types.Name name' _ -> do
    debug log "Name" name' (Span.name name'')
    sourceToken log indent prefix name'

newline :: Utf8Builder
newline = "\n"

oneOrDelimited ::
  (Show a) =>
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.OneOrDelimited a ->
  IO Utf8Builder
oneOrDelimited log indent f g oneOrDelimited' = case oneOrDelimited' of
  Language.PureScript.CST.Types.One a -> do
    debug log "One" oneOrDelimited' (Span.oneOrDelimited f oneOrDelimited')
    g a
  Language.PureScript.CST.Types.Many delimitedNonEmpty' -> do
    debug log "Many" oneOrDelimited' (Span.oneOrDelimited f oneOrDelimited')
    delimitedNonEmpty log indent f g delimitedNonEmpty'

parens ::
  Show a =>
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  (Indent -> a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.Wrapped a ->
  IO Utf8Builder
parens log span indentation indent' f wrapped' = do
  let indent = case span of
        Span.MultipleLines ->
          indent' <> indentation
        Span.SingleLine ->
          indent'
  wrapped log indent' (f indent) wrapped'

patternGuard ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.PatternGuard Span.Span ->
  IO Utf8Builder
patternGuard log indentation indent' patternGuard' = case patternGuard' of
  Language.PureScript.CST.Types.PatternGuard binder'' expr' -> do
    let span = Span.patternGuard patternGuard'
    debug log "PatternGuard" patternGuard' span
    case binder'' of
      Just (binder', arrow) ->
        binder log indentation indent' binder'
          <> sourceToken log indent' space arrow
          <> exprPrefix log span indentation indent' expr'
      Nothing ->
        expr log indentation indent' expr'

qualifiedName ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Types.QualifiedName a ->
  IO Utf8Builder
qualifiedName log indent prefix qualifiedName'' = case qualifiedName'' of
  Language.PureScript.CST.Types.QualifiedName qualifiedName' _ _ -> do
    debug log "QualifiedName" qualifiedName' (Span.qualifiedName qualifiedName'')
    sourceToken log indent prefix qualifiedName'

record ::
  (Show a) =>
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.Delimited a ->
  IO Utf8Builder
record log indent f g record'' = case record'' of
  Language.PureScript.CST.Types.Wrapped open Nothing close -> do
    debug log "Delimited" record'' (Span.wrapped record'')
    sourceToken log indent blank open
      <> sourceToken log indent blank close
  Language.PureScript.CST.Types.Wrapped open (Just record') close ->
    recordNonEmpty
      log
      indent
      f
      g
      (Language.PureScript.CST.Types.Wrapped open record' close)

recordNonEmpty ::
  (Show a) =>
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.DelimitedNonEmpty a ->
  IO Utf8Builder
recordNonEmpty log indent f g record' = do
  let (before, after) = case span of
        Span.MultipleLines ->
          (blank, blank)
        Span.SingleLine ->
          (space, space)
      span = Span.wrapped record'
  debug log "DelimitedNonEmpty" record' span
  wrapped
    log
    indent
    ( \separated' ->
        pure before
          <> separated log (Span.separated f separated') indent space g separated'
          <> pure after
    )
    record'

recordAccessor ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.RecordAccessor Span.Span ->
  IO Utf8Builder
recordAccessor log span indentation indent' recordAccessor' = case recordAccessor' of
  Language.PureScript.CST.Types.RecordAccessor expr' dot path -> do
    debug log "RecordAccessor" recordAccessor' span
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', blank)
    expr log indentation indent expr'
      <> pure prefix
      <> sourceToken log indent blank dot
      <> separated
        log
        (Span.separated SourceRange.label path)
        indent
        blank
        (label log indent' blank)
        path

recordLabeled ::
  (Show a) =>
  Log.Handle ->
  Indentation ->
  Indent ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (Indent -> a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.RecordLabeled a ->
  IO Utf8Builder
recordLabeled log indentation indent' f g recordLabeled' = case recordLabeled' of
  Language.PureScript.CST.Types.RecordPun name' -> do
    debug log "RecordPun" recordLabeled' (Span.recordLabeled f recordLabeled')
    name log indent' blank name'
  Language.PureScript.CST.Types.RecordField label' colon a -> do
    let span = Span.recordLabeled f recordLabeled'
        (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "RecordField" recordLabeled' span
    label log indent' blank label'
      <> sourceToken log indent' blank colon
      <> pure prefix
      <> g indent a

recordUpdate ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.RecordUpdate Span.Span ->
  IO Utf8Builder
recordUpdate log indentation indent' recordUpdate' = case recordUpdate' of
  Language.PureScript.CST.Types.RecordUpdateBranch label' delimitedNonEmpty' -> do
    let span = Span.recordUpdate recordUpdate'
        (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "RecordUpdateBranch" recordUpdate' span
    label log indent' blank label'
      <> pure prefix
      <> recordNonEmpty
        log
        indent
        SourceRange.recordUpdate
        (recordUpdate log indentation indent)
        delimitedNonEmpty'
  Language.PureScript.CST.Types.RecordUpdateLeaf label' equals expr' -> do
    let span = Span.recordUpdate recordUpdate'
        (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "RecordUpdateLeaf" recordUpdate' span
    label log indent' blank label'
      <> sourceToken log indent' space equals
      <> pure prefix
      <> expr log indentation indent expr'

role ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Role ->
  IO Utf8Builder
role log indentation indent role'' = case role'' of
  Language.PureScript.CST.Types.Role role' _ -> do
    debug log "Role" role'' Span.SingleLine
    sourceToken log indentation indent role'

row ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Row Span.Span ->
  IO Utf8Builder
row log span indentation indent' row' = case row' of
  Language.PureScript.CST.Types.Row Nothing Nothing -> do
    debug log "Row" row' span
    pure blank
  Language.PureScript.CST.Types.Row (Just labels) Nothing -> do
    let (before, indent, after) = case span of
          Span.MultipleLines ->
            (blank, indent' <> indentation, blank)
          Span.SingleLine ->
            (space, indent', space)
    debug log "Row" row' span
    pure before
      <> separated
        log
        ( Span.separated
            (SourceRange.labeled SourceRange.label SourceRange.type')
            labels
        )
        indent'
        space
        (labeledLabelType log indentation indent)
        labels
      <> pure after
  Language.PureScript.CST.Types.Row Nothing (Just (bar, type'')) -> do
    let (before, after) = case span of
          Span.MultipleLines ->
            (newline <> indent', blank)
          Span.SingleLine ->
            (space, space)
    debug log "Row" row' span
    pure before
      <> sourceToken log indent' blank bar
      <> pure space
      <> type' log indentation indent' type''
      <> pure after
  Language.PureScript.CST.Types.Row (Just labels) (Just (bar, type'')) -> do
    let (before, indent, after, prefix) = case span of
          Span.MultipleLines ->
            (blank, indent' <> indentation, blank, newline <> indent')
          Span.SingleLine ->
            (space, indent', space, space)
    debug log "Row" row' span
    pure before
      <> separated
        log
        ( Span.separated
            (SourceRange.labeled SourceRange.label SourceRange.type')
            labels
        )
        indent'
        space
        (labeledLabelType log indentation indent)
        labels
      <> pure prefix
      <> sourceToken log indent' blank bar
      <> pure space
      <> type' log indentation indent' type''
      <> pure after

separated ::
  forall a.
  (Show a) =>
  Log.Handle ->
  Span.Span ->
  Indent ->
  Prefix ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.Separated a ->
  IO Utf8Builder
separated log span indent prefix' f separated' = case separated' of
  Language.PureScript.CST.Types.Separated head tail -> do
    debug log "Separated" separated' span
    f head
      <> foldMap go tail
  where
    go :: (Language.PureScript.CST.Types.SourceToken, a) -> IO Utf8Builder
    go x = case x of
      (separator, value) -> do
        let prefix = case span of
              Span.MultipleLines ->
                newline <> indent
              Span.SingleLine ->
                blank
        pure prefix
          <> sourceToken log indent blank separator
          <> pure prefix'
          <> f value

sourceToken ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Types.SourceToken ->
  IO Utf8Builder
sourceToken log indent prefix sourceToken' = case sourceToken' of
  Language.PureScript.CST.Types.SourceToken ann token ->
    tokenAnn log indent prefix ann $ do
      debug log "SourceToken" sourceToken' (Span.sourceToken sourceToken')
      pure (prefix <> display (Language.PureScript.CST.Print.printToken token))

space :: Utf8Builder
space = " "

tokenAnn ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Types.TokenAnn ->
  IO Utf8Builder ->
  IO Utf8Builder
tokenAnn log indent prefix tokenAnn' inside = case tokenAnn' of
  Language.PureScript.CST.Types.TokenAnn sourceRange leading trailing -> do
    debug log "TokenAnn" tokenAnn' (Span.sourceRange sourceRange)
    commentsLeading log indent prefix leading
      <> inside
      <> commentsTrailing log absurd prefix trailing

type' ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Type Span.Span ->
  IO Utf8Builder
type' log indentation indent' type''' = case type''' of
  Language.PureScript.CST.Types.TypeApp span t1 t2 -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "TypeApp" type''' span
    type' log indentation indent' t1
      <> pure prefix
      <> type' log indentation indent t2
  Language.PureScript.CST.Types.TypeArr span t1 arrow t2 -> do
    let prefix = case span of
          Span.MultipleLines ->
            newline <> indent'
          Span.SingleLine ->
            space
    debug log "TypeArr" type''' span
    type' log indentation indent' t1
      <> pure space
      <> sourceToken log indent' blank arrow
      <> pure prefix
      <> type' log indentation indent' t2
  Language.PureScript.CST.Types.TypeArrName span arrName -> do
    debug log "TypeArrName" type''' span
    sourceToken log indent' blank arrName
  Language.PureScript.CST.Types.TypeConstrained span constraint' arrow type'' -> do
    let prefix = case span of
          Span.MultipleLines ->
            newline <> indent'
          Span.SingleLine ->
            space
    debug log "TypeConstrained" type''' span
    constraint log indentation indent' constraint'
      <> pure space
      <> sourceToken log indent' blank arrow
      <> pure prefix
      <> type' log indentation indent' type''
  Language.PureScript.CST.Types.TypeConstructor span name' -> do
    debug log "TypeConstructor" type''' span
    qualifiedName log indent' blank name'
  Language.PureScript.CST.Types.TypeForall span forall' typeVarBindings dot type'' -> do
    let prefix = case span of
          Span.MultipleLines ->
            newline <> indent'
          Span.SingleLine ->
            space
    debug log "TypeForall" type''' span
    sourceToken log indent' blank forall'
      <> foldMap
        ( \typeVarBinding' ->
            pure space
              <> typeVarBinding log indentation indent' typeVarBinding'
        )
        typeVarBindings
      <> sourceToken log indent' blank dot
      <> pure prefix
      <> type' log indentation indent' type''
  Language.PureScript.CST.Types.TypeHole span hole -> do
    debug log "TypeHole" type''' span
    name log indent' blank hole
  Language.PureScript.CST.Types.TypeKinded span type'' colons kind' -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "TypeKinded" type''' span
    type' log indentation indent' type''
      <> pure space
      <> sourceToken log indent' blank colons
      <> pure prefix
      <> type' log indentation indent kind'
  Language.PureScript.CST.Types.TypeOp span type1 op type2 -> do
    let (indent, prefix) = case span of
          Span.MultipleLines ->
            (indent' <> indentation, newline <> indent)
          Span.SingleLine ->
            (indent', space)
    debug log "TypeOp" type''' span
    type' log indentation indent' type1
      <> qualifiedName log indent prefix op
      <> pure space
      <> type' log indentation indent type2
  Language.PureScript.CST.Types.TypeOpName span name' -> do
    debug log "TypeOpName" type''' span
    qualifiedName log indent' blank name'
  Language.PureScript.CST.Types.TypeParens span wrapped' -> do
    debug log "TypeParens" type''' span
    parens log span indentation indent' (type' log indentation) wrapped'
  Language.PureScript.CST.Types.TypeRecord span wrapped' -> do
    debug log "TypeRecord" type''' span
    wrappedRow log span indentation indent' wrapped'
  Language.PureScript.CST.Types.TypeRow span wrapped' -> do
    debug log "TypeRow" type''' span
    wrappedRow log span indentation indent' wrapped'
  Language.PureScript.CST.Types.TypeString span string _ -> do
    debug log "TypeString" type''' span
    sourceToken log indent' blank string
  Language.PureScript.CST.Types.TypeUnaryRow span sourceToken' type'' -> do
    let prefix = case span of
          Span.MultipleLines ->
            newline <> indent'
          Span.SingleLine ->
            space
    debug log "TypeUnaryRow" type''' span
    sourceToken log indent' blank sourceToken'
      <> pure prefix
      <> type' log indentation indent' type''
  Language.PureScript.CST.Types.TypeVar span var -> do
    debug log "TypeVar" type''' span
    name log indent' blank var
  Language.PureScript.CST.Types.TypeWildcard span wildcard -> do
    debug log "TypeWildcard" type''' span
    sourceToken log indent' blank wildcard

typeVarBinding ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.TypeVarBinding Span.Span ->
  IO Utf8Builder
typeVarBinding log indentation indent' typeVarBinding' = case typeVarBinding' of
  Language.PureScript.CST.Types.TypeVarName name' -> do
    debug log "TypeVarName" typeVarBinding' (Span.typeVarBinding typeVarBinding')
    name log indent' blank name'
  Language.PureScript.CST.Types.TypeVarKinded wrapped' -> do
    debug log "TypeVarKinded" typeVarBinding' (Span.typeVarBinding typeVarBinding')
    wrapped log indent' (labeledNameKind log indentation indent') wrapped'

valueBindingFields ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.ValueBindingFields Span.Span ->
  IO Utf8Builder
valueBindingFields log indentation indent' valueBindingFields' = case valueBindingFields' of
  Language.PureScript.CST.Types.ValueBindingFields name' binders guarded' -> do
    debug log "ValueBindingFields" valueBindingFields' (Span.valueBindingFields valueBindingFields')
    name log indent' blank name'
      <> foldMap
        ( \binder' ->
            pure space
              <> binder log indentation indent' binder'
        )
        binders
      <> guarded log indentation indent' guarded'

where' ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Where Span.Span ->
  IO Utf8Builder
where' log indentation indent' where''' = case where''' of
  Language.PureScript.CST.Types.Where expr' letBindings'' -> do
    let indent = indent' <> indentation
    debug log "Where" where''' (Span.where' where''')
    exprPrefix log (Span.expr expr') indentation indent' expr'
      <> foldMap
        ( \(where'', letBindings') ->
            pure (newline <> indent)
              <> sourceToken log indent blank where''
              <> foldMap
                (letBinding log indentation indent (newline <> indent) newline)
                (RIO.NonEmpty.init letBindings')
              <> letBinding
                log
                indentation
                indent
                (newline <> indent)
                blank
                (RIO.NonEmpty.last letBindings')
        )
        letBindings''

wrapped ::
  (Show a) =>
  Log.Handle ->
  Indent ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Types.Wrapped a ->
  IO Utf8Builder
wrapped log indent f wrapped' = case wrapped' of
  Language.PureScript.CST.Types.Wrapped open value close -> do
    let (before, after) = case span of
          Span.MultipleLines ->
            (space, newline <> indent)
          Span.SingleLine ->
            (blank, blank)
    debug log "Wrapped" wrapped' span
    sourceToken log indent blank open
      <> pure before
      <> f value
      <> pure after
      <> sourceToken log indent blank close
  where
    span :: Span.Span
    span = Span.wrapped wrapped'

wrappedRow ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Types.Wrapped (Language.PureScript.CST.Types.Row Span.Span) ->
  IO Utf8Builder
wrappedRow log span indentation indent wrapped' = case wrapped' of
  Language.PureScript.CST.Types.Wrapped open row' close -> do
    let (before, after) = case (row', span) of
          (Language.PureScript.CST.Types.Row (Just _) _, Span.MultipleLines) ->
            (space, newline <> indent)
          (Language.PureScript.CST.Types.Row Nothing _, Span.MultipleLines) ->
            (blank, newline <> indent)
          (_, Span.SingleLine) ->
            (blank, blank)
    debug log "Wrapped" wrapped' span
    sourceToken log indent blank open
      <> pure before
      <> row log span indentation indent row'
      <> pure after
      <> sourceToken log indent blank close
