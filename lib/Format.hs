module Format
  ( module'
  ) where

import "rio" RIO hiding (bool, log, span)

import qualified "purescript" Language.PureScript.CST
import qualified "this" Span
import qualified "this" Log

type Indent
  = Utf8Builder

type Indentation
  = Utf8Builder

type Prefix
  = Utf8Builder

adoBlock ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.AdoBlock Span.Span ->
  IO Utf8Builder
adoBlock log span indentation indent' adoBlock' = case adoBlock' of
  Language.PureScript.CST.AdoBlock ado doStatements in' expr' -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log "Formatting `AdoBlock`"
    sourceToken log indent' blank ado
      <> foldMap
        (\doStatement' ->
          pure prefix
            <> doStatement log indentation indent doStatement'
        )
        doStatements
      <> pure prefix
      <> sourceToken log indent blank in'
      <> exprPrefix log indentation indent expr'

blank :: Utf8Builder
blank = ""

binder ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Binder Span.Span ->
  IO Utf8Builder
binder log indentation indent' binder'' = case binder'' of
  Language.PureScript.CST.BinderArray span delimited' -> do
    Log.debug log ("Formatting `BinderArray` as `" <> displayShow span <> "`")
    delimited
      log
      indent'
      Span.sourceRangeFromBinder
      (binder log indentation indent')
      delimited'
  Language.PureScript.CST.BinderBoolean _ bool _ -> do
    Log.debug log "Formatting `BinderBoolean`"
    sourceToken log indent' blank bool
  Language.PureScript.CST.BinderChar _ char _ -> do
    Log.debug log "Formatting `BinderChar`"
    sourceToken log indent' blank char
  Language.PureScript.CST.BinderConstructor span name' binders -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log "Formatting `BinderConstructor`"
    qualifiedName log indent' blank name'
      <> foldMap
        (\binder' ->
          pure prefix
            <> binder log indentation indent' binder'
        )
        binders
  Language.PureScript.CST.BinderNamed _ name' at binder' -> do
    Log.debug log "Formatting `BinderNamed`"
    name log indent' blank name'
      <> sourceToken log indent' blank at
      <> binder log indentation indent' binder'
  Language.PureScript.CST.BinderNumber _ negative number _ -> do
    Log.debug log "Formatting `BinderNumber`"
    foldMap (sourceToken log indent' blank) negative
      <> sourceToken log indent' blank number
  Language.PureScript.CST.BinderOp span binder1 name' binder2 -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log ("Formatting `BinderOp` as `" <> displayShow span <> "`")
    binder log indentation indent' binder1
      <> qualifiedName log indent' prefix name'
      <> pure prefix
      <> binder log indentation indent binder2
  Language.PureScript.CST.BinderParens span wrapped' -> do
    Log.debug log ("Formatting `BinderParens` as `" <> displayShow span <> "`")
    wrapped log indent' (binder log indentation indent') wrapped'
  Language.PureScript.CST.BinderRecord span delimited' -> do
    Log.debug log ("Formatting `BinderRecord` as `" <> displayShow span <> "`")
    delimited
      log
      indent'
      (Span.sourceRangeFromRecordLabeled Span.sourceRangeFromBinder)
      ( recordLabeled
        log
        indentation
        indent'
        Span.sourceRangeFromBinder
        (binder log indentation indent')
      )
      delimited'
  Language.PureScript.CST.BinderString _ string _ -> do
    Log.debug log "Formatting `BinderString`"
    sourceToken log indent' blank string
  Language.PureScript.CST.BinderTyped span binder' colons type'' -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log ("Formatting `BinderTyped` as `" <> displayShow span <> "`")
    binder log indentation indent' binder'
      <> sourceToken log indent' blank colons
      <> pure prefix
      <> type' log indentation indent type''
  Language.PureScript.CST.BinderVar _ name' -> do
    Log.debug log "Formatting `BinderVar`"
    name log indent' blank name'
  Language.PureScript.CST.BinderWildcard _ wildcard -> do
    Log.debug log "Formatting `BinderWildcard`"
    sourceToken log indent' blank wildcard

  -- _ -> do
  --   Log.info log "Formatting binder not implemented"
  --   mempty

classFundep ::
  Log.Handle ->
  Indent ->
  Language.PureScript.CST.ClassFundep ->
  IO Utf8Builder
classFundep log indent classFundep' = case classFundep' of
  Language.PureScript.CST.FundepDetermined arrow names -> do
    Log.debug log "Formatting `FundepDetermined`"
    sourceToken log indent blank arrow
      <> foldMap (name log indent space) names
  Language.PureScript.CST.FundepDetermines names arrow names' -> do
    Log.debug log "Formatting `FundepDetermines`"
    foldMap
      (\name' ->
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
  Language.PureScript.CST.ClassHead Span.Span ->
  IO Utf8Builder
classHead log span indentation indent' classHead' = case classHead' of
  Language.PureScript.CST.ClassHead class' super name' typeVarBindings fundeps -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log "Formatting `ClassHead`"
    sourceToken log indent' blank class'
      <> foldMap
        (\(constraints, arrow) ->
          pure prefix
            <> oneOrDelimited
              log
              indent
              Span.sourceRangeFromConstraint
              (constraint log indentation indent)
              constraints
            <> sourceToken log indent space arrow
        )
        super
      <> name log indent space name'
      <> foldMap
        (\typeVarBinding' ->
          pure space
            <> typeVarBinding log indent typeVarBinding'
        )
        typeVarBindings
      <> foldMap
        (\(bar, classFundeps) ->
          sourceToken log indent space bar
            <> pure space
            <> separated
              log
              indent
              Span.sourceRangeFromClassFundep
              (classFundep log indent)
              classFundeps
        )
        fundeps

commentLeading ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Comment a ->
  IO Utf8Builder
commentLeading log indent prefix comment'' = case comment'' of
  Language.PureScript.CST.Comment comment' -> do
    Log.debug log ("Formatting comment: " <> display comment')
    pure (prefix <> display comment' <> newline <> indent)
  Language.PureScript.CST.Line _ -> do
    Log.debug log "Not formatting `Line`"
    pure blank
  Language.PureScript.CST.Space _ -> do
    Log.debug log "Not formatting `Space`"
    pure blank

commentTrailing ::
  Log.Handle ->
  Prefix ->
  Language.PureScript.CST.Comment a ->
  IO Utf8Builder
commentTrailing log prefix comment'' = case comment'' of
  Language.PureScript.CST.Comment comment' -> do
    Log.debug log ("Formatting comment: " <> display comment')
    pure (prefix <> display comment')
  Language.PureScript.CST.Line _ -> do
    Log.debug log "Not formatting `Line`"
    pure blank
  Language.PureScript.CST.Space _ -> do
    Log.debug log "Not formatting `Space`"
    pure blank

commentsLeading ::
  Log.Handle ->
  Indent ->
  Prefix ->
  [Language.PureScript.CST.Comment a] ->
  IO Utf8Builder
commentsLeading log indent prefix commentsLeading' = case commentsLeading' of
  [] -> do
    Log.debug log "No leading comments to format"
    pure blank
  _ -> do
    Log.debug log "Formatting leading comments"
    foldMap (commentLeading log indent prefix) commentsLeading'

commentsTrailing ::
  Log.Handle ->
  Prefix ->
  [Language.PureScript.CST.Comment a] ->
  IO Utf8Builder
commentsTrailing log prefix commentsTrailing' = case commentsTrailing' of
  [] -> do
    Log.debug log "No trailing comments to format"
    pure blank
  _ -> do
    Log.debug log "Formatting trailing comments"
    foldMap (commentTrailing log prefix) commentsTrailing'

constraint ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Constraint Span.Span ->
  IO Utf8Builder
constraint log indentation indent' constraint' = case constraint' of
  Language.PureScript.CST.Constraint span name' types -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log ("Formatting `Constraint` as `" <> displayShow span <> "`")
    qualifiedName log indent' space name'
      <> foldMap
        (\type'' ->
          pure prefix
            <> type' log indentation indent type''
        )
        types
  Language.PureScript.CST.ConstraintParens span wrapped' -> do
    Log.debug
      log
      ("Formatting `ConstraintParens` as `" <> displayShow span <> "`")
    wrapped log indent' (constraint log indentation indent') wrapped'

dataCtor ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.DataCtor Span.Span ->
  IO Utf8Builder
dataCtor log indentation indent' dataCtor' = case dataCtor' of
  Language.PureScript.CST.DataCtor span name' types -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log ("Formatting `DataCtor` as `" <> displayShow span <> "`")
    name log indent' space name'
      <> foldMap
        (\type'' ->
          pure prefix
            <> type' log indentation indent type''
        )
        types

dataHead ::
  Log.Handle ->
  Indent ->
  Language.PureScript.CST.DataHead Span.Span ->
  IO Utf8Builder
dataHead log indent dataHead' = case dataHead' of
  Language.PureScript.CST.DataHead data' name' typeVarBindings -> do
    Log.debug log "Formatting `DataHead`"
    sourceToken log indent blank data'
      <> name log indent space name'
      <> foldMap
        (\typeVarBinding' ->
          pure space
            <> typeVarBinding log indent typeVarBinding'
        )
        typeVarBindings

dataMembers ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.DataMembers Span.Span ->
  IO Utf8Builder
dataMembers log indentation indent' dataMembers' = case dataMembers' of
  Language.PureScript.CST.DataAll _ sourceToken' -> do
    Log.debug log "Formatting `DataAll`"
    sourceToken log indent' blank sourceToken'
  Language.PureScript.CST.DataEnumerated span delimited' -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent')
        Span.SingleLine ->
          (indent', blank)
    Log.debug log ("Formatting `DataEnumerated` as `" <> displayShow span <> "`")
    pure prefix
      <> delimited
        log
        indent'
        Span.sourceRangeFromName
        (name log indent blank)
        delimited'

declaration ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Declaration Span.Span ->
  IO Utf8Builder
declaration log indentation indent' declaration' = case declaration' of
  Language.PureScript.CST.DeclClass span classHead' members -> do
    let
      indent = indent' <> indentation
    Log.debug log ("Formatting `DeclClass` as `" <> displayShow span <> "`")
    classHead log span indentation indent' classHead'
      <> foldMap
        (\(where'', labeleds) ->
          sourceToken log indent' space where''
            <> foldMap
              (\labeled' ->
                pure (newline <> indent)
                  <> labeledNameType log indent labeled'
              )
              labeleds
        )
        members
      <> pure newline
  Language.PureScript.CST.DeclData span dataHead' dataCtors' -> do
    let
      indent = indent' <> indentation
    Log.debug log ("Formatting `DeclData` as `" <> displayShow span <> "`")
    dataHead log indent' dataHead'
      <> foldMap
        (\(equals, dataCtors) ->
          pure (newline <> indent)
            <> sourceToken log indent blank equals
            <> separated
              log
              indent
              Span.sourceRangeFromDataCtor
              (dataCtor log indentation indent)
              dataCtors
        )
        dataCtors'
      <> pure newline
  Language.PureScript.CST.DeclDerive span derive newtype' instanceHead' -> do
    Log.debug log ("Formatting `DeclDerive` as `" <> displayShow span <> "`")
    sourceToken log indent' blank derive
      <> foldMap (sourceToken log indent' space) newtype'
      <> instanceHead log span indentation indent' instanceHead'
      <> pure newline
  Language.PureScript.CST.DeclFixity span fixityFields' -> do
    Log.debug log ("Formatting `DeclFixity` as `" <> displayShow span <> "`")
    fixityFields log indent' fixityFields'
      <> pure newline
  Language.PureScript.CST.DeclForeign span foreign'' import'' foreign''' -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log ("Formatting `DeclForeign` as `" <> displayShow span <> "`")
    sourceToken log indent' blank foreign''
      <> sourceToken log indent' space import''
      <> pure prefix
      <> foreign' log span indentation indent foreign'''
      <> pure newline
  Language.PureScript.CST.DeclInstanceChain span instances -> do
    let
      indent = case span of
        Span.MultipleLines ->
          indent' <> indentation
        Span.SingleLine ->
          indent'
    Log.debug log ("Formatting `DeclInstanceChain` as `" <> displayShow span <> "`")
    separated
      log
      indent
      Span.sourceRangeFromInstance
      (instance' log span indentation indent)
      instances
      <> pure newline
  Language.PureScript.CST.DeclNewtype span dataHead' equals name' type'' -> do
    let
      indent = indent' <> indentation
    Log.debug log ("Formatting `DeclNewtype` as `" <> displayShow span <> "`")
    dataHead log indent' dataHead'
      <> pure (newline <> indent)
      <> sourceToken log indent blank equals
      <> name log indent space name'
      <> type' log indentation indent type''
      <> pure newline
  Language.PureScript.CST.DeclSignature span labeled' -> do
    Log.debug log ("Formatting `DeclSignature` as `" <> displayShow span <> "`")
    labeledNameType log indent' labeled'
  Language.PureScript.CST.DeclType span dataHead' equals type'' -> do
    let
      indent = indent' <> indentation
    Log.debug log ("Formatting `DeclType` as `" <> displayShow span <> "`")
    dataHead log indent' dataHead'
      <> pure (newline <> indent)
      <> sourceToken log indent blank equals
      <> type' log indentation indent type''
      <> pure newline
  Language.PureScript.CST.DeclValue span valueBindingFields' -> do
    Log.debug log ("Formatting `DeclValue` as `" <> displayShow span <> "`")
    valueBindingFields log indentation indent' valueBindingFields'
      <> pure newline

declarations ::
  Log.Handle ->
  Indentation ->
  [Language.PureScript.CST.Declaration Span.Span] ->
  IO Utf8Builder
declarations log indentation declarations' = case declarations' of
  [] -> do
    Log.debug log "No declarations to format"
    pure blank
  _ -> do
    let
      indent = blank
    Log.debug log "Formatting declarations"
    foldMap
      (\declaration' ->
        pure newline
          <> declaration log indentation indent declaration'
      )
      declarations'

delimited ::
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Delimited a ->
  IO Utf8Builder
delimited log indent f g delimited' = do
  Log.debug log "Formatting `Delimited`"
  wrapped log indent (foldMap $ separated log indent f g) delimited'

delimitedNonEmpty ::
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.DelimitedNonEmpty a ->
  IO Utf8Builder
delimitedNonEmpty log indent f g delimitedNonEmpty' = do
  Log.debug log "Formatting `DelimitedNonEmpty`"
  wrapped log indent (separated log indent f g) delimitedNonEmpty'

doStatement ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.DoStatement Span.Span ->
  IO Utf8Builder
doStatement log indentation indent' doStatement' = case doStatement' of
  Language.PureScript.CST.DoBind binder' arrow expr' -> do
    let
      span = Span.doStatement doStatement'

      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log ("Formatting `DoBind` as `" <> displayShow span <> "`")
    binder log indentation indent' binder'
      <> sourceToken log indent' space arrow
      <> pure prefix
      <> expr log indentation indent expr'
  Language.PureScript.CST.DoDiscard expr' -> do
    Log.debug log "Formatting `DoDiscard`"
    expr log indentation indent' expr'
  Language.PureScript.CST.DoLet let' letBindings -> do
    let
      indent = indent' <> indentation
    Log.debug log "Formatting `DoDiscard`"
    sourceToken log indent' blank let'
      <> foldMap
        (\letBinding' ->
          pure (newline <> indent)
            <> letBinding log indentation indent letBinding'
        )
        letBindings

export ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Export Span.Span ->
  IO Utf8Builder
export log indentation indent' export' = case export' of
  Language.PureScript.CST.ExportClass _ class' name' -> do
    Log.debug log ("Formatting `ExportClass`: " <> displayShow name')
    sourceToken log indent' blank class'
      <> name log indent' space name'
  Language.PureScript.CST.ExportKind _ kind' name' -> do
    Log.debug log ("Formatting `ExportKind`: " <> displayShow name')
    sourceToken log indent' blank kind'
      <> name log indent' space name'
  Language.PureScript.CST.ExportModule _ module'' name' -> do
    Log.debug log ("Formatting `ExportModule`: " <> displayShow name')
    sourceToken log indent' blank module''
      <> name log indent' space name'
  Language.PureScript.CST.ExportOp _ name' -> do
    Log.debug log ("Formatting `ExportOp`: " <> displayShow name')
    name log indent' blank name'
  Language.PureScript.CST.ExportType span name' dataMembers' -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', blank)
    Log.debug
      log
      ( "Formatting `ExportType`: "
        <> displayShow name'
        <> " as `"
        <> displayShow span
        <> "`"
      )
    name log indent' blank name'
      <> pure prefix
      <> foldMap (dataMembers log indentation indent) dataMembers'
  Language.PureScript.CST.ExportTypeOp _ type'' name' -> do
    Log.debug log ("Formatting `ExportTypeOp`: " <> displayShow name')
    sourceToken log indent' blank type''
      <> name log indent' space name'
  Language.PureScript.CST.ExportValue _ name' -> do
    Log.debug log ("Formatting `ExportValue`: " <> displayShow name')
    name log indent' blank name'

exports ::
  Log.Handle ->
  Indentation ->
  Maybe
    ( Language.PureScript.CST.DelimitedNonEmpty
      (Language.PureScript.CST.Export Span.Span)
    ) ->
  IO Utf8Builder
exports log indentation exports'' = case exports'' of
  Nothing -> do
    Log.debug log "No exports to format"
    pure blank
  Just exports' -> do
    let
      indent = indentation

      prefix = case span of
        Span.MultipleLines ->
          newline <> indent
        Span.SingleLine ->
          space

      span = Span.delimitedNonEmpty exports'
    Log.debug log ("Formatting exports as `" <> displayShow span <> "`")
    pure prefix
      <> delimitedNonEmpty
        log
        indent
        Span.sourceRangeFromExport
        (export log indentation indent)
        exports'

expr ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Expr Span.Span ->
  IO Utf8Builder
expr log indentation indent' expr' = case expr' of
  Language.PureScript.CST.ExprAdo span adoBlock' -> do
    Log.debug log ("Formatting `ExprAdo` as `" <> displayShow span <> "`")
    adoBlock log span indentation indent' adoBlock'
  Language.PureScript.CST.ExprApp span expr1 expr2 -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log ("Formatting `ExprApp` as `" <> displayShow span <> "`")
    expr log indentation indent' expr1
      <> pure prefix
      <> expr log indentation indent expr2
  Language.PureScript.CST.ExprArray span delimited' -> do
    Log.debug log ("Formatting `ExprArray` as `" <> displayShow span <> "`")
    delimited
      log
      indent'
      Span.sourceRangeFromExpr
      (expr log indentation indent')
      delimited'
  _ -> do
    Log.info log "Formatting expr not implemented"
    mempty

exprPrefix ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Expr Span.Span ->
  IO Utf8Builder
exprPrefix = expr

fixityFields ::
  Log.Handle ->
  Indent ->
  Language.PureScript.CST.FixityFields ->
  IO Utf8Builder
fixityFields log indent fixityFields' = case fixityFields' of
  Language.PureScript.CST.FixityFields (infix', _) (precedence, _) fixityOp' -> do
    Log.debug log "Formatting `FixityFields`"
    sourceToken log indent blank infix'
      <> sourceToken log indent space precedence
      <> pure space
      <> fixityOp log indent fixityOp'

fixityOp ::
  Log.Handle ->
  Indent ->
  Language.PureScript.CST.FixityOp ->
  IO Utf8Builder
fixityOp log indent fixityOp' = case fixityOp' of
  Language.PureScript.CST.FixityType type'' name' as op -> do
    Log.debug log "Formatting `FixityType`"
    sourceToken log indent blank type''
      <> qualifiedName log indent space name'
      <> sourceToken log indent space as
      <> name log indent space op
  Language.PureScript.CST.FixityValue name' as op -> do
    Log.debug log "Formatting `FixityValue`"
    qualifiedName log indent blank name'
      <> sourceToken log indent space as
      <> name log indent space op

foreign' ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Foreign Span.Span ->
  IO Utf8Builder
foreign' log span indentation indent' foreign'' = case foreign'' of
  Language.PureScript.CST.ForeignData data' labeled' -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log "Formatting `ForeignData`"
    sourceToken log indent' blank data'
      <> pure prefix
      <> labeledNameKind log indent labeled'
  Language.PureScript.CST.ForeignKind kind' name' -> do
    Log.debug log "Formatting `ForeignKind`"
    sourceToken log indent' blank kind'
      <> name log indent' space name'
  Language.PureScript.CST.ForeignValue labeled' -> do
    Log.debug log "Formatting `ForeignValue`"
    labeledNameType log indent' labeled'

guarded ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Guarded Span.Span ->
  IO Utf8Builder
guarded log indentation indent' guarded' = case guarded' of
  Language.PureScript.CST.Guarded guardedExprs -> do
    let
      indent = indent' <> indentation
    Log.debug log "Formatting `Guarded`"
    foldMap
      (\guardedExpr' ->
        pure (newline <> indent')
          <> guardedExpr log indentation indent guardedExpr'
      )
      guardedExprs
  Language.PureScript.CST.Unconditional separator where'' -> do
    Log.debug log "Formatting `Unconditional`"
    sourceToken log indent' space separator
      <> where' log indentation indent' where''

guardedExpr ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.GuardedExpr Span.Span ->
  IO Utf8Builder
guardedExpr log indentation indent' guardedExpr' = case guardedExpr' of
  Language.PureScript.CST.GuardedExpr bar patternGuards separator where'' -> do
    let
      indent = indent' <> indentation
    Log.debug log "Formatting `GuardedExpr`"
    sourceToken log indent' blank bar
      <> separated
        log
        indent'
        Span.sourceRangeFromPatternGuard
        (patternGuard log indentation indent)
        patternGuards
      <> sourceToken log indent' blank separator
      <> where' log indentation indent' where''

import' ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Import Span.Span ->
  IO Utf8Builder
import' log indentation indent' import'' = case import'' of
  Language.PureScript.CST.ImportClass _ class' name' -> do
    Log.debug log ("Formatting `ImportClass`: " <> displayShow name')
    sourceToken log indent' blank class'
      <> name log indent' space name'
  Language.PureScript.CST.ImportKind _ kind' name' -> do
    Log.debug log ("Formatting `ImportKind`: " <> displayShow name')
    sourceToken log indent' blank kind'
      <> name log indent' space name'
  Language.PureScript.CST.ImportOp _ name' -> do
    Log.debug log ("Formatting `ImportOp`: " <> displayShow name')
    name log indent' blank name'
  Language.PureScript.CST.ImportType span name' dataMembers' -> do
    let
      indent = case span of
        Span.MultipleLines ->
          indent' <> indentation
        Span.SingleLine ->
          indent'
    Log.debug
      log
      ( "Formatting `ImportType`: "
        <> displayShow name'
        <> " as `"
        <> displayShow span
        <> "`"
      )
    name log indent' blank name'
      <> foldMap (dataMembers log indentation indent) dataMembers'
  Language.PureScript.CST.ImportTypeOp _ type'' name' -> do
    Log.debug log ("Formatting `ImportTypeOp`: " <> displayShow name')
    sourceToken log indent' blank type''
      <> name log indent' space name'
  Language.PureScript.CST.ImportValue _ name' -> do
    Log.debug log ("Formatting `ImportValue`: " <> displayShow name')
    name log indent' blank name'

importDecl ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.ImportDecl Span.Span ->
  IO Utf8Builder
importDecl log indentation indent'' importDecl' = case importDecl' of
  Language.PureScript.CST.ImportDecl span import'' name'' imports'' rename -> do
    let indent' = indent'' <> indentation
    Log.debug log ("Formatting `ImportDecl` as `" <> displayShow span <> "`")
    sourceToken log indent'' blank import''
      <> name log indent'' space name''
      <> foldMap
        (\(hiding', imports') ->
          case hiding' of
            Just hiding -> do
              let
                hidingPrefix = case span of
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
                  Span.sourceRangeFromImport
                  (import' log indentation indent')
                  imports'
            Nothing -> do
              let
                importPrefix = case span of
                  Span.MultipleLines ->
                    newline <> indent'
                  Span.SingleLine ->
                    space
              pure importPrefix
                <> delimitedNonEmpty
                  log
                  indent'
                  Span.sourceRangeFromImport
                  (import' log indentation indent')
                  imports'
        )
        imports''
      <> foldMap
        (\(as, name') -> do
          let
            prefix = case span of
              Span.MultipleLines ->
                newline <> indent''
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
  [Language.PureScript.CST.ImportDecl Span.Span] ->
  IO Utf8Builder
imports log indentation imports' = case imports' of
  [] -> do
    Log.debug log "No imports to format"
    pure blank
  _ -> do
    let
      indent = blank
    Log.debug log "Formatting imports"
    foldMap
      (\importDecl' ->
        pure newline
          <> importDecl log indentation indent importDecl'
      )
      imports'
      <> pure newline

instance' ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Instance Span.Span ->
  IO Utf8Builder
instance' log span indentation indent' instance'' = case instance'' of
  Language.PureScript.CST.Instance instanceHead' body -> do
    let indent = indent' <> indentation
    Log.debug log "Formatting `Instance`"
    instanceHead log span indentation indent' instanceHead'
      <> foldMap
        (\(where'', instanceBindings) ->
          sourceToken log indent' space where''
            <> foldMap
              (\instanceBinding' ->
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
  Language.PureScript.CST.InstanceBinding Span.Span ->
  IO Utf8Builder
instanceBinding log indentation indent instanceBinding' = case instanceBinding' of
  Language.PureScript.CST.InstanceBindingName span valueBindingFields' -> do
    Log.debug
      log
      ("Formatting `InstanceBindingName` as `" <> displayShow span <> "`")
    valueBindingFields log indentation indent valueBindingFields'
  Language.PureScript.CST.InstanceBindingSignature span labeled' -> do
    Log.debug
      log
      ("Formatting `InstanceBindingSignature` as `" <> displayShow span <> "`")
    labeledNameType log indent labeled'

instanceHead ::
  Log.Handle ->
  Span.Span ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.InstanceHead Span.Span ->
  IO Utf8Builder
instanceHead log span indentation indent' instanceHead' = case instanceHead' of
  Language.PureScript.CST.InstanceHead instance'' name' colons constraints' className types -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log "Formatting `InstanceHead`"
    sourceToken log indent' blank instance''
      <> name log indent' space name'
      <> sourceToken log indent' space colons
      <> foldMap
        (\(constraints, arrow) ->
          pure prefix
            <> oneOrDelimited
              log
              indent
              Span.sourceRangeFromConstraint
              (constraint log indentation indent)
              constraints
            <> sourceToken log indent space arrow
        )
        constraints'
      <> qualifiedName log indent space className
      <> foldMap
        (\type'' ->
          pure space
            <> type' log indentation indent type''
        )
        types

kind ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Kind Span.Span ->
  IO Utf8Builder
kind log indentation indent' kind' = case kind' of
  Language.PureScript.CST.KindArr span k1 arrow k2 -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log ("Formatting `KindArr` as `" <> displayShow span <> "`")
    kind log indentation indent' k1
      <> sourceToken log indent' space arrow
      <> pure prefix
      <> kind log indentation indent k2
  _ -> do
    Log.info log "Formatting kind not implemented"
    mempty

label ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Label ->
  IO Utf8Builder
label log indent prefix label'' = case label'' of
  Language.PureScript.CST.Label label' _ -> do
    Log.debug log ("Formatting `Label`: " <> displayShow label')
    sourceToken log indent prefix label'

labeled ::
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.SourceRange) ->
  (a -> IO Utf8Builder) ->
  (b -> Language.PureScript.CST.SourceRange) ->
  (b -> IO Utf8Builder) ->
  Language.PureScript.CST.Labeled a b ->
  IO Utf8Builder
labeled log indent f g h i labeled' = case labeled' of
  Language.PureScript.CST.Labeled label' separator value -> do
    let
      span = Span.labeled f h labeled'

      prefix = case span of
        Span.MultipleLines ->
          newline <> indent
        Span.SingleLine ->
          space
    Log.debug log ("Formatting `Labeled` as `" <> displayShow span <> "`")
    g label'
      <> sourceToken log indent space separator
      <> pure prefix
      <> i value

labeledNameKind ::
  Log.Handle ->
  Indent ->
  Language.PureScript.CST.Labeled
    (Language.PureScript.CST.Name a)
    (Language.PureScript.CST.Kind Span.Span) ->
  IO Utf8Builder
labeledNameKind log indent =
  labeled
    log
    indent
    Span.sourceRangeFromName
    (name log indent blank)
    Span.sourceRangeFromKind
    (kind log indent blank)

labeledNameType ::
  Log.Handle ->
  Indent ->
  Language.PureScript.CST.Labeled
    (Language.PureScript.CST.Name a)
    (Language.PureScript.CST.Type Span.Span) ->
  IO Utf8Builder
labeledNameType log indent =
  labeled
    log
    indent
    Span.sourceRangeFromName
    (name log indent blank)
    Span.sourceRangeFromType
    (type' log indent blank)

letBinding ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.LetBinding Span.Span ->
  IO Utf8Builder
letBinding log indentation indent' letBinding' = case letBinding' of
  Language.PureScript.CST.LetBindingName span valueBindingFields' -> do
    Log.debug log ("Formatting `LetBindingName` as `" <> displayShow span <> "`")
    valueBindingFields log indentation indent' valueBindingFields'
  Language.PureScript.CST.LetBindingPattern span binder' equals where'' -> do
    Log.debug
      log
      ("Formatting `LetBindingPattern` as `" <> displayShow span <> "`")
    binder log indentation indent' binder'
      <> sourceToken log indent' space equals
      <> where' log indentation indent' where''
  Language.PureScript.CST.LetBindingSignature span labeled' -> do
    Log.debug
      log
      ("Formatting `LetBindingSignature` as `" <> displayShow span <> "`")
    labeledNameType log indent' labeled'

module' ::
  Log.Handle ->
  Indentation ->
  Language.PureScript.CST.Module Span.Span ->
  IO Utf8Builder
module' log indentation module''' = case module''' of
  Language.PureScript.CST.Module _ module'' name' exports' where'' imports' declarations' trailing -> do
    Log.debug log "Formatting `Module`"
    sourceToken log blank blank module''
      <> name log blank space name'
      <> exports log indentation exports'
      <> sourceToken log blank space where''
      <> pure newline
      <> imports log indentation imports'
      <> declarations log indentation declarations'
      <> commentsTrailing log blank trailing

name ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Name a ->
  IO Utf8Builder
name log indent prefix name'' = case name'' of
  Language.PureScript.CST.Name name' _ -> do
    Log.debug log ("Formatting `Name`: " <> displayShow name')
    sourceToken log indent prefix name'

newline :: Utf8Builder
newline = "\n"

oneOrDelimited ::
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.OneOrDelimited a ->
  IO Utf8Builder
oneOrDelimited log indent f g oneOrDelimited' = case oneOrDelimited' of
  Language.PureScript.CST.One a -> do
    Log.debug log "Formatting `One`"
    g a
  Language.PureScript.CST.Many delimitedNonEmpty' -> do
    Log.debug log "Formatting `Many`"
    delimitedNonEmpty log indent f g delimitedNonEmpty'

patternGuard ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.PatternGuard Span.Span ->
  IO Utf8Builder
patternGuard log indentation indent' patternGuard' = case patternGuard' of
  Language.PureScript.CST.PatternGuard binder'' expr' -> do
    Log.debug log "Formatting `PatternGuard`"
    foldMap
      (\(binder', arrow) ->
        binder log indentation indent' binder'
          <> sourceToken log indent' space arrow
      )
      binder''
      <> exprPrefix log indentation indent' expr'

qualifiedName ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.QualifiedName a ->
  IO Utf8Builder
qualifiedName log indent prefix qualifiedName'' = case qualifiedName'' of
  Language.PureScript.CST.QualifiedName qualifiedName' _ _ -> do
    Log.debug log ("Formatting `QualifiedName`: " <> displayShow qualifiedName')
    sourceToken log indent prefix qualifiedName'

recordLabeled ::
  Log.Handle ->
  Indentation ->
  Indent ->
  (a -> Language.PureScript.CST.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.RecordLabeled a ->
  IO Utf8Builder
recordLabeled log indentation indent' f g recordLabeled' = case recordLabeled' of
  Language.PureScript.CST.RecordPun name' -> do
    Log.debug log "Formatting `RecordPun`"
    name log indent' blank name'
  Language.PureScript.CST.RecordField label' colon a -> do
    let
      span = Span.recordLabeled f recordLabeled'

      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log ("Formatting `RecordField` as `" <> displayShow span <> "`")
    label log indent' blank label'
      <> sourceToken log indent' blank colon
      <> pure prefix
      <> g a

separated ::
  forall a.
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Separated a ->
  IO Utf8Builder
separated log indent f g separated' = case separated' of
  Language.PureScript.CST.Separated head tail -> do
    Log.debug log ("Formatting `Separated` as " <> displayShow span <> "`")
    g head
      <> foldMap go tail
  where
  go :: (Language.PureScript.CST.SourceToken, a) -> IO Utf8Builder
  go x = case x of
    (separator, value) -> do
      let
        prefix = case span of
          Span.MultipleLines ->
            newline <> indent
          Span.SingleLine ->
            blank
      pure prefix
        <> sourceToken log indent blank separator
        <> pure space
        <> g value
  span :: Span.Span
  span = Span.separated f separated'

sourceToken ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.SourceToken ->
  IO Utf8Builder
sourceToken log indent prefix sourceToken' = case sourceToken' of
  Language.PureScript.CST.SourceToken ann token ->
    tokenAnn log indent prefix ann $ do
      Log.debug log ("Formatting `SourceToken`: " <> displayShow token)
      pure (prefix <> display (Language.PureScript.CST.printToken token))

space :: Utf8Builder
space = " "

tokenAnn ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.TokenAnn ->
  IO Utf8Builder ->
  IO Utf8Builder
tokenAnn log indent prefix tokenAnn' inside = case tokenAnn' of
  Language.PureScript.CST.TokenAnn _ leading trailing -> do
    Log.debug log "Formatting `TokenAnn`"
    commentsLeading log indent prefix leading
      <> inside
      <> commentsTrailing log prefix trailing

type' ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Type Span.Span ->
  IO Utf8Builder
type' log indentation indent' type'' = case type'' of
  Language.PureScript.CST.TypeApp span t1 t2 -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', space)
    Log.debug log ("Formatting `Type` as " <> displayShow span <> "`")
    type' log indentation indent' t1
      <> pure prefix
      <> type' log indentation indent t2
  _ -> do
    Log.info log "Formatting type not implemented"
    mempty

typeVarBinding ::
  Log.Handle ->
  Indent ->
  Language.PureScript.CST.TypeVarBinding Span.Span ->
  IO Utf8Builder
typeVarBinding log indent' typeVarBinding' = case typeVarBinding' of
  Language.PureScript.CST.TypeVarName name' -> do
    Log.debug log "Formatting `TypeVarName`"
    name log indent' blank name'
  Language.PureScript.CST.TypeVarKinded wrapped' -> do
    Log.debug log "Formatting `TypeVarKinded`"
    wrapped log indent' (labeledNameKind log indent') wrapped'

valueBindingFields ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.ValueBindingFields Span.Span ->
  IO Utf8Builder
valueBindingFields log indentation indent' valueBindingFields' = case valueBindingFields' of
  Language.PureScript.CST.ValueBindingFields name' binders guarded' -> do
    Log.debug log "Formatting `ValueBindingFields`"
    name log indent' blank name'
      <> foldMap
        (\binder' ->
          pure space
            <> binder log indentation indent' binder'
        )
        binders
      <> guarded log indentation indent' guarded'

where' ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Where Span.Span ->
  IO Utf8Builder
where' log indentation indent' where''' = case where''' of
  Language.PureScript.CST.Where expr' letBindings'' -> do
    let
      indent = indent' <> indentation
    Log.debug log ("Formatting `Where`: " <> displayShow where''')
    exprPrefix log indentation indent' expr'
      <> foldMap
        (\(where'', letBindings') ->
          pure (newline <> indent)
            <> sourceToken log indent blank where''
            <> foldMap
              (\letBinding' ->
                pure (newline <> indent)
                  <> letBinding log indentation indent letBinding'
              )
              letBindings'
        )
        letBindings''

wrapped ::
  Log.Handle ->
  Indent ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Wrapped a ->
  IO Utf8Builder
wrapped log indent f wrapped' = case wrapped' of
  Language.PureScript.CST.Wrapped open value close -> do
    let
      (before, after) = case span of
        Span.MultipleLines ->
          (space, newline <> indent)
        Span.SingleLine ->
          (blank, blank)
    Log.debug log ("Formatting `Wrapped` as " <> displayShow span <> "`")
    sourceToken log indent blank open
      <> pure before
      <> f value
      <> pure after
      <> sourceToken log indent blank close
  where
  span :: Span.Span
  span = Span.wrapped wrapped'
