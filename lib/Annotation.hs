module Annotation
  ( module'
  ) where

import "rio" RIO hiding (log, span)

import qualified "purescript" Language.PureScript.CST
import qualified "base" Data.Bitraversable
import qualified "this" Log
import qualified "this" SourceRange
import qualified "this" Span

adoBlock ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.AdoBlock a ->
  IO (Language.PureScript.CST.AdoBlock Span.Span)
adoBlock log adoBlock' = case adoBlock' of
  Language.PureScript.CST.AdoBlock ado doStatements' in' expr'' -> do
    let span = Span.adoBlock adoBlock'
    debug log "AdoBlock" adoBlock' span
    doStatements <- traverse (doStatement log) doStatements'
    expr' <- expr log expr''
    pure (Language.PureScript.CST.AdoBlock ado doStatements in' expr')

binder ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Binder a ->
  IO (Language.PureScript.CST.Binder Span.Span)
binder log binder''' = case binder''' of
  Language.PureScript.CST.BinderBoolean _ boolean x -> do
    let span = Span.binder binder'''
    debug log "BinderBoolean" binder''' span
    pure (Language.PureScript.CST.BinderBoolean span boolean x)
  Language.PureScript.CST.BinderChar _ char x -> do
    let span = Span.binder binder'''
    debug log "BinderChar" binder''' span
    pure (Language.PureScript.CST.BinderChar span char x)
  Language.PureScript.CST.BinderConstructor _ name' binders' -> do
    let span = Span.binder binder'''
    debug log "BinderConstructor" binder''' span
    binders <- traverse (binder log) binders'
    pure (Language.PureScript.CST.BinderConstructor span name' binders)
  Language.PureScript.CST.BinderNamed _ name' at binder'' -> do
    let span = Span.binder binder'''
    debug log "BinderNamed" binder''' span
    binder' <- binder log binder''
    pure (Language.PureScript.CST.BinderNamed span name' at binder')
  Language.PureScript.CST.BinderNumber _ negative number x -> do
    let span = Span.binder binder'''
    debug log "BinderNumber" binder''' span
    pure (Language.PureScript.CST.BinderNumber span negative number x)
  Language.PureScript.CST.BinderOp _ binder1' op binder2' -> do
    let span = Span.binder binder'''
    debug log "BinderOp" binder''' span
    binder1 <- binder log binder1'
    binder2 <- binder log binder2'
    pure (Language.PureScript.CST.BinderOp span binder1 op binder2)
  Language.PureScript.CST.BinderString _ string x -> do
    let span = Span.binder binder'''
    debug log "BinderString" binder''' span
    pure (Language.PureScript.CST.BinderString span string x)
  Language.PureScript.CST.BinderVar _ var -> do
    let span = Span.binder binder'''
    debug log "BinderVar" binder''' span
    pure (Language.PureScript.CST.BinderVar span var)
  Language.PureScript.CST.BinderWildcard _ wildcard -> do
    let span = Span.binder binder'''
    debug log "BinderWildcard" binder''' span
    pure (Language.PureScript.CST.BinderWildcard span wildcard)
  _ -> do
    let span = Span.MultipleLines
    notImplemented log "Binder" binder''' span
    pure (span <$ binder''')

dataMembers ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.DataMembers a ->
  IO (Language.PureScript.CST.DataMembers Span.Span)
dataMembers log dataMembers' = case dataMembers' of
  Language.PureScript.CST.DataAll _ sourceToken' -> do
    let span = Span.SingleLine
    debug log "DataAll" dataMembers' span
    pure (Language.PureScript.CST.DataAll span sourceToken')
  Language.PureScript.CST.DataEnumerated _ delimited' -> do
    let span = Span.dataMembers dataMembers'
    debug log "DataEnumerated" dataMembers' span
    pure (Language.PureScript.CST.DataEnumerated span delimited')

debug :: (Show a) => Log.Handle -> Utf8Builder -> a -> Span.Span -> IO ()
debug log x y z =
  Log.debug
    log
    ( "Annotating `"
      <> x
      <> "`: "
      <> displayShow y
      <> " as `"
      <> displayShow z
      <> "`"
    )

declaration ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Declaration a ->
  IO (Language.PureScript.CST.Declaration Span.Span)
declaration log declaration' = case declaration' of
  Language.PureScript.CST.DeclValue _ valueBindingFields'' -> do
    let span = Span.valueBindingFields valueBindingFields''
    debug log "DeclValue" declaration' span
    valueBindingFields' <- valueBindingFields log valueBindingFields''
    pure (Language.PureScript.CST.DeclValue span valueBindingFields')
  _ -> do
    let span = Span.MultipleLines
    notImplemented log "Declaration" declaration' span
    pure (span <$ declaration')

doBlock ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.DoBlock a ->
  IO (Language.PureScript.CST.DoBlock Span.Span)
doBlock log doBlock' = case doBlock' of
  Language.PureScript.CST.DoBlock do' doStatements' -> do
    let span = Span.doBlock doBlock'
    debug log "DoBlock" doBlock' span
    doStatements <- traverse (doStatement log) doStatements'
    pure (Language.PureScript.CST.DoBlock do' doStatements)

doStatement ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.DoStatement a ->
  IO (Language.PureScript.CST.DoStatement Span.Span)
doStatement log doStatement' = case doStatement' of
  Language.PureScript.CST.DoBind binder'' arrow expr'' -> do
    let span = Span.doStatement doStatement'
    debug log "DoBind" doStatement' span
    binder' <- binder log binder''
    expr' <- expr log expr''
    pure (Language.PureScript.CST.DoBind binder' arrow expr')
  Language.PureScript.CST.DoDiscard expr'' -> do
    let span = Span.doStatement doStatement'
    debug log "DoDiscard" doStatement' span
    expr' <- expr log expr''
    pure (Language.PureScript.CST.DoDiscard expr')
  Language.PureScript.CST.DoLet let' letBindings'' -> do
    let span = Span.doStatement doStatement'
    debug log "DoLet" doStatement' span
    letBindings' <- traverse (letBinding log) letBindings''
    pure (Language.PureScript.CST.DoLet let' letBindings')

export ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Export a ->
  IO (Language.PureScript.CST.Export Span.Span)
export log export' = case export' of
  Language.PureScript.CST.ExportClass _ class' name' -> do
    debug log "ExportClass" name' span
    pure (Language.PureScript.CST.ExportClass span class' name')
  Language.PureScript.CST.ExportKind _ kind' name' -> do
    debug log "ExportKind" name' span
    pure (Language.PureScript.CST.ExportKind span kind' name')
  Language.PureScript.CST.ExportModule _ module'' name' -> do
    debug log "ExportModule" name' span
    pure (Language.PureScript.CST.ExportModule span module'' name')
  Language.PureScript.CST.ExportOp _ name' -> do
    debug log "ExportOp" name' span
    pure (Language.PureScript.CST.ExportOp span name')
  Language.PureScript.CST.ExportType _ name' dataMembers'' -> do
    debug log "ExportType" name' span
    dataMembers' <- traverse (dataMembers log) dataMembers''
    pure (Language.PureScript.CST.ExportType span name' dataMembers')
  Language.PureScript.CST.ExportTypeOp _ type'' name' -> do
    debug log "ExportTypeOp" name' span
    pure (Language.PureScript.CST.ExportTypeOp span type'' name')
  Language.PureScript.CST.ExportValue _ name' -> do
    debug log "ExportValue" name' span
    pure (Language.PureScript.CST.ExportValue span name')
  where
  span :: Span.Span
  span = Span.export export'

expr ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Expr a ->
  IO (Language.PureScript.CST.Expr Span.Span)
expr log expr' = case expr' of
  Language.PureScript.CST.ExprAdo _ adoBlock'' -> do
    let span = Span.expr expr'
    debug log "ExprAdo" expr' span
    adoBlock' <- adoBlock log adoBlock''
    pure (Language.PureScript.CST.ExprAdo span adoBlock')
  Language.PureScript.CST.ExprApp _ expr1' expr2' -> do
    let span = Span.expr expr'
    debug log "ExprApp" expr' span
    expr1 <- expr log expr1'
    expr2 <- expr log expr2'
    pure (Language.PureScript.CST.ExprApp span expr1 expr2)
  Language.PureScript.CST.ExprBoolean _ boolean x -> do
    let span = Span.expr expr'
    debug log "ExprBoolean" expr' span
    pure (Language.PureScript.CST.ExprBoolean span boolean x)
  Language.PureScript.CST.ExprChar _ char x -> do
    let span = Span.expr expr'
    debug log "ExprChar" expr' span
    pure (Language.PureScript.CST.ExprChar span char x)
  Language.PureScript.CST.ExprConstructor _ name' -> do
    let span = Span.expr expr'
    debug log "ExprConstructor" expr' span
    pure (Language.PureScript.CST.ExprConstructor span name')
  Language.PureScript.CST.ExprDo _ doBlock'' -> do
    let span = Span.expr expr'
    debug log "ExprDo" expr' span
    doBlock' <- doBlock log doBlock''
    pure (Language.PureScript.CST.ExprDo span doBlock')
  Language.PureScript.CST.ExprHole _ hole -> do
    let span = Span.expr expr'
    debug log "ExprHole" expr' span
    pure (Language.PureScript.CST.ExprHole span hole)
  Language.PureScript.CST.ExprIdent _ name' -> do
    let span = Span.expr expr'
    debug log "ExprIdent" expr' span
    pure (Language.PureScript.CST.ExprIdent span name')
  Language.PureScript.CST.ExprNumber _ number x -> do
    let span = Span.expr expr'
    debug log "ExprNumber" expr' span
    pure (Language.PureScript.CST.ExprNumber span number x)
  Language.PureScript.CST.ExprString _ string x -> do
    let span = Span.expr expr'
    debug log "ExprString" expr' span
    pure (Language.PureScript.CST.ExprString span string x)
  Language.PureScript.CST.ExprOpName _ name' -> do
    let span = Span.expr expr'
    debug log "ExprOpName" expr' span
    pure (Language.PureScript.CST.ExprOpName span name')
  Language.PureScript.CST.ExprOp _ expr1' op expr2' -> do
    let span = Span.expr expr'
    debug log "ExprOp" expr' span
    expr1 <- expr log expr1'
    expr2 <- expr log expr2'
    pure (Language.PureScript.CST.ExprOp span expr1 op expr2)
  Language.PureScript.CST.ExprRecordAccessor _ recordAccessor'' -> do
    let span = Span.expr expr'
    debug log "ExprRecordAccessor" expr' span
    recordAccessor' <- recordAccessor log recordAccessor''
    pure (Language.PureScript.CST.ExprRecordAccessor span recordAccessor')
  _ -> do
    let span = Span.MultipleLines
    notImplemented log "Expr" expr' span
    pure (span <$ expr')

guarded ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Guarded a ->
  IO (Language.PureScript.CST.Guarded Span.Span)
guarded log guarded' = case guarded' of
  Language.PureScript.CST.Guarded guardedExprs' -> do
    let span = Span.guarded guarded'
    debug log "Guarded" guarded' span
    guardedExprs <- traverse (guardedExpr log) guardedExprs'
    pure (Language.PureScript.CST.Guarded guardedExprs)
  Language.PureScript.CST.Unconditional sourceToken' where''' -> do
    let span = Span.guarded guarded'
    debug log "Unconditional" guarded' span
    where'' <- where' log where'''
    pure (Language.PureScript.CST.Unconditional sourceToken' where'')

guardedExpr ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.GuardedExpr a ->
  IO (Language.PureScript.CST.GuardedExpr Span.Span)
guardedExpr log guardedExpr' = case guardedExpr' of
  Language.PureScript.CST.GuardedExpr bar patternGuards' comma where''' -> do
    let span = Span.guardedExpr guardedExpr'
    debug log "GuardedExpr" guardedExpr' span
    patternGuards <- traverse (patternGuard log) patternGuards'
    where'' <- where' log where'''
    pure (Language.PureScript.CST.GuardedExpr bar patternGuards comma where'')

import' ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Import a ->
  IO (Language.PureScript.CST.Import Span.Span)
import' log import'' = case import'' of
  Language.PureScript.CST.ImportClass _ class' name' -> do
    debug log "ImportClass" name' span
    pure (Language.PureScript.CST.ImportClass span class' name')
  Language.PureScript.CST.ImportKind _ kind' name' -> do
    debug log "ImportKind" name' span
    pure (Language.PureScript.CST.ImportKind span kind' name')
  Language.PureScript.CST.ImportOp _ name' -> do
    debug log "ImportOp" name' span
    pure (Language.PureScript.CST.ImportOp span name')
  Language.PureScript.CST.ImportType _ name' dataMembers'' -> do
    debug log "ImportType" name' span
    dataMembers' <- traverse (dataMembers log) dataMembers''
    pure (Language.PureScript.CST.ImportType span name' dataMembers')
  Language.PureScript.CST.ImportTypeOp _ type'' name' -> do
    debug log "ImportTypeOp" name' span
    pure (Language.PureScript.CST.ImportTypeOp span type'' name')
  Language.PureScript.CST.ImportValue _ name' -> do
    debug log "ImportValue" name' span
    pure (Language.PureScript.CST.ImportValue span name')
  where
  span :: Span.Span
  span = Span.import' import''

importDecl ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.ImportDecl a ->
  IO (Language.PureScript.CST.ImportDecl Span.Span)
importDecl log importDecl' = case importDecl' of
  Language.PureScript.CST.ImportDecl _ import'' name' imports' rename -> do
    let span = Span.importDecl importDecl'
    debug log "ImportDecl" importDecl' span
    imports <- (traverse . traverse . traverse . traverse) (import' log) imports'
    pure (Language.PureScript.CST.ImportDecl span import'' name' imports rename)

labeled ::
  (Show a, Show b) =>
  Log.Handle ->
  (a -> Language.PureScript.CST.SourceRange) ->
  (b -> Language.PureScript.CST.SourceRange) ->
  (b -> IO c) ->
  Language.PureScript.CST.Labeled a b ->
  IO (Language.PureScript.CST.Labeled a c)
labeled log f g h labeled' = case labeled' of
  Language.PureScript.CST.Labeled label' separator value' -> do
    let span = Span.labeled f g labeled'
    debug log "Labeled" labeled' span
    value <- h value'
    pure (Language.PureScript.CST.Labeled label' separator value)

labeledNameType ::
  (Show a, Show b) =>
  Log.Handle ->
  Language.PureScript.CST.Labeled
    (Language.PureScript.CST.Name a)
    (Language.PureScript.CST.Type b) ->
  IO
    ( Language.PureScript.CST.Labeled
      (Language.PureScript.CST.Name a)
      (Language.PureScript.CST.Type Span.Span)
    )
labeledNameType log = labeled log SourceRange.name SourceRange.type' (type' log)

letBinding ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.LetBinding a ->
  IO (Language.PureScript.CST.LetBinding Span.Span)
letBinding log letBinding' = case letBinding' of
  Language.PureScript.CST.LetBindingName _ valueBindingFields'' -> do
    let span = Span.letBinding letBinding'
    debug log "LetBindingName" letBinding' span
    valueBindingFields' <- valueBindingFields log valueBindingFields''
    pure (Language.PureScript.CST.LetBindingName span valueBindingFields')
  Language.PureScript.CST.LetBindingPattern _ binder'' equals where''' -> do
    let span = Span.letBinding letBinding'
    debug log "LetBindingPattern" letBinding' span
    binder' <- binder log binder''
    where'' <- where' log where'''
    pure (Language.PureScript.CST.LetBindingPattern span binder' equals where'')
  Language.PureScript.CST.LetBindingSignature _ labeled'' -> do
    let span = Span.letBinding letBinding'
    debug log "LetBindingSignature" letBinding' span
    labeled' <- labeledNameType log labeled''
    pure (Language.PureScript.CST.LetBindingSignature span labeled')

ltraverse ::
  (Data.Bitraversable.Bitraversable t, Applicative f) =>
  (a -> f c) ->
  t a d ->
  f (t c d)
ltraverse f = Data.Bitraversable.bitraverse f pure

module' ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Module a ->
  IO (Language.PureScript.CST.Module Span.Span)
module' log module''' = case module''' of
  Language.PureScript.CST.Module _ module'' name exports' where'' imports' declarations' trailing -> do
    let span = Span.betweenSourceTokens module'' where''
    debug log "Module" module''' span
    exports <- (traverse . traverse . traverse) (export log) exports'
    imports <- traverse (importDecl log) imports'
    declarations <- traverse (declaration log) declarations'
    pure
      ( Language.PureScript.CST.Module
        span
        module''
        name
        exports
        where''
        imports
        declarations
        trailing
      )

notImplemented :: (Show a) => Log.Handle -> Utf8Builder -> a -> Span.Span -> IO ()
notImplemented log x y z =
  Log.info
    log
    ( "Annotating `"
      <> x
      <> "`: "
      <> displayShow y
      <> " not implemented. Defaulting to `"
      <> displayShow z
      <> "`"
    )

patternGuard ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.PatternGuard a ->
  IO (Language.PureScript.CST.PatternGuard Span.Span)
patternGuard log patternGuard' = case patternGuard' of
  Language.PureScript.CST.PatternGuard binder'' expr'' -> do
    let span = Span.patternGuard patternGuard'
    debug log "PatternGuard" patternGuard' span
    binder' <- (traverse . ltraverse) (binder log) binder''
    expr' <- expr log expr''
    pure (Language.PureScript.CST.PatternGuard binder' expr')

recordAccessor ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.RecordAccessor a ->
  IO (Language.PureScript.CST.RecordAccessor Span.Span)
recordAccessor log recordAccessor' = case recordAccessor' of
  Language.PureScript.CST.RecordAccessor expr'' dot path -> do
    let span = Span.recordAccessor recordAccessor'
    debug log "RecordAccessor" recordAccessor' span
    expr' <- expr log expr''
    pure (Language.PureScript.CST.RecordAccessor expr' dot path)

type' ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Type a ->
  IO (Language.PureScript.CST.Type Span.Span)
type' log type'' = case type'' of
  _ -> do
    let span = Span.MultipleLines
    notImplemented log "Type" type'' span
    pure (span <$ type'')

valueBindingFields ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.ValueBindingFields a ->
  IO (Language.PureScript.CST.ValueBindingFields Span.Span)
valueBindingFields log valueBindingFields' = case valueBindingFields' of
  Language.PureScript.CST.ValueBindingFields name' binders' guarded'' -> do
    let span = Span.valueBindingFields valueBindingFields'
    debug log "ValueBindingFields" valueBindingFields' span
    binders <- traverse (binder log) binders'
    guarded' <- guarded log guarded''
    pure (Language.PureScript.CST.ValueBindingFields name' binders guarded')

where' ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Where a ->
  IO (Language.PureScript.CST.Where Span.Span)
where' log where'' = case where'' of
  Language.PureScript.CST.Where expr'' letBindings' -> do
    let span = Span.where' where''
    debug log "Where" where'' span
    letBindings <- (traverse . traverse . traverse) (letBinding log) letBindings'
    expr' <- expr log expr''
    pure (Language.PureScript.CST.Where expr' letBindings)
