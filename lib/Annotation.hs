module Annotation
  ( module'
  ) where

import "rio" RIO hiding (log, span)

import qualified "base" Data.Bitraversable
import qualified "purescript" Language.PureScript.CST.Types
import qualified "this" Log
import qualified "this" SourceRange
import qualified "this" Span

adoBlock ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.AdoBlock a ->
  IO (Language.PureScript.CST.Types.AdoBlock Span.Span)
adoBlock log adoBlock' = case adoBlock' of
  Language.PureScript.CST.Types.AdoBlock ado doStatements' in' expr'' -> do
    let span = Span.adoBlock adoBlock'
    debug log "AdoBlock" adoBlock' span
    doStatements <- traverse (doStatement log) doStatements'
    expr' <- expr log expr''
    pure (Language.PureScript.CST.Types.AdoBlock ado doStatements in' expr')

binder ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Binder a ->
  IO (Language.PureScript.CST.Types.Binder Span.Span)
binder log binder''' = case binder''' of
  Language.PureScript.CST.Types.BinderArray _ delimited'' -> do
    let span = Span.binder binder'''
    debug log "BinderArray" binder''' span
    delimited' <- delimited log (binder log) delimited''
    pure (Language.PureScript.CST.Types.BinderArray span delimited')
  Language.PureScript.CST.Types.BinderBoolean _ boolean x -> do
    let span = Span.binder binder'''
    debug log "BinderBoolean" binder''' span
    pure (Language.PureScript.CST.Types.BinderBoolean span boolean x)
  Language.PureScript.CST.Types.BinderChar _ char x -> do
    let span = Span.binder binder'''
    debug log "BinderChar" binder''' span
    pure (Language.PureScript.CST.Types.BinderChar span char x)
  Language.PureScript.CST.Types.BinderConstructor _ name' binders' -> do
    let span = Span.binder binder'''
    debug log "BinderConstructor" binder''' span
    binders <- traverse (binder log) binders'
    pure (Language.PureScript.CST.Types.BinderConstructor span name' binders)
  Language.PureScript.CST.Types.BinderNamed _ name' at binder'' -> do
    let span = Span.binder binder'''
    debug log "BinderNamed" binder''' span
    binder' <- binder log binder''
    pure (Language.PureScript.CST.Types.BinderNamed span name' at binder')
  Language.PureScript.CST.Types.BinderNumber _ negative number x -> do
    let span = Span.binder binder'''
    debug log "BinderNumber" binder''' span
    pure (Language.PureScript.CST.Types.BinderNumber span negative number x)
  Language.PureScript.CST.Types.BinderOp _ binder1' op binder2' -> do
    let span = Span.binder binder'''
    debug log "BinderOp" binder''' span
    binder1 <- binder log binder1'
    binder2 <- binder log binder2'
    pure (Language.PureScript.CST.Types.BinderOp span binder1 op binder2)
  Language.PureScript.CST.Types.BinderParens _ wrapped'' -> do
    let span = Span.binder binder'''
    debug log "BinderParens" binder''' span
    wrapped' <- wrapped log (binder log) wrapped''
    pure (Language.PureScript.CST.Types.BinderParens span wrapped')
  Language.PureScript.CST.Types.BinderRecord _ delimited'' -> do
    let span = Span.binder binder'''
    debug log "BinderRecord" binder''' span
    delimited' <-
      delimited
        log
        (recordLabeled log SourceRange.binder $ binder log)
        delimited''
    pure (Language.PureScript.CST.Types.BinderRecord span delimited')
  Language.PureScript.CST.Types.BinderString _ string x -> do
    let span = Span.binder binder'''
    debug log "BinderString" binder''' span
    pure (Language.PureScript.CST.Types.BinderString span string x)
  Language.PureScript.CST.Types.BinderTyped _ binder'' typed type''' -> do
    let span = Span.binder binder'''
    debug log "BinderTyped" binder''' span
    binder' <- binder log binder''
    type'' <- type' log type'''
    pure (Language.PureScript.CST.Types.BinderTyped span binder' typed type'')
  Language.PureScript.CST.Types.BinderVar _ var -> do
    let span = Span.binder binder'''
    debug log "BinderVar" binder''' span
    pure (Language.PureScript.CST.Types.BinderVar span var)
  Language.PureScript.CST.Types.BinderWildcard _ wildcard -> do
    let span = Span.binder binder'''
    debug log "BinderWildcard" binder''' span
    pure (Language.PureScript.CST.Types.BinderWildcard span wildcard)

caseOf ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.CaseOf a ->
  IO (Language.PureScript.CST.Types.CaseOf Span.Span)
caseOf log caseOf' = case caseOf' of
  Language.PureScript.CST.Types.CaseOf case' head' of' branches' -> do
    let span = Span.caseOf caseOf'
    debug log "CaseOf" caseOf' span
    head <- traverse (expr log) head'
    branches <-
      traverse
        (Data.Bitraversable.bitraverse (traverse $ binder log) $ guarded log)
        branches'
    pure (Language.PureScript.CST.Types.CaseOf case' head of' branches)

classHead ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.ClassHead a ->
  IO (Language.PureScript.CST.Types.ClassHead Span.Span)
classHead log classHead' = case classHead' of
  Language.PureScript.CST.Types.ClassHead class' super' name' typeVarBindings'' fundeps -> do
    let span = Span.SingleLine
    debug log "ClassHead" classHead' span
    super <- (traverse . ltraverse . traverse) (constraint log) super'
    typeVarBindings' <- traverse (typeVarBinding log) typeVarBindings''
    pure
      ( Language.PureScript.CST.Types.ClassHead
        class'
        super
        name'
        typeVarBindings'
        fundeps
      )

constraint ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Constraint a ->
  IO (Language.PureScript.CST.Types.Constraint Span.Span)
constraint log constraint' = case constraint' of
  Language.PureScript.CST.Types.Constraint _ name' types' -> do
    let span = Span.constraint constraint'
    debug log "Constraint" constraint' span
    types <- traverse (type' log) types'
    pure (Language.PureScript.CST.Types.Constraint span name' types)
  Language.PureScript.CST.Types.ConstraintParens _ wrapped'' -> do
    let span = Span.constraint constraint'
    debug log "ConstraintParens" constraint' span
    wrapped' <- wrapped log (constraint log) wrapped''
    pure (Language.PureScript.CST.Types.ConstraintParens span wrapped')

dataCtor ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.DataCtor a ->
  IO (Language.PureScript.CST.Types.DataCtor Span.Span)
dataCtor log dataCtor' = case dataCtor' of
  Language.PureScript.CST.Types.DataCtor _ name' types' -> do
    let span = Span.dataCtor dataCtor'
    debug log "DataCtor" dataCtor' span
    types <- traverse (type' log) types'
    pure (Language.PureScript.CST.Types.DataCtor span name' types)

dataHead ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.DataHead a ->
  IO (Language.PureScript.CST.Types.DataHead Span.Span)
dataHead log dataHead' = case dataHead' of
  Language.PureScript.CST.Types.DataHead sourceToken' name' typeVarBindings'' -> do
    let span = Span.SingleLine
    debug log "DataHead" dataHead' span
    typeVarBindings' <- traverse (typeVarBinding log) typeVarBindings''
    pure (Language.PureScript.CST.Types.DataHead sourceToken' name' typeVarBindings')

dataMembers ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.DataMembers a ->
  IO (Language.PureScript.CST.Types.DataMembers Span.Span)
dataMembers log dataMembers' = case dataMembers' of
  Language.PureScript.CST.Types.DataAll _ sourceToken' -> do
    let span = Span.SingleLine
    debug log "DataAll" dataMembers' span
    pure (Language.PureScript.CST.Types.DataAll span sourceToken')
  Language.PureScript.CST.Types.DataEnumerated _ delimited' -> do
    let span = Span.dataMembers dataMembers'
    debug log "DataEnumerated" dataMembers' span
    pure (Language.PureScript.CST.Types.DataEnumerated span delimited')

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
  Language.PureScript.CST.Types.Declaration a ->
  IO (Language.PureScript.CST.Types.Declaration Span.Span)
declaration log declaration' = case declaration' of
  Language.PureScript.CST.Types.DeclData _ dataHead'' dataCtors' -> do
    let span = Span.MultipleLines
    debug log "DeclData" declaration' span
    dataHead' <- dataHead log dataHead''
    dataCtors <- (traverse . traverse . traverse) (dataCtor log) dataCtors'
    pure (Language.PureScript.CST.Types.DeclData span dataHead' dataCtors)
  Language.PureScript.CST.Types.DeclClass _ classHead'' body' -> do
    let span = Span.declaration declaration'
    debug log "DeclClass" declaration' span
    classHead' <- classHead log classHead''
    body <- (traverse . traverse . traverse . traverse) (type' log) body'
    pure (Language.PureScript.CST.Types.DeclClass span classHead' body)
  Language.PureScript.CST.Types.DeclDerive _ derive newtype' instanceHead'' -> do
    let span = Span.declaration declaration'
    debug log "DeclDerive" declaration' span
    instanceHead' <- instanceHead log instanceHead''
    pure (Language.PureScript.CST.Types.DeclDerive span derive newtype' instanceHead')
  Language.PureScript.CST.Types.DeclFixity _ fixityFields -> do
    let span = Span.SingleLine
    debug log "DeclFixity" declaration' span
    pure (Language.PureScript.CST.Types.DeclFixity span fixityFields)
  Language.PureScript.CST.Types.DeclForeign _ foreign'' import'' foreign'''' -> do
    let span = Span.declaration declaration'
    debug log "DeclForeign" declaration' span
    foreign''' <- foreign' log foreign''''
    pure (Language.PureScript.CST.Types.DeclForeign span foreign'' import'' foreign''')
  Language.PureScript.CST.Types.DeclInstanceChain _ separated'' -> do
    let span = Span.declaration declaration'
    debug log "DeclInstanceChain" declaration' span
    separated' <- traverse (instance' log) separated''
    pure (Language.PureScript.CST.Types.DeclInstanceChain span separated')
  Language.PureScript.CST.Types.DeclNewtype _ dataHead'' equals name' type''' -> do
    let span = Span.MultipleLines
    debug log "DeclNewtype" declaration' span
    dataHead' <- dataHead log dataHead''
    type'' <- type' log type'''
    pure (Language.PureScript.CST.Types.DeclNewtype span dataHead' equals name' type'')
  Language.PureScript.CST.Types.DeclSignature _ labeled'' -> do
    let span = Span.labeled SourceRange.name SourceRange.type' labeled''
    debug log "DeclSignature" declaration' span
    labeled' <- labeledNameType log labeled''
    pure (Language.PureScript.CST.Types.DeclSignature span labeled')
  Language.PureScript.CST.Types.DeclType _ dataHead'' equals type''' -> do
    let span = Span.MultipleLines
    debug log "DeclType" declaration' span
    dataHead' <- dataHead log dataHead''
    type'' <- type' log type'''
    pure (Language.PureScript.CST.Types.DeclType span dataHead' equals type'')
  Language.PureScript.CST.Types.DeclValue _ valueBindingFields'' -> do
    let span = Span.valueBindingFields valueBindingFields''
    debug log "DeclValue" declaration' span
    valueBindingFields' <- valueBindingFields log valueBindingFields''
    pure (Language.PureScript.CST.Types.DeclValue span valueBindingFields')

delimited ::
  (Show a) =>
  Log.Handle ->
  (a -> IO b) ->
  Language.PureScript.CST.Types.Delimited a ->
  IO (Language.PureScript.CST.Types.Delimited b)
delimited log f =
  wrapped log ((traverse . traverse) f)

delimitedNonEmpty ::
  (Show a) =>
  Log.Handle ->
  (a -> IO b) ->
  Language.PureScript.CST.Types.DelimitedNonEmpty a ->
  IO (Language.PureScript.CST.Types.DelimitedNonEmpty b)
delimitedNonEmpty log f =
  wrapped log (traverse f)

doBlock ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.DoBlock a ->
  IO (Language.PureScript.CST.Types.DoBlock Span.Span)
doBlock log doBlock' = case doBlock' of
  Language.PureScript.CST.Types.DoBlock do' doStatements' -> do
    let span = Span.doBlock doBlock'
    debug log "DoBlock" doBlock' span
    doStatements <- traverse (doStatement log) doStatements'
    pure (Language.PureScript.CST.Types.DoBlock do' doStatements)

doStatement ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.DoStatement a ->
  IO (Language.PureScript.CST.Types.DoStatement Span.Span)
doStatement log doStatement' = case doStatement' of
  Language.PureScript.CST.Types.DoBind binder'' arrow expr'' -> do
    let span = Span.doStatement doStatement'
    debug log "DoBind" doStatement' span
    binder' <- binder log binder''
    expr' <- expr log expr''
    pure (Language.PureScript.CST.Types.DoBind binder' arrow expr')
  Language.PureScript.CST.Types.DoDiscard expr'' -> do
    let span = Span.doStatement doStatement'
    debug log "DoDiscard" doStatement' span
    expr' <- expr log expr''
    pure (Language.PureScript.CST.Types.DoDiscard expr')
  Language.PureScript.CST.Types.DoLet let' letBindings'' -> do
    let span = Span.doStatement doStatement'
    debug log "DoLet" doStatement' span
    letBindings' <- traverse (letBinding log) letBindings''
    pure (Language.PureScript.CST.Types.DoLet let' letBindings')

export ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Export a ->
  IO (Language.PureScript.CST.Types.Export Span.Span)
export log export' = case export' of
  Language.PureScript.CST.Types.ExportClass _ class' name' -> do
    debug log "ExportClass" name' span
    pure (Language.PureScript.CST.Types.ExportClass span class' name')
  Language.PureScript.CST.Types.ExportKind _ kind' name' -> do
    debug log "ExportKind" name' span
    pure (Language.PureScript.CST.Types.ExportKind span kind' name')
  Language.PureScript.CST.Types.ExportModule _ module'' name' -> do
    debug log "ExportModule" name' span
    pure (Language.PureScript.CST.Types.ExportModule span module'' name')
  Language.PureScript.CST.Types.ExportOp _ name' -> do
    debug log "ExportOp" name' span
    pure (Language.PureScript.CST.Types.ExportOp span name')
  Language.PureScript.CST.Types.ExportType _ name' dataMembers'' -> do
    debug log "ExportType" name' span
    dataMembers' <- traverse (dataMembers log) dataMembers''
    pure (Language.PureScript.CST.Types.ExportType span name' dataMembers')
  Language.PureScript.CST.Types.ExportTypeOp _ type'' name' -> do
    debug log "ExportTypeOp" name' span
    pure (Language.PureScript.CST.Types.ExportTypeOp span type'' name')
  Language.PureScript.CST.Types.ExportValue _ name' -> do
    debug log "ExportValue" name' span
    pure (Language.PureScript.CST.Types.ExportValue span name')
  where
  span :: Span.Span
  span = Span.export export'

expr ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Expr a ->
  IO (Language.PureScript.CST.Types.Expr Span.Span)
expr log expr''' = case expr''' of
  Language.PureScript.CST.Types.ExprAdo _ adoBlock'' -> do
    let span = Span.expr expr'''
    debug log "ExprAdo" expr''' span
    adoBlock' <- adoBlock log adoBlock''
    pure (Language.PureScript.CST.Types.ExprAdo span adoBlock')
  Language.PureScript.CST.Types.ExprApp _ expr1' expr2' -> do
    let span = Span.expr expr'''
    debug log "ExprApp" expr''' span
    expr1 <- expr log expr1'
    expr2 <- expr log expr2'
    pure (Language.PureScript.CST.Types.ExprApp span expr1 expr2)
  Language.PureScript.CST.Types.ExprArray _ delimited'' -> do
    let span = Span.expr expr'''
    debug log "ExprArray" expr''' span
    delimited' <- delimited log (expr log) delimited''
    pure (Language.PureScript.CST.Types.ExprArray span delimited')
  Language.PureScript.CST.Types.ExprBoolean _ boolean x -> do
    let span = Span.expr expr'''
    debug log "ExprBoolean" expr''' span
    pure (Language.PureScript.CST.Types.ExprBoolean span boolean x)
  Language.PureScript.CST.Types.ExprCase _ caseOf'' -> do
    let span = Span.expr expr'''
    debug log "ExprCase" expr''' span
    caseOf' <- caseOf log caseOf''
    pure (Language.PureScript.CST.Types.ExprCase span caseOf')
  Language.PureScript.CST.Types.ExprChar _ char x -> do
    let span = Span.expr expr'''
    debug log "ExprChar" expr''' span
    pure (Language.PureScript.CST.Types.ExprChar span char x)
  Language.PureScript.CST.Types.ExprConstructor _ name' -> do
    let span = Span.expr expr'''
    debug log "ExprConstructor" expr''' span
    pure (Language.PureScript.CST.Types.ExprConstructor span name')
  Language.PureScript.CST.Types.ExprDo _ doBlock'' -> do
    let span = Span.expr expr'''
    debug log "ExprDo" expr''' span
    doBlock' <- doBlock log doBlock''
    pure (Language.PureScript.CST.Types.ExprDo span doBlock')
  Language.PureScript.CST.Types.ExprHole _ hole -> do
    let span = Span.expr expr'''
    debug log "ExprHole" expr''' span
    pure (Language.PureScript.CST.Types.ExprHole span hole)
  Language.PureScript.CST.Types.ExprIdent _ name' -> do
    let span = Span.expr expr'''
    debug log "ExprIdent" expr''' span
    pure (Language.PureScript.CST.Types.ExprIdent span name')
  Language.PureScript.CST.Types.ExprIf _ ifThenElse'' -> do
    let span = Span.expr expr'''
    debug log "ExprIf" expr''' span
    ifThenElse' <- ifThenElse log ifThenElse''
    pure (Language.PureScript.CST.Types.ExprIf span ifThenElse')
  Language.PureScript.CST.Types.ExprInfix _ expr1' wrapped'' expr2' -> do
    let span = Span.expr expr'''
    debug log "ExprInfix" expr''' span
    expr1 <- expr log expr1'
    wrapped' <- wrapped log (expr log) wrapped''
    expr2 <- expr log expr2'
    pure (Language.PureScript.CST.Types.ExprInfix span expr1 wrapped' expr2)
  Language.PureScript.CST.Types.ExprLambda _ lambda'' -> do
    let span = Span.expr expr'''
    debug log "ExprLambda" expr''' span
    lambda' <- lambda log lambda''
    pure (Language.PureScript.CST.Types.ExprLambda span lambda')
  Language.PureScript.CST.Types.ExprLet _ letIn'' -> do
    let span = Span.expr expr'''
    debug log "ExprLet" expr''' span
    letIn' <- letIn log letIn''
    pure (Language.PureScript.CST.Types.ExprLet span letIn')
  Language.PureScript.CST.Types.ExprNegate _ negative expr'' -> do
    let span = Span.expr expr'''
    debug log "ExprNegate" expr''' span
    expr' <- expr log expr''
    pure (Language.PureScript.CST.Types.ExprNegate span negative expr')
  Language.PureScript.CST.Types.ExprNumber _ number x -> do
    let span = Span.expr expr'''
    debug log "ExprNumber" expr''' span
    pure (Language.PureScript.CST.Types.ExprNumber span number x)
  Language.PureScript.CST.Types.ExprOp _ expr1' op expr2' -> do
    let span = Span.expr expr'''
    debug log "ExprOp" expr''' span
    expr1 <- expr log expr1'
    expr2 <- expr log expr2'
    pure (Language.PureScript.CST.Types.ExprOp span expr1 op expr2)
  Language.PureScript.CST.Types.ExprOpName _ name' -> do
    let span = Span.expr expr'''
    debug log "ExprOpName" expr''' span
    pure (Language.PureScript.CST.Types.ExprOpName span name')
  Language.PureScript.CST.Types.ExprParens _ wrapped'' -> do
    let span = Span.expr expr'''
    debug log "ExprParens" expr''' span
    wrapped' <- wrapped log (expr log) wrapped''
    pure (Language.PureScript.CST.Types.ExprParens span wrapped')
  Language.PureScript.CST.Types.ExprRecord _ delimited'' -> do
    let span = Span.expr expr'''
    debug log "ExprRecord" expr''' span
    delimited' <-
      delimited
        log
        (recordLabeled log SourceRange.expr $ expr log)
        delimited''
    pure (Language.PureScript.CST.Types.ExprRecord span delimited')
  Language.PureScript.CST.Types.ExprRecordAccessor _ recordAccessor'' -> do
    let span = Span.expr expr'''
    debug log "ExprRecordAccessor" expr''' span
    recordAccessor' <- recordAccessor log recordAccessor''
    pure (Language.PureScript.CST.Types.ExprRecordAccessor span recordAccessor')
  Language.PureScript.CST.Types.ExprRecordUpdate _ expr'' delimitedNonEmpty'' -> do
    let span = Span.expr expr'''
    debug log "ExprRecordUpdate" expr''' span
    expr' <- expr log expr''
    delimitedNonEmpty' <-
      delimitedNonEmpty log (recordUpdate log) delimitedNonEmpty''
    pure (Language.PureScript.CST.Types.ExprRecordUpdate span expr' delimitedNonEmpty')
  Language.PureScript.CST.Types.ExprSection _ section -> do
    let span = Span.expr expr'''
    debug log "ExprSection" expr''' span
    pure (Language.PureScript.CST.Types.ExprSection span section)
  Language.PureScript.CST.Types.ExprString _ string x -> do
    let span = Span.expr expr'''
    debug log "ExprString" expr''' span
    pure (Language.PureScript.CST.Types.ExprString span string x)
  Language.PureScript.CST.Types.ExprTyped _ expr'' op type''' -> do
    let span = Span.expr expr'''
    debug log "ExprTyped" expr''' span
    expr' <- expr log expr''
    type'' <- type' log type'''
    pure (Language.PureScript.CST.Types.ExprTyped span expr' op type'')

foreign' ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Foreign a ->
  IO (Language.PureScript.CST.Types.Foreign Span.Span)
foreign' log foreign'' = case foreign'' of
  Language.PureScript.CST.Types.ForeignData data' labeled'' -> do
    let span = Span.foreign' foreign''
    debug log "ForeignData" foreign'' span
    labeled' <- traverse (kind log) labeled''
    pure (Language.PureScript.CST.Types.ForeignData data' labeled')
  Language.PureScript.CST.Types.ForeignKind kind' name' -> do
    let span = Span.foreign' foreign''
    debug log "ForeignKind" foreign'' span
    pure (Language.PureScript.CST.Types.ForeignKind kind' name')
  Language.PureScript.CST.Types.ForeignValue labeled'' -> do
    let span = Span.foreign' foreign''
    debug log "ForeignValue" foreign'' span
    labeled' <- traverse (type' log) labeled''
    pure (Language.PureScript.CST.Types.ForeignValue labeled')

guarded ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Guarded a ->
  IO (Language.PureScript.CST.Types.Guarded Span.Span)
guarded log guarded' = case guarded' of
  Language.PureScript.CST.Types.Guarded guardedExprs' -> do
    let span = Span.guarded guarded'
    debug log "Guarded" guarded' span
    guardedExprs <- traverse (guardedExpr log) guardedExprs'
    pure (Language.PureScript.CST.Types.Guarded guardedExprs)
  Language.PureScript.CST.Types.Unconditional sourceToken' where''' -> do
    let span = Span.guarded guarded'
    debug log "Unconditional" guarded' span
    where'' <- where' log where'''
    pure (Language.PureScript.CST.Types.Unconditional sourceToken' where'')

guardedExpr ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.GuardedExpr a ->
  IO (Language.PureScript.CST.Types.GuardedExpr Span.Span)
guardedExpr log guardedExpr' = case guardedExpr' of
  Language.PureScript.CST.Types.GuardedExpr bar patternGuards' comma where''' -> do
    let span = Span.guardedExpr guardedExpr'
    debug log "GuardedExpr" guardedExpr' span
    patternGuards <- traverse (patternGuard log) patternGuards'
    where'' <- where' log where'''
    pure (Language.PureScript.CST.Types.GuardedExpr bar patternGuards comma where'')

ifThenElse ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.IfThenElse a ->
  IO (Language.PureScript.CST.Types.IfThenElse Span.Span)
ifThenElse log ifThenElse' = case ifThenElse' of
  Language.PureScript.CST.Types.IfThenElse if' cond' then' true' else' false' -> do
    let span = Span.ifThenElse ifThenElse'
    debug log "IfThenElse" ifThenElse' span
    cond <- expr log cond'
    true <- expr log true'
    false <- expr log false'
    pure (Language.PureScript.CST.Types.IfThenElse if' cond then' true else' false)

import' ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Import a ->
  IO (Language.PureScript.CST.Types.Import Span.Span)
import' log import'' = case import'' of
  Language.PureScript.CST.Types.ImportClass _ class' name' -> do
    debug log "ImportClass" name' span
    pure (Language.PureScript.CST.Types.ImportClass span class' name')
  Language.PureScript.CST.Types.ImportKind _ kind' name' -> do
    debug log "ImportKind" name' span
    pure (Language.PureScript.CST.Types.ImportKind span kind' name')
  Language.PureScript.CST.Types.ImportOp _ name' -> do
    debug log "ImportOp" name' span
    pure (Language.PureScript.CST.Types.ImportOp span name')
  Language.PureScript.CST.Types.ImportType _ name' dataMembers'' -> do
    debug log "ImportType" name' span
    dataMembers' <- traverse (dataMembers log) dataMembers''
    pure (Language.PureScript.CST.Types.ImportType span name' dataMembers')
  Language.PureScript.CST.Types.ImportTypeOp _ type'' name' -> do
    debug log "ImportTypeOp" name' span
    pure (Language.PureScript.CST.Types.ImportTypeOp span type'' name')
  Language.PureScript.CST.Types.ImportValue _ name' -> do
    debug log "ImportValue" name' span
    pure (Language.PureScript.CST.Types.ImportValue span name')
  where
  span :: Span.Span
  span = Span.import' import''

importDecl ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.ImportDecl a ->
  IO (Language.PureScript.CST.Types.ImportDecl Span.Span)
importDecl log importDecl' = case importDecl' of
  Language.PureScript.CST.Types.ImportDecl _ import'' name' imports' rename -> do
    let span = Span.importDecl importDecl'
    debug log "ImportDecl" importDecl' span
    imports <- (traverse . traverse . traverse . traverse) (import' log) imports'
    pure (Language.PureScript.CST.Types.ImportDecl span import'' name' imports rename)

instance' ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Instance a ->
  IO (Language.PureScript.CST.Types.Instance Span.Span)
instance' log instance'' = case instance'' of
  Language.PureScript.CST.Types.Instance instanceHead'' body' -> do
    let span = Span.instance' instance''
    debug log "Instance" instance'' span
    instanceHead' <- instanceHead log instanceHead''
    body <- (traverse . traverse . traverse) (instanceBinding log) body'
    pure (Language.PureScript.CST.Types.Instance instanceHead' body)

instanceBinding ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.InstanceBinding a ->
  IO (Language.PureScript.CST.Types.InstanceBinding Span.Span)
instanceBinding log instanceBinding' = case instanceBinding' of
  Language.PureScript.CST.Types.InstanceBindingName _ valueBindingFields'' -> do
    let span = Span.instanceBinding instanceBinding'
    debug log "InstanceBindingName" instanceBinding' span
    valueBindingFields' <- valueBindingFields log valueBindingFields''
    pure ( Language.PureScript.CST.Types.InstanceBindingName span valueBindingFields')
  Language.PureScript.CST.Types.InstanceBindingSignature _ labeled'' -> do
    let span = Span.instanceBinding instanceBinding'
    debug log "InstanceBindingSignature" instanceBinding' span
    labeled' <- labeledNameType log labeled''
    pure ( Language.PureScript.CST.Types.InstanceBindingSignature span labeled')

instanceHead ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.InstanceHead a ->
  IO (Language.PureScript.CST.Types.InstanceHead Span.Span)
instanceHead log instanceHead' = case instanceHead' of
  Language.PureScript.CST.Types.InstanceHead instance'' name' colons constraints' class' types' -> do
    let span = Span.instanceHead instanceHead'
    debug log "InstanceHead" instanceHead' span
    constraints <-
      (traverse . ltraverse . traverse) (constraint log) constraints'
    types <- traverse (type' log) types'
    pure
      ( Language.PureScript.CST.Types.InstanceHead
        instance''
        name'
        colons
        constraints
        class'
        types
      )

kind ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Kind a ->
  IO (Language.PureScript.CST.Types.Kind Span.Span)
kind log kind''' = case kind''' of
  Language.PureScript.CST.Types.KindArr _ kind1' arrow kind2' -> do
    let span = Span.kind kind'''
    debug log "KindArr" kind''' span
    kind1 <- kind log kind1'
    kind2 <- kind log kind2'
    pure (Language.PureScript.CST.Types.KindArr span kind1 arrow kind2)
  Language.PureScript.CST.Types.KindName _ name' -> do
    let span = Span.kind kind'''
    debug log "KindName" kind''' span
    pure (Language.PureScript.CST.Types.KindName span name')
  Language.PureScript.CST.Types.KindParens _ wrapped'' -> do
    let span = Span.kind kind'''
    debug log "KindParens" kind''' span
    wrapped' <- wrapped log (kind log) wrapped''
    pure (Language.PureScript.CST.Types.KindParens span wrapped')
  Language.PureScript.CST.Types.KindRow _ sourceToken kind'' -> do
    let span = Span.kind kind'''
    debug log "KindRow" kind''' span
    kind' <- kind log kind''
    pure (Language.PureScript.CST.Types.KindRow span sourceToken kind')

labeled ::
  (Show a, Show b) =>
  Log.Handle ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (b -> Language.PureScript.CST.Types.SourceRange) ->
  (b -> IO c) ->
  Language.PureScript.CST.Types.Labeled a b ->
  IO (Language.PureScript.CST.Types.Labeled a c)
labeled log f g h labeled' = case labeled' of
  Language.PureScript.CST.Types.Labeled label' separator value' -> do
    let span = Span.labeled f g labeled'
    debug log "Labeled" labeled' span
    value <- h value'
    pure (Language.PureScript.CST.Types.Labeled label' separator value)

labeledNameKind ::
  (Show a, Show b) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Labeled
    (Language.PureScript.CST.Types.Name a)
    (Language.PureScript.CST.Types.Kind b) ->
  IO
    ( Language.PureScript.CST.Types.Labeled
      (Language.PureScript.CST.Types.Name a)
      (Language.PureScript.CST.Types.Kind Span.Span)
    )
labeledNameKind log = labeled log SourceRange.name SourceRange.kind (kind log)

labeledNameType ::
  (Show a, Show b) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Labeled
    (Language.PureScript.CST.Types.Name a)
    (Language.PureScript.CST.Types.Type b) ->
  IO
    ( Language.PureScript.CST.Types.Labeled
      (Language.PureScript.CST.Types.Name a)
      (Language.PureScript.CST.Types.Type Span.Span)
    )
labeledNameType log = labeled log SourceRange.name SourceRange.type' (type' log)

lambda ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Lambda a ->
  IO (Language.PureScript.CST.Types.Lambda Span.Span)
lambda log lambda' = case lambda' of
  Language.PureScript.CST.Types.Lambda reverseSolidus binders' arrow expr'' -> do
    let span = Span.lambda lambda'
    debug log "Lambda" lambda' span
    binders <- traverse (binder log) binders'
    expr' <- expr log expr''
    pure (Language.PureScript.CST.Types.Lambda reverseSolidus binders arrow expr')

letBinding ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.LetBinding a ->
  IO (Language.PureScript.CST.Types.LetBinding Span.Span)
letBinding log letBinding' = case letBinding' of
  Language.PureScript.CST.Types.LetBindingName _ valueBindingFields'' -> do
    let span = Span.letBinding letBinding'
    debug log "LetBindingName" letBinding' span
    valueBindingFields' <- valueBindingFields log valueBindingFields''
    pure (Language.PureScript.CST.Types.LetBindingName span valueBindingFields')
  Language.PureScript.CST.Types.LetBindingPattern _ binder'' equals where''' -> do
    let span = Span.letBinding letBinding'
    debug log "LetBindingPattern" letBinding' span
    binder' <- binder log binder''
    where'' <- where' log where'''
    pure (Language.PureScript.CST.Types.LetBindingPattern span binder' equals where'')
  Language.PureScript.CST.Types.LetBindingSignature _ labeled'' -> do
    let span = Span.letBinding letBinding'
    debug log "LetBindingSignature" letBinding' span
    labeled' <- labeledNameType log labeled''
    pure (Language.PureScript.CST.Types.LetBindingSignature span labeled')

letIn ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.LetIn a ->
  IO (Language.PureScript.CST.Types.LetIn Span.Span)
letIn log letIn' = case letIn' of
  Language.PureScript.CST.Types.LetIn let' bindings' in' expr'' -> do
    let span = Span.letIn letIn'
    debug log "LetIn" letIn' span
    expr' <- expr log expr''
    bindings <- traverse (letBinding log) bindings'
    pure (Language.PureScript.CST.Types.LetIn let' bindings in' expr')

ltraverse ::
  (Data.Bitraversable.Bitraversable t, Applicative f) =>
  (a -> f c) ->
  t a d ->
  f (t c d)
ltraverse f = Data.Bitraversable.bitraverse f pure

module' ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Module a ->
  IO (Language.PureScript.CST.Types.Module Span.Span)
module' log module''' = case module''' of
  Language.PureScript.CST.Types.Module _ module'' name exports' where'' imports' declarations' trailing -> do
    let span = Span.betweenSourceTokens module'' where''
    debug log "Module" module''' span
    exports <- (traverse . traverse . traverse) (export log) exports'
    imports <- traverse (importDecl log) imports'
    declarations <- traverse (declaration log) declarations'
    pure
      ( Language.PureScript.CST.Types.Module
        span
        module''
        name
        exports
        where''
        imports
        declarations
        trailing
      )

patternGuard ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.PatternGuard a ->
  IO (Language.PureScript.CST.Types.PatternGuard Span.Span)
patternGuard log patternGuard' = case patternGuard' of
  Language.PureScript.CST.Types.PatternGuard binder'' expr'' -> do
    let span = Span.patternGuard patternGuard'
    debug log "PatternGuard" patternGuard' span
    binder' <- (traverse . ltraverse) (binder log) binder''
    expr' <- expr log expr''
    pure (Language.PureScript.CST.Types.PatternGuard binder' expr')

recordAccessor ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.RecordAccessor a ->
  IO (Language.PureScript.CST.Types.RecordAccessor Span.Span)
recordAccessor log recordAccessor' = case recordAccessor' of
  Language.PureScript.CST.Types.RecordAccessor expr'' dot path -> do
    let span = Span.recordAccessor recordAccessor'
    debug log "RecordAccessor" recordAccessor' span
    expr' <- expr log expr''
    pure (Language.PureScript.CST.Types.RecordAccessor expr' dot path)

recordLabeled ::
  (Show a) =>
  Log.Handle ->
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (a -> IO b) ->
  Language.PureScript.CST.Types.RecordLabeled a ->
  IO (Language.PureScript.CST.Types.RecordLabeled b)
recordLabeled log f g recordLabeled' = case recordLabeled' of
  Language.PureScript.CST.Types.RecordPun name' -> do
    let span = Span.recordLabeled f recordLabeled'
    debug log "RecordPun" recordLabeled' span
    pure (Language.PureScript.CST.Types.RecordPun name')
  Language.PureScript.CST.Types.RecordField label' colon a -> do
    let span = Span.recordLabeled f recordLabeled'
    debug log "RecordField" recordLabeled' span
    b <- g a
    pure (Language.PureScript.CST.Types.RecordField label' colon b)

recordUpdate ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.RecordUpdate a ->
  IO (Language.PureScript.CST.Types.RecordUpdate Span.Span)
recordUpdate log recordUpdate' = case recordUpdate' of
  Language.PureScript.CST.Types.RecordUpdateBranch label' delimitedNonEmpty'' -> do
    let span = Span.recordUpdate recordUpdate'
    debug log "RecordPun" recordUpdate' span
    delimitedNonEmpty' <-
      delimitedNonEmpty log (recordUpdate log) delimitedNonEmpty''
    pure (Language.PureScript.CST.Types.RecordUpdateBranch label' delimitedNonEmpty')
  Language.PureScript.CST.Types.RecordUpdateLeaf label' equals expr'' -> do
    let span = Span.recordUpdate recordUpdate'
    debug log "RecordField" recordUpdate' span
    expr' <- expr log expr''
    pure (Language.PureScript.CST.Types.RecordUpdateLeaf label' equals expr')

row ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Row a ->
  IO (Language.PureScript.CST.Types.Row Span.Span)
row log row' = case row' of
  Language.PureScript.CST.Types.Row labels' tail' -> do
    let span = Span.row row'
    debug log "Row" row' span
    labels <- (traverse . traverse . traverse) (type' log) labels'
    tail <- (traverse . traverse) (type' log) tail'
    pure (Language.PureScript.CST.Types.Row labels tail)

type' ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Type a ->
  IO (Language.PureScript.CST.Types.Type Span.Span)
type' log type'''' = case type'''' of
  Language.PureScript.CST.Types.TypeApp _ type1' type2' -> do
    let span = Span.type' type''''
    debug log "TypeApp" type'''' span
    type1 <- type' log type1'
    type2 <- type' log type2'
    pure (Language.PureScript.CST.Types.TypeApp span type1 type2)
  Language.PureScript.CST.Types.TypeArr _ type1' arrow type2' -> do
    let span = Span.type' type''''
    debug log "TypeArr" type'''' span
    type1 <- type' log type1'
    type2 <- type' log type2'
    pure (Language.PureScript.CST.Types.TypeArr span type1 arrow type2)
  Language.PureScript.CST.Types.TypeArrName _ arrow -> do
    let span = Span.type' type''''
    debug log "TypeArrName" type'''' span
    pure (Language.PureScript.CST.Types.TypeArrName span arrow)
  Language.PureScript.CST.Types.TypeConstrained _ constraint'' arrow type''' -> do
    let span = Span.type' type''''
    debug log "TypeConstrained" type'''' span
    constraint' <- constraint log constraint''
    type'' <- type' log type'''
    pure (Language.PureScript.CST.Types.TypeConstrained span constraint' arrow type'')
  Language.PureScript.CST.Types.TypeConstructor _ name' -> do
    let span = Span.type' type''''
    debug log "TypeConstructor" type'''' span
    pure (Language.PureScript.CST.Types.TypeConstructor span name')
  Language.PureScript.CST.Types.TypeForall _ forall' typeVarBindings' dot type''' -> do
    let span = Span.type' type''''
    debug log "TypeForall" type'''' span
    typeVarBindings <- traverse (typeVarBinding log) typeVarBindings'
    type'' <- type' log type'''
    pure (Language.PureScript.CST.Types.TypeForall span forall' typeVarBindings dot type'')
  Language.PureScript.CST.Types.TypeHole _ hole -> do
    let span = Span.type' type''''
    debug log "TypeHole" type'''' span
    pure (Language.PureScript.CST.Types.TypeHole span hole)
  Language.PureScript.CST.Types.TypeKinded _ type''' colons kind'' -> do
    let span = Span.type' type''''
    debug log "TypeKinded" type'''' span
    type'' <- type' log type'''
    kind' <- kind log kind''
    pure (Language.PureScript.CST.Types.TypeKinded span type'' colons kind')
  Language.PureScript.CST.Types.TypeOp _ type1' op type2' -> do
    let span = Span.type' type''''
    debug log "TypeOp" type'''' span
    type1 <- type' log type1'
    type2 <- type' log type2'
    pure (Language.PureScript.CST.Types.TypeOp span type1 op type2)
  Language.PureScript.CST.Types.TypeOpName _ op -> do
    let span = Span.type' type''''
    debug log "TypeOpName" type'''' span
    pure (Language.PureScript.CST.Types.TypeOpName span op)
  Language.PureScript.CST.Types.TypeParens _ wrapped'' -> do
    let span = Span.type' type''''
    debug log "TypeParens" type'''' span
    wrapped' <- wrapped log (type' log) wrapped''
    pure (Language.PureScript.CST.Types.TypeParens span wrapped')
  Language.PureScript.CST.Types.TypeRecord _ wrapped'' -> do
    let span = Span.type' type''''
    debug log "TypeRecord" type'''' span
    wrapped' <- wrapped log (row log) wrapped''
    pure (Language.PureScript.CST.Types.TypeRecord span wrapped')
  Language.PureScript.CST.Types.TypeRow _ wrapped'' -> do
    let span = Span.type' type''''
    debug log "TypeRow" type'''' span
    wrapped' <- wrapped log (row log) wrapped''
    pure (Language.PureScript.CST.Types.TypeRow span wrapped')
  Language.PureScript.CST.Types.TypeString _ sourceToken' string -> do
    let span = Span.type' type''''
    debug log "TypeString" type'''' span
    pure (Language.PureScript.CST.Types.TypeString span sourceToken' string)
  Language.PureScript.CST.Types.TypeVar _ var -> do
    let span = Span.type' type''''
    debug log "TypeVar" type'''' span
    pure (Language.PureScript.CST.Types.TypeVar span var)
  Language.PureScript.CST.Types.TypeWildcard _ wildcard -> do
    let span = Span.type' type''''
    debug log "TypeWildcard" type'''' span
    pure (Language.PureScript.CST.Types.TypeWildcard span wildcard)

typeVarBinding ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.TypeVarBinding a ->
  IO (Language.PureScript.CST.Types.TypeVarBinding Span.Span)
typeVarBinding log typeVarBinding' = case typeVarBinding' of
  Language.PureScript.CST.Types.TypeVarKinded wrapped'' -> do
    let span = Span.typeVarBinding typeVarBinding'
    debug log "TypeVarKinded" typeVarBinding' span
    wrapped' <- wrapped log (labeledNameKind log) wrapped''
    pure (Language.PureScript.CST.Types.TypeVarKinded wrapped')
  Language.PureScript.CST.Types.TypeVarName name' -> do
    let span = Span.typeVarBinding typeVarBinding'
    debug log "TypeVarName" typeVarBinding' span
    pure (Language.PureScript.CST.Types.TypeVarName name')

valueBindingFields ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.ValueBindingFields a ->
  IO (Language.PureScript.CST.Types.ValueBindingFields Span.Span)
valueBindingFields log valueBindingFields' = case valueBindingFields' of
  Language.PureScript.CST.Types.ValueBindingFields name' binders' guarded'' -> do
    let span = Span.valueBindingFields valueBindingFields'
    debug log "ValueBindingFields" valueBindingFields' span
    binders <- traverse (binder log) binders'
    guarded' <- guarded log guarded''
    pure (Language.PureScript.CST.Types.ValueBindingFields name' binders guarded')

where' ::
  (Show a) =>
  Log.Handle ->
  Language.PureScript.CST.Types.Where a ->
  IO (Language.PureScript.CST.Types.Where Span.Span)
where' log where'' = case where'' of
  Language.PureScript.CST.Types.Where expr'' letBindings' -> do
    let span = Span.where' where''
    debug log "Where" where'' span
    letBindings <- (traverse . traverse . traverse) (letBinding log) letBindings'
    expr' <- expr log expr''
    pure (Language.PureScript.CST.Types.Where expr' letBindings)

wrapped ::
  (Show a) =>
  Log.Handle ->
  (a -> IO b) ->
  Language.PureScript.CST.Types.Wrapped a ->
  IO (Language.PureScript.CST.Types.Wrapped b)
wrapped log f wrapped' = case wrapped' of
  Language.PureScript.CST.Types.Wrapped open a close -> do
    debug log "Wrapped" wrapped' (Span.wrapped wrapped')
    b <- f a
    pure (Language.PureScript.CST.Types.Wrapped open b close)
