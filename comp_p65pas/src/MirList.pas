unit MirList;
{Unidad con la definiicón del objeto MIR. A diferencia del árbol de sintaxis esta
representación intermedia es lineal.
                                          Por Tito Hinostroza 21/10/2023.}
{$mode ObjFPC}{$H+}
{$Define DEBUGMODE}   //Enable MIR visualization
interface
uses
  Classes, SysUtils, fgl, AstElemP65;
type  //MIR base class
  //MIR Element type
  TMirType = (
    //Declarations
     mtyVarDec      //Variable declaration
    ,mtyConDec      //Constant declaration
    ,mtyFunDec      //Function declaration
    //Instructions
    ,mtyAsgnSim     //Assignment from simple operand
    ,mtyAsgnFun     //Assignment from function
    ,mtyFunCall     //Procedure or Function call
    ,mtyIfJump      //Conditional jump
    );

  { TMirElement }
  TMirElement = Class;
  TMirElements = specialize TFPGObjectList<TMirElement>;
  TMirElement = Class
    mirType: TMirType;
    text   : String;  //Label to show
  end;

type  //MIR declarations
  { TMirVarDec }
  TMirVarDec = Class(TMirElement)
    typ      : TEleTypeDec; //Variable type.
    vardec   : TEleVarDec;  //AST Declared variable, when it's associated to AST. If not it's NIL.
  public  //Campos para guardar las direcciones físicas asignadas en RAM.
    allocated: boolean;    //Activated when variable is allocated (RAM or register).
    storage  : TStorage;   //Depend on adicPar.hasAdic.
    addr     : word;       //Base address.
    function addrL: word; inline;  //Devuelve la dirección absoluta de la variable (LOW)
    function addrH: word; inline;  //Devuelve la dirección absoluta de la variable (HIGH)
    function addrE: word; inline;  //Devuelve la dirección absoluta de la variable (EXTRA)
    function addrU: word; inline;  //Devuelve la dirección absoluta de la variable (ULTRA)
    function AddrString: string;   //Devuelve la dirección física como cadena
    procedure ResetAddress; //Limpia las direcciones físicas
    function stoStr: string;
  public
    constructor Create; virtual;
  end;

  { TMirConDec }
  TMirConDec = Class(TMirElement)
  public
    condec : TEleConsDec;  //AST Declared variable.
    value  : TConsValue;   //Constant value.
  public
    constructor Create; virtual;
  end;

  TMirParam = object
//    name    : string;      //Parameter name
//    typ     : TEleTypeDec; //Reference to type
    vardec  : TMirVarDec;  //Reference to variable used for this parameter
//    srcPos  : TSrcPos;     //Parameter location.
//    adicVar : TAdicVarDec; //Aditional option for "vardec".
  end;
  TMirParamArray = array of TMirParam;

  { TMirFunDec }
  TMirFunDec = Class(TMirElement)
    pars     : TMirParamArray; //Reference to paramenters.
    astFunDec: TEleFunDec;     //AST function.
    //binOper  : char;         //Binary operator when it's associated to an operator.
    items    : TMirElements;   //Instruction list.
    ndecs    : Integer;        //Number of declarations in items list.
    procedure ReadParamsFromAST(astFunDec0: TEleFunDec);
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type  //MIR elements for expressions

  { TMirOperand }

  TMirOperand = object
    Text    : string;        //Label for the operand.
    opType  : TopType;       //Operand type (otVariab, otConst, otFunct) like AST elements.
    varDec  : TMirVarDec;    //Ref. to var declaration, when it's variable. Otherwise it' will be NIL.
    conDec  : TMirConDec;    //Ref. to constant declaration.
    consType: TConsType;     //Constant type.
    value   : TConsValue;    //Constant value.
    astOperand: TEleExpress; //Ref. to AST element. Should be used only for error location.
  end;

  TMirFunction = object
    Text    : string;        //Label for the operand.
    funDec  : TMirFunDec;    //Reference to function declaration, when it's accesible.
    astOperand: TEleExpress; //Ref. to AST element. Should be used only for error location.
  end;

type  //MIR instructions
  { TMirAsgn }
  TMirAsgn = Class(TMirElement)
    dest  : TMirOperand;         //Target variable.
    procedure SetDestFromVarDec(vardec: TMirVarDec);
  end;

  { TMirAsgnSim }
  TMirAsgnSim = Class(TMirAsgn)
    opSrc : TMirOperand;         //Source operand.
    constructor Create; virtual;
  end;

  { TMirAsgnFun }
  TMirAsgnFun = Class(TMirAsgn)
    func  : TMirFunction;         //Source function.
    pars  : array of TMirOperand; //Parameters.
    procedure UpdateText;         //Updates "Text" attribute.
    procedure SetParAsVar(i: Integer; vardec: TMirVarDec);
    constructor Create; virtual;
  end;

  { TMirFunCall }
  TMirFunCall = Class(TMirElement)
    func  : TMirFunction;         //Function called.
    pars  : array of TMirOperand; //Parameter list.
    procedure UpdateText;         //Updates "Text" attribute.
    constructor Create; virtual;
  end;

type  //Main Container
  { TMirList }
  TMirList = class
    items  : TMirElements;
    ndecs  : Integer;         //Number of declarations.
  public  //Adding declarations
    function AddVarDec(mcont: TMirFunDec; varDec0: TEleVarDec): TMirVarDec;
    function AddVarDec(mcont: TMirFunDec; varName: string; eleTyp: TEleTypeDec
      ): TMirVarDec;
    function AddFunDecSNF(funcName0: TEleFunDec): TMirFunDec;
    function AddFunDecUNF(funcName0: TEleFunDec): TMirFunDec;
  public  //Adding instructions
    function AddAssignSim(mcont: TMirFunDec; vardec: TMirVarDec; Op2: TEleExpress
      ): TMirAsgnSim;
    function NewAssignFun(mcont: TMirFunDec; vardec: TMirVarDec; Op2: TEleExpress
      ): TMirAsgnFun;
    function AddAssignFun(mcont: TMirFunDec; const vdec: TMirVarDec;
      Op2: TEleExpress): TMirAsgnFun;

    function NewFunCall(mcont: TMirFunDec; Op1: TEleExpress): TMirFunCall;
    function AddFunCall(mcont: TMirFunDec; Op1: TEleExpress): TMirFunCall;
  public  //Reading from AST
    procedure ConvertBody(cntFunct: TMirFunDec; sntBlock: TEleCodeCont);
  public  //Initialization
    procedure Clear;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TMirVarDec }
function TMirVarDec.addrL: word;
{Dirección absoluta de la variable de menor pero, cuando es de tipo WORD.}
begin
  Result := addr;
end;
function TMirVarDec.addrH: word;
{Dirección absoluta de la variable de mayor pero, cuando es de tipo WORD.}
begin
  Result := addr + 1;
end;
function TMirVarDec.addrE: word;
begin
  Result := addr + 2;
end;
function TMirVarDec.addrU: word;
begin
  Result := addr + 3;
end;
function TMirVarDec.AddrString: string;
{Devuelve una cadena, que representa a la dirección física.}
begin
  if vardec.typ.IsByteSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else if vardec.typ.IsWordSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else if vardec.typ.IsDWordSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else begin
    Result := '';   //Error
  end;
end;
procedure TMirVarDec.ResetAddress;
begin
  addr := 0;
end;
function TMirVarDec.stoStr: string;
begin
  WriteStr(Result, storage);
end;
constructor TMirVarDec.Create;
begin
  mirType := mtyVarDec;
end;
{ TMirConDec }
constructor TMirConDec.Create;
begin
  mirType := mtyConDec;
end;
procedure TMirFunDec.ReadParamsFromAST(astFunDec0: TEleFunDec);
{Read parameters from an AST function declaration.}
var
  i: Integer;
begin
  //Add parameteres
  SetLength(pars, length(astFunDec0.pars));
  for i:=0 to High(astFunDec0.pars) do begin
    pars[i].vardec := TMirVarDec(astFunDec0.pars[i].vardec.mirVarDec);

  end;
end;
{ TMirFunDec }
constructor TMirFunDec.Create;
begin
  mirType := mtyFunDec;
  items:= TMirElements.Create(true);
end;
destructor TMirFunDec.Destroy;
begin
  items.Destroy;
  inherited Destroy;
end;
{ TMirAsgn }
procedure TMirAsgn.SetDestFromVarDec(vardec: TMirVarDec);
{Set "dest" attribute from a Mir Variable declaration. It's equivalent to create a new
MIR Operand (from a MIR variable declaration) and set "dest" to that new operand.
That's why this operand doesn't have an AST reference.}
begin
  dest.Text := vardec.text;
  dest.opType := otVariab;
  dest.varDec := vardec;
  dest.astOperand := nil;
end;
{ TMirAsgnSim }
constructor TMirAsgnSim.Create;
begin
  mirType := mtyAsgnSim;
end;
{ TMirAsgnFun }
procedure TMirAsgnFun.UpdateText;
{Set the "Text" attribute from the content of the instruction.}
var
  i: Integer;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  Text := dest.Text + ' := ' + func.Text;
  for i:=0 to High(pars) do begin
    //Agrega nombre de parámetro
      if i=0 then Text += '(' + pars[i].Text
      else        Text += ',' + pars[i].Text;
  end;
  Text += ')';
  {$ENDIF}
end;
procedure TMirAsgnFun.SetParAsVar(i: Integer; vardec: TMirVarDec);
{Set a parameter like a variable}
var
  par: ^TMirOperand;
begin
  par := @pars[i];
  //Convert "par1" to the temporal variable
  par^.Text   := vardec.text;
  par^.opType := otVariab;
  par^.varDec := vardec;
  par^.astOperand := nil;
end;
constructor TMirAsgnFun.Create;
begin
  mirType := mtyAsgnFun;
end;
procedure TMirFunCall.UpdateText;
var
  i: Integer;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  Text := func.Text;
  for i:=0 to High(pars) do begin
    //Agrega nombre de parámetro
      if i=0 then Text += '(' + pars[i].Text
      else        Text += ',' + pars[i].Text;
  end;
  Text += ')';
  {$ENDIF}
end;
{ TMirFunCall }
constructor TMirFunCall.Create;
begin
  mirType := mtyFunCall;
end;
{ TMirList }
function TMirList.AddVarDec(mcont: TMirFunDec; varDec0: TEleVarDec): TMirVarDec;
{Add a Variable  declaration}
begin
  Result := TMirVarDec.Create;
  Result.text := varDec0.name;
  Result.typ  := varDec0.typ;
  Result.vardec := varDec0;
  if mcont = nil then begin   //In main container
    items.Add(Result);
    inc(ndecs);
  end else begin              //In function container.
    mcont.items.Add(Result);
    inc(mcont.ndecs);
  end;
end;
function TMirList.AddVarDec(mcont: TMirFunDec;
    varName: string; eleTyp: TEleTypeDec): TMirVarDec;
{Add a variable declaration to the container "fdest". The declaration is created
after the last declaration.}
var
  n: Integer;
begin
  //Create unique name
  if mcont=nil then n := ndecs else n := mcont.ndecs;
  if varName='' then varName := '#' + IntToStr(n);

  Result := TMirVarDec.Create;
  Result.text := varName;
  Result.typ  := eleTyp;
  if mcont = nil then begin   //In main container
    items.Insert(n, Result);
    inc(ndecs);
  end else begin              //In function container.
    mcont.items.Insert(n, Result);
    inc(mcont.ndecs);
  end;
end;
function TMirList.AddFunDecSNF(funcName0: TEleFunDec): TMirFunDec;
begin
  Result := TMirFunDec.Create;
  Result.text := funcName0.name;
  Result.astFunDec := funcName0;
  items.Add(Result);
end;
function TMirList.AddFunDecUNF(funcName0: TEleFunDec): TMirFunDec;
{Add a User Normal Function to the MIR list.}
begin
  Result := TMirFunDec.Create;
  Result.text := funcName0.name;
  Result.astFunDec := funcName0;
  items.Add(Result);
end;
procedure GetMIROperandFromASTExpress(out MirOper: TMirOperand;
                                      const AstOper: TEleExpress);
{Read data from a TEleExpress and set a TMirOperand}
begin
  MirOper.opType := AstOper.opType;  //Must be set
  MirOper.Text := AstOper.name;
  if AstOper.opType = otConst then begin
     MirOper.consType := AstOper.consType;  //Copy type too
     MirOper.varDec := nil;
     MirOper.astOperand := AstOper;
  end else if AstOper.opType = otVariab then begin
     MirOper.varDec := TMirVarDec(AstOper.rvar.mirVarDec);  //Must be set
     MirOper.astOperand := AstOper;
  end else begin
    MirOper.varDec := nil;
    MirOper.astOperand := nil;
  end;
end;
procedure GetMIRFunctionFromASTExpress(out MirFunc: TMirFunction;
                                      const AstOper: TEleExpress);
{Read data from a TEleExpress and set a TMirFunction}
begin
  MirFunc.Text := AstOper.name;
  if AstOper.opType = otFunct then begin
    MirFunc.funDec := TMirFunDec(AstOper.fundec.mirFunDec);
    MirFunc.astOperand := AstOper;
  end else begin
    MirFunc.funDec := nil;
    MirFunc.astOperand := nil;
  end;
end;
function TMirList.AddAssignSim(mcont: TMirFunDec;
    vardec: TMirVarDec; //Ref. to create target variable.
    Op2: TEleExpress): TMirAsgnSim;  //Source expression
{Add element to the MIR container.}
begin
  //Set destination
  Result:= TMirAsgnSim.Create;
  Result.SetDestFromVarDec(vardec);  //Set variable destination.
  //Set right operand
  GetMIROperandFromASTExpress(Result.opSrc, Op2);
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  Result.text := Result.dest.Text + ' := ' + Op2.name;
  {$ENDIF}
  //Add to list
  if mcont = nil then items.Add(Result) else mcont.items.Add(Result);
end;
function TMirList.NewAssignFun(mcont: TMirFunDec;
    vardec: TMirVarDec; //Ref. to create target variable.
    Op2: TEleExpress): TMirAsgnFun;
var
  astPar: TAstElement;
  i: Integer;
begin
  Result:= TMirAsgnFun.Create;
  Result.SetDestFromVarDec(vardec);  //Set variable destination.
  //Set function operand
  GetMIRFunctionFromASTExpress(Result.func, Op2);
  //Set parameters
  SetLength(Result.pars, Op2.elements.count);
  i := 0;
  for astPar in Op2.elements do begin
    GetMIROperandFromASTExpress(Result.pars[i], TEleExpress(astPar));
    inc(i);
  end;
end;
function TMirList.AddAssignFun(mcont: TMirFunDec; const vdec: TMirVarDec;
  Op2: TEleExpress): TMirAsgnFun;
{Add and assigment instruction to the MIR container "mcont". This is a recursive
function.}
var
  astPar: TEleExpress;
  i: Integer;
  tmpdec: TMirVarDec;
begin
  Result := NewAssignFun(mcont, vdec, Op2);
  //----------- Recursive part -------------
  //Check if some parameter needs to be converted in an assignment
  for i:=0 to Op2.elements.Count-1 do begin
    astPar := TEleExpress(Op2.elements[i]);
    if astPar.opType = otFunct then begin
      //Create a new temporal variable declaration.
      tmpdec := AddVarDec(mcont, '', astPar.Typ);
      //Insert a new assigment
      AddAssignFun(mcont, tmpdec, astPar);
      //Update the parameter
      Result.SetParAsVar(i, tmpdec);  //Convert parameter to the temporal variable.
    end;
  end;
  Result.UpdateText;              //Update label.
  //----------- End Recursive part -------------
  //Add to list
  if mcont=nil then items.Add(Result) else mcont.items.Add(Result);
end;

function TMirList.NewFunCall(mcont: TMirFunDec; Op1: TEleExpress): TMirFunCall;
var
  astPar: TAstElement;
  i: Integer;
begin
  Result:= TMirFunCall.Create;

  //Set function operand
  GetMIRFunctionFromASTExpress(Result.func, Op1);
  //Set parameters
  SetLength(Result.pars, Op1.elements.count);
  i := 0;
  for astPar in Op1.elements do begin
    GetMIROperandFromASTExpress(Result.pars[i], TEleExpress(astPar));
    inc(i);
  end;
end;
function TMirList.AddFunCall(mcont: TMirFunDec; Op1: TEleExpress): TMirFunCall;
{Add a new Function call instruction to the MIR container "mcont".}
begin
  Result := NewFunCall(mcont, Op1);

  Result.UpdateText;              //Update label.
  //Add to list
  if mcont=nil then items.Add(Result) else mcont.items.Add(Result);
end;
//Reading from AST
procedure TMirList.ConvertBody(cntFunct: TMirFunDec; sntBlock: TEleCodeCont);
{Convert a ASTBody to instructions in MIR representation.
Parameters:
  cntFunct  -> The function where MIR will be created, or the main program. This will
               be used as reference to locate the new variable declarations.
  sntBlock  -> Block of code where are the sentences to need be prepared. It's the
               same of "cntFunct" except when "block" is nested like in a condiitonal.
}
  procedure AddAssign(const vardec: TMirVarDec;
    Op2: TEleExpress);
  {General function to add a new assignment from a var declaration and an AST expression
  element.}
  begin
    if (Op2.opType = otVariab) then begin       //x := var1
      AddAssignSim(cntFunct, vardec, Op2);
    end else if (Op2.opType = otConst) then begin //x := CONS1
      AddAssignSim(cntFunct, vardec, Op2);
    end else if (Op2.opType = otFunct) then begin
      //Op2 is a function: 2 or more operands
      if Op2.fundec.callType in [ctSysNormal, ctUsrNormal] then begin  //Normal function
//        {IT's the form:
//             x := func(x,y);
//                  \_______/
//                     Op2
//        }
//        //Generates an asignment for each parameter.
//        SplitProcCall(curContainer, Op2);
      end else if Op2.fundec.callType = ctSysInline then begin       //INLINE function
        {IT's the form:
             x := A + B
                  \___/
                   Op2
        or:
             x := A++        }
        {We expect parameters A, B should be simple operands (Constant or variables)
        otherwise we will move them to a separate assignment}
        AddAssignFun(cntFunct, vardec, Op2);      //Create assignment in TAC format.
      end;
    end;
  end;
{  function MoveParamToAssign(curContainer: TAstElement; Op: TEleExpress;
                             parvar: TEleVarDec): TEleExpress;
  {Mueve el nodo especificado "Op", que representa a un parámetro de la función, a una
  nueva instruccion de asignación (que es creada al inicio del bloque "curContainer") y
  reemplaza el nodo faltante por una variable temporal que es la que se crea en la
  instrucción de asignación.
  Es similar a MoveNodeToAssign() pero no reemplaza el nodo movido y no crea una variable
  auxiliar, sino que usa "parvar".
  Retorna la instrucción de asignación creada.
  }
  var
    _setaux: TEleExpress;
    Op1aux: TEleExpress;
    funSet: TEleFunBase;
  begin
    //Create the new _set() expression.
    _setaux := CreateExpression('_set', typNull, otFunct, Op.srcDec);
    funSet := MethodFromBinOperator(Op.Typ, ':=', Op.Typ);
    if funSet = nil then begin   //Operator not found
      GenError('Undefined operation: %s %s %s', [Op.Typ.name, ':=', Op.Typ.name], Op.srcDec);
      _setaux.Destroy;    //We destroy because it hasn't been included in the AST.
      exit(nil);
    end;
    _setaux.rfun := funSet;

    //Add the new assigment before the main
    TreeElems.openElement(curContainer);
    TreeElems.AddElement(_setaux, 0);    //Add a new assigmente before
    _setaux.elements := TxpElements.Create(true);  //Create list
    TreeElems.openElement(_setaux);

    //Add first operand (variable) of the assignment.
    Op1aux := CreateExpression(parvar.name, parvar.typ, otVariab, Op.srcDec);
    Op1aux.SetVariab(parvar);
    TreeElems.addElement(Op1aux);
    AddCallerToFromCurr(parvar); //Add reference to auxiliar variable.

    //Move the second operand to the previous _set created
    TreeElems.ChangeParentTo(_setaux, Op);

    exit(_setaux);
  end;
  function SplitProcCall(curContainer: TAstElement; expMethod: TEleExpress): boolean; forward;
}  function SplitSet(setMethod: TAstElement): boolean;
  {Process a set sentence. If a set expression has more than three operands
  it's splitted adding one or more aditional set sentences, at the beggining of
  "curContainer".
  If at least one new set sentence is added, returns TRUE.}
  var
    Op2, Op1: TEleExpress;
    vardec: TMirVarDec;
  begin
    Result := false;
    if TEleExpress(setMethod).fundec.getset <> gsSetInSimple then exit;
    Op1 := TEleExpress(setMethod.elements[0]);  //Takes target.
    if Op1.opType <> otVariab then exit;
    //Split expressions in second operand of assignment.
    Op2 := TEleExpress(setMethod.elements[1]);  //Takes assignment source.
    vardec := TMirVarDec(Op1.rvar.mirVarDec);
    AddAssign(vardec, Op2);
  end;
{  function SplitExpress(curContainer: TAstElement; expMethod: TEleExpress): boolean;
  {Verify if an expression has more than three operands. If so then
  it's splitted adding one or more set sentences.
  If at least one new set sentence is added, returns TRUE.}
  var
    parExp, new_set: TEleExpress;
    par: TAstElement;
  begin
    Result := false;
    if (expMethod.opType = otFunct) then begin  //Neither variables nor constants.
      {We expect parameters should be simple operands (Constant or variables)
      otherwise we will move them to a separate assignment}
      if expMethod.rfun.callType in [ctSysNormal, ctUsrNormal] then begin  //Normal function

        //Generates an asignment for each parameter.
        SplitProcCall(curContainer, expMethod);
      end else if expMethod.rfun.callType = ctSysInline then begin  //Like =, >, and, ...
        for par in expMethod.elements do begin
          parExp := TEleExpress(par);
          if parExp.opType = otFunct then begin
            new_set := MoveNodeToAssign(cntFunct, curContainer, parExp);
            if HayError then exit;
            SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
            Result := true;
          end;
        end;
      end;
    end;
  end;}
  function SplitProcCall(expMethod: TEleExpress): boolean;
  {Split a procedure (not INLINE) call instruction, inserting an assignment instruction
  for each parameter.}
  var
    new_set, astPar: TEleExpress;
    fundec: TEleFunDec;
    i: Integer;
    mirfun: TMirFunCall;
    vardec: TMirVarDec;
  begin
    Result := false;
    if expMethod.opType <> otFunct then exit;   //Not a fucntion call
    fundec := expMethod.fundec;    //Base function reference
    if fundec.codSysInline=nil then begin   //Not INLINE
      mirfun := NewFunCall(cntFunct, expMethod);
      {Move all parameters (children nodes) to a separate assignment}
      for i:=0 to expMethod.elements.Count-1 do begin
        astPar := TEleExpress(expMethod.elements[i]);
        vardec := mirfun.func.funDec.pars[i].vardec;
        AddAssignSim(cntFunct, vardec, astPar); //???? Y no hay que buscar el método _set?

      end;
      mirfun.UpdateText;              //Update label.
      //Add to list
      if cntFunct=nil then items.Add(mirfun) else cntFunct.items.Add(mirfun);
    end;
  end;

var
  sen: TEleSentence;
  eleSen, _set, ele, _proc: TAstElement;
  _exp, Op1, Op2, val1, val2: TEleExpress;
  _blk, _blk0: TEleCodeCont;
begin
  //Prepare assignments for arrays.
//  for eleSen in sntBlock.elements do begin
//    if eleSen.idClass <> eleSenten then continue;
//    //We have a sentence here.
//    sen := TEleSentence(eleSen);
//    if sen.sntType = sntAssign then begin  //Assignment
//      _set := sen.elements[0];  //Takes the _set method.
//      Op1 := TEleExpress(_set.elements[0]);  //Takes assigment target.
//      Op2 := TEleExpress(_set.elements[1]);  //Takes assigment target.
//      if (Op1.opType = otFunct) and (Op1.fundec.getset = gsGetInItem) then begin
//        //It's a _set() for a _getitem() INLINE assignment for array.
//        if Op1.fundec.funset = nil then begin
//          GenError('Cannot locate the setter for this type.');
//          exit;
//        end;
//        //Convert getter() to setter().
//        Op1.fundec := Op1.fundec.funset;     //Must be gsSetInItem
//        Op1.name := Op1.fundec.name;
//        Op1.Typ  := Op1.fundec.retType;
//        //Move third parameter to _setitem() and locate it at the Top
//        TreeElems.ChangeParentTo(Op1, Op2);
//        TreeElems.ChangeParentTo(Op1.Parent.Parent, Op1);
//        _set.Parent.elements.Remove(_set);
//      end else if (Op1.opType = otFunct) and (Op1.fundec.getset = gsGetInPtr) then begin
//        //It's a _set() for a _getptr() INLINE assignment for POINTER.
//        if Op1.fundec.funset = nil then begin
//          GenError('Cannot locate the setter for this type.');
//          exit;
//        end;
//        //Convert getter() to setter().
//        Op1.fundec := Op1.fundec.funset;     //Must be gsSetInPtr;
//        Op1.name := Op1.fundec.name;
//        Op1.Typ  := Op1.fundec.retType;
//        //Move third parameter to _setptr() and locate it at the Top
//        TreeElems.ChangeParentTo(Op1, Op2);
//        TreeElems.ChangeParentTo(Op1.Parent.Parent, Op1);
//        _set.Parent.elements.Remove(_set);
//      end;
//    end;
//  end;
  //Prepare sentences
  for eleSen in sntBlock.elements do begin
    if eleSen.idClass <> eleSenten then continue;
    //We have a sentence here.
    sen := TEleSentence(eleSen);
    if sen.sntType = sntAssign then begin  //Assignment
      _set := sen.elements[0];  //Takes the one _set method.
      SplitSet( _set)  //Might generate additional assignments sentences
    end else if sen.sntType = sntProcCal then begin  //Procedure call
      _proc := sen.elements[0];  //Takes the proc.
      SplitProcCall(TEleExpress(_proc))
{    end else if sen.sntType in [sntIF, sntREPEAT, sntWHILE] then begin
      //There are expressions and blocks inside conditions and loops.
      for ele in sen.elements do begin
        if ele.idClass = eleCondit then begin  //It's a condition
          _exp := TEleExpress(ele.elements[0]);  //The first item is a TEleExpress
          SplitExpress(ele, _exp)
        end else if ele.idClass = eleBlock then begin   //body of IF
          _blk := TEleCodeCont(ele);  //The first item is a TEleExpress
          PrepareBody(cntFunct, _blk);
        end;
      end;
    end else if sen.sntType = sntFOR then begin
      //FOR sentences could need some aditional changes.
      _blk0 := nil;
      for ele in sen.elements do begin
        if ele.idClass = eleCondit then begin  //It's a condition
          _exp := TEleExpress(ele.elements[0]);  //The first item is a TEleExpress
          SplitExpress(ele, _exp)
        end else if ele.idClass = eleBlock then begin   //Initialization or body
          _blk := TEleCodeCont(ele);  //The first item is a TEleExpress
          PrepareBody(cntFunct, _blk);
          if _blk0 = nil then _blk0 := _blk;  //Get intialization block
        end;
      end;
      //Get first and last value of index.
      val1 := TEleExpress(_blk0.elements[0].elements[1]);
      val2 := TEleExpress(_exp.elements[1]);
      //Special cases
      if (val1.opType = otConst) and (val2.opType = otConst) then begin
        if val1.val > val2.val then begin
          //Unreachable code
//          TreeElems.DeleteTypeNode();
        end;
      end;
    end else if sen.sntType = sntExit then begin
      if sen.elements.Count>0 then begin   //If there is argument
        _exp := TEleExpress(sen.elements[0]);  //The first item is a TEleExpress
        SplitExpress(sen, _exp)
      end;
 }   end;

  end;
end;

//Initialization
procedure TMirList.Clear;
begin
  items.Clear;
  ndecs := 0;
end;
constructor TMirList.Create;
begin
  items:= TMirElements.Create(true);
end;
destructor TMirList.Destroy;
begin
  items.Destroy;
  inherited Destroy;
end;

end.

