unit MirList;
{Unidad con la definiicón del objeto MIR. A diferencia del árbol de sintaxis esta
representación intermedia es lineal.
                                          Por Tito Hinostroza 21/10/2023.}
{$mode ObjFPC}{$H+}
{$Define DEBUGMODE}   //Enable MIR visualization
interface
uses
  Classes, SysUtils, fgl, AstElemP65, LCLProc;
type  //MIR base class
  //MIR Element type
  TMirType = (
    //Declarations
     mtyVarDec      //Variable declaration
    ,mtyConDec      //Constant declaration
    ,mtyFunDec      //Function declaration
    //Instructions
    ,mtyAssign      //Assignment from function
    ,mtyFunCall     //Procedure or Function call
    ,mtyLabel       //Label
    ,mtyGoto        //Goto
    ,mtyIfJump      //Conditional jump
    );

  { TMirElement }
  TMirElement = Class;
  TMirElements = specialize TFPGObjectList<TMirElement>;
  TMirElement = Class
    mirType: TMirType;
    text   : String;  //Text representing the instruction.
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

type  //MIR Operand for expressions

  { TMirOperand }

  TMirOperand = object
    Text    : string;        //Label for the operand.
    opType  : TopType;       //Operand type (otVariab, otConst, otFunct) like AST elements.
    Sto     : TStorage;     //Storage of the value (memory, register, value)
    Typ     : TEleTypeDec;  //Data type for the operand.
    varDec  : TMirVarDec;    //Ref. to var declaration, when it's variable. Otherwise it' will be NIL.
    conDec  : TMirConDec;    //Ref. to constant declaration.
    astOperand: TEleExpress; //Ref. to AST element. Should be used only for error location.
  public //Fields used when "opType" is otFunc.
    funDec  : TMirFunDec;    //Reference to function declaration, when it's accesible.
    pars    : array of TMirOperand; //Parameter list.
    function FunCallText: string;
    procedure SetParAsVar(i: Integer; vardec0: TMirVarDec);
  public  //Fields used when "Sto" is stConst
    evaluated: boolean;     //Activated when constant is evaluated.
    consType : TConsType;   //Constant type
    //Fields used according to "consType" value.
    value    : TConsValue;  //Constant value, when consType=ctLiteral
    private
    consRef  : TEleConsDec;  //Ref. to TEleConsDec when consType=ctConsRef *** ¿Se necesita además de "conDec"?
    addrVar  : TEleVarDec;   //Ref. to TEleVarDec  when consType=ctVarAddr
    addrFun  : TEleFunDec;   //Ref. to TEleFun when consType=ctFunAddr
    public
    //Functions to read values.
    function val: dword;
    function valL: word;
    function valH: word;
    function valU: word;
    function valE: word;
    function valWlo: word;
    function valWhi: word;
    procedure SetLiteralBoolConst(valBool: Boolean);
    procedure SetLiteraltIntConst(valInt: Int64);
    procedure SetConstRef(cons0: TEleConsDec);
    procedure SetAddrVar(var0: TEleVarDec);
    procedure SetAddrFun(fun0: TEleFunDec);
    procedure Evaluate();
  end;

type  //MIR instructions

  { TMirFunCall }
  TMirFunCall = Class(TMirElement)
    func  : TMirOperand;     //Function called.
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create;
  end;

  { TMirAssign }
  TMirAssign = Class(TMirElement)
  public
    isSimple: Boolean;       //Activated when instruction is: var_a := <simple operamd>; *** Es necesario?
    dest    : TMirOperand;   //Target variable.
    opSrc   : TMirOperand;   //Source operand, when "isSimple = TRUE".
    procedure SetDestFromVarDec(vardec: TMirVarDec);
  public
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create;
  end;

  { TMirLabel }
  TMirLabel = class(TMirElement)
    ilabel: integer;    //Idntifier for label used in Goto instructions. We use number
                      //instead of string for speed.
  public
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create;
  end;

  { TMirGoto }
  TMirGoto = Class(TMirElement)
  public
    ilabel  : integer;          //Label identifier. We used a number instead of a string for speed.
  public
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create;
  end;

  { TMirIfGoto }
  TMirIfGoto = Class(TMirElement)
  public
    condit  : TMirOperand;   //Condition operand.
    negated : Boolean;       //Flag to indicate the condition is inverted.
    ilabel  : integer;          //Label identifier. We used a number instead of a string for speed.
  public
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create;
  end;

type  //Main Container
  { TMirList }
  TMirList = class
  public
    items   : TMirElements;
    ndecs   : Integer;      //Number of declarations.
    ngotos  : Integer;      //NUmber of gotos
    insPoint: Integer;      //Insert point for instructions
  private  //Insertion
    procedure AddItem(mcont: TMirFunDec; itm: TMirElement; position: integer = 1);
    procedure SetInsertMode(iPoint: integer); inline;
    procedure ClearInsertMode; inline;
    function InsertModeActive: Boolean; inline;
  public  //Adding declarations
    function AddVarDec(mcont: TMirFunDec; varDec0: TEleVarDec): TMirVarDec;
    function AddVarDec(mcont: TMirFunDec; varName: string; eleTyp: TEleTypeDec
      ): TMirVarDec;
    function AddFunDecSNF(funcName0: TEleFunDec): TMirFunDec;
    function AddFunDecUNF(funcName0: TEleFunDec): TMirFunDec;
  public  //Adding instructions
    procedure AddFunParamAssign(mcont: TMirFunDec; var func: TMirOperand;
      Op1: TEleExpress);
    function AddFunCall(mcont: TMirFunDec; Op1: TEleExpress): TMirFunCall;
    function AddAssign(mcont: TMirFunDec; vardec: TMirVarDec;
      Op2: TEleExpress): TMirAssign;
    function AddGoto(mcont: TMirFunDec; ilabel: integer = - 1): TMirGoto;
    function AddGoto(mcont: TMirFunDec; mlabel: TMirLabel): TMirGoto;
    function AddLabel(mcont: TMirFunDec): TMirLabel;
    procedure EndGoto(mcont: TMirFunDec; gotoInstr: TMirGoto);
    procedure EndGoto(mcont: TMirFunDec; ilabel: integer);
    function AddIfGoto(mcont: TMirFunDec; condition: TEleExpress; negated: boolean
      ): TMirIfGoto;
    procedure EndIfGoto(mcont: TMirFunDec; ifInstruc: TMirIfGoto);
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
{ TMirOperand }
function TMirOperand.FunCallText: string;
//Returns the function call in text.
//Only works when "opType" is otFunc.
var
  i: Integer;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  Result := Text + '(';
  if (funDec=Nil) or  //Is System
     (funDec.astFunDec.callType in [ctUsrInline, ctSysInline]) then begin
      //Only in this case, shows parameters
      for i:=0 to High(pars) do begin
        //Agrega nombre de parámetro
          if i=0 then Result += pars[i].Text
          else        Result += ',' + pars[i].Text;
      end;
      Result += ')';
  end;
  {$ENDIF}
end;
procedure TMirOperand.SetParAsVar(i: Integer; vardec0: TMirVarDec);
{Set a parameter like a variable}
var
  par: ^TMirOperand;
begin
  par := @pars[i];
  //Convert "par1" to the temporal variable
  par^.Text   := vardec0.text;
  par^.opType := otVariab;
  par^.varDec := vardec0;
  par^.astOperand := nil;
end;
//Fields used when "Sto" is stConst
function TMirOperand.val: dword; inline;
begin
  Result := value.ValInt;
end;
function TMirOperand.valL: word; inline;
begin
  Result := LO(word(value.ValInt));
end;
function TMirOperand.valH: word; inline;
begin
  Result := HI(word(value.ValInt));
end;
function TMirOperand.valU: word; inline;
begin
  Result := (value.valInt >> 24) and $FF;
end;
function TMirOperand.valE: word; inline;
begin
  Result := (value.valInt >> 16) and $FF;
end;
function TMirOperand.valWlo: word; inline;
begin
  Result := word(value.ValInt);
end;
function TMirOperand.valWhi: word; inline;
begin
  Result := (value.valInt >> 16) and $FFFF;
end;
procedure TMirOperand.SetLiteralBoolConst(valBool: Boolean);
{Set the value of a Constant boolean expression.}
begin
  consType := ctLiteral;
  evaluated := true;
  value.valBool := valBool;    //Tal vez no sea necesario si usamos solo "value.ValInt"
  //Como en algunos casos se usa el valor numérico, lo fijamos también.
  if valBool then begin
    value.ValInt := 255;
  end else begin
    value.ValInt := 0;
  end;
end;
procedure TMirOperand.SetLiteraltIntConst(valInt: Int64);
begin
  consType := ctLiteral;
  evaluated := true;
  value.ValInt := valInt;
end;
procedure TMirOperand.SetConstRef(cons0: TEleConsDec);
begin
  consType := ctConsRef;
  consRef := cons0;  //Keep reference
  evaluated := false;  //To force evaluation
  Evaluate;
end;
procedure TMirOperand.SetAddrVar(var0: TEleVarDec);
begin
  consType := ctVarAddr;
  addrVar := var0;  //Keep reference
  evaluated := false;  //To force evaluation
  Evaluate;
end;
procedure TMirOperand.SetAddrFun(fun0: TEleFunDec);
begin
  consType := ctFunAddr;
  addrFun := fun0;  //Keep reference
  evaluated := false;  //To force evaluation
  Evaluate;
end;
procedure TMirOperand.Evaluate();
var
  xfun: TEleFunDec;
  ele: TAstElement;
  itemExp: TEleExpress;
  i: Integer;
begin
  if evaluated then exit;  //Already evaluated
  case consType of
//  ctLiteral: begin  //Literal like $1234 or 123
//      //This shouldn't happens, because literal are always evaluated.
//      //Unless it's an array literal like [@var1, @var2]
//      i := 0;
//      if self.Typ.catType = tctArray then begin
//        //Constant array. Let's evaluate by items
//        evaluated := true;    //Defaule
//        for ele in elements do begin
//          itemExp := TEleExpress(ele);   //Recover type.
//          itemExp.Evaluate();
//          if not itemExp.evaluated then begin
//            evaluated := false;
//            break;
//          end;
//          value.items[i] := itemExp.value;
//          inc(i);
//        end;
//      end;
//  end;
  ctConsRef: begin  //Reference to a constant declared like "CONST1"
      //Could be evaluated using "consRef"
      if consRef.evaluated then begin
        value := consRef.value^;
        evaluated := true;
      end else begin
        //Constant not yet evaluated.
      end;
  end;
  ctVarAddr: begin  //Constant expression like addr(<variable>)
      if addrVar.allocated then begin
        value.ValInt := addrVar.addr;
        evaluated := true;
      end else begin
        //Variable not yet allocated
      end;
  end;
  ctFunAddr: begin  //Constant expression like addr(<function>)
      //Get  declaration
      xfun := addrFun;
      if xfun = nil then exit;  //Not implemented
      //We have an implementation.
      if xfun.coded then begin
        value.ValInt := xfun.adrr;
        evaluated := true;
      end else begin
        //Function not yet allocated
//        if not pic.disableCodegen then begin  //Verify if we are in mode no-code-generation.
//          xfun.AddAddresPend(pic.iRam-2);  //Register the address to complete later
//        end;
//        AddrUndef := true;
      end;
  end;
  end;
end;
{ TMirFunCall }
procedure TMirFunCall.UpdateText;
begin
  Text := func.FunCallText;
end;
constructor TMirFunCall.Create;
begin
  mirType := mtyFunCall;
end;
{ TMirAssign }
procedure TMirAssign.SetDestFromVarDec(vardec: TMirVarDec);
{Set "dest" attribute from a Mir Variable declaration. It's equivalent to create a new
MIR Operand (from a MIR variable declaration) and set "dest" to that new operand.
That's why this operand doesn't have an AST reference.}
begin
  dest.Text := vardec.text;
  dest.opType := otVariab;
  dest.varDec := vardec;
  dest.astOperand := nil;
end;
procedure TMirAssign.UpdateText;
{Set the "Text" attribute from the content of the instruction.}
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  if isSimple then begin  //Simple assignment
    Text := dest.Text + ' := ' + opSrc.Text;
  end else begin   //Assignment from function
    Text := dest.Text + ' := ' + opSrc.FunCallText;
  end;
  {$ENDIF}
end;
constructor TMirAssign.Create;
begin
  mirType := mtyAssign;
end;
{ TMirLabel }
procedure TMirLabel.UpdateText;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  Text := 'L' + IntToStr(ilabel) + ':';
  {$ENDIF}
end;
constructor TMirLabel.Create;
begin
  mirType := mtyLabel;
end;
{ TMirGoto }
procedure TMirGoto.UpdateText;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  Text := 'GOTO L' + IntToStr(ilabel);
  {$ENDIF}
end;
constructor TMirGoto.Create;
begin
  mirType := mtyGoto;
end;
{ TMirIfGoto }
procedure TMirIfGoto.UpdateText;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  if negated then Text := 'IFNOT ' else Text := 'IF ';
  if condit.opType = otFunct then begin  //Simple assignment
    Text += condit.FunCallText + ' GOTO L' + IntToStr(ilabel);
  end else begin   //Assignment from function
    Text += condit.Text + ' GOTO L' + IntToStr(ilabel);
  end;
  {$ENDIF}
end;
constructor TMirIfGoto.Create;
begin
  mirType := mtyIfJump;
end;
{ TMirList }
//Insertion
procedure TMirList.AddItem(mcont: TMirFunDec;
  itm: TMirElement; position: integer);
{Add an item to the current conatiner}
begin
  if mcont=nil then begin   //The main container
    if insPoint<>-1 then begin
      items.Insert(insPoint, itm);
      inc(insPoint);
    end else begin
      items.Add(itm);
    end;
  end else begin           //A function container
    if insPoint<>-1 then begin
      mcont.items.Insert(insPoint, itm);
      inc(insPoint);
    end else begin
      mcont.items.Add(itm);
    end;
  end;
end;
procedure TMirList.SetInsertMode(iPoint: integer);
{Set the MIR List in mode "Insert".}
begin
  insPoint := iPoint;
end;
procedure TMirList.ClearInsertMode;
{Leaves the mode "Insert".}
begin
  insPoint := -1;
end;
function TMirList.InsertModeActive: Boolean;
{Indicates if the mode "Insert" is activated.}
begin
  exit(insPoint <> -1);
end;
//Adding declarations
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
    if AstOper.consType in [ctVarAddr, ctFunAddr] then begin
      MirOper.Text := '@' + AstOper.name;  //UPdate name including indicator.
    end;
    MirOper.consType := AstOper.consType;  //Copy type too
    MirOper.varDec := nil;
    MirOper.astOperand := AstOper;
  end else if AstOper.opType = otVariab then begin
    MirOper.varDec := TMirVarDec(AstOper.vardec.mirVarDec);  //Must be set
    MirOper.funDec := nil;
    MirOper.astOperand := AstOper;
  end else if AstOper.opType = otFunct then begin
    MirOper.varDec := nil;
    MirOper.funDec := TMirFunDec(AstOper.fundec.mirFunDec);
    MirOper.astOperand := AstOper;
  end;
end;
//Adding instructions
procedure TMirList.AddFunParamAssign(mcont: TMirFunDec;
                                     var func: TMirOperand; Op1: TEleExpress);
var
  astPar: TEleExpress;
  vardec, tmpdec: TMirVarDec;
  i: Integer;
begin
  if Op1.opType <> otFunct then exit;  //Protection
  //Set parameters number.
  SetLength(func.pars, Op1.elements.count);
  //Proccess according to the function type
  case Op1.fundec.callType of
  ctUsrNormal, ctSysNormal: begin      //Normal function
    //        {IT's the form:
    //             x := func(x,y);
    //                  \_______/
    //                     Op2
    //        }
    {Move all parameters (children nodes) to a separate assignment}
    for i:=0 to Op1.elements.Count-1 do begin
      astPar := TEleExpress(Op1.elements[i]);
      GetMIROperandFromASTExpress(func.pars[i], astPar);
      vardec := func.funDec.pars[i].vardec;
      AddAssign(mcont, vardec, astPar); //???? Y no hay que buscar el método _set?
    end;
  end;
  ctUsrInline, ctSysInline: begin       //INLINE function
    {IT's the form:
         x := A + B
              \___/
               Op2
    or:
         x := A++        }
    {We expect parameters A, B should be simple operands (Constant or variables)
    otherwise we will move them to a separate assignment}
    //Check if some parameter needs to be converted in an assignment
    for i:=0 to Op1.elements.Count-1 do begin
      astPar := TEleExpress(Op1.elements[i]);
      GetMIROperandFromASTExpress(func.pars[i], astPar);
      if astPar.opType = otFunct then begin
        //Create a new temporal variable declaration.
        tmpdec := AddVarDec(mcont, '', astPar.Typ);
        //Insert a new assigment
        AddAssign(mcont, tmpdec, astPar);
        //Update the parameter
        func.SetParAsVar(i, tmpdec);  //Convert parameter to the temporal variable.
      end;
    end;
  end;
  end;
end;
function TMirList.AddFunCall(mcont: TMirFunDec; Op1: TEleExpress
  ): TMirFunCall;
begin
  Result:= TMirFunCall.Create;
  //Set function operand
  GetMIROperandFromASTExpress(Result.func, Op1);
  AddFunParamAssign(mcont, Result.func, Op1);
  Result.UpdateText;              //Update label.
  //Add to list
  AddItem(mcont, Result);
end;
function TMirList.AddAssign(mcont: TMirFunDec; vardec: TMirVarDec;
  Op2: TEleExpress): TMirAssign;
{General function to add an assignment instruction to the MIR container "mcont".
 The assignment in created in TAC format. This is a recursive function.
Parameters:
  "vardec" -> Variable declaration.
  "Op2"    -> Operando from the value for assignment is taken. Can be a simple Operand
              or a function call. It is an AST expression element.}
var
  astPar: TEleExpress;
  i: Integer;
  tmpdec: TMirVarDec;
begin
  Result:= TMirAssign.Create;
  if Op2.opType <> otFunct then Result.isSimple := true else Result.isSimple := false;
  Result.SetDestFromVarDec(vardec);  //Set variable destination.
  //Set function operand
  GetMIROperandFromASTExpress(Result.opSrc, Op2);
  //Set parameters
  if Op2.opType = otFunct then begin
    //Only functions should have parameters
    AddFunParamAssign(mcont, Result.opSrc, Op2);
  end;
  Result.UpdateText;              //Update label.
  //Add to list
  AddItem(mcont, Result);
end;
function TMirList.AddGoto(mcont: TMirFunDec; ilabel: integer = -1): TMirGoto;
begin
  Result:= TMirGoto.Create;
  if ilabel=-1 then begin   //Generates the index label.
    inc(ngotos);  //Count
    Result.ilabel := ngotos; //Update label
  end else begin    //Uses the parameter index.
    Result.ilabel := ilabel; //Update label
  end;
  Result.UpdateText;              //Update label.
  //Add to list
  AddItem(mcont, Result);
end;
function TMirList.AddGoto(mcont: TMirFunDec; mlabel: TMirLabel): TMirGoto;
begin
  Result:= TMirGoto.Create;
  Result.ilabel := mlabel.ilabel; //Update label
  Result.UpdateText;              //Update label.
  //Add to list
  AddItem(mcont, Result);
end;
function TMirList.AddLabel(mcont: TMirFunDec): TMirLabel;
var
  lblInstruct: TMirLabel;
begin
  Result := TMirLabel.Create;
  inc(ngotos);  //Count
  Result.ilabel := ngotos;
  Result.UpdateText;
  AddItem(mcont, Result);
end;
procedure TMirList.EndGoto(mcont: TMirFunDec; gotoInstr: TMirGoto);
var
  lblInstruct: TMirLabel;
begin
  lblInstruct := TMirLabel.Create;
  lblInstruct.ilabel := gotoInstr.ilabel;
  lblInstruct.UpdateText;
  AddItem(mcont, lblInstruct);
end;
procedure TMirList.EndGoto(mcont: TMirFunDec; ilabel: integer);
var
  lblInstruct: TMirLabel;
begin
  lblInstruct := TMirLabel.Create;
  lblInstruct.ilabel := ilabel;
  lblInstruct.UpdateText;
  AddItem(mcont, lblInstruct);
end;
function TMirList.AddIfGoto(mcont: TMirFunDec;
  condition: TEleExpress; negated: boolean): TMirIfGoto;
{General function to add an IF instruction to the MIR container "mcont".
Parameters:
  "condition" -> The condition expression.
}
begin
  Result:= TMirIfGoto.Create;
  Result.negated := negated;
  inc(ngotos);  //Count
  Result.ilabel := ngotos; //Update label
  //Set function operand
  GetMIROperandFromASTExpress(Result.condit, condition);
  //Set parameters
  if condition.opType = otFunct then begin
    //Only functions should have parameters
    AddFunParamAssign(mcont, Result.condit, condition);
  end;
  Result.UpdateText;              //Update label.
  //Add to list
  AddItem(mcont, Result);
end;
procedure TMirList.EndIfGoto(mcont: TMirFunDec; ifInstruc: TMirIfGoto);
{Finish the IF ... GOTO instruction.}
var
  lblInstruct: TMirLabel;
begin
  lblInstruct := TMirLabel.Create;
  lblInstruct.ilabel := ifInstruc.ilabel;
  lblInstruct.UpdateText;
  AddItem(mcont, lblInstruct);
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
}
  function SplitSet(setMethod: TAstElement): boolean;
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
    vardec := TMirVarDec(Op1.vardec.mirVarDec);
    AddAssign(cntFunct, vardec, Op2);
  end;
  function SplitExpress(expMethod: TEleExpress): boolean;
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
      AddFunCall(cntFunct, expMethod);
    end;
  end;
  function SplitProcCall(expMethod: TEleExpress): boolean;
  {Split a procedure (not INLINE) call instruction, inserting an assignment instruction
  for each parameter.}
  begin
    Result := false;
    if expMethod.opType <> otFunct then exit;   //Not a fucntion call
    AddFunCall(cntFunct, expMethod);
  end;
//  procedure GotoToEnd;
//  {Add a goto to the End of code is there is some aditional code after teh point we
//  are inserting instructions.}
//  var
//    gotToEnd: TMirGoto;
//    insPoint0: Integer;
//  begin
//    if InsertModeActive then begin
//      //There are instruction after. We need a GOTO to the end.
//      gotToEnd := AddGoto(cntFunct);
//      insPoint0 := insPoint;  //Save position.
//      ClearInsertMode;
//      EndGoto(cntFunct, gotToEnd);
//      SetInsertMode(insPoint0);
//    end;
//  end;
  function ReadValidConditionBlock(sen: TEleSentence; var i: Integer;
           out condit: TEleExpress; out block: TEleCodeCont): boolean;
  var
    expBool: TEleExpress;
  begin
    while i<sen.elements.Count do begin
      expBool := TEleExpress(sen.elements[i]);   //Even element is condition
      block   := TEleCodeCont(sen.elements[i+1]); //Odd element is block
      condit := TEleExpress(expBool.elements[0]);
      if (condit.opType = otConst) and condit.evaluated and
         (condit.value.ValBool=false) then begin
         //FALSE conditions are not considered.
         inc(i, 2);      //Try the next.
         Continue;
      end;
      if block.elements.Count = 0 then begin
         //FALSE conditions are not considered.
        inc(i, 2);      //Try the next.
        Continue;
      end;
      //Is valid
      inc(i, 2);  //Point to next position
      Exit(true);
    end;
    //Not found
    Exit(false);
  end;
  procedure ConvertIF(sen: TEleSentence);
  {COnvert an IF AST structure to the MIR representation. Only a BASIC simplification is
  applied. Optimization needs to be done later.}
  var
    i, lblEndIf: Integer;
    ifgot: TMirIfGoto;
    condit: TEleExpress;
    _blk: TEleCodeCont;
    gotToEnd: TMirGoto;
  begin
    //There are expressions and blocks inside conditions and loops.
    gotToEnd := Nil;
    lblEndIf := -1;
    i := 0;
    while ReadValidConditionBlock(sen, i, condit, _blk) do begin
      //if (condit.opType = otConst) and (condit.value.ValBool=true) then begin
      //  //True conditions (or ELSE) are the last to be executed.
      //  ConvertBody(cntFunct, _blk);
      //  GotoToEnd;
      //  break;  //No more is executed.
      //end else begin      //It's function
      //  if nextCondit<>nil then begin  //There are more conditions.
          //We add IF negated because normal form is: IF NOT ... GOTO ...
          ifgot := AddIfGoto(cntFunct, condit, true);
          ConvertBody(cntFunct, _blk);
          //Add goto to the end of IF structure (including ELSEIF ...).
          if gotToEnd=Nil then begin  //First Goto
            gotToEnd := AddGoto(cntFunct);
            lblEndIf := gotToEnd.ilabel;
          end else begin
            gotToEnd := AddGoto(cntFunct, lblEndIf);
          end;
          //Add label
          EndIfGoto(cntFunct, ifgot);
    end;
    if lblEndIf<>-1 then begin
      //There is al least one GOTO to the end of IF.
      EndGoto(cntFunct, lblEndIf);
    end;
  end;
  procedure ConvertWHILE(sen: TEleSentence);
  {Convert an WHILE AST structure to the MIR representation.}
  var
    ifgot: TMirIfGoto;
    condit, expBool: TEleExpress;
    _blk: TEleCodeCont;
    lblBegin: TMirLabel;
  begin
    //There are expressions and blocks inside conditions and loops.
    expBool := TEleExpress(sen.elements[0]);   //Even element is condition
    _blk   := TEleCodeCont(sen.elements[1]); //Odd element is block
    if _blk.elements.Count=0 then exit;   //Empty block
    condit := TEleExpress(expBool.elements[0]);
    //Label to the beginning of the WHILE to test condition.
    lblBegin := AddLabel(cntFunct);
    ifgot := AddIfGoto(cntFunct, condit, true);
    ConvertBody(cntFunct, _blk);
    //Add goto to the begin of IF structure (including ELSEIF ...).
    AddGoto(cntFunct, lblBegin);
    //Add label
    EndIfGoto(cntFunct, ifgot);
  end;
var
  sen: TEleSentence;
  eleSen, _set, ele, _proc: TAstElement;
  Op1, Op2, val1, val2: TEleExpress;
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
    end else if sen.sntType = sntIF then begin
      ConvertIF(sen);
    end else if sen.sntType = sntWHILE then begin
      ConvertWHILE(sen);
{    end else if sen.sntType = sntFOR then begin
      //FOR sentences could need some aditional changes.
      _blk0 := nil;
      for ele in sen.elements do begin
        if ele.idClass = eleCondit then begin  //It's a condition
          expBool := TEleExpress(ele.elements[0]);  //The first item is a TEleExpress
          SplitExpress(ele, expBool)
        end else if ele.idClass = eleBlock then begin   //Initialization or body
          _blk := TEleCodeCont(ele);  //The first item is a TEleExpress
          PrepareBody(cntFunct, _blk);
          if _blk0 = nil then _blk0 := _blk;  //Get intialization block
        end;
      end;
      //Get first and last value of index.
      val1 := TEleExpress(_blk0.elements[0].elements[1]);
      val2 := TEleExpress(expBool.elements[1]);
      //Special cases
      if (val1.opType = otConst) and (val2.opType = otConst) then begin
        if val1.val > val2.val then begin
          //Unreachable code
//          TreeElems.DeleteTypeNode();
        end;
      end;
    end else if sen.sntType = sntExit then begin
      if sen.elements.Count>0 then begin   //If there is argument
        expBool := TEleExpress(sen.elements[0]);  //The first item is a TEleExpress
        SplitExpress(sen, expBool)
      end;
 }   end;

  end;
end;
//Initialization
procedure TMirList.Clear;
begin
  items.Clear;
  ndecs := 0;
  ngotos := 0;
  insPoint := -1;     //Disable
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

