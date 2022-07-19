program P65Pas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, FormPrincipal, Globales, FormConfig,
  PicPasProject, FrameEditView, FrameMessagesWin, FormElemProperty,
  FrameCfgExtTool, ParserDirec, Analyzer,
  LexPas, CompContexts, ParserASM_6502, XpresElemP65, FrameLateralPanel,
  //Adaptador para P65Pas
  adapter6502, FormAdapter6502, FrameStatist6502, FrameCfgAfterChg6502, FrameSynTree6502,
  FrameCfgCompiler6502, FrameCfgAsmOut6502, FormDebugger6502, FormRAMExplorer6502,
  adapterKickc, unit1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TConfig, Config);
//  Application.CreateForm(TfraCfgGeneral, fraCfgGeneral);
  Application.Run;
end.

