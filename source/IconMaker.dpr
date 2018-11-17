program IconMaker;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitFormMain in 'UnitFormMain.pas' {FormMain},
  UnitFormSettings in 'UnitFormSettings.pas' {FormSettings},
  Icon in 'Icon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
