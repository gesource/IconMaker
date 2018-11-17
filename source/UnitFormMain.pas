unit UnitFormMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Layouts, System.Actions, FMX.ActnList, Icon;

type
  TFormMain = class(TForm)
    LayoutImage: TLayout;
    LayoutSettings: TLayout;
    Splitter1: TSplitter;
    LabelImageTitle: TLabel;
    ImageSource: TImage;
    LabelSettings: TLabel;
    MemoSettings: TMemo;
    ButtonSettings: TButton;
    ButtonCreateImage: TButton;
    LabelImageFile: TLabel;
    ActionList1: TActionList;
    ActionSettings: TAction;
    LayoutImageBottom: TLayout;
    ActionImageSelect: TAction;
    OpenDialogPng: TOpenDialog;
    ActionImageCreate: TAction;
    Button1: TButton;
    procedure ImageSourceDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF;
      var Operation: TDragOperation);
    procedure ImageSourceDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure ActionSettingsExecute(Sender: TObject);
    procedure ActionImageSelectExecute(Sender: TObject);
    procedure ActionImageCreateExecute(Sender: TObject);
    procedure ActionSettingsUpdate(Sender: TObject);
    procedure ActionImageCreateUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private 宣言 }
    FIconList: TIconList;
    FImageSize: Integer;
    FImageFileName: string;
    /// <summary>
    /// ドラッグされているファイルがPNGファイルのときはTrue
    /// </summary>
    function DragedFileIsPng(const Files: array of string): Boolean;
    /// <summary>
    /// PNGファイルを読み込む
    /// </summary>
    procedure LoadFromFile(const AFileName: string);
    procedure SetIconList(const AIconList: TIconList);
  public
    { public 宣言 }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Math,
  System.IOUtils,
  FMX.DialogService.Sync,
  UnitFormSettings;

procedure TFormMain.ActionImageCreateExecute(Sender: TObject);
const
  Caption = 'アイコンを保存するフォルダー';
  Root = '';
var
  Directory: string;
begin
  if not SelectDirectory(Caption, Root, Directory) then Exit;

  try
    Self.FIconList.CreateFile(Directory, Self.FImageFileName);
    TDialogServiceSync.MessageDialog('作成しました。', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
  except on E: Exception do
    TDialogServiceSync.MessageDialog(E.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
  end;
end;

procedure TFormMain.ActionImageCreateUpdate(Sender: TObject);
begin
  ActionImageCreate.Enabled := (not ImageSource.Bitmap.IsEmpty) and (not Self.FIconList.IsEmpty);
end;

procedure TFormMain.ActionImageSelectExecute(Sender: TObject);
begin
  if OpenDialogPng.Execute then
  begin
    Self.LoadFromFile(OpenDialogPng.FileName);
  end;
end;

procedure TFormMain.ActionSettingsExecute(Sender: TObject);
var
  Form: TFormSettings;
begin
  Form := UnitFormSettings.CreateFormSettings(Self, Self.FImageSize, Self.FIconList);
  if Form.ShowModal = mrOk then
  begin
    Self.SetIconList(Form.IconList);
  end;
end;

procedure TFormMain.ActionSettingsUpdate(Sender: TObject);
begin
  ActionSettings.Enabled := not ImageSource.Bitmap.IsEmpty;
end;

function TFormMain.DragedFileIsPng(const Files: array of string): Boolean;
begin
  if Length(Files) = 0 then
    Exit(False);
  if UpperCase(TPath.GetExtension(Files[0])) <> '.PNG' then
    Exit(False);
  if not TFile.Exists(Files[0]) then
    Exit(False);
  Result := True;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Self.FIconList := TIconList.CreateEmpty;
end;

procedure TFormMain.ImageSourceDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  if not DragedFileIsPng(Data.Files) then
    Exit;
  Self.LoadFromFile(Data.Files[0]);
end;

procedure TFormMain.ImageSourceDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF;
  var Operation: TDragOperation);
begin
  Operation := TDragOperation.None;
  if not DragedFileIsPng(Data.Files) then
    Exit;
  Operation := TDragOperation.Link;
end;

procedure TFormMain.LoadFromFile(const AFileName: string);
begin
  Self.FIconList := TIconList.CreateEmpty;
  Self.FImageFileName := AFileName;
  ImageSource.Bitmap.LoadFromFile(AFileName);
  Self.FImageSize := Min(ImageSource.Bitmap.Width, ImageSource.Bitmap.Height);
  LabelImageFile.Text := Format('%s(%dx%d)', [TPath.GetFileName(AFileName), ImageSource.Bitmap.Width,
    ImageSource.Bitmap.Height]);
end;

procedure TFormMain.SetIconList(const AIconList: TIconList);
begin
  Self.FIconList := AIconList;
  MemoSettings.Text := AIconList.ToString;
end;

end.
