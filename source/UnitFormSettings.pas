unit UnitFormSettings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, Icon;

type
  TFormSettings = class(TForm)
    LayoutWindows: TLayout;
    CheckBoxWindows: TCheckBox;
    CheckBoxWindows256: TCheckBox;
    CheckBoxWindows48: TCheckBox;
    CheckBoxWindows32: TCheckBox;
    CheckBoxWindows16: TCheckBox;
    LayoutMac: TLayout;
    CheckBoxMac: TCheckBox;
    CheckBoxMac1024: TCheckBox;
    CheckBoxMac512: TCheckBox;
    CheckBoxMac256: TCheckBox;
    CheckBoxMac128: TCheckBox;
    CheckBoxMac32: TCheckBox;
    CheckBoxMac16: TCheckBox;
    LayoutIPhone: TLayout;
    CheckBoxIPhone: TCheckBox;
    CheckBoxIPhoneApp: TCheckBox;
    CheckBoxIPhoneApp180: TCheckBox;
    CheckBoxIPhoneApp120: TCheckBox;
    CheckBoxIPhoneApp114: TCheckBox;
    CheckBoxIPhoneApp87: TCheckBox;
    CheckBoxIPhoneApp60: TCheckBox;
    CheckBoxIPhoneApp57: TCheckBox;
    CheckBoxIPhoneSpotlight: TCheckBox;
    CheckBoxIPhoneSpotlight40: TCheckBox;
    CheckBoxIPhoneSpotlight58: TCheckBox;
    CheckBoxIPhoneSpotlight80: TCheckBox;
    CheckBoxIPhoneSpotlight29: TCheckBox;
    LayoutIPad: TLayout;
    CheckBoxIPad: TCheckBox;
    CheckBoxIPadApp: TCheckBox;
    CheckBoxIPadApp76: TCheckBox;
    CheckBoxIPadApp144: TCheckBox;
    CheckBoxIPadApp152: TCheckBox;
    CheckBoxIPadApp72: TCheckBox;
    CheckBoxIPadSpotlight: TCheckBox;
    CheckBoxIPadSpotlight50: TCheckBox;
    CheckBoxIPadSpotlight58: TCheckBox;
    CheckBoxIPadSpotlight80: TCheckBox;
    CheckBoxIPadSpotlight100: TCheckBox;
    CheckBoxIPadSpotlight29: TCheckBox;
    CheckBoxIPadSpotlight40: TCheckBox;
    LayoutAndroid: TLayout;
    CheckBoxAndroid: TCheckBox;
    CheckBoxAndroidApp: TCheckBox;
    CheckBoxAndroidApp96: TCheckBox;
    CheckBoxAndroidApp144: TCheckBox;
    CheckBoxAndroidApp48: TCheckBox;
    CheckBoxAndroidApp72: TCheckBox;
    CheckBoxAndroidApp36: TCheckBox;
    Layout1: TLayout;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure CheckBoxWindowsChange(Sender: TObject);
    procedure ChangeCheckBox(const AParentCheckBoxIsChecked: Boolean; const AChildCheckBox: TCheckBox);
    procedure CheckBoxMacChange(Sender: TObject);
    procedure CheckBoxIPhoneChange(Sender: TObject);
    procedure CheckBoxIPhoneAppChange(Sender: TObject);
    procedure CheckBoxIPhoneSpotlightChange(Sender: TObject);
    procedure CheckBoxIPadChange(Sender: TObject);
    procedure CheckBoxIPadAppChange(Sender: TObject);
    procedure CheckBoxIPadSpotlightChange(Sender: TObject);
    procedure CheckBoxAndroidChange(Sender: TObject);
    procedure CheckBoxAndroidAppChange(Sender: TObject);
  private
    function GetIconList: TIconList;
    /// <summary>
    ///   すべてのチェックボックスがチェックされているときはTrue
    /// </summary>
    function IsAllChecked(const CheckBoxes: TArray<TCheckBox>): Boolean;
    { private 宣言 }
  public
    { public 宣言 }
    property IconList: TIconList read GetIconList;
    /// <summary>
    ///   元画像より小さい画像のチェックボックスのみ有効にする
    /// </summary>
    procedure SetSourceImageSize(const ASize: Integer);
    /// <summary>
    ///   チェックボックスの状態を更新する
    /// </summary>
    procedure SetIconList(const ASize: Integer; const AIconList: TIconList);
  end;

function CreateFormSettings(const AForm: TForm; const ASize: Integer; const AIconList: TIconList): TFormSettings;

implementation

{$R *.fmx}


function CreateFormSettings(const AForm: TForm; const ASize: Integer; const AIconList: TIconList): TFormSettings;
begin
  Result := TFormSettings.Create(AForm);
  Result.SetSourceImageSize(Asize);
  Result.SetIconList(Asize, AIconList);
end;

{ TFormSettings }

procedure TFormSettings.ChangeCheckBox(const AParentCheckBoxIsChecked: Boolean; const AChildCheckBox: TCheckBox);
begin
  AChildCheckBox.IsChecked := (AChildCheckBox.Enabled and AParentCheckBoxIsChecked);
end;

procedure TFormSettings.CheckBoxAndroidAppChange(Sender: TObject);
begin
  ChangeCheckBox(CheckBoxAndroidApp.IsChecked, CheckBoxAndroidApp144);
  ChangeCheckBox(CheckBoxAndroidApp.IsChecked, CheckBoxAndroidApp96);
  ChangeCheckBox(CheckBoxAndroidApp.IsChecked, CheckBoxAndroidApp72);
  ChangeCheckBox(CheckBoxAndroidApp.IsChecked, CheckBoxAndroidApp48);
  ChangeCheckBox(CheckBoxAndroidApp.IsChecked, CheckBoxAndroidApp36);
end;

procedure TFormSettings.CheckBoxAndroidChange(Sender: TObject);
begin
  ChangeCheckBox(CheckBoxAndroid.IsChecked, CheckBoxAndroidApp);
end;

procedure TFormSettings.CheckBoxIPadAppChange(Sender: TObject);
begin
  ChangeCheckBox(CheckBoxIPadApp.IsChecked, CheckBoxIPadApp152);
  ChangeCheckBox(CheckBoxIPadApp.IsChecked, CheckBoxIPadApp144);
  ChangeCheckBox(CheckBoxIPadApp.IsChecked, CheckBoxIPadApp76);
  ChangeCheckBox(CheckBoxIPadApp.IsChecked, CheckBoxIPadApp72);
end;

procedure TFormSettings.CheckBoxIPadChange(Sender: TObject);
begin
  ChangeCheckBox(CheckBoxIPad.IsChecked, CheckBoxIPadApp);
  ChangeCheckBox(CheckBoxIPad.IsChecked, CheckBoxIPadSpotlight);
end;

procedure TFormSettings.CheckBoxIPadSpotlightChange(Sender: TObject);
begin
  ChangeCheckBox(CheckBoxIPadSpotlight.IsChecked, CheckBoxIPadSpotlight100);
  ChangeCheckBox(CheckBoxIPadSpotlight.IsChecked, CheckBoxIPadSpotlight80);
  ChangeCheckBox(CheckBoxIPadSpotlight.IsChecked, CheckBoxIPadSpotlight58);
  ChangeCheckBox(CheckBoxIPadSpotlight.IsChecked, CheckBoxIPadSpotlight50);
  ChangeCheckBox(CheckBoxIPadSpotlight.IsChecked, CheckBoxIPadSpotlight40);
  ChangeCheckBox(CheckBoxIPadSpotlight.IsChecked, CheckBoxIPadSpotlight29);
end;

procedure TFormSettings.CheckBoxIPhoneAppChange(Sender: TObject);
begin
  ChangeCheckBox(CheckBoxIPhoneApp.IsChecked, CheckBoxIPhoneApp180);
  ChangeCheckBox(CheckBoxIPhoneApp.IsChecked, CheckBoxIPhoneApp120);
  ChangeCheckBox(CheckBoxIPhoneApp.IsChecked, CheckBoxIPhoneApp114);
  ChangeCheckBox(CheckBoxIPhoneApp.IsChecked, CheckBoxIPhoneApp87);
  ChangeCheckBox(CheckBoxIPhoneApp.IsChecked, CheckBoxIPhoneApp60);
  ChangeCheckBox(CheckBoxIPhoneApp.IsChecked, CheckBoxIPhoneApp57);
end;

procedure TFormSettings.CheckBoxIPhoneChange(Sender: TObject);
begin
  ChangeCheckBox(CheckBoxIPhone.IsChecked, CheckBoxIPhoneApp);
  ChangeCheckBox(CheckBoxIPhone.IsChecked, CheckBoxIPhoneSpotlight);
end;

procedure TFormSettings.CheckBoxIPhoneSpotlightChange(Sender: TObject);
begin
  ChangeCheckBox(CheckBoxIPhoneSpotlight.IsChecked, CheckBoxIPhoneSpotlight80);
  ChangeCheckBox(CheckBoxIPhoneSpotlight.IsChecked, CheckBoxIPhoneSpotlight58);
  ChangeCheckBox(CheckBoxIPhoneSpotlight.IsChecked, CheckBoxIPhoneSpotlight40);
  ChangeCheckBox(CheckBoxIPhoneSpotlight.IsChecked, CheckBoxIPhoneSpotlight29);
end;

procedure TFormSettings.CheckBoxMacChange(Sender: TObject);
begin
  ChangeCheckBox(CheckBoxMac.IsChecked, CheckBoxMac1024);
  ChangeCheckBox(CheckBoxMac.IsChecked, CheckBoxMac512);
  ChangeCheckBox(CheckBoxMac.IsChecked, CheckBoxMac256);
  ChangeCheckBox(CheckBoxMac.IsChecked, CheckBoxMac128);
  ChangeCheckBox(CheckBoxMac.IsChecked, CheckBoxMac32);
  ChangeCheckBox(CheckBoxMac.IsChecked, CheckBoxMac16);
end;

procedure TFormSettings.CheckBoxWindowsChange(Sender: TObject);
begin
  ChangeCheckBox(CheckBoxWindows.IsChecked, CheckBoxWindows256);
  ChangeCheckBox(CheckBoxWindows.IsChecked, CheckBoxWindows48);
  ChangeCheckBox(CheckBoxWindows.IsChecked, CheckBoxWindows32);
  ChangeCheckBox(CheckBoxWindows.IsChecked, CheckBoxWindows16);
end;

function TFormSettings.GetIconList: TIconList;
begin
  Result := TIconList.CreateEmpty;
  if CheckBoxWindows256.IsChecked then Result.AddIcon(TIconTypeWindows.isw256x256);
  if CheckBoxWindows48.IsChecked then Result.AddIcon(TIconTypeWindows.isw48x48);
  if CheckBoxWindows32.IsChecked then Result.AddIcon(TIconTypeWindows.isw32x32);
  if CheckBoxWindows16.IsChecked then Result.AddIcon(TIconTypeWindows.isw16x16);

  if CheckBoxMac1024.IsChecked then Result.AddIcon(TIconTypeMac.ism1024x1024);
  if CheckBoxMac512.IsChecked then Result.AddIcon(TIconTypeMac.ism512x512);
  if CheckBoxMac256.IsChecked then Result.AddIcon(TIconTypeMac.ism256x256);
  if CheckBoxMac128.IsChecked then Result.AddIcon(TIconTypeMac.ism128x128);
  if CheckBoxMac32.IsChecked then Result.AddIcon(TIconTypeMac.ism32x32);
  if CheckBoxMac16.IsChecked then Result.AddIcon(TIconTypeMac.ism16x16);

  if CheckBoxIPhoneApp180.IsChecked then Result.AddIcon(TIconTypeIPhoneApp.ispa180x180);
  if CheckBoxIPhoneApp120.IsChecked then Result.AddIcon(TIconTypeIPhoneApp.ispa120x120);
  if CheckBoxIPhoneApp114.IsChecked then Result.AddIcon(TIconTypeIPhoneApp.ispa114x114);
  if CheckBoxIPhoneApp87.IsChecked then Result.AddIcon(TIconTypeIPhoneApp.ispa87x87);
  if CheckBoxIPhoneApp60.IsChecked then Result.AddIcon(TIconTypeIPhoneApp.ispa60x60);
  if CheckBoxIPhoneApp57.IsChecked then Result.AddIcon(TIconTypeIPhoneApp.ispa57x57);

  if CheckBoxIPhoneSpotlight80.IsChecked then Result.AddIcon(TIconTypeIPhoneSpotlight.isps80x80);
  if CheckBoxIPhoneSpotlight58.IsChecked then Result.AddIcon(TIconTypeIPhoneSpotlight.isps58x58);
  if CheckBoxIPhoneSpotlight40.IsChecked then Result.AddIcon(TIconTypeIPhoneSpotlight.isps40x40);
  if CheckBoxIPhoneSpotlight29.IsChecked then Result.AddIcon(TIconTypeIPhoneSpotlight.isps29x29);

  if CheckBoxIPadApp152.IsChecked then Result.AddIcon(TIconTypeIPadApp.ispa152x152);
  if CheckBoxIPadApp144.IsChecked then Result.AddIcon(TIconTypeIPadApp.ispa144x144);
  if CheckBoxIPadApp76.IsChecked then Result.AddIcon(TIconTypeIPadApp.ispa76x76);
  if CheckBoxIPadApp72.IsChecked then Result.AddIcon(TIconTypeIPadApp.ispa72x72);

  if CheckBoxIPadSpotlight100.IsChecked then Result.AddIcon(TIconTypeIPadSpotlight.isps100x100);
  if CheckBoxIPadSpotlight80.IsChecked then Result.AddIcon(TIconTypeIPadSpotlight.isps80x80);
  if CheckBoxIPadSpotlight58.IsChecked then Result.AddIcon(TIconTypeIPadSpotlight.isps58x58);
  if CheckBoxIPadSpotlight50.IsChecked then Result.AddIcon(TIconTypeIPadSpotlight.isps50x50);
  if CheckBoxIPadSpotlight40.IsChecked then Result.AddIcon(TIconTypeIPadSpotlight.isps40x40);
  if CheckBoxIPadSpotlight29.IsChecked then Result.AddIcon(TIconTypeIPadSpotlight.isps29x29);

  if CheckBoxAndroidApp144.IsChecked then Result.AddIcon(TIconTypeAndroid.isa144x144);
  if CheckBoxAndroidApp96.IsChecked then Result.AddIcon(TIconTypeAndroid.isa96x96);
  if CheckBoxAndroidApp72.IsChecked then Result.AddIcon(TIconTypeAndroid.isa72x72);
  if CheckBoxAndroidApp48.IsChecked then Result.AddIcon(TIconTypeAndroid.isa48x48);
  if CheckBoxAndroidApp36.IsChecked then Result.AddIcon(TIconTypeAndroid.isa36x36);
end;

function TFormSettings.IsAllChecked(
  const CheckBoxes: TArray<TCheckBox>): Boolean;
var
  CheckBox: TCheckBox;
begin
  for CheckBox in CheckBoxes do
  begin
    if (CheckBox.Enabled) and (not CheckBox.IsChecked) then
      Exit(False);
  end;
  Result := True;
end;

procedure TFormSettings.SetIconList(const ASize: Integer; const AIconList: TIconList);
begin
  CheckBoxWindows256.IsChecked := CheckBoxWindows256.Enabled and AIconList.HasIcon(TIconTypeWindows.isw256x256);
  CheckBoxWindows48.IsChecked := CheckBoxWindows48.Enabled and AIconList.HasIcon(TIconTypeWindows.isw48x48);
  CheckBoxWindows32.IsChecked := CheckBoxWindows32.Enabled and AIconList.HasIcon(TIconTypeWindows.isw32x32);
  CheckBoxWindows16.IsChecked := CheckBoxWindows16.Enabled and AIconList.HasIcon(TIconTypeWindows.isw16x16);
  CheckBoxWindows.IsChecked := IsAllChecked(TArray<TCheckBox>.Create(CheckBoxWindows256, CheckBoxWindows48, CheckBoxWindows32, CheckBoxWindows16));

  CheckBoxMac1024.IsChecked := CheckBoxMac1024.Enabled and AIconList.HasIcon(TIconTypeMac.ism1024x1024);
  CheckBoxMac512.IsChecked := CheckBoxMac512.Enabled and AIconList.HasIcon(TIconTypeMac.ism512x512);
  CheckBoxMac256.IsChecked := CheckBoxMac256.Enabled and AIconList.HasIcon(TIconTypeMac.ism256x256);
  CheckBoxMac128.IsChecked := CheckBoxMac128.Enabled and AIconList.HasIcon(TIconTypeMac.ism128x128);
  CheckBoxMac32.IsChecked := CheckBoxMac32.Enabled and AIconList.HasIcon(TIconTypeMac.ism32x32);
  CheckBoxMac16.IsChecked := CheckBoxMac16.Enabled and AIconList.HasIcon(TIconTypeMac.ism16x16);
  CheckBoxMac.IsChecked := IsAllChecked(TArray<TCheckBox>.Create(CheckBoxMac1024, CheckBoxMac512, CheckBoxMac256, CheckBoxMac128, CheckBoxMac32, CheckBoxMac16));

  CheckBoxIPhoneApp180.IsChecked := CheckBoxIPhoneApp180.Enabled and AIconList.HasIcon(TIconTypeIPhoneApp.ispa180x180);
  CheckBoxIPhoneApp120.IsChecked := CheckBoxIPhoneApp120.Enabled and AIconList.HasIcon(TIconTypeIPhoneApp.ispa120x120);
  CheckBoxIPhoneApp114.IsChecked := CheckBoxIPhoneApp114.Enabled and AIconList.HasIcon(TIconTypeIPhoneApp.ispa114x114);
  CheckBoxIPhoneApp87.IsChecked := CheckBoxIPhoneApp87.Enabled and AIconList.HasIcon(TIconTypeIPhoneApp.ispa87x87);
  CheckBoxIPhoneApp60.IsChecked := CheckBoxIPhoneApp60.Enabled and AIconList.HasIcon(TIconTypeIPhoneApp.ispa60x60);
  CheckBoxIPhoneApp57.IsChecked := CheckBoxIPhoneApp57.Enabled and AIconList.HasIcon(TIconTypeIPhoneApp.ispa57x57);
  CheckBoxIPhoneApp.IsChecked := IsAllChecked(TArray<TCheckBox>.Create(CheckBoxIPhoneApp180, CheckBoxIPhoneApp120, CheckBoxIPhoneApp114, CheckBoxIPhoneApp87, CheckBoxIPhoneApp60, CheckBoxIPhoneApp57));

  CheckBoxIPhoneSpotlight80.IsChecked := CheckBoxIPhoneSpotlight80.Enabled and AIconList.HasIcon(TIconTypeIPhoneSpotlight.isps80x80);
  CheckBoxIPhoneSpotlight58.IsChecked := CheckBoxIPhoneSpotlight58.Enabled and AIconList.HasIcon(TIconTypeIPhoneSpotlight.isps58x58);
  CheckBoxIPhoneSpotlight40.IsChecked := CheckBoxIPhoneSpotlight40.Enabled and AIconList.HasIcon(TIconTypeIPhoneSpotlight.isps40x40);
  CheckBoxIPhoneSpotlight29.IsChecked := CheckBoxIPhoneSpotlight29.Enabled and AIconList.HasIcon(TIconTypeIPhoneSpotlight.isps29x29);
  CheckBoxIPhoneSpotlight.IsChecked := IsAllChecked(TArray<TCheckBox>.Create(CheckBoxIPhoneSpotlight80, CheckBoxIPhoneSpotlight58, CheckBoxIPhoneSpotlight40, CheckBoxIPhoneSpotlight29));
  CheckBoxIPhone.IsChecked := IsAllChecked(TArray<TCheckBox>.Create(CheckBoxIPhoneApp, CheckBoxIPhoneSpotlight));

  CheckBoxIPadApp152.IsChecked := CheckBoxIPadApp152.Enabled and AIconList.HasIcon(TIconTypeIPadApp.ispa152x152);
  CheckBoxIPadApp144.IsChecked := CheckBoxIPadApp144.Enabled and AIconList.HasIcon(TIconTypeIPadApp.ispa144x144);
  CheckBoxIPadApp76.IsChecked := CheckBoxIPadApp76.Enabled and AIconList.HasIcon(TIconTypeIPadApp.ispa76x76);
  CheckBoxIPadApp72.IsChecked := CheckBoxIPadApp72.Enabled and AIconList.HasIcon(TIconTypeIPadApp.ispa72x72);
  CheckBoxIPadApp.IsChecked := IsAllChecked(TArray<TCheckBox>.Create(CheckBoxIPadApp152, CheckBoxIPadApp144, CheckBoxIPadApp76, CheckBoxIPadApp72));

  CheckBoxIPadSpotlight100.IsChecked := CheckBoxIPadSpotlight100.Enabled and AIconList.HasIcon(TIconTypeIPadSpotlight.isps100x100);
  CheckBoxIPadSpotlight80.IsChecked := CheckBoxIPadSpotlight80.Enabled and AIconList.HasIcon(TIconTypeIPadSpotlight.isps80x80);
  CheckBoxIPadSpotlight58.IsChecked := CheckBoxIPadSpotlight58.Enabled and AIconList.HasIcon(TIconTypeIPadSpotlight.isps58x58);
  CheckBoxIPadSpotlight50.IsChecked := CheckBoxIPadSpotlight50.Enabled and AIconList.HasIcon(TIconTypeIPadSpotlight.isps50x50);
  CheckBoxIPadSpotlight40.IsChecked := CheckBoxIPadSpotlight40.Enabled and AIconList.HasIcon(TIconTypeIPadSpotlight.isps40x40);
  CheckBoxIPadSpotlight29.IsChecked := CheckBoxIPadSpotlight29.Enabled and AIconList.HasIcon(TIconTypeIPadSpotlight.isps29x29);
  CheckBoxIPadSpotlight.IsChecked := IsAllChecked(TArray<TCheckBox>.Create(CheckBoxIPadSpotlight100, CheckBoxIPadSpotlight80, CheckBoxIPadSpotlight58, CheckBoxIPadSpotlight50, CheckBoxIPadSpotlight40, CheckBoxIPadSpotlight29));
  CheckBoxIPad.IsChecked := IsAllChecked(TArray<TCheckBox>.Create(CheckBoxIPadApp, CheckBoxIPadSpotlight));

  CheckBoxAndroidApp144.IsChecked := CheckBoxAndroidApp144.Enabled and AIconList.HasIcon(TIconTypeAndroid.isa144x144);
  CheckBoxAndroidApp96.IsChecked := CheckBoxAndroidApp96.Enabled and AIconList.HasIcon(TIconTypeAndroid.isa96x96);
  CheckBoxAndroidApp72.IsChecked := CheckBoxAndroidApp72.Enabled and AIconList.HasIcon(TIconTypeAndroid.isa72x72);
  CheckBoxAndroidApp48.IsChecked := CheckBoxAndroidApp48.Enabled and AIconList.HasIcon(TIconTypeAndroid.isa48x48);
  CheckBoxAndroidApp36.IsChecked := CheckBoxAndroidApp36.Enabled and AIconList.HasIcon(TIconTypeAndroid.isa36x36);
  CheckBoxAndroidApp.IsChecked := IsAllChecked(TArray<TCheckBox>.Create(CheckBoxAndroidApp144, CheckBoxAndroidApp96, CheckBoxAndroidApp72, CheckBoxAndroidApp48, CheckBoxAndroidApp36));
  CheckBoxAndroid.IsChecked := IsAllChecked(TArray<TCheckBox>.Create(CheckBoxAndroidApp));
end;

procedure TFormSettings.SetSourceImageSize(const ASize: Integer);
var
  IconList: TIconList;
begin
  IconList := TIconList.Create(ASize);

  CheckBoxWindows256.Enabled := TIcon.Create(TIconTypeWindows.isw256x256).CanCreate(ASize);
  CheckBoxWindows48.Enabled := TIcon.Create(TIconTypeWindows.isw48x48).CanCreate(ASize);
  CheckBoxWindows32.Enabled := TIcon.Create(TIconTypeWindows.isw32x32).CanCreate(ASize);
  CheckBoxWindows16.Enabled := TIcon.Create(TIconTypeWindows.isw16x16).CanCreate(ASize);

  CheckBoxMac1024.Enabled := TIcon.Create(TIconTypeMac.ism1024x1024).CanCreate(ASize);
  CheckBoxMac512.Enabled := TIcon.Create(TIconTypeMac.ism512x512).CanCreate(ASize);
  CheckBoxMac256.Enabled := TIcon.Create(TIconTypeMac.ism256x256).CanCreate(ASize);
  CheckBoxMac128.Enabled := TIcon.Create(TIconTypeMac.ism128x128).CanCreate(ASize);
  CheckBoxMac32.Enabled := TIcon.Create(TIconTypeMac.ism32x32).CanCreate(ASize);
  CheckBoxMac16.Enabled := TIcon.Create(TIconTypeMac.ism16x16).CanCreate(ASize);

  CheckBoxIPhoneApp180.Enabled := TIcon.Create(TIconTypeIPhoneApp.ispa180x180).CanCreate(ASize);
  CheckBoxIPhoneApp120.Enabled := TIcon.Create(TIconTypeIPhoneApp.ispa120x120).CanCreate(ASize);
  CheckBoxIPhoneApp114.Enabled := TIcon.Create(TIconTypeIPhoneApp.ispa114x114).CanCreate(ASize);
  CheckBoxIPhoneApp87.Enabled := TIcon.Create(TIconTypeIPhoneApp.ispa87x87).CanCreate(ASize);
  CheckBoxIPhoneApp60.Enabled := TIcon.Create(TIconTypeIPhoneApp.ispa60x60).CanCreate(ASize);
  CheckBoxIPhoneApp57.Enabled := TIcon.Create(TIconTypeIPhoneApp.ispa57x57).CanCreate(ASize);

  CheckBoxIPhoneSpotlight80.Enabled := TIcon.Create(TIconTypeIPhoneSpotlight.isps80x80).CanCreate(ASize);
  CheckBoxIPhoneSpotlight58.Enabled := TIcon.Create(TIconTypeIPhoneSpotlight.isps58x58).CanCreate(ASize);
  CheckBoxIPhoneSpotlight40.Enabled := TIcon.Create(TIconTypeIPhoneSpotlight.isps40x40).CanCreate(ASize);
  CheckBoxIPhoneSpotlight29.Enabled := TIcon.Create(TIconTypeIPhoneSpotlight.isps29x29).CanCreate(ASize);

  CheckBoxIPadApp152.Enabled := TIcon.Create(TIconTypeIPadApp.ispa152x152).CanCreate(ASize);
  CheckBoxIPadApp144.Enabled := TIcon.Create(TIconTypeIPadApp.ispa144x144).CanCreate(ASize);
  CheckBoxIPadApp76.Enabled := TIcon.Create(TIconTypeIPadApp.ispa76x76).CanCreate(ASize);
  CheckBoxIPadApp72.Enabled := TIcon.Create(TIconTypeIPadApp.ispa72x72).CanCreate(ASize);

  CheckBoxIPadSpotlight100.Enabled := TIcon.Create(TIconTypeIPadSpotlight.isps100x100).CanCreate(ASize);
  CheckBoxIPadSpotlight80.Enabled := TIcon.Create(TIconTypeIPadSpotlight.isps80x80).CanCreate(ASize);
  CheckBoxIPadSpotlight58.Enabled := TIcon.Create(TIconTypeIPadSpotlight.isps58x58).CanCreate(ASize);
  CheckBoxIPadSpotlight50.Enabled := TIcon.Create(TIconTypeIPadSpotlight.isps50x50).CanCreate(ASize);
  CheckBoxIPadSpotlight40.Enabled := TIcon.Create(TIconTypeIPadSpotlight.isps40x40).CanCreate(ASize);
  CheckBoxIPadSpotlight29.Enabled := TIcon.Create(TIconTypeIPadSpotlight.isps29x29).CanCreate(ASize);

  CheckBoxAndroidApp144.Enabled := TIcon.Create(TIconTypeAndroid.isa144x144).CanCreate(ASize);
  CheckBoxAndroidApp96.Enabled := TIcon.Create(TIconTypeAndroid.isa96x96).CanCreate(ASize);
  CheckBoxAndroidApp72.Enabled := TIcon.Create(TIconTypeAndroid.isa72x72).CanCreate(ASize);
  CheckBoxAndroidApp48.Enabled := TIcon.Create(TIconTypeAndroid.isa48x48).CanCreate(ASize);
  CheckBoxAndroidApp36.Enabled := TIcon.Create(TIconTypeAndroid.isa36x36).CanCreate(ASize);

  CheckBoxWindows.Enabled := (CheckBoxWindows256.Enabled or CheckBoxWindows48.Enabled or CheckBoxWindows32.Enabled or CheckBoxWindows16.Enabled);
  CheckBoxMac.Enabled := (CheckBoxMac1024.Enabled or CheckBoxMac512.Enabled or CheckBoxMac256.Enabled or CheckBoxMac128.Enabled or CheckBoxMac32.Enabled or CheckBoxMac16.Enabled);
  CheckBoxIPhoneApp.Enabled := (CheckBoxIPhoneApp180.Enabled or CheckBoxIPhoneApp120.Enabled or CheckBoxIPhoneApp114.Enabled or CheckBoxIPhoneApp87.Enabled or CheckBoxIPhoneApp60.Enabled or CheckBoxIPhoneApp57.Enabled);
  CheckBoxIPhoneSpotlight.Enabled := (CheckBoxIPhoneSpotlight80.Enabled or CheckBoxIPhoneSpotlight58.Enabled or CheckBoxIPhoneSpotlight40.Enabled or CheckBoxIPhoneSpotlight29.Enabled);
  CheckBoxIPadApp.Enabled := (CheckBoxIPadApp152.Enabled or CheckBoxIPadApp144.Enabled or CheckBoxIPadApp76.Enabled or CheckBoxIPadApp72.Enabled);
  CheckBoxIPadSpotlight.Enabled := (CheckBoxIPadSpotlight100.Enabled or CheckBoxIPadSpotlight80.Enabled or CheckBoxIPadSpotlight58.Enabled or CheckBoxIPadSpotlight50.Enabled or CheckBoxIPadSpotlight40.Enabled or CheckBoxIPadSpotlight29.Enabled);
  CheckBoxAndroidApp.Enabled := (CheckBoxAndroidApp144.Enabled or CheckBoxAndroidApp96.Enabled or CheckBoxAndroidApp72.Enabled or CheckBoxAndroidApp48.Enabled or CheckBoxAndroidApp36.Enabled);
end;

end.
