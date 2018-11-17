unit Icon;

interface

uses System.Classes,   FMX.Graphics;

type
{$SCOPEDENUMS ON}
  /// <summary>
  /// Windowsのアイコンサイズ
  /// </summary>
  TIconTypeWindows = (isw256x256, isw48x48, isw32x32, isw16x16);
  /// <summary>
  /// Macのアイコンサイズ
  /// </summary>
  TIconTypeMac = (ism1024x1024, ism512x512, ism256x256, ism128x128, ism32x32, ism16x16);
  /// <summary>
  /// iPhoneのアプリケーションアイコンサイズ
  /// </summary>
  TIconTypeIPhoneApp = (ispa180x180, ispa120x120, ispa114x114, ispa87x87, ispa60x60, ispa57x57);
  /// <summary>
  /// iPhoneのスポットライト検索アイコンサイズ
  /// </summary>
  TIconTypeIPhoneSpotlight = (isps80x80, isps58x58, isps40x40, isps29x29);
  /// <summary>
  /// iPadのアプリケーションアイコンサイズ
  /// </summary>
  TIconTypeIPadApp = (ispa152x152, ispa144x144, ispa76x76, ispa72x72);
  /// <summary>
  /// iPadのスポットライト検索アイコンサイズ <br />
  /// </summary>
  TIconTypeIPadSpotlight = (isps100x100, isps80x80, isps58x58, isps50x50, isps40x40, isps29x29);
  /// <summary>
  /// Androidのランチャアイコンサイズ
  /// </summary>
  TIconTypeAndroid = (isa144x144, isa96x96, isa72x72, isa48x48, isa36x36);
{$SCOPEDENUMS OFF}
  TIconTypeSetWindows = set of TIconTypeWindows;
  TIconTypeSetMac = set of TIconTypeMac;
  TIconTypeSetIPhoneApp = set of TIconTypeIPhoneApp;
  TIconTypeSetIPhoneSpotlight = set of TIconTypeIPhoneSpotlight;
  TIconTypeSetIPadApp = set of TIconTypeIPadApp;
  TIconTypeSetIPadSpotlight = set of TIconTypeIPadSpotlight;
  TIconTypeSetAndroid = set of TIconTypeAndroid;

  TIconTypeSetWindowsHelper = record helper for TIconTypeSetWindows
  public
    /// <summary>
    ///   作成するアイコン一覧の文字列
    /// </summary>
    function ToString: string;
    function IsEmpty: Boolean;
  end;
  TIconTypeSetMacHelper = record helper for TIconTypeSetMac
  public
    /// <summary>
    ///   作成するアイコン一覧の文字列
    /// </summary>
    function ToString: string;
    function IsEmpty: Boolean;
  end;
  TIconTypeSetIPhoneAppHelper = record helper for TIconTypeSetIPhoneApp
  public
    /// <summary>
    ///   作成するアイコン一覧の文字列
    /// </summary>
    function ToString: string;
    function IsEmpty: Boolean;
  end;
  TIconTypeSetIPhoneSpotlightHelper = record helper for TIconTypeSetIPhoneSpotlight
  public
    /// <summary>
    ///   作成するアイコン一覧の文字列
    /// </summary>
    function ToString: string;
    function IsEmpty: Boolean;
  end;
  TIconTypeSetIPadAppHelper = record helper for TIconTypeSetIPadApp
  public
    /// <summary>
    ///   作成するアイコン一覧の文字列
    /// </summary>
    function ToString: string;
    function IsEmpty: Boolean;
  end;
  TIconTypeSetIPadSpotlightHelper = record helper for TIconTypeSetIPadSpotlight
  public
    /// <summary>
    ///   作成するアイコン一覧の文字列
    /// </summary>
    function ToString: string;
    function IsEmpty: Boolean;
  end;
  TIconTypeSetAndroidHelper = record helper for TIconTypeSetAndroid
  public
    /// <summary>
    ///   作成するアイコン一覧の文字列
    /// </summary>
    function ToString: string;
    function IsEmpty: Boolean;
  end;

  TIcon = record
  private
    FFileName: string;
    FDirectory: string;
    FSize: Integer;
    /// <summary>
    /// Windowsのアイコンのファイル名
    /// </summary>
    class function GetFileName(const AIconType: TIconTypeWindows): string; overload; static;
    /// <summary>
    /// Macアイコンのファイル名
    /// </summary>
    class function GetFileName(const AIconType: TIconTypeMac): string; overload; static;
    /// <summary>
    /// iPhoneのアプリケーションアイコンのファイル名
    /// </summary>
    class function GetFileName(const AIconType: TIconTypeIPhoneApp): string; overload; static;
    /// <summary>
    /// iPhoneのスポットライト検索アイコンのファイル名
    /// </summary>
    class function GetFileName(const AIconType: TIconTypeIPhoneSpotlight): string; overload; static;
    /// <summary>
    /// iPadのアプリケーションアイコンのファイル名
    /// </summary>
    class function GetFileName(const AIconType: TIconTypeIPadApp): string; overload; static;
    /// <summary>
    /// iPadのスポットライト検索アイコンのファイル名
    /// </summary>
    class function GetFileName(const AIconType: TIconTypeIPadSpotlight): string; overload; static;
    /// <summary>
    /// Androidのランチャアイコンのファイル名
    /// </summary>
    class function GetFileName(const AIconType: TIconTypeAndroid): string; overload; static;

    class function DirectoryNameForWindows: string; static;
    class function DirectoryNameForMac: string; static;
    class function DirectoryNameForIPhone: string; static;
    class function DirectoryNameForIPad: string; static;
    class function DirectoryNameForAndroid: string; static;
  public
    property Size: Integer read FSize;
    property FileName: string read FFileName;
    property Directory: string read FDirectory;
    /// <summary>
    /// 元ファイルのサイズより小さいときは作成可能
    /// </summary>
    function CanCreate(const ASize: Integer): Boolean;
    /// <summary>
    ///   アイコンファイルを保存する
    /// </summary>
    /// <param name="ADirectory">
    ///   ファイルを保存するフォルダー
    /// </param>
    /// <param name="ABitmap">
    ///   元画像
    /// </param>
    procedure SaveToFile(const ADirectory, AFileName: string);

    /// <summary>
    /// Windowsのアイコン
    /// </summary>
    class function Create(const AIconType: TIconTypeWindows): TIcon; overload; static;
    /// <summary>
    /// Macアイコン
    /// </summary>
    class function Create(const AIconType: TIconTypeMac): TIcon; overload; static;
    /// <summary>
    /// iPhoneのアプリケーションアイコン
    /// </summary>
    class function Create(const AIconType: TIconTypeIPhoneApp): TIcon; overload; static;
    /// <summary>
    /// iPhoneのスポットライト検索アイコン
    /// </summary>
    class function Create(const AIconType: TIconTypeIPhoneSpotlight): TIcon; overload; static;
    /// <summary>
    /// iPadのアプリケーションアイコン
    /// </summary>
    class function Create(const AIconType: TIconTypeIPadApp): TIcon; overload; static;
    /// <summary>
    /// iPadのスポットライト検索アイコン
    /// </summary>
    class function Create(const AIconType: TIconTypeIPadSpotlight): TIcon; overload; static;
    /// <summary>
    /// Androidのランチャアイコン
    /// </summary>
    class function Create(const AIconType: TIconTypeAndroid): TIcon; overload; static;
    /// <summary>
    /// Windowsのアイコンサイズの文字列
    /// </summary>
    class function GetCaption(const AIconType: TIconTypeWindows): string; overload; static;
    /// <summary>
    /// Macのアイコンサイズの文字列
    /// </summary>
    class function GetCaption(const AIconType: TIconTypeMac): string; overload; static;
    /// <summary>
    /// iPhoneのアプリケーションアイコンサイズの文字列
    /// </summary>
    class function GetCaption(const AIconType: TIconTypeIPhoneApp): string; overload; static;
    /// <summary>
    /// iPhoneのスポットライト検索アイコンサイズの文字列
    /// </summary>
    class function GetCaption(const AIconType: TIconTypeIPhoneSpotlight): string; overload; static;
    /// <summary>
    /// iPadのアプリケーションアイコンサイズの文字列
    /// </summary>
    class function GetCaption(const AIconType: TIconTypeIPadApp): string; overload; static;
    /// <summary>
    /// iPadのスポットライト検索アイコンサイズの文字列 <br />
    /// </summary>
    class function GetCaption(const AIconType: TIconTypeIPadSpotlight): string; overload; static;
    /// <summary>
    /// Androidのランチャアイコンサイズの文字列
    /// </summary>
    class function GetCaption(const AIconType: TIconTypeAndroid): string; overload; static;
  end;

  TIconList = record
  public const
    /// <summary>
    /// Windowsのアイコンサイズ
    /// </summary>
    ICON_TYPE_Windows: array [0 .. 3] of TIconTypeWindows = (TIconTypeWindows.isw256x256, TIconTypeWindows.isw48x48,
      TIconTypeWindows.isw32x32, TIconTypeWindows.isw16x16);
    /// <summary>
    /// Macのアイコンサイズ
    /// </summary>
    ICON_TYPE_Mac: array [0 .. 5] of TIconTypeMac = (TIconTypeMac.ism1024x1024, TIconTypeMac.ism512x512,
      TIconTypeMac.ism256x256, TIconTypeMac.ism128x128, TIconTypeMac.ism32x32, TIconTypeMac.ism16x16);
    /// <summary>
    /// iPhoneのアプリケーションアイコンサイズ
    /// </summary>
    ICON_TYPE_IPhoneApp: array [0 .. 5] of TIconTypeIPhoneApp = (TIconTypeIPhoneApp.ispa180x180,
      TIconTypeIPhoneApp.ispa120x120, TIconTypeIPhoneApp.ispa114x114, TIconTypeIPhoneApp.ispa87x87,
      TIconTypeIPhoneApp.ispa60x60, TIconTypeIPhoneApp.ispa57x57);
    /// <summary>
    /// iPhoneのスポットライト検索アイコンサイズ
    /// </summary>
    ICON_TYPE_IPhoneSpotlight: array [0 .. 3] of TIconTypeIPhoneSpotlight = (TIconTypeIPhoneSpotlight.isps80x80,
      TIconTypeIPhoneSpotlight.isps58x58, TIconTypeIPhoneSpotlight.isps40x40, TIconTypeIPhoneSpotlight.isps29x29);
    /// <summary>
    /// iPadのアプリケーションアイコンサイズ
    /// </summary>
    ICON_TYPE_IPadApp: array [0 .. 3] of TIconTypeIPadApp = (TIconTypeIPadApp.ispa152x152, TIconTypeIPadApp.ispa144x144,
      TIconTypeIPadApp.ispa76x76, TIconTypeIPadApp.ispa72x72);
    /// <summary>
    /// iPadのスポットライト検索アイコンサイズ
    /// </summary>
    ICON_TYPE_IPadSpotlight: array [0 .. 5] of TIconTypeIPadSpotlight = (TIconTypeIPadSpotlight.isps100x100,
      TIconTypeIPadSpotlight.isps80x80, TIconTypeIPadSpotlight.isps58x58, TIconTypeIPadSpotlight.isps50x50,
      TIconTypeIPadSpotlight.isps40x40, TIconTypeIPadSpotlight.isps29x29);
    /// <summary>
    /// Androidのランチャアイコンサイズ
    /// </summary>
    ICON_TYPE_Android: array [0 .. 4] of TIconTypeAndroid = (TIconTypeAndroid.isa144x144, TIconTypeAndroid.isa96x96, TIconTypeAndroid.isa72x72, TIconTypeAndroid.isa48x48, TIconTypeAndroid.isa36x36);
  private
    FIconTypeSetWindows: TIconTypeSetWindows;
    FIconTypeSetMac: TIconTypeSetMac;
    FIconTypeSetIPhoneApp: TIconTypeSetIPhoneApp;
    FIconTypeSetIPhoneSpotlight: TIconTypeSetIPhoneSpotlight;
    FIconTypeSetIPadApp: TIconTypeSetIPadApp;
    FIconTypeSetIPadSpotlight: TIconTypeSetIPadSpotlight;
    FIconTypeSetAndroid: TIconTypeSetAndroid;
  public
    /// <summary>
    /// 元画像のサイズから、作成可能なアイコンの一覧を取得する
    /// </summary>
    /// <param name="ASize">
    /// 元画像のサイズ
    /// </param>
    class function Create(const ASize: Integer): TIconList; static;
    /// <summary>
    /// 空のアイコン一覧を取得する
    /// </summary>
    class function CreateEmpty: TIconList; static;
    procedure AddIcon(const AIconType: TIconTypeWindows); overload;
    procedure AddIcon(const AIconType: TIconTypeMac); overload;
    procedure AddIcon(const AIconType: TIconTypeIPhoneApp); overload;
    procedure AddIcon(const AIconType: TIconTypeIPhoneSpotlight); overload;
    procedure AddIcon(const AIconType: TIconTypeIPadApp); overload;
    procedure AddIcon(const AIconType: TIconTypeIPadSpotlight); overload;
    procedure AddIcon(const AIconType: TIconTypeAndroid); overload;
    function HasIcon(const AIconType: TIconTypeWindows): Boolean; overload;
    function HasIcon(const AIconType: TIconTypeMac): Boolean; overload;
    function HasIcon(const AIconType: TIconTypeIPhoneApp): Boolean; overload;
    function HasIcon(const AIconType: TIconTypeIPhoneSpotlight): Boolean; overload;
    function HasIcon(const AIconType: TIconTypeIPadApp): Boolean; overload;
    function HasIcon(const AIconType: TIconTypeIPadSpotlight): Boolean; overload;
    function HasIcon(const AIconType: TIconTypeAndroid): Boolean; overload;
    /// <summary>
    ///   作成するアイコン一覧の文字列
    /// </summary>
    function ToString: string;
    /// <summary>
    ///   アイコンが空の時はTrue
    /// </summary>
    function IsEmpty: Boolean;
    /// <summary>
    ///   アイコンファイルを作成する
    /// </summary>
    /// <param name="ADirectory">
    ///   ファイルを保存するフォルダー
    /// </param>
    /// <param name="ABitmap">
    ///   元画像
    /// </param>
    procedure CreateFile(const ADirectory, AFileName: string);
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils;

{ TIcon }

class function TIcon.GetFileName(const AIconType: TIconTypeIPhoneApp): string;
begin
  case AIconType of
    TIconTypeIPhoneApp.ispa180x180:
      Result := 'application_180x180.png';
    TIconTypeIPhoneApp.ispa120x120:
      Result := 'application_120x120.png';
    TIconTypeIPhoneApp.ispa114x114:
      Result := 'application_114x114.png';
    TIconTypeIPhoneApp.ispa87x87:
      Result := 'application_87x87.png';
    TIconTypeIPhoneApp.ispa60x60:
      Result := 'application_60x60.png';
    TIconTypeIPhoneApp.ispa57x57:
      Result := 'application_57x57.png';
  end;
end;

class function TIcon.GetFileName(const AIconType: TIconTypeMac): string;
begin
  case AIconType of
    TIconTypeMac.ism1024x1024:
      Result := 'icon_512x512@2x.png';
    TIconTypeMac.ism512x512:
      Result := 'icon_512x512.png';
    TIconTypeMac.ism256x256:
      Result := 'icon_256x256.png';
    TIconTypeMac.ism128x128:
      Result := 'icon_128x128.png';
    TIconTypeMac.ism32x32:
      Result := 'icon_32x32.png';
    TIconTypeMac.ism16x16:
      Result := 'icon_32x32.png';
  end;
end;

class function TIcon.GetFileName(const AIconType: TIconTypeWindows): string;
begin
  case AIconType of
    TIconTypeWindows.isw256x256:
      Result := '256x256.png';
    TIconTypeWindows.isw48x48:
      Result := '48x48.png';
    TIconTypeWindows.isw32x32:
      Result := '32x32.png';
    TIconTypeWindows.isw16x16:
      Result := '16x16.png';
  end;
end;

class function TIcon.GetFileName(const AIconType: TIconTypeIPadSpotlight): string;
begin
  case AIconType of
    TIconTypeIPadSpotlight.isps100x100:
      Result := 'spotlight_100x100.png';
    TIconTypeIPadSpotlight.isps80x80:
      Result := 'spotlight_80x80.png';
    TIconTypeIPadSpotlight.isps58x58:
      Result := 'spotlight_58x58.png';
    TIconTypeIPadSpotlight.isps50x50:
      Result := 'spotlight_50x50.png';
    TIconTypeIPadSpotlight.isps40x40:
      Result := 'spotlight_40x40.png';
    TIconTypeIPadSpotlight.isps29x29:
      Result := 'spotlight_29x29.png';
  end;
end;

class function TIcon.GetFileName(const AIconType: TIconTypeIPadApp): string;
begin
  case AIconType of
    TIconTypeIPadApp.ispa152x152:
      Result := 'application_152x152.png';
    TIconTypeIPadApp.ispa144x144:
      Result := 'application_144x144.png';
    TIconTypeIPadApp.ispa76x76:
      Result := 'application_76x76.png';
    TIconTypeIPadApp.ispa72x72:
      Result := 'application_72x72.png';
  end;
end;

class function TIcon.GetFileName(const AIconType: TIconTypeIPhoneSpotlight): string;
begin
  case AIconType of
    TIconTypeIPhoneSpotlight.isps80x80:
      Result := 'spotlight_80x80.png';
    TIconTypeIPhoneSpotlight.isps58x58:
      Result := 'spotlight_58x68.png';
    TIconTypeIPhoneSpotlight.isps40x40:
      Result := 'spotlight_40x40.png';
    TIconTypeIPhoneSpotlight.isps29x29:
      Result := 'spotlight_29x29.png';
  end;
end;

class function TIcon.Create(const AIconType: TIconTypeIPhoneApp): TIcon;
begin
  Result.FFileName := GetFileName(AIconType);
  Result.FDirectory := DirectoryNameForIPhone;
  case AIconType of
    TIconTypeIPhoneApp.ispa180x180:
      Result.FSize := 180;
    TIconTypeIPhoneApp.ispa120x120:
      Result.FSize := 120;
    TIconTypeIPhoneApp.ispa114x114:
      Result.FSize := 114;
    TIconTypeIPhoneApp.ispa87x87:
      Result.FSize := 87;
    TIconTypeIPhoneApp.ispa60x60:
      Result.FSize := 60;
    TIconTypeIPhoneApp.ispa57x57:
      Result.FSize := 75;
  end;
end;

class function TIcon.Create(const AIconType: TIconTypeMac): TIcon;
begin
  Result.FFileName := GetFileName(AIconType);
  Result.FDirectory := DirectoryNameForMac;
  case AIconType of
    TIconTypeMac.ism1024x1024:
      Result.FSize := 1024;
    TIconTypeMac.ism512x512:
      Result.FSize := 512;
    TIconTypeMac.ism256x256:
      Result.FSize := 256;
    TIconTypeMac.ism128x128:
      Result.FSize := 128;
    TIconTypeMac.ism32x32:
      Result.FSize := 32;
    TIconTypeMac.ism16x16:
      Result.FSize := 16;
  end;
end;

class function TIcon.Create(const AIconType: TIconTypeWindows): TIcon;
begin
  Result.FFileName := GetFileName(AIconType);
  Result.FDirectory := DirectoryNameForWindows;
  case AIconType of
    TIconTypeWindows.isw256x256:
      Result.FSize := 256;
    TIconTypeWindows.isw48x48:
      Result.FSize := 48;
    TIconTypeWindows.isw32x32:
      Result.FSize := 32;
    TIconTypeWindows.isw16x16:
      Result.FSize := 16;
  end;
end;

class function TIcon.Create(const AIconType: TIconTypeIPhoneSpotlight): TIcon;
begin
  Result.FFileName := GetFileName(AIconType);
  Result.FDirectory := DirectoryNameForIPhone;
  case AIconType of
    TIconTypeIPhoneSpotlight.isps80x80:
      Result.FSize := 80;
    TIconTypeIPhoneSpotlight.isps58x58:
      Result.FSize := 58;
    TIconTypeIPhoneSpotlight.isps40x40:
      Result.FSize := 40;
    TIconTypeIPhoneSpotlight.isps29x29:
      Result.FSize := 29;
  end;
end;

function TIcon.CanCreate(const ASize: Integer): Boolean;
begin
  Result := (Self.FSize <= ASize);
end;

class function TIcon.Create(const AIconType: TIconTypeAndroid): TIcon;
begin
  Result.FFileName := GetFileName(AIconType);
  Result.FDirectory := DirectoryNameForAndroid;
  case AIconType of
    TIconTypeAndroid.isa36x36:
      Result.FSize := 36;
    TIconTypeAndroid.isa48x48:
      Result.FSize := 48;
    TIconTypeAndroid.isa72x72:
      Result.FSize := 72;
    TIconTypeAndroid.isa96x96:
      Result.FSize := 96;
    TIconTypeAndroid.isa144x144:
      Result.FSize := 144;
  end;
end;

class function TIcon.Create(const AIconType: TIconTypeIPadSpotlight): TIcon;
begin
  Result.FFileName := GetFileName(AIconType);
  Result.FDirectory := DirectoryNameForIPad;
  case AIconType of
    TIconTypeIPadSpotlight.isps100x100:
      Result.FSize := 100;
    TIconTypeIPadSpotlight.isps80x80:
      Result.FSize := 80;
    TIconTypeIPadSpotlight.isps58x58:
      Result.FSize := 58;
    TIconTypeIPadSpotlight.isps50x50:
      Result.FSize := 50;
    TIconTypeIPadSpotlight.isps40x40:
      Result.FSize := 40;
    TIconTypeIPadSpotlight.isps29x29:
      Result.FSize := 29;
  end;
end;

class function TIcon.Create(const AIconType: TIconTypeIPadApp): TIcon;
begin
  Result.FFileName := GetFileName(AIconType);
  Result.FDirectory := DirectoryNameForIPad;
  case AIconType of
    TIconTypeIPadApp.ispa152x152:
      Result.FSize := 152;
    TIconTypeIPadApp.ispa144x144:
      Result.FSize := 144;
    TIconTypeIPadApp.ispa76x76:
      Result.FSize := 76;
    TIconTypeIPadApp.ispa72x72:
      Result.FSize := 72;
  end;
end;

class function TIcon.DirectoryNameForAndroid: string;
begin
  Result := 'android';
end;

class function TIcon.DirectoryNameForIPad: string;
begin
  Result := 'iPad';
end;

class function TIcon.DirectoryNameForIPhone: string;
begin
  Result := 'iPhone';
end;

class function TIcon.DirectoryNameForMac: string;
begin
  Result := 'mac';
end;

class function TIcon.DirectoryNameForWindows: string;
begin
  Result := 'windows';
end;

class function TIcon.GetCaption(const AIconType: TIconTypeIPhoneApp): string;
begin
  case AIconType of
    TIconTypeIPhoneApp.ispa180x180: Result := '180x180';
    TIconTypeIPhoneApp.ispa120x120: Result := '120x120';
    TIconTypeIPhoneApp.ispa114x114: Result := '114x114';
    TIconTypeIPhoneApp.ispa87x87: Result := '87x87';
    TIconTypeIPhoneApp.ispa60x60: Result := '60x60';
    TIconTypeIPhoneApp.ispa57x57: Result := '57x57';
  end;
end;

class function TIcon.GetCaption(const AIconType: TIconTypeMac): string;
begin
  case AIconType of
    TIconTypeMac.ism1024x1024: Result := '1024x1024';
    TIconTypeMac.ism512x512:  Result := '512x512';
    TIconTypeMac.ism256x256:  Result := '256x256';
    TIconTypeMac.ism128x128:  Result := '128x128';
    TIconTypeMac.ism32x32:  Result := '32x32';
    TIconTypeMac.ism16x16:  Result := '16x16';
  end;
end;

class function TIcon.GetCaption(const AIconType: TIconTypeWindows): string;
begin
  case AIconType of
    TIconTypeWindows.isw256x256: Result := '256x256';
    TIconTypeWindows.isw48x48: Result := '48x48';
    TIconTypeWindows.isw32x32: Result := '32x32';
    TIconTypeWindows.isw16x16: Result := '16x16';
  end;
end;

class function TIcon.GetCaption(
  const AIconType: TIconTypeIPhoneSpotlight): string;
begin
  case AIconType of
    TIconTypeIPhoneSpotlight.isps80x80: Result := '80x80';
    TIconTypeIPhoneSpotlight.isps58x58: Result := '58x58';
    TIconTypeIPhoneSpotlight.isps40x40: Result := '40x40';
    TIconTypeIPhoneSpotlight.isps29x29: Result := '29x29';
  end;
end;

class function TIcon.GetCaption(const AIconType: TIconTypeAndroid): string;
begin
  case AIconType of
    TIconTypeAndroid.isa36x36: Result := '36x36';
    TIconTypeAndroid.isa48x48: Result := '48x48';
    TIconTypeAndroid.isa72x72: Result := '72x72';
    TIconTypeAndroid.isa96x96: Result := '96x96';
    TIconTypeAndroid.isa144x144: Result := '144x144';
  end;
end;

class function TIcon.GetCaption(
  const AIconType: TIconTypeIPadSpotlight): string;
begin
  case AIconType of
    TIconTypeIPadSpotlight.isps100x100: Result := '100x100';
    TIconTypeIPadSpotlight.isps80x80: Result := '80x80';
    TIconTypeIPadSpotlight.isps58x58: Result := '58x58';
    TIconTypeIPadSpotlight.isps50x50: Result := '50x50';
    TIconTypeIPadSpotlight.isps40x40: Result := '40x40';
    TIconTypeIPadSpotlight.isps29x29: Result := '29x29';
  end;
end;

class function TIcon.GetCaption(const AIconType: TIconTypeIPadApp): string;
begin
  case AIconType of
    TIconTypeIPadApp.ispa152x152: Result := '152x152';
    TIconTypeIPadApp.ispa144x144: Result := '144x144';
    TIconTypeIPadApp.ispa76x76: Result := '76x76';
    TIconTypeIPadApp.ispa72x72: Result := '72x72';
  end;
end;

class function TIcon.GetFileName(const AIconType: TIconTypeAndroid): string;
begin
  case AIconType of
    TIconTypeAndroid.isa36x36:
      Result := 'LauncherIcon_36x36.png';
    TIconTypeAndroid.isa48x48:
      Result := 'LauncherIcon_48x48.png';
    TIconTypeAndroid.isa72x72:
      Result := 'LauncherIcon_72x72.png';
    TIconTypeAndroid.isa96x96:
      Result := 'LauncherIcon_96x96.png';
    TIconTypeAndroid.isa144x144:
      Result := 'LauncherIcon_144x144.png';
  end;
end;

procedure TIcon.SaveToFile(const ADirectory, AFileName: string);
var
  Dir: string;
  Bitmap: TBitmap;
  Path: string;
begin
  Dir := TPath.Combine(ADirectory, Self.FDirectory);
  if not TDirectory.Exists(Dir) then
    System.SysUtils.ForceDirectories(Dir);
  Path := TPath.Combine(Dir, Self.FFileName);

  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadThumbnailFromFile(AFileName, Self.FSize, Self.FSize);
    Bitmap.SaveToFile(Path);
  finally
    Bitmap.Free;
  end;
end;

{ TIconList }

procedure TIconList.AddIcon(const AIconType: TIconTypeIPhoneApp);
begin
  Include(Self.FIconTypeSetIPhoneApp, AIconType);
end;

procedure TIconList.AddIcon(const AIconType: TIconTypeMac);
begin
  Include(Self.FIconTypeSetMac, AIconType);
end;

procedure TIconList.AddIcon(const AIconType: TIconTypeWindows);
begin
  Include(Self.FIconTypeSetWindows, AIconType);
end;

procedure TIconList.AddIcon(const AIconType: TIconTypeIPhoneSpotlight);
begin
  Include(Self.FIconTypeSetIPhoneSpotlight, AIconType);
end;

procedure TIconList.AddIcon(const AIconType: TIconTypeAndroid);
begin
  Include(Self.FIconTypeSetAndroid, AIconType);
end;

procedure TIconList.AddIcon(const AIconType: TIconTypeIPadSpotlight);
begin
  Include(Self.FIconTypeSetIPadSpotlight, AIconType);
end;

procedure TIconList.AddIcon(const AIconType: TIconTypeIPadApp);
begin
  Include(Self.FIconTypeSetIPadApp, AIconType);
end;

class function TIconList.Create(const ASize: Integer): TIconList;
var
  IconTypeWindows: TIconTypeWindows;
  IconTypeMac: TIconTypeMac;
  IconTypeIPhoneApp: TIconTypeIPhoneApp;
  IconTypeIPhoneSpotlight: TIconTypeIPhoneSpotlight;
  IconTypeIPadApp: TIconTypeIPadApp;
  IconTypeIPadSpotlight: TIconTypeIPadSpotlight;
  IconTypeAndroid: TIconTypeAndroid;
begin
  for IconTypeWindows in ICON_TYPE_Windows do
  begin
    if TIcon.Create(IconTypeWindows).CanCreate(ASize) then
      Include(Result.FIconTypeSetWindows, IconTypeWindows);
  end;
  for IconTypeMac in ICON_TYPE_Mac do
  begin
    if TIcon.Create(IconTypeMac).CanCreate(ASize) then
      Include(Result.FIconTypeSetMac, IconTypeMac);
  end;
  for IconTypeIPhoneApp in ICON_TYPE_IPhoneApp do
  begin
    if TIcon.Create(IconTypeIPhoneApp).CanCreate(ASize) then
      Include(Result.FIconTypeSetIPhoneApp, IconTypeIPhoneApp);
  end;
  for IconTypeIPhoneSpotlight in ICON_TYPE_IPhoneSpotlight do
  begin
    if TIcon.Create(IconTypeIPhoneSpotlight).CanCreate(ASize) then
      Include(Result.FIconTypeSetIPhoneSpotlight, IconTypeIPhoneSpotlight);
  end;
  for IconTypeIPadApp in ICON_TYPE_IPadApp do
  begin
    if TIcon.Create(IconTypeIPadApp).CanCreate(ASize) then
      Include(Result.FIconTypeSetIPadApp, IconTypeIPadApp);
  end;
  for IconTypeIPadSpotlight in ICON_TYPE_IPadSpotlight do
  begin
    if TIcon.Create(IconTypeIPadSpotlight).CanCreate(ASize) then
      Include(Result.FIconTypeSetIPadSpotlight, IconTypeIPadSpotlight);
  end;
  for IconTypeAndroid in ICON_TYPE_Android do
  begin
    if TIcon.Create(IconTypeAndroid).CanCreate(ASize) then
      Include(Result.FIconTypeSetAndroid, IconTypeAndroid);
  end;
end;

class function TIconList.CreateEmpty: TIconList;
begin
  Result.FIconTypeSetWindows:=[];
  Result.FIconTypeSetMac:=[];
  Result.FIconTypeSetIPhoneApp:=[];
  Result.FIconTypeSetIPhoneSpotlight:=[];
  Result.FIconTypeSetIPadApp:=[];
  Result.FIconTypeSetIPadSpotlight:=[];
  Result.FIconTypeSetAndroid:=[];
end;

procedure TIconList.CreateFile(const ADirectory, AFileName: string);
var
  IconWindows: TIconTypeWindows;
  IconMac: TIconTypeMac;
  IconIPhoneApp: TIconTypeIPhoneApp;
  IconIPhoneSpotlight: TIconTypeIPhoneSpotlight;
  IconIPadApp: TIconTypeIPadApp;
  IconIPadSpotlight: TIconTypeIPadSpotlight;
  IconAndroid: TIconTypeAndroid;
begin
  for IconWindows in ICON_TYPE_Windows do
  begin
    if IconWindows in Self.FIconTypeSetWindows then
    begin
      TIcon.Create(IconWindows).SaveToFile(ADirectory, AFileName);
    end;
  end;
  for IconMac in ICON_TYPE_Mac do
  begin
    if IconMac in Self.FIconTypeSetMac then
    begin
      TIcon.Create(IconMac).SaveToFile(ADirectory, AFileName);
    end;
  end;
  for IconIPhoneApp in ICON_TYPE_IPhoneApp do
  begin
    if IconIPhoneApp in Self.FIconTypeSetIPhoneApp then
    begin
      TIcon.Create(IconIPhoneApp).SaveToFile(ADirectory, AFileName);
    end;
  end;
  for IconIPhoneSpotlight in ICON_TYPE_IPhoneSpotlight do
  begin
    if IconIPhoneSpotlight in Self.FIconTypeSetIPhoneSpotlight then
    begin
      TIcon.Create(IconIPhoneSpotlight).SaveToFile(ADirectory, AFileName);
    end;
  end;
  for IconIPadApp in ICON_TYPE_IPadApp do
  begin
    if IconIPadApp in Self.FIconTypeSetIPadApp then
    begin
      TIcon.Create(IconIPadApp).SaveToFile(ADirectory, AFileName);
    end;
  end;
  for IconIPadSpotlight in ICON_TYPE_IPadSpotlight do
  begin
    if IconIPadSpotlight in Self.FIconTypeSetIPadSpotlight then
    begin
      TIcon.Create(IconIPadSpotlight).SaveToFile(ADirectory, AFileName);
    end;
  end;
  for IconAndroid in ICON_TYPE_Android do
  begin
    if IconAndroid in Self.FIconTypeSetAndroid then
    begin
      TIcon.Create(IconAndroid).SaveToFile(ADirectory, AFileName);
    end;
  end;
end;

function TIconList.HasIcon(const AIconType: TIconTypeIPhoneApp): Boolean;
begin
  Result := AIconType in Self.FIconTypeSetIPhoneApp;
end;

function TIconList.HasIcon(const AIconType: TIconTypeMac): Boolean;
begin
  Result := AIconType in Self.FIconTypeSetMac;
end;

function TIconList.HasIcon(const AIconType: TIconTypeWindows): Boolean;
begin
  Result := AIconType in Self.FIconTypeSetWindows;
end;

function TIconList.HasIcon(const AIconType: TIconTypeIPhoneSpotlight): Boolean;
begin
  Result := AIconType in Self.FIconTypeSetIPhoneSpotlight;
end;

function TIconList.HasIcon(const AIconType: TIconTypeAndroid): Boolean;
begin
  Result := AIconType in Self.FIconTypeSetAndroid;
end;

function TIconList.IsEmpty: Boolean;
begin
  Result := FIconTypeSetWindows.IsEmpty and
            FIconTypeSetMac.IsEmpty and
            FIconTypeSetIPhoneApp.IsEmpty and
            FIconTypeSetIPhoneSpotlight.IsEmpty and
            FIconTypeSetIPadApp.IsEmpty and
            FIconTypeSetIPadSpotlight.IsEmpty and
            FIconTypeSetAndroid.IsEmpty;
end;

function TIconList.ToString: string;
begin
  Result := FIconTypeSetWindows.ToString +
            FIconTypeSetMac.ToString +
            FIconTypeSetIPhoneApp.ToString +
            FIconTypeSetIPhoneSpotlight.ToString +
            FIconTypeSetIPadApp.ToString +
            FIconTypeSetIPadSpotlight.ToString +
            FIconTypeSetAndroid.ToString;
end;

function TIconList.HasIcon(const AIconType: TIconTypeIPadSpotlight): Boolean;
begin
  Result := AIconType in Self.FIconTypeSetIPadSpotlight;
end;

function TIconList.HasIcon(const AIconType: TIconTypeIPadApp): Boolean;
begin
  Result := AIconType in Self.FIconTypeSetIPadApp;
end;


{ TIconTypeSetWindowsHelper }

function TIconTypeSetWindowsHelper.IsEmpty: Boolean;
var
  IconType: TIconTypeWindows;
begin
  for IconType in TIconList.ICON_TYPE_Windows do
  begin
    if IconType in Self then
      Exit(False);
  end;
  Result := True;
end;

function TIconTypeSetWindowsHelper.ToString: string;
var
  SL: TStringList;
  IconType: TIconTypeWindows;
begin
  SL := TStringList.Create;
  try
    for IconType in TIconList.ICON_TYPE_Windows do
    begin
      if IconType in Self then
        SL.Add(TIcon.GetCaption(IconType));
    end;
    if SL.Count > 0 then
    begin
      SL.Insert(0, '[Windows]');
      Result := SL.Text;
    end;
  finally
    SL.Free;
  end;
end;

{ TIconTypeSetMacHelper }

function TIconTypeSetMacHelper.IsEmpty: Boolean;
var
  IconType: TIconTypeMac;
begin
  for IconType in TIconList.ICON_TYPE_Mac do
  begin
    if IconType in Self then
      Exit(False);
  end;
  Result := True;
end;

function TIconTypeSetMacHelper.ToString: string;
var
  SL: TStringList;
  IconType: TIconTypeMac;
begin
  SL := TStringList.Create;
  try
    for IconType in TIconList.ICON_TYPE_Mac do
    begin
      if IconType in Self then
        SL.Add(TIcon.GetCaption(IconType));
    end;
    if SL.Count > 0 then
    begin
      SL.Insert(0, '[Mac OSX]');
      Result := SL.Text;
    end;
  finally
    SL.Free;
  end;
end;

{ TIconTypeSetIPhoneAppHelper }

function TIconTypeSetIPhoneAppHelper.IsEmpty: Boolean;
var
  IconType: TIconTypeIPhoneApp;
begin
  for IconType in TIconList.ICON_TYPE_IPhoneApp do
  begin
    if IconType in Self then
      Exit(False);
  end;
  Result := True;
end;

function TIconTypeSetIPhoneAppHelper.ToString: string;
var
  SL: TStringList;
  IconType: TIconTypeIPhoneApp;
begin
  SL := TStringList.Create;
  try
    for IconType in TIconList.ICON_TYPE_IPhoneApp do
    begin
      if IconType in Self then
        SL.Add(TIcon.GetCaption(IconType));
    end;
    if SL.Count > 0 then
    begin
      SL.Insert(0, '[iPhone アプリケーションアイコン]');
      Result := SL.Text;
    end;
  finally
    SL.Free;
  end;
end;

{ TIconTypeSetIPhoneSpotlightHelper }

function TIconTypeSetIPhoneSpotlightHelper.IsEmpty: Boolean;
var
  IconType: TIconTypeIPhoneSpotlight;
begin
  for IconType in TIconList.ICON_TYPE_IPhoneSpotlight do
  begin
    if IconType in Self then
      Exit(False);
  end;
  Result := True;
end;

function TIconTypeSetIPhoneSpotlightHelper.ToString: string;
var
  SL: TStringList;
  IconType: TIconTypeIPhoneSpotlight;
begin
  SL := TStringList.Create;
  try
    for IconType in TIconList.ICON_TYPE_IPhoneSpotlight do
    begin
      if IconType in Self then
        SL.Add(TIcon.GetCaption(IconType));
    end;
    if SL.Count > 0 then
    begin
      SL.Insert(0, '[iPhone スポットライト検索アイコン]');
      Result := SL.Text;
    end;
  finally
    SL.Free;
  end;
end;

{ TIconTypeSetIPadAppHelper }

function TIconTypeSetIPadAppHelper.IsEmpty: Boolean;
var
  IconType: TIconTypeIPadApp;
begin
  for IconType in TIconList.ICON_TYPE_IPadApp do
  begin
    if IconType in Self then
      Exit(False);
  end;
  Result := True;
end;

function TIconTypeSetIPadAppHelper.ToString: string;
var
  SL: TStringList;
  IconType: TIconTypeIPadApp;
begin
  SL := TStringList.Create;
  try
    for IconType in TIconList.ICON_TYPE_IPadApp do
    begin
      if IconType in Self then
        SL.Add(TIcon.GetCaption(IconType));
    end;
    if SL.Count > 0 then
    begin
      SL.Insert(0, '[iPad アプリケーションアイコン]');
      Result := SL.Text;
    end;
  finally
    SL.Free;
  end;
end;

{ TIconTypeSetIPadSpotlightHelper }

function TIconTypeSetIPadSpotlightHelper.IsEmpty: Boolean;
var
  IconType: TIconTypeIPadSpotlight;
begin
  for IconType in TIconList.ICON_TYPE_IPadSpotlight do
  begin
    if IconType in Self then
      Exit(False);
  end;
  Result := True;
end;

function TIconTypeSetIPadSpotlightHelper.ToString: string;
var
  SL: TStringList;
  IconType: TIconTypeIPadSpotlight;
begin
  SL := TStringList.Create;
  try
    for IconType in TIconList.ICON_TYPE_IPadSpotlight do
    begin
      if IconType in Self then
        SL.Add(TIcon.GetCaption(IconType));
    end;
    if SL.Count > 0 then
    begin
      SL.Insert(0, '[iPad スポットライト検索アイコン]');
      Result := SL.Text;
    end;
  finally
    SL.Free;
  end;
end;

{ TIconTypeSetAndroidHelper }

function TIconTypeSetAndroidHelper.IsEmpty: Boolean;
var
  IconType: TIconTypeAndroid;
begin
  for IconType in TIconList.ICON_TYPE_Android do
  begin
    if IconType in Self then
      Exit(False);
  end;
  Result := True;
end;

function TIconTypeSetAndroidHelper.ToString: string;
var
  SL: TStringList;
  IconType: TIconTypeAndroid;
begin
  SL := TStringList.Create;
  try
    for IconType in TIconList.ICON_TYPE_Android do
    begin
      if IconType in Self then
        SL.Add(TIcon.GetCaption(IconType));
    end;
    if SL.Count > 0 then
    begin
      SL.Insert(0, '[Android ランチャアイコン]');
      Result := SL.Text;
    end;
  finally
    SL.Free;
  end;
end;

end.
