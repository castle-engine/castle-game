{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "castle".

  "castle" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ TLevelAvailable class and related things. }
unit GameLevelAvailable;

{$I castleconf.inc}

interface

uses GameLevel, CastleUtils, Classes, CastleClassUtils, DOM, GL, GLU,
  CastleGLUtils, CastleProgress, GameObjectKinds, FGL;

type
  { }
  TLevelAvailable = class
  private
    { We keep Document reference through lifetime of this object,
      because actual TLevel instance also reads some stuff from it. }
    Document: TXMLDocument;
    DocumentBasePath: string;
    procedure LoadFromDocument;
  public
    constructor Create;
    destructor Destroy; override;

  public
    AvailableForNewGame: boolean;
    DefaultAvailableForNewGame: boolean;

    { Class to use when constructing this level. }
    LevelClass: TLevelClass;

    { These will be passed to TLevel constructor --- see appropriate TLevel
      properties for description. }
    Id: string;
    SceneFileName: string;
    Title: string;
    TitleHint: string;
    Number: Integer;

    LoadingBarYPosition: Single;

    { Loading level background image (in OpenGL list), 0 if none. }
    GLList_LoadingBgImage: TGLuint;

    Element: TDOMElement;

    Resources: T3DResourceList;

    Demo: boolean;

    function LoadLevel(MenuBackground: boolean = false): TGameSceneManager;
  end;

  TLevelAvailableList = class(specialize TFPGObjectList<TLevelAvailable>)
  private
    procedure LoadIndexXml(const FileName: string);
  public
    { raises Exception if such Id is not on the list. }
    function FindId(const AId: string): TLevelAvailable;

    procedure SortByNumber;

    { This loads and saves AvailableForNewGame properties of every item. }
    procedure LoadFromConfig;
    procedure SaveToConfig;

    { Fill this list with items read from levels/index.xml.

      All AvailableForNewGame are initially set to @false
      (LoadFromConfig will set them to something read from user's
      config file or to DefaultAvailableForNewGame if nothing in user's
      config file).

      This can be done only once Window is open,
      because loading levels requires knowledge of window size
      (to scale level loading background images).
      Also, this can be done only once creatures and items resources are known,
      as they may be referenced in levels XML files. }
    procedure LoadFromFiles;
  end;

var
  { List of all available levels.
    This has all the information needed to present user a list of levels,
    and to actually load a given level (create suitable TLevel instance).
    Created in initialization of this unit, destroyed in finalization.
    Owns it's Items. }
  LevelsAvailable: TLevelAvailableList;

implementation

uses SysUtils, GameConfig, CastleXMLUtils, CastleFilesUtils,
  XMLRead, GameWindow, GLImages, GameItems,
  Images, CastleWindow, WindowModes, UIControls;

{ TLevelAvailable ------------------------------------------------------------ }

constructor TLevelAvailable.Create;
begin
  inherited;
  Resources := T3DResourceList.Create(false);
end;

destructor TLevelAvailable.Destroy;
begin
  FreeAndNil(Document);
  FreeAndNil(Resources);

  { Thanks to WindowClose implementation, we can be sure that gl context
    is active now, so it's not a problem to call glFreeDisplayList now. }

  glFreeDisplayList(GLList_LoadingBgImage);
  inherited;
end;

procedure TLevelAvailable.LoadFromDocument;

  procedure MissingRequiredAttribute(const AttrName: string);
  begin
    raise Exception.CreateFmt(
      'Missing required attribute "%s" of <level> element', [AttrName]);
  end;

  { Like DOMGetAttribute, but reads TLevelClass value. }
  function DOMGetLevelClassAttribute(const Element: TDOMElement;
    const AttrName: string; var Value: TLevelClass): boolean;
  var
    ValueStr: string;
    LevelClassIndex: Integer;
  begin
    Result := DOMGetAttribute(Element, AttrName, ValueStr);
    LevelClassIndex := LevelClasses.IndexOf(ValueStr);
    if LevelClassIndex <> -1 then
      Value := LevelClasses.Data[LevelClassIndex] else
      raise Exception.CreateFmt('Unknown level type "%s"', [ValueStr]);
  end;

  { Add all item kinds (TItemKind from AllResources) to the Resources.
    We use this now, as all levels should prepare all items -- always.
    This is easier, as player may pick/drop any item on any level,
    so it's best to have all items prepared. }
  procedure AddItems(Resources: T3DResourceList);
  var
    I: Integer;
  begin
    for I := 0 to AllResources.Count - 1 do
      if (AllResources[I] is TItemKind) and
         (Resources.IndexOf(AllResources[I]) = -1) then
      Resources.Add(AllResources[I]);
  end;

var
  LoadingBgFileName: string;
begin
  Element := Document.DocumentElement;

  if Element.TagName <> 'level' then
    raise Exception.CreateFmt('Root node of levels/*/index.xml file must be <level>, but is "%s", in index.xml inside "%s"',
      [Element.TagName, DocumentBasePath]);

  { Required atttributes }

  if not DOMGetAttribute(Element, 'id', Id) then
    MissingRequiredAttribute('id');

  if not DOMGetAttribute(Element, 'scene_file_name', SceneFileName) then
    MissingRequiredAttribute('scene_file_name');
  SceneFileName := CombinePaths(DocumentBasePath, SceneFileName);

  if not DOMGetAttribute(Element, 'title', Title) then
    MissingRequiredAttribute('title');

  if not DOMGetIntegerAttribute(Element, 'number', Number) then
    MissingRequiredAttribute('number');

  { Optional attributes }

  if not DOMGetBooleanAttribute(Element, 'demo', Demo) then
    Demo := false;

  if not DOMGetAttribute(Element, 'title_hint', TitleHint) then
    TitleHint := '';

  if not DOMGetBooleanAttribute(Element, 'default_available_for_new_game',
    DefaultAvailableForNewGame) then
    DefaultAvailableForNewGame := false;

  if not DOMGetLevelClassAttribute(Element, 'type', LevelClass) then
    LevelClass := TLevel;

  if DOMGetAttribute(Element, 'loading_bg', LoadingBgFileName) then
  begin
    LoadingBgFileName := CombinePaths(DocumentBasePath, LoadingBgFileName);
    GLList_LoadingBgImage := LoadImageToDisplayList(LoadingBgFileName,
      [TRGBImage], [], Window.Width, Window.Height);
  end else
    glFreeDisplayList(GLList_LoadingBgImage);

  if not DOMGetSingleAttribute(Element, 'loading_bar_y_position',
    LoadingBarYPosition) then
    LoadingBarYPosition := DefaultBarYPosition;

  Resources.LoadResources(Element);
  AddItems(Resources);
end;

procedure DrawLoadLevel(Window: TCastleWindowBase);
begin
  glClear(GL_COLOR_BUFFER_BIT);
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glCallList(TLevelAvailable(Window.UserData).GLList_LoadingBgImage);
  glPopAttrib;
end;

function TLevelAvailable.LoadLevel(MenuBackground: boolean): TGameSceneManager;

  procedure LoadLevelCore;
  begin
    Result := TGameSceneManager.Create(Id, SceneFileName,
      Title, TitleHint, Number, Element, Resources,
      MenuBackground, LevelClass);
    if not MenuBackground then
      AvailableForNewGame := true;
  end;

  procedure LoadLevelNoBackground;
  var
    SavedBarYPosition: Single;
  begin
    SavedBarYPosition := WindowProgressInterface.BarYPosition;
    WindowProgressInterface.BarYPosition := LoadingBarYPosition;
    try
      LoadLevelCore;
    finally
      WindowProgressInterface.BarYPosition := SavedBarYPosition;
    end;
  end;

  procedure SleepIfNeeded;
  begin
    Sleep(2000);
  end;

var
  SavedMode: TGLMode;
begin
  if GLList_LoadingBgImage <> 0 then
  begin
    SavedMode := TGLMode.CreateReset(Window, 0, true,
      @DrawLoadLevel, @Resize2D, @NoClose);
    try
      Window.UserData := Self;

      Window.OnDrawStyle := ds3D;

      Window.EventResize;

      LoadLevelNoBackground;

      { Just a dirty hack to make user to actually see the
        "DOOM E1M1" loading background --- otherwise loading of doom
        level goes so fast that it's practically not noticeable.
        Maybe in the future I'll remove this hack (if level loading
        will be slow enough to actually make time for user to see
        background image), or make this hack more intelligent
        (like checking how much time was spent inside LoadLevelNoBackground
        and doing Sleep() only if necessary). }
      SleepIfNeeded;

    finally FreeAndNil(SavedMode) end;
  end else
    LoadLevelNoBackground;
end;

{ TLevelAvailableList ------------------------------------------------------- }

function TLevelAvailableList.FindId(const AId: string): TLevelAvailable;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Id = AId then
      Exit(Items[I]);

  raise Exception.CreateFmt(
    'Level id "%s" not found on LevelsAvailable list', [AId]);
end;

function IsSmallerByNumber(const A, B: TLevelAvailable): Integer;
begin
  Result := A.Number - B.Number;
end;

procedure TLevelAvailableList.SortByNumber;
begin
  Sort(@IsSmallerByNumber);
end;

procedure TLevelAvailableList.LoadFromConfig;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].AvailableForNewGame := ConfigFile.GetValue(
      'levels_available/' + Items[I].Id,
      Items[I].DefaultAvailableForNewGame);
end;

procedure TLevelAvailableList.SaveToConfig;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ConfigFile.SetDeleteValue(
      'levels_available/' + Items[I].Id,
      Items[I].AvailableForNewGame,
      Items[I].DefaultAvailableForNewGame);
end;

procedure TLevelAvailableList.LoadIndexXml(const FileName: string);
var
  NewLevelAvailable: TLevelAvailable;
begin
  NewLevelAvailable := TLevelAvailable.Create;
  Add(NewLevelAvailable);
  NewLevelAvailable.AvailableForNewGame := false;

  ReadXMLFile(NewLevelAvailable.Document, FileName);
  NewLevelAvailable.DocumentBasePath := ExtractFilePath(FileName);
  NewLevelAvailable.LoadFromDocument;
end;

procedure TLevelAvailableList.LoadFromFiles;
begin
  ScanForFiles(ProgramDataPath + 'data' +  PathDelim + 'levels', 'index.xml', @LoadIndexXml);
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowOpen(Window: TCastleWindowBase);
begin
end;

procedure WindowClose(Window: TCastleWindowBase);
begin
  if LevelsAvailable <> nil then
  begin
    LevelsAvailable.SaveToConfig;
    FreeAndNil(LevelsAvailable);
  end;
end;

initialization
  LevelsAvailable := TLevelAvailableList.Create(true);
  Window.OnOpenList.Add(@WindowOpen);
  Window.OnCloseList.Add(@WindowClose);
finalization
  { Call WindowClose in case OnClose event would happen after finalization
    of this unit. }
  WindowClose(Window);
end.