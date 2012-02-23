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

uses GameLevel, CastleUtils, Classes,
  CastleClassUtils, DOM, GL, GLU, CastleGLUtils, CastleProgress,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

type
  { }
  TLevelAvailable = class
  private
    procedure LoadFromDOMElement(Element: TDOMElement; const BasePath: string);
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
    Name: string;
    SceneFileName: string;
    Title: string;
    TitleHint: string;
    Number: Integer;

    LoadingBarYPosition: Single;

    { Loading level background image (in OpenGL list), 0 if none. }
    GLList_LoadingBgImage: TGLuint;

    LevelDOMElement: TDOMElement;

    RequiredCreatures: TStringList;

    Demo: boolean;

    function CreateLevel(MenuBackground: boolean = false): TLevel;
  end;

  TLevelAvailableList = class(specialize TFPGObjectList<TLevelAvailable>)
  private
    { We keep Document reference through lifetime of this object,
      because each TLevelAvailable instance needs a reference to
      LevelDOMElement (this is parsed during TLevel.Create). }
    Document: TXMLDocument;
    FMenuBackgroundLevelName: string;
  public
    destructor Destroy; override;

    { raises Exception if such Name is not on the list. }
    function FindName(const AName: string): TLevelAvailable;

    procedure SortByNumber;

    { This loads and saves AvailableForNewGame properties of every item. }
    procedure LoadFromConfig;
    procedure SaveToConfig;

    { This fills this list with items read from levels/index.xml.

      All AvailableForNewGame are initially set to @false
      (LoadFromConfig will set them to something read from user's
      config file or to DefaultAvailableForNewGame if nothing in user's
      config file). }
    procedure LoadFromFile;

    property MenuBackgroundLevelName: string read FMenuBackgroundLevelName write FMenuBackgroundLevelName;
  end;

var
  { List of all available TLevel classes, along with information
    whether they are allowed to be placed in "New Game" levels.
    Created in initialization of this unit, destroyed in finalization.
    Owns it's Items. }
  LevelsAvailable: TLevelAvailableList;

implementation

uses SysUtils, GameConfig, CastleXMLUtils, CastleFilesUtils,
  GameLevelSpecific, XMLRead, GameWindow, GLImages,
  Images, CastleWindow, WindowModes, UIControls,
  GameRequiredResources;

{ TLevelAvailable ------------------------------------------------------------ }

constructor TLevelAvailable.Create;
begin
  inherited;
  RequiredCreatures := TStringList.Create;
end;

destructor TLevelAvailable.Destroy;
begin
  FreeAndNil(RequiredCreatures);

  { Thanks to WindowClose implementation, we can be sure that gl context
    is active now, so it's not a problem to call glFreeDisplayList now. }

  glFreeDisplayList(GLList_LoadingBgImage);
  inherited;
end;

procedure TLevelAvailable.LoadFromDOMElement(Element: TDOMElement;
  const BasePath: string);

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
  begin
    Result := DOMGetAttribute(Element, AttrName, ValueStr);
    if Result then
    begin
      { TODO: I would like to use RTTI here.
        Also GameLevelSpecific will be removed from uses clause then. }
      if ValueStr = 'TLevel' then
        Value := TLevel else
      if ValueStr = 'TCagesLevel' then
        Value := TCagesLevel else
      if ValueStr = 'TGateLevel' then
        Value := TGateLevel else
      if ValueStr = 'TGateBackgroundLevel' then
        Value := TGateBackgroundLevel else
      if ValueStr = 'TCastleHallLevel' then
        Value := TCastleHallLevel else
      if ValueStr = 'TDoomE1M1Level' then
        Value := TDoomE1M1Level else
      if ValueStr = 'TTowerLevel' then
        Value := TTowerLevel else
      if ValueStr = 'TFountainLevel' then
        Value := TFountainLevel else
        raise Exception.CreateFmt('Unknown level class "%s"', [ValueStr]);
    end;
  end;

var
  LoadingBgFileName: string;
begin
  Check(Element.TagName = 'level',
    'Each child of levels/index.xml root node must be the <level> element');

  LevelDOMElement := Element;

  { Required atttributes }

  if not DOMGetAttribute(Element, 'name', Name) then
    MissingRequiredAttribute('name');

  if not DOMGetAttribute(Element, 'scene_file_name', SceneFileName) then
    MissingRequiredAttribute('scene_file_name');
  SceneFileName := CombinePaths(BasePath, SceneFileName);

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

  if not DOMGetLevelClassAttribute(Element, 'implementation_class',
    LevelClass) then
    LevelClass := TLevel;

  if DOMGetAttribute(Element, 'loading_bg', LoadingBgFileName) then
  begin
    LoadingBgFileName := CombinePaths(BasePath, LoadingBgFileName);
    GLList_LoadingBgImage := LoadImageToDisplayList(LoadingBgFileName,
      [TRGBImage], [], Window.Width, Window.Height);
  end else
    glFreeDisplayList(GLList_LoadingBgImage);

  if not DOMGetSingleAttribute(Element, 'loading_bar_y_position',
    LoadingBarYPosition) then
    LoadingBarYPosition := DefaultBarYPosition;

  LoadRequiredResources(Element, RequiredCreatures);
end;

procedure DrawCreateLevel(Window: TCastleWindowBase);
begin
  glClear(GL_COLOR_BUFFER_BIT);
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glCallList(TLevelAvailable(Window.UserData).GLList_LoadingBgImage);
  glPopAttrib;
end;

function TLevelAvailable.CreateLevel(MenuBackground: boolean): TLevel;

  procedure CreateLevelCore;
  begin
    Result := LevelClass.Create(Name, SceneFileName,
      Title, TitleHint, Number, LevelDOMElement, RequiredCreatures, 
      MenuBackground);
    if not MenuBackground then
      AvailableForNewGame := true;
  end;

  procedure CreateLevelNoBackground;
  var
    SavedBarYPosition: Single;
  begin
    SavedBarYPosition := WindowProgressInterface.BarYPosition;
    WindowProgressInterface.BarYPosition := LoadingBarYPosition;
    try
      CreateLevelCore;
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
      @DrawCreateLevel, @Resize2D, @NoClose);
    try
      Window.UserData := Self;

      Window.OnDrawStyle := ds3D;

      Window.EventResize;

      CreateLevelNoBackground;

      { Just a dirty hack to make user to actually see the
        "DOOM E1M1" loading background --- otherwise loading of doom
        level goes so fast that it's practically not noticeable.
        Maybe in the future I'll remove this hack (if level loading
        will be slow enough to actually make time for user to see
        background image), or make this hack more intelligent
        (like checking how much time was spent inside CreateLevelNoBackground
        and doing Sleep() only if necessary). }
      SleepIfNeeded;

    finally FreeAndNil(SavedMode) end;
  end else
    CreateLevelNoBackground;
end;

{ TLevelAvailableList ------------------------------------------------------- }

destructor TLevelAvailableList.Destroy;
begin
  SysUtils.FreeAndNil(Document);
  inherited;
end;

function TLevelAvailableList.FindName(const AName: string): TLevelAvailable;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
      Exit(Items[I]);

  raise Exception.CreateFmt(
    'Level "%s" not found on LevelsAvailable list', [AName]);
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
      'levels_available/' + Items[I].Name,
      Items[I].DefaultAvailableForNewGame);
end;

procedure TLevelAvailableList.SaveToConfig;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ConfigFile.SetDeleteValue(
      'levels_available/' + Items[I].Name,
      Items[I].AvailableForNewGame,
      Items[I].DefaultAvailableForNewGame);
end;

procedure TLevelAvailableList.LoadFromFile;
var
  LevelsList: TDOMNodeList;
  LevelNode: TDOMNode;
  I: Integer;
  NewLevelAvailable: TLevelAvailable;
  BasePath: string;
begin
  SysUtils.FreeAndNil(Document);

  BasePath := ProgramDataPath + 'data' +  PathDelim + 'levels' + PathDelim;
  ReadXMLFile(Document, BasePath + 'index.xml');

  Check(Document.DocumentElement.TagName = 'levels',
    'Root node of levels/index.xml must be <levels>');

  Check(DOMGetAttribute(Document.DocumentElement, 'background_level_name',
    FMenuBackgroundLevelName), '<levels> must have attribute "background_level_name"');

  LevelsList := Document.DocumentElement.ChildNodes;
  try
    for I := 0 to LevelsList.Count - 1 do
    begin
      LevelNode := LevelsList.Item[I];
      if LevelNode.NodeType = ELEMENT_NODE then
      begin
        NewLevelAvailable := TLevelAvailable.Create;
        Add(NewLevelAvailable);
        NewLevelAvailable.AvailableForNewGame := false;
        NewLevelAvailable.LoadFromDOMElement(LevelNode as TDOMElement,
          BasePath);
      end;
    end;
  finally FreeChildNodes(LevelsList); end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowOpen(Window: TCastleWindowBase);
begin
  { Do this now (not at initialization), because loading available
    levels requires knowledge of Glw window sizes (because LoadingBg
    image is scaled to this size). }
  LevelsAvailable.LoadFromFile;
  LevelsAvailable.LoadFromConfig;
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
  { Call CloseGLW in case OnClose event would happen after finalization
    of this unit. }
  WindowClose(Window);
end.