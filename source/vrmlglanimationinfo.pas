{
  Copyright 2006-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Castle Game Engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ }
unit VRMLGLAnimationInfo;

interface

uses Classes, VRMLGLAnimation, VRMLGLRenderer, CastleUtils, CastleStringUtils, DOM;

type
  { Simple class to postpone loading vrml files that will be used
    for T3DPrecalculatedAnimation, and actually creating T3DPrecalculatedAnimation instances.
    Why ? Because loading all these files can be quite time-consuming. }
  T3DPrecalculatedAnimationInfo = class
  private
    FModelFileNames: TKamStringList;
    FTimes: TSingleList;
    FScenesPerTime: Cardinal;
    FTimeLoop, FTimeBackwards: boolean;
    FCache: TVRMLGLRendererContextCache;
    FEqualityEpsilon: Single;
  public
    constructor Create(
      const AModelFileNames: array of string;
      const ATimes: array of Single;
      AScenesPerTime: Cardinal;
      const AEqualityEpsilon: Single;
      ATimeLoop, ATimeBackwards: boolean;
      ACache: TVRMLGLRendererContextCache);

    { Constructor that loads animation settings from a *.kanim file.
      File format is described in ../../doc/kanim_format.txt file. }
    constructor CreateFromFile(const FileName: string;
      ACache: TVRMLGLRendererContextCache);

    { Constructor that loads animation settings from DOM node representing
      *.kanim file.
      File format is described in ../../doc/kanim_format.txt file.
      @seealso T3DPrecalculatedAnimation.LoadFromDOMElement. }
    constructor CreateFromDOMElement(Element: TDOMElement;
      const BasePath: string;
      ACache: TVRMLGLRendererContextCache = nil);

    destructor Destroy; override;

    { These properties are just initialized in our constructor
      and then used when calling CreateAnimation.
      You can freely change them after creation.

      In particular, this way you can change whatever setting was read
      from *.kanim file if you used CreateFromFile.

      @groupBegin }
    property ModelFileNames: TKamStringList read FModelFileNames;
    property Times: TSingleList read FTimes;

    property ScenesPerTime: Cardinal read FScenesPerTime write FScenesPerTime;
    property EqualityEpsilon: Single read FEqualityEpsilon write FEqualityEpsilon;
    property TimeLoop: boolean read FTimeLoop write FTimeLoop;
    property TimeBackwards: boolean read FTimeBackwards write FTimeBackwards;
    property Cache: TVRMLGLRendererContextCache
      read FCache write FCache;
    { @groupEnd }

    { Remember that you're responsible for returned T3DPrecalculatedAnimation
      instance after calling this function.

      @param(FirstRootNodesPool
        If non-nil, this can be used to slightly reduce animation loading
        time and memory use:

        We check whether we have AModelFileNames[0] among FirstRootNodesPool.
        If yes, then we will reuse this FirstRootNodesPool.Objects[]
        (and create animation with OwnsFirstRootNode = false).

        This allows you to prepare some commonly used root nodes and put
        them in FirstRootNodesPool before calling
        T3DPrecalculatedAnimationInfo.CreateAnimation. This way loading time
        will be faster, and also display lists sharing may be greater,
        as the same RootNode may be shared by a couple of T3DPrecalculatedAnimation
        instances.

        Be sure to remove these nodes (free them and remove them from
        FirstRootNodesPool) after you're sure that all animations that
        used them were destroyed.) }
    function CreateAnimation(FirstRootNodesPool: TStringList): T3DPrecalculatedAnimation;
  end;

implementation

uses SysUtils, VRMLNodes, X3DLoad;

constructor T3DPrecalculatedAnimationInfo.Create(
  const AModelFileNames: array of string;
  const ATimes: array of Single;
  AScenesPerTime: Cardinal;
  const AEqualityEpsilon: Single;
  ATimeLoop, ATimeBackwards: boolean;
  ACache: TVRMLGLRendererContextCache);
begin
  inherited Create;

  FModelFileNames := TKamStringList.Create;
  FTimes := TSingleList.Create;

  FModelFileNames.AddArray(AModelFileNames);
  FTimes.AddArray(ATimes);

  FScenesPerTime := AScenesPerTime;
  FEqualityEpsilon := AEqualityEpsilon;
  FTimeLoop := ATimeLoop;
  FTimeBackwards := ATimeBackwards;

  FCache := ACache;
end;

constructor T3DPrecalculatedAnimationInfo.CreateFromFile(const FileName: string;
  ACache: TVRMLGLRendererContextCache);
begin
  inherited Create;

  FModelFileNames := TKamStringList.Create;
  FTimes := TSingleList.Create;

  T3DPrecalculatedAnimation.LoadFromFileToVars(FileName,
    FModelFileNames, FTimes, FScenesPerTime,
    FEqualityEpsilon, FTimeLoop, FTimeBackwards);

  FCache := ACache;
end;

constructor T3DPrecalculatedAnimationInfo.CreateFromDOMElement(Element: TDOMElement;
  const BasePath: string;
  ACache: TVRMLGLRendererContextCache);
begin
  inherited Create;

  FModelFileNames := TKamStringList.Create;
  FTimes := TSingleList.Create;

  T3DPrecalculatedAnimation.LoadFromDOMElementToVars(Element, BasePath,
    FModelFileNames, FTimes, FScenesPerTime,
    FEqualityEpsilon, FTimeLoop, FTimeBackwards);

  FCache := ACache;
end;

destructor T3DPrecalculatedAnimationInfo.Destroy;
begin
  FreeAndNil(FModelFileNames);
  FreeAndNil(FTimes);
  inherited;
end;

function T3DPrecalculatedAnimationInfo.CreateAnimation(
  FirstRootNodesPool: TStringList): T3DPrecalculatedAnimation;
var
  RootNodes: TX3DNodeList;
  I: Integer;
  FirstRootNodeIndex: Integer;
  OwnsFirstRootNode: boolean;
begin
  RootNodes := TX3DNodeList.Create(false);
  try
    RootNodes.Count := FModelFileNames.Count;

    { $define DEBUG_ANIMATION_LOADING}

    {$ifdef DEBUG_ANIMATION_LOADING}
    Writeln('Loading animation:');
    for I := 0 to RootNodes.High do
      Writeln('  RootNodes[I] from "', FModelFileNames[I], '"');
    {$endif}

    if FirstRootNodesPool <> nil then
      FirstRootNodeIndex := FirstRootNodesPool.IndexOf(FModelFileNames[0]) else
      FirstRootNodeIndex := -1;
    OwnsFirstRootNode := FirstRootNodeIndex = -1;
    if not OwnsFirstRootNode then
      RootNodes[0] := FirstRootNodesPool.Objects[FirstRootNodeIndex] as TX3DNode else
      RootNodes[0] := LoadVRML(FModelFileNames[0], false);

    for I := 1 to RootNodes.Count - 1 do
      RootNodes[I] := LoadVRML(FModelFileNames[I], false);

    Result := T3DPrecalculatedAnimation.CreateCustomCache(nil, FCache);
    Result.Load(RootNodes, OwnsFirstRootNode, FTimes,
      FScenesPerTime, FEqualityEpsilon);
    Result.TimeLoop := FTimeLoop;
    Result.TimeBackwards := FTimeBackwards;

  finally
    FreeAndNil(RootNodes)
  end;
end;

end.