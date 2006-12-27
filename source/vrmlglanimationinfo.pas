{
  Copyright 2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ }
unit VRMLGLAnimationInfo;

interface

uses Classes, VRMLFlatSceneGL, VRMLGLAnimation, VRMLOpenGLRenderer;

type
  { Simple class to postpone loading vrml files that will be used
    for TVRMLGLAnimation, and actually creating TVRMLGLAnimation instances.
    Why ? Because loading all these files can be quite time-consuming. }
  TVRMLGLAnimationInfo = class
  private
    FModelFileNames: array of string;
    FTimes: array of Single;
    FScenesPerTime: Cardinal;
    FOptimization: TGLRendererOptimization;
    FTimeLoop, FTimeBackwards: boolean;
    FCache: TVRMLOpenGLRendererContextCache;
    FEqualityEpsilon: Single;

    function GetModelFileNames(I: Cardinal): string;
  public
    constructor Create(
      const AModelFileNames: array of string;
      const ATimes: array of Single;
      AScenesPerTime: Cardinal;
      AOptimization: TGLRendererOptimization;
      const AEqualityEpsilon: Single;
      ATimeLoop, ATimeBackwards: boolean;
      ACache: TVRMLOpenGLRendererContextCache = nil);

    { Constructor that loads animation settings from a *.kanim file.
      File format is described in ../../doc/kanim_format.txt file. }
    {constructor CreateFromFile(const FileName: string;
      ACache: TVRMLOpenGLRendererContextCache = nil);}

    property ModelFileNames[I: Cardinal]: string read GetModelFileNames;
    function ModelFileNamesCount: Cardinal;

    { These properties are just initialized in our constructor
      and then used when calling CreateAnimation.
      You can freely change them after creation.

      In particular, this way you can change whatever setting was read
      from *.kanim file if you used CreateFromFile.

      @groupBegin }
    property ScenesPerTime: Cardinal read FScenesPerTime write FScenesPerTime;
    property Optimization: TGLRendererOptimization
      read FOptimization write FOptimization;
    property EqualityEpsilon: Single read FEqualityEpsilon write FEqualityEpsilon;
    property TimeLoop: boolean read FTimeLoop write FTimeLoop;
    property TimeBackwards: boolean read FTimeBackwards write FTimeBackwards;
    property Cache: TVRMLOpenGLRendererContextCache
      read FCache write FCache;
    { @groupEnd }

    { Remember that you're responsible for returned TVRMLGLAnimation
      instance after calling this function.

      @param(FirstRootNodesPool
        If non-nil, this can be used to slightly reduce animation loading
        time and memory use:

        We check whether we have AModelFileNames[0] among FirstRootNodesPool.
        If yes, then we will reuse this FirstRootNodesPool.Objects[]
        (and create animation with OwnsFirstRootNode = false).

        This allows you to prepare some commonly used root nodes and put
        them in FirstRootNodesPool before calling
        TVRMLGLAnimationInfo.CreateAnimation. This way loading time
        will be faster, and also display lists sharing may be greater,
        as the same RootNode may be shared by a couple of TVRMLGLAnimation
        instances.

        Be sure to remove these nodes (free them and remove them from
        FirstRootNodesPool) after you're sure that all animations that
        used them were destroyed.) }
    function CreateAnimation(FirstRootNodesPool: TStringList): TVRMLGLAnimation;
  end;

implementation

uses SysUtils, VRMLNodes, Object3dAsVRML;

constructor TVRMLGLAnimationInfo.Create(
  const AModelFileNames: array of string;
  const ATimes: array of Single;
  AScenesPerTime: Cardinal;
  AOptimization: TGLRendererOptimization;
  const AEqualityEpsilon: Single;
  ATimeLoop, ATimeBackwards: boolean;
  ACache: TVRMLOpenGLRendererContextCache);
var
  I: Integer;
begin
  inherited Create;

  { FModelFileNames := AModelFileNames; }
  SetLength(FModelFileNames, High(AModelFileNames) + 1);
  for I := 0 to High(FModelFileNames) do
    FModelFileNames[I] := AModelFileNames[I];

  { FTimes := ATimes; }
  SetLength(FTimes, High(ATimes) + 1);
  for I := 0 to High(FTimes) do
    FTimes[I] := ATimes[I];

  FScenesPerTime := AScenesPerTime;
  FOptimization := AOptimization;
  FEqualityEpsilon := AEqualityEpsilon;
  FTimeLoop := ATimeLoop;
  FTimeBackwards := ATimeBackwards;
  FCache := ACache;
end;

function TVRMLGLAnimationInfo.CreateAnimation(
  FirstRootNodesPool: TStringList): TVRMLGLAnimation;
var
  RootNodes: array of TVRMLNode;
  I: Integer;
  FirstRootNodeIndex: Integer;
  OwnsFirstRootNode: boolean;
begin
  SetLength(RootNodes, Length(FModelFileNames));

  { $define DEBUG_ANIMATION_LOADING}

  {$ifdef DEBUG_ANIMATION_LOADING}
  Writeln('Loading animation:');
  for I := 0 to High(RootNodes) do
    Writeln('  RootNodes[I] from "', FModelFileNames[I], '"');
  {$endif}

  if FirstRootNodesPool <> nil then
    FirstRootNodeIndex := FirstRootNodesPool.IndexOf(FModelFileNames[0]) else
    FirstRootNodeIndex := -1;
  OwnsFirstRootNode := FirstRootNodeIndex = -1;
  if not OwnsFirstRootNode then
    RootNodes[0] := FirstRootNodesPool.Objects[FirstRootNodeIndex] as TVRMLNode else
    RootNodes[0] := LoadAsVRML(FModelFileNames[0], false);

  for I := 1 to High(RootNodes) do
    RootNodes[I] := LoadAsVRML(FModelFileNames[I], false);

  Result := TVRMLGLAnimation.Create(RootNodes, OwnsFirstRootNode, FTimes,
    FScenesPerTime, FOptimization, FEqualityEpsilon,
    FTimeLoop, FTimeBackwards, FCache);
end;

function TVRMLGLAnimationInfo.GetModelFileNames(I: Cardinal): string;
begin
  Result := FModelFileNames[I];
end;

function TVRMLGLAnimationInfo.ModelFileNamesCount: Cardinal;
begin
  Result := High(FModelFileNames) + 1;
end;

end.