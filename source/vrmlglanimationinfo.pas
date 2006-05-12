{
  Copyright 2006 Michalis Kamburelis.

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
}

{ }
unit VRMLGLAnimationInfo;

interface

uses VRMLFlatSceneGL, VRMLGLAnimation, VRMLOpenGLRenderer;

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
  public
    constructor Create(
      const AModelFileNames: array of string;
      const ATimes: array of Single;
      AScenesPerTime: Cardinal;
      AOptimization: TGLRendererOptimization;
      ATimeLoop, ATimeBackwards: boolean;
      ACache: TVRMLOpenGLRendererContextCache = nil);

    { Remember that you're responsible for returned TVRMLGLAnimation
      instance after calling this function. }
    function CreateAnimation: TVRMLGLAnimation;
  end;

implementation

uses VRMLNodes, Object3dAsVRML;

constructor TVRMLGLAnimationInfo.Create(
  const AModelFileNames: array of string;
  const ATimes: array of Single;
  AScenesPerTime: Cardinal;
  AOptimization: TGLRendererOptimization;
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
  FTimeLoop := ATimeLoop;
  FTimeBackwards := ATimeBackwards;
  FCache := ACache;
end;

function TVRMLGLAnimationInfo.CreateAnimation: TVRMLGLAnimation;
var
  RootNodes: array of TVRMLNode;
  I: Integer;
begin
  SetLength(RootNodes, Length(FModelFileNames));
  for I := 0 to High(RootNodes) do
    RootNodes[I] := LoadAsVRML(FModelFileNames[I], false);

  Result := TVRMLGLAnimation.Create(RootNodes, FTimes,
    FScenesPerTime, FOptimization, FTimeLoop, FTimeBackwards, FCache);
end;

end.