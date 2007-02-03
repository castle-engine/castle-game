{
  Copyright 2006,2007 Michalis Kamburelis.

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

{$apptype GUI}

program castle;

uses GLWindow, SysUtils, KambiUtils, ProgressUnit, ProgressGL, OpenAL, ALUtils,
  Classes, ParseParametersUnit, GLWinMessages, KambiGLUtils,
  CastleWindow, CastleStartMenu, CastleLevel, CastleHelp, CastleSound,
  KambiClassUtils, CastleVideoOptions, CastleInitialBackground,
  CastleCreatures, CastleObjectKinds, CastlePlay, CastleLog,
  CastleLevelSpecific, CastleLevelAvailable;

{ parsing parameters --------------------------------------------------------- }

var
  WasParam_NoSound: boolean = false;
  WasParam_NoScreenChange: boolean = false;

const
  Options: array[0..6]of TOption =
  ( (Short:'h'; Long: 'help'; Argument: oaNone),
    (Short: #0; Long: 'no-sound'; Argument: oaNone),
    (Short:'v'; Long: 'version'; Argument: oaNone),
    (Short:'n'; Long: 'no-screen-change'; Argument: oaNone),
    (Short: #0; Long: 'no-shadows'; Argument: oaNone),
    (Short: #0; Long: 'debug-no-creatures'; Argument: oaNone),
    (Short: #0; Long: 'debug-log'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         InfoWrite(
           SCastleVersionWWW +nl+
           nl+
           'Options:' +nl+
           HelpOptionHelp +nl+
           VersionOptionHelp +nl+
           OpenALOptionsHelp(true) +nl+
           '  --no-sound            Turn off sound' +nl+
           '  -n / --no-screen-resize' +nl+
           '                        Do not try to resize the screen.' +nl+
           '                        If your screen size is not ' +
             RequiredScreenSize +nl+
           '                        then will run in windowed mode.' +nl+
           '  --no-shadows          Disable initializing and using shadows.' +nl+
           nl+
           Glw.ParseParametersHelp([poDisplay], true) +nl+
           nl+
           'Debug options (don''t use unless you know what you''re doing):' +nl+
           '  --debug-log           Write various log info on stdout' +nl+
           '  --debug-no-creatures  Disable loading creatures animations');
         ProgramBreak;
       end;
    1: WasParam_NoSound := true;
    2: begin
         WritelnStr(Version);
         ProgramBreak;
       end;
    3: WasParam_NoScreenChange := true;
    4: RenderShadowsPossible := false;
    5: WasParam_DebugNoCreatures := true;
    6: InitializeLog;
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ This will be removed from here to XML data file. }
procedure FillLevelsAvailable;
var
  LevelAvailable: TLevelAvailable;
begin
  LevelAvailable := TLevelAvailable.Create;
  LevelsAvailable.Add(LevelAvailable);
  LevelAvailable.Name := 'gate';
  LevelAvailable.LevelClass := TGateLevel;
  LevelAvailable.DefaultAvailableForNewGame := true;
  LevelAvailable.SceneFileName := CastleLevelsPath + 'gate' + PathDelim + 'gate_final.wrl';
  LevelAvailable.LightSetFileName := CastleLevelsPath + 'gate' + PathDelim + 'gate_lights.wrl';
  LevelAvailable.Title := 'The Gate';
  LevelAvailable.Number := 1;

  LevelAvailable := TLevelAvailable.Create;
  LevelsAvailable.Add(LevelAvailable);
  LevelAvailable.Name := 'castle_hall';
  LevelAvailable.LevelClass := TCastleHallLevel;
  LevelAvailable.DefaultAvailableForNewGame := false;
  LevelAvailable.SceneFileName := CastleLevelsPath + 'castle_hall' + PathDelim + 'castle_hall_final.wrl';
  LevelAvailable.LightSetFileName := CastleLevelsPath + 'castle_hall' + PathDelim + 'castle_hall_lights.wrl';
  LevelAvailable.Title := 'Castle Hall';
  LevelAvailable.Number := 2;

  LevelAvailable := TLevelAvailable.Create;
  LevelsAvailable.Add(LevelAvailable);
  LevelAvailable.Name := 'cages';
  LevelAvailable.LevelClass := TCagesLevel;
  LevelAvailable.DefaultAvailableForNewGame := false;
  LevelAvailable.SceneFileName := CastleLevelsPath + 'cages' + PathDelim + 'cages_final.wrl';
  LevelAvailable.LightSetFileName := CastleLevelsPath + 'cages' + PathDelim + 'cages_lights.wrl';
  LevelAvailable.Title := 'Cages';
  LevelAvailable.Number := 3;

  LevelAvailable := TLevelAvailable.Create;
  LevelsAvailable.Add(LevelAvailable);
  LevelAvailable.Name := 'tower';
  LevelAvailable.LevelClass := TTowerLevel;
  LevelAvailable.DefaultAvailableForNewGame := false;
  LevelAvailable.SceneFileName := CastleLevelsPath + 'tower' + PathDelim + 'basic_castle_final.wrl';
  LevelAvailable.LightSetFileName := CastleLevelsPath + 'tower' + PathDelim + 'basic_castle_lights.wrl';
  LevelAvailable.Title := 'Tower';
  LevelAvailable.Number := 99;

  LevelAvailable := TLevelAvailable.Create;
  LevelsAvailable.Add(LevelAvailable);
  LevelAvailable.Name := 'doom_e1m1';
  LevelAvailable.LevelClass := TDoomE1M1Level;
  LevelAvailable.DefaultAvailableForNewGame := false;
  LevelAvailable.SceneFileName := CastleLevelsPath + 'doom' + PathDelim + 'e1m1' + PathDelim + 'e1m1_final.wrl';
  LevelAvailable.LightSetFileName := CastleLevelsPath + 'doom' + PathDelim + 'e1m1' + PathDelim + 'e1m1_lights.wrl';
  LevelAvailable.Title := 'Doom E1M1';
  LevelAvailable.Number := 90;
end;

{ main -------------------------------------------------------------------- }

begin
  { TODO: This should be done in GLWindow when using IdleCompSpeed,
    no time to write it cleaner now. }
  Check( PerfTimerInit, 'performance timer not supported on this hardware');

  { parse parameters }
  OpenALOptionsParse;
  Glw.ParseParameters([poDisplay]);
  ParseParameters(Options, @OptionProc, nil);

  Glw.Width := RequiredScreenWidth;
  Glw.Height := RequiredScreenHeight;
  Glw.ColorBits := ColorDepthBits;
  if WasParam_NoScreenChange or (not AllowScreenChange) then
  begin
    Glw.FullScreen :=
      (Glwm.ScreenWidth = RequiredScreenWidth) and
      (Glwm.ScreenHeight = RequiredScreenHeight);
  end else
  begin
    Glw.FullScreen := true;
    if (Glwm.ScreenWidth <> RequiredScreenWidth) or
       (Glwm.ScreenHeight <> RequiredScreenHeight) or
       (VideoFrequency <> 0) or
       (ColorDepthBits <> 0) then
    begin
      Glwm.VideoColorBits := ColorDepthBits;
      Glwm.VideoFrequency := VideoFrequency;
      Glwm.VideoResize := true;
      Glwm.VideoResizeWidth := RequiredScreenWidth;
      Glwm.VideoResizeHeight := RequiredScreenHeight;

      if not Glwm.TryVideoChange then
      begin
        WarningWrite('Can''t change display settings to: ' +nl+
          Glwm.VideoSettingsDescribe +
          nl+
          'I will set "Allow screen settings change on startup" option to "No". ' +
          'You may want to review settings in "Video options" menu and then ' +
          'set "Allow screen settings change on startup" back to "Yes".' +nl+
          nl+
          'Now I will just continue with default system settings. ');
        Glw.FullScreen :=
          (Glwm.ScreenWidth = RequiredScreenWidth) and
          (Glwm.ScreenHeight = RequiredScreenHeight);
        AllowScreenChange := false;
      end;
    end;
  end;

  { init glwindow }
  Glw.Caption := 'The Castle';
  Glw.ResizeAllowed := raOnlyAtInit;
  if RenderShadowsPossible then
    Glw.StencilBufferBits := 8;
  Glw.Init;
  if WasParam_DebugLog then
    WritelnLog(ltOpenGLInitialization, GLCapsString);

  { init progress }
  ProgressGLInterface.Window := Glw;
  Progress.UserInterface := ProgressGLInterface;
  { I'm turning UseDescribePosition to false, because it's usually
    confusing for the user.
    E.g. each creature is counted as PrepareRenderSteps steps,
    each item is counted as PrepareRenderSteps steps,
    when loading levels user would have to know what an "octree" is. }
  Progress.UseDescribePosition := false;

  { See CastleLevel initialization comments for why it's called here. }
  FillLevelsAvailable;
  LevelsAvailable.LoadFromConfig;

  { init OpenAL (after initing Glw and Progress, because ALContextInit
    wants to display progress of "Loading sounds") }
  DrawInitialBackground;
  ALContextInit(WasParam_NoSound);
  try
    ShowStartMenu(Glw.OnDraw);
  finally
    { Usually Glw.Closed = false here.
      But this is finally...end clause so we try hard to avoid raising
      another exception here --- so we safeguard and eventually change
      Progress.UserInterface here. }
    if Glw.Closed then
      Progress.UserInterface := ProgressNullInterface;

    ALContextClose;
  end;
end.

{
  For now: do
    make clean
  always before compiling, to avoid FPC 2.0.4 getting lost in the dependencies.
  This may cause Internal Error 200310221,
  but it can also cause some unit to not get
  recompiled when they should... It's a good thing that FPC is so goddamn
  fast that this workaround doesn't hurt us so much.

  Local Variables:
  compile-command: "make clean; fpcdebug castle.dpr --exe-output-dir ../"
  kam-compile-release-command-win32: "clean_glwindow_unit; make clean; fpcrelease --exe-output-dir ../"
  kam-compile-release-command-unix: "clean_glwindow_unit; make clean; fpcrelease -dGLWINDOW_XLIB --exe-output-dir ../"
  End:
}